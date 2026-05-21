# =========================================================================
# run_analysis.R
# -------------------------------------------------------------------------
# Полный вычислительный пайплайн стилометрического анализа.
# Запускается один раз вручную и сохраняет все промежуточные объекты
# в data/cache/*.rds. После этого отчёт index.qmd подгружает готовые
# результаты и собирается за секунды, без повторного пересчёта.
#
# Как запускать:
#   1. Положить данные в папку data/:
#        data/A_Small_Collection_of_British_Fiction.zip
#        data/overview.tsv
#   2. В R-консоли (рабочая директория = папка проекта):
#        source("run_analysis.R")
#   3. После завершения в data/cache/ появятся .rds-файлы.
# =========================================================================

set.seed(2025)
options(scipen = 999)

# -------------------- 1. Пакеты --------------------
needed <- c(
  "dplyr", "tidyr", "readr", "purrr", "stringr", "tibble", "forcats", "ggplot2",
  "tidymodels", "glmnet", "ranger", "vip",
  "quanteda", "quanteda.textstats", "textrecipes",
  "patchwork", "ggrepel", "knitr", "kableExtra"
)
for (pkg in needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(dplyr); library(tidyr); library(readr); library(purrr)
library(stringr); library(tibble); library(forcats); library(ggplot2)
library(tidymodels)
library(quanteda); library(quanteda.textstats); library(textrecipes)

tidymodels_prefer()
# stringr::fixed конфликтует с recipes::fixed — для подсчёта пунктуации
# нужен именно stringr.
if (requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflicts_prefer(stringr::fixed, .quiet = TRUE)
}

CACHE_DIR <- "data/cache"
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# Небольшая утилита: сохранить объект в .rds и сообщить размер.
save_obj <- \(obj, name) {
  path <- file.path(CACHE_DIR, paste0(name, ".rds"))
  saveRDS(obj, path)
  message(sprintf("Сохранено: %s (%.1f КБ)", path, file.size(path) / 1024))
  invisible(obj)
}

# -------------------- 2. Распаковка корпуса --------------------
zip_file      <- "data/A_Small_Collection_of_British_Fiction.zip"
overview_path <- "data/overview.tsv"
stopifnot(file.exists(zip_file), file.exists(overview_path))

if (!dir.exists("data/A_Small_Collection_of_British_Fiction")) {
  unzip(zip_file, exdir = "data")
}

# Папка с текстами ищется устойчиво: либо подпапка "corpus",
# либо первая папка, где лежит не меньше 20 .txt-файлов.
all_dirs   <- list.dirs("data", recursive = TRUE)
corpus_dir <- all_dirs[basename(all_dirs) == "corpus"]
if (length(corpus_dir) == 0) {
  tc <- sapply(all_dirs, \(d) length(list.files(d, "\\.txt$")))
  corpus_dir <- all_dirs[which(tc >= 20)]
}
corpus_dir <- head(corpus_dir, 1)
stopifnot(length(corpus_dir) == 1, dir.exists(corpus_dir))

# -------------------- 3. Метаданные --------------------
# В overview.tsv заголовок содержит 6 имён, а строки данных — 7 полей
# (первое поле "textID author" разделено пробелом). Разбираю это явно.
message("Парсинг overview.tsv…")
raw_lines <- readLines(overview_path, encoding = "UTF-8")
header_cells <- strsplit(raw_lines[1], "\t")[[1]]
header_cells <- c("textID", "author", header_cells[-1])

parse_row <- \(line) {
  cells <- strsplit(line, "\t")[[1]]
  first <- sub("^(\\S+)\\s+(.*)$", "\\1\t\\2", cells[1])
  c(strsplit(first, "\t")[[1]], cells[-1])
}
body <- do.call(rbind, lapply(raw_lines[-1], parse_row))
colnames(body) <- header_cells

meta <- as_tibble(body) |>
  mutate(
    textID        = as.integer(textID),
    `1stPubl`     = as.integer(`1stPubl`),
    author_gender = as.integer(author_gender),
    author        = str_trim(author),
    authorID      = str_trim(authorID),
    title         = str_trim(title)
  )
save_obj(meta, "meta")

# -------------------- 4. Сопоставление файлов и метаданных --------------------
# Файлы названы по схеме <Surname>_<Title>.txt — связываю фамилию с authorID.
fname_lookup <- tribble(
  ~surname,     ~authorID,
  "Austen",     "JA",
  "ABronte",    "AB",
  "CBronte",    "CB",
  "EBronte",    "EB",
  "Dickens",    "CD",
  "Eliot",      "GE",
  "Fielding",   "HF",
  "Richardson", "SR",
  "Sterne",     "LS",
  "Trollope",   "AT",
  "Thackeray",  "WT"
)

files_df <- tibble(path = list.files(corpus_dir, "\\.txt$", full.names = TRUE)) |>
  mutate(
    file       = basename(path),
    surname    = str_extract(file, "^[A-Za-z]+"),
    title_key  = str_remove(file, "^[A-Za-z]+_") |> str_remove("\\.txt$")
  ) |>
  left_join(fname_lookup, by = "surname") |>
  left_join(meta |> select(authorID, author),
            by = "authorID", relationship = "many-to-many") |>
  distinct(file, .keep_all = TRUE)
save_obj(files_df, "files_df")

# -------------------- 5. Чтение и очистка текстов --------------------
message("Чтение и очистка 27 текстов…")
clean_one <- \(path) {
  raw <- readr::read_file(path)

  # Концевые маркеры удаляю только в самом конце текста — иначе можно
  # случайно зацепить упоминание "the end" внутри прозы.
  tail_chunk <- substr(raw, max(1, nchar(raw) - 500), nchar(raw))
  m <- regexpr("\\b(FINIS|THE END|End of the (Fourth|Fifth|Sixth|Project Gutenberg).*)\\b",
               tail_chunk, ignore.case = TRUE)
  if (m > 0) {
    cut_at <- nchar(raw) - 500 + m
    raw <- substr(raw, 1, cut_at - 1)
  }

  # Первые короткие строки шапки (заглавие, "BY", "VOLUME I", "CHAPTER I")
  # отбрасываю, пока встречаются короткие/прописные/служебные.
  lines <- strsplit(raw, "\r?\n")[[1]]
  header_re <- "^\\s*(by|volume|chapter|book|preface|to the|dedication|a work by|nine volumes|one of|history)\\b"
  body_start <- 1L
  for (i in seq_len(min(25, length(lines)))) {
    L <- str_trim(lines[i])
    if (L == "") next
    if (nchar(L) < 40 && (toupper(L) == L || grepl(header_re, L, ignore.case = TRUE))) {
      body_start <- i + 1L; next
    }
    break
  }
  paste(lines[body_start:length(lines)], collapse = "\n")
}

texts <- files_df |>
  mutate(text    = map_chr(path, clean_one),
         text_lc = str_replace_all(tolower(text), "_", " "))
save_obj(texts, "texts")

# -------------------- 6. Чанки --------------------
# Разрезаю каждую книгу на чанки по 5000 слов. Чанк всегда отслеживается
# до своей книги — это нужно для честной групповой кросс-валидации.
message("Токенизация и чанкование…")

CHUNK_SIZE <- 5000
MIN_CHUNK  <- 4500

corp <- corpus(texts, docid_field = "file", text_field = "text_lc")
docvars(corp, "book_id") <- docnames(corp)

toks_full   <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE,
                      remove_symbols = TRUE)
toks_chunks <- toks_full |>
  tokens_chunk(size = CHUNK_SIZE, use_docvars = TRUE)
toks_chunks <- tokens_subset(toks_chunks, ntoken(toks_chunks) >= MIN_CHUNK)

chunk_meta <- tibble(
  doc_id   = docnames(toks_chunks),
  book_id  = docvars(toks_chunks, "book_id"),
  authorID = docvars(toks_chunks, "authorID"),
  author   = docvars(toks_chunks, "author"),
  n_tokens = ntoken(toks_chunks)
)

# В .rds кладу только лёгкий chunk_meta. Сам объект toks_chunks весит
# сотни МБ и нужен лишь дальше в этом же скрипте — на диск его не пишу.
save_obj(list(chunk_meta = chunk_meta), "chunking")

message(sprintf("  получено %d чанков ≥ %d токенов.", nrow(chunk_meta), MIN_CHUNK))

# -------------------- 7. MFW + структурные признаки --------------------
message("Расчёт признаков (MFW и структурные)…")

MFW_N <- 200

# MFW: относительная частота 200 самых частотных слов корпуса на чанк.
dfm_full  <- dfm(toks_chunks)
top_terms <- names(topfeatures(dfm_full, n = MFW_N))
dfm_mfw   <- dfm_full |>
  dfm_select(pattern = top_terms, selection = "keep") |>
  dfm_weight(scheme = "prop")
mfw_df <- dfm_mfw |>
  convert(to = "data.frame") |>
  as_tibble() |>
  rename_with(\(x) paste0("mfw_", x), -doc_id)

# Индекс Оноре (R) — мера лексического богатства через долю hapax legomena.
honore_r <- \(tw) {
  N <- length(tw); if (N == 0) return(NA_real_)
  tab <- table(tw); V <- length(tab); V1 <- sum(tab == 1)
  if (V1 == V) return(NA_real_)
  100 * log(N) / (1 - V1 / V)
}

# Структурные признаки считаю на тех же чанках, что и MFW. Для каждого
# чанка достаю оригинальный (с пунктуацией) отрезок текста по индексам
# слов в книге — так длины строго совпадают с MFW-чанками.
chunk_substrings <- character(nrow(chunk_meta))
names(chunk_substrings) <- chunk_meta$doc_id

for (bk in unique(chunk_meta$book_id)) {
  text_raw <- texts$text[texts$file == bk]
  pos <- gregexpr("[A-Za-z][A-Za-z']*", text_raw, perl = TRUE)[[1]]
  match_lens <- attr(pos, "match.length")
  bk_chunks <- filter(chunk_meta, book_id == bk)
  for (i in seq_len(nrow(bk_chunks))) {
    chunk_n <- bk_chunks$n_tokens[i]
    word_start <- (i - 1) * CHUNK_SIZE + 1
    word_end   <- word_start + chunk_n - 1
    if (word_end > length(pos)) word_end <- length(pos)
    char_start <- pos[word_start]
    char_end   <- pos[word_end] + match_lens[word_end] - 1
    chunk_substrings[bk_chunks$doc_id[i]] <- substr(text_raw, char_start, char_end)
  }
}

struct_df <- tibble(
  doc_id        = chunk_meta$doc_id,
  mean_word_len = sapply(as.list(toks_chunks), \(w) mean(nchar(w))),
  ttr           = sapply(as.list(toks_chunks), \(w) length(unique(w)) / length(w)),
  honore_r      = sapply(as.list(toks_chunks), honore_r),
  mean_sent_len = sapply(chunk_substrings[chunk_meta$doc_id], \(t) {
                    n_sent <- max(1, length(strsplit(t, "[.!?]+")[[1]]))
                    n_word <- length(strsplit(t, "\\s+")[[1]])
                    n_word / n_sent
                  }),
  comma_rate    = sapply(chunk_substrings[chunk_meta$doc_id],
                         \(t) 1000 * str_count(t, ",")  / nchar(t)),
  semicolon_rate= sapply(chunk_substrings[chunk_meta$doc_id],
                         \(t) 1000 * str_count(t, ";")  / nchar(t)),
  dash_rate     = sapply(chunk_substrings[chunk_meta$doc_id],
                         \(t) 1000 * str_count(t, "--") / nchar(t)),
  question_rate = sapply(chunk_substrings[chunk_meta$doc_id],
                         \(t) 1000 * str_count(t, stringr::fixed("?")) / nchar(t)),
  excl_rate     = sapply(chunk_substrings[chunk_meta$doc_id],
                         \(t) 1000 * str_count(t, stringr::fixed("!")) / nchar(t))
)

features <- chunk_meta |>
  left_join(mfw_df,    by = "doc_id") |>
  left_join(struct_df, by = "doc_id") |>
  mutate(authorID = factor(authorID))

stopifnot(nrow(features) == nrow(chunk_meta),
          !any(is.na(features$mean_sent_len)))
save_obj(features, "features")

# -------------------- 8. Разбиение на train/test --------------------
message("Разбиение на train/test и подготовка фолдов…")

# Разбиение делаю на уровне книги, а не чанка: для каждого автора
# с двумя и более книгами одна книга уходит в тест. Эмили Бронте
# с её единственным романом остаётся в обучающей выборке.
book_table <- distinct(features, book_id, authorID)
set.seed(2025)
test_books <- book_table |>
  group_by(authorID) |>
  filter(n() >= 2) |>
  slice_sample(n = 1) |>
  pull(book_id)

split_data <- list(
  train      = filter(features, !book_id %in% test_books),
  test       = filter(features,  book_id %in% test_books),
  test_books = test_books
)
save_obj(split_data, "split_data")

# -------------------- 9. Рецепт и две модели --------------------
rec <- recipe(authorID ~ ., data = split_data$train) |>
  update_role(doc_id, book_id, author, n_tokens, new_role = "ID") |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

# Модель 1: мультиномиальная регрессия с регуляризацией (glmnet).
multinom_spec <- multinom_reg(penalty = tune(), mixture = tune()) |>
  set_engine("glmnet") |>
  set_mode("classification")

# Модель 2: случайный лес — нелинейный бейзлайн для сравнения.
rf_spec <- rand_forest(trees = 500, mtry = tune(), min_n = tune()) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

wf_multinom <- workflow() |> add_recipe(rec) |> add_model(multinom_spec)
wf_rf       <- workflow() |> add_recipe(rec) |> add_model(rf_spec)

# Фолды для кросс-валидации — с группировкой по книге, чтобы чанки
# одной книги не попадали одновременно в обучение и проверку.
set.seed(2025)
folds <- group_vfold_cv(split_data$train, group = "book_id", v = 5)

# -------------------- 10. Кросс-валидация (самый долгий этап) --------------------
message("Тюнинг multinomial glmnet (5-fold групповая CV)…")
set.seed(2025)
tune_multinom <- tune_grid(
  wf_multinom,
  resamples = folds,
  grid      = grid_regular(penalty(range = c(-4, 0)),
                            mixture(range = c(0, 1)), levels = c(8, 3)),
  metrics   = metric_set(accuracy, f_meas),
  control   = control_grid(save_pred = TRUE, save_workflow = TRUE)
)
save_obj(tune_multinom, "tune_multinom")

message("Тюнинг Random Forest (5-fold групповая CV)…")
set.seed(2025)
tune_rf <- tune_grid(
  wf_rf,
  resamples = folds,
  grid      = grid_regular(mtry(range = c(5, 50)),
                            min_n(range = c(2, 10)), levels = 4),
  metrics   = metric_set(accuracy, f_meas),
  control   = control_grid(save_pred = TRUE, save_workflow = TRUE)
)
save_obj(tune_rf, "tune_rf")

# -------------------- 11. Финальные модели --------------------
message("Обучение финальных моделей и предсказания на тесте…")
best_multinom <- select_best(tune_multinom, metric = "accuracy")
best_rf       <- select_best(tune_rf,       metric = "accuracy")

final_multinom <- wf_multinom |>
  finalize_workflow(best_multinom) |>
  fit(data = split_data$train)
final_rf <- wf_rf |>
  finalize_workflow(best_rf) |>
  fit(data = split_data$train)

test_preds <- bind_rows(
  augment(final_multinom, split_data$test) |> mutate(model = "multinom"),
  augment(final_rf,       split_data$test) |> mutate(model = "RF")
)

final_fits <- list(
  multinom      = final_multinom,
  rf            = final_rf,
  best_multinom = best_multinom,
  best_rf       = best_rf,
  test_preds    = test_preds
)
save_obj(final_fits, "final_fits")

message("\nГотово. Все объекты сохранены в data/cache/.")
message("Теперь можно открыть index.qmd и нажать Render — отчёт соберётся за секунды.")
