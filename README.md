# Стилометрия британской прозы — Quarto-проект

Атрибуция авторства в корпусе
[A Small Collection of British Fiction](https://github.com/computationalstylistics/A_Small_Collection_of_British_Fiction/)
средствами `{tidymodels}`.

## Структура

```
british-fiction-quarto/
├── _quarto.yml           # конфигурация проекта
├── styles.css            # пользовательские стили
├── run_analysis.R        # ① вычислительный скрипт — запускается ВРУЧНУЮ один раз
├── index.qmd             # ② отчёт — рендерится за секунды
├── data/
│   ├── A_Small_Collection_of_British_Fiction.zip
│   ├── overview.tsv
│   └── cache/            # ← .rds-файлы появляются после прогона run_analysis.R
└── docs/                 # ← собранный HTML появляется здесь после Render
```

## Workflow (два шага)

### Шаг 1. Прогон тяжёлых вычислений

В RStudio откройте `run_analysis.R` и выполните **один раз**:

```r
source("run_analysis.R")
```

Это займёт 5–10 минут. На выходе — 8 файлов в `data/cache/`:
`meta.rds`, `files_df.rds`, `texts.rds`, `chunking.rds`,
`features.rds`, `split_data.rds`, `tune_multinom.rds`, `tune_rf.rds`,
`final_fits.rds`.

### Шаг 2. Render отчёта

Откройте `index.qmd` и нажмите **Render** (`Ctrl/Cmd + Shift + K`).
В HTML попадает **весь** код анализа в виде «листинга», но
**фактически он не выполняется**: чанки помечены `#| eval: false`.
Параллельно в скрытых служебных чанках идёт `readRDS()` готовых
объектов, и они используются для отрисовки таблиц/графиков/метрик.

Render занимает **5–10 секунд**.

## Если нужно что-то пересчитать

Удалите соответствующий `.rds` и снова запустите `run_analysis.R` —
он пересчитает только удалённое (если переписать скрипт под это) или
всё с нуля. Самый простой путь: удалить весь `data/cache/` и
перезапустить `run_analysis.R`.

## Что внутри отчёта

1. Постановка задачи и обоснование chunked-дизайна.
2. Загрузка и парсинг метаданных (формат overview неровный, разбирается явно).
3. Очистка текстов (Project Gutenberg-преамбулы, концевые маркеры).
4. Инжиниринг признаков: 200 MFW + 9 структурных метрик
   (выровненных на одни и те же чанки).
5. EDA: баланс классов, бокс-плоты, PCA по MFW.
6. Моделирование с `{tidymodels}`: multinom (glmnet) vs Random Forest,
   groupwise CV, тюнинг гиперпараметров.
7. Финальная оценка на отложенных книгах + голосование по чанкам.
8. Интерпретация: ROC, importance, коэффициенты glmnet.
9. Выводы с разбором кейса Emily Brontë.
