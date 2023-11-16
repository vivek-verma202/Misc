#' Generate discriptive stats with respect to col_TRT (factor) with 
#' Q1 as the reference group using t test and Fisher's exact test without
#' multiple correction.
#' 
#' df is the original data, df1 is the data proccessed for summary stats

library(tidyverse)
`%+%` <- paste0
formals(table)$useNA <- "ifany"
formals(sort.default)$decreasing <- T
# options(digits = 2, scipen = 999)
# functions ----
# summarize numerics
fun_num <- lst(
  `N (%)` = ~format(sum(!is.na(.)), big.mark = ",", trim = T) %+% " (" %+% 
    round(sum(!is.na(.)) * 100/nrow(df1), 2) %+% "%)",
  `Mean (sd)` = ~round(mean(., na.rm = T),2) %+% " (" %+% 
    round(sd(., na.rm = T),2) %+% ")",
  `Median [min-max]` = ~round(median(., na.rm = T),2) %+% " [" %+% 
    round(min(., na.rm = T),2) %+% "-" %+% 
    format(round(max(., na.rm = T),2), big.mark = ",", trim = T) %+% "]"
)
# summarize factors
fun_fac <- function(x) {
  tab <- table(x)
  return(tibble(subgroup = names(tab),
                `N (%)` = format(tab, big.mark = ",", trim = T) %+% " (" %+% 
                  round(tab * 100/nrow(df1), 2) %+% "%)"))
}
# functions for p-values
# T test
t_test_pval <- possibly(function(data, ref_group, comp_group) {
  data %>%
    filter(col_TRT %in% c(ref_group, comp_group)) %>%
    t.test(value ~ col_TRT, data = ., var.equal = F) %>%
    broom::tidy() %>%
    pull(p.value) %>%
    format(scientific = T, digits = 3)
}, NA_real_)
# Fisher test
fisher_pval <- possibly(function(data, ref_group, comp_group) {
  data %>%
    filter(col_TRT %in% c(ref_group, comp_group)) %>%
    droplevels() %>%
    table() %>%
    fisher.test() %>%
    broom::tidy() %>%
    pull(p.value) %>%
    format(scientific = T, digits = 3)
}, NA_real_)
# toy data ----
df <- tribble(
  ~col_id, ~col_cat, ~col_num, ~col_TRT, ~col_log,
  1, "a", 1, "Q1", T,
  2, "a", 2, "Q2", F,
  3, "b", 2, "Q3", T,
  4, "b", 4, "Q4", F,
  5, "b", 2, "Q1", T,
  6, "c", 3, "Q2", F,
  7, "c", 6, "Q3", T,
  8, "c", 8, "Q4", F,
  9, "c", 2, "Q2", T
)

# Descriptive stats ----
df1 <- df |> 
  select(!col_id) |> 
  mutate(
    across(where(is.logical),
           ~ifelse(.x, as.character(cur_column()), "z"),
           .names = "{col}"),
    across(where(is.character), as.factor))
# check missing values
sapply(df1, \(x) sum(is.na(x)))
# summary of numeric variables
tab1 <- df1 |> 
  summarise(across(where(is.numeric), .fns = fun_num)) |> 
  pivot_longer(everything(), names_pattern = "(.+)_(.+)$",
               names_to = c( "variable", ".value"))
# summary of categorical variables
tab1 <- df1 |> 
  select_if(is.factor) |> 
  purrr::map(fun_fac) |> 
  bind_rows(.id = "variable") |> 
  filter(!(subgroup %in% c("z", "Q" %+% c(1:4)))) |> 
  bind_rows(tab1)

## numeric summary by col TRT
# pvals
tab2 <- df1 |>
  select(col_TRT, where(is.numeric)) |>
  pivot_longer(!col_TRT, names_to = "variable", values_to = "value") |>
  nest(data = -variable) |>
  mutate(p_Q2 = map_chr(data, ~ t_test_pval(.x, "Q1", "Q2")),
         p_Q3 = map_chr(data, ~ t_test_pval(.x, "Q1", "Q3")),
         p_Q4 = map_chr(data, ~ t_test_pval(.x, "Q1", "Q4"))) |>
  select(!data)
# descriptive stats by col TRT
tab2 <- df1 |> 
  group_by(col_TRT) |> 
  summarise(across(where(is.numeric), .fns = fun_num)) |> 
  pivot_longer(!col_TRT, names_pattern = "(.+)_(.+)$",
               names_to = c( "variable", ".value")) |> 
  pivot_wider(names_from = col_TRT,
              values_from = c(`N (%)`,
                              `Mean (sd)`,
                              `Median [min-max]`)) |> 
  left_join(tab2)
# factor summary by col TRT
# pvals
tab3 <- df1 |>
  select_if(is.factor) |>
  pivot_longer(!col_TRT, names_to = "variable", values_to = "value") |>
  nest(data = -variable) |>
  mutate(p_Q2 = map_chr(data, ~ fisher_pval(.x, "Q1", "Q2")),
         p_Q3 = map_chr(data, ~ fisher_pval(.x, "Q1", "Q3")),
         p_Q4 = map_chr(data, ~ fisher_pval(.x, "Q1", "Q4"))) |>
  select(!data)
# descriptive stats
factor_vars <- setdiff(names(df1)[sapply(df1, is.factor)],"col_TRT")
tab4 <- tibble()
# Loop over each factor variable
for (var in factor_vars) {
  # Skip if the variable has only one level
  if (nlevels(df1[[var]]) <= 1) next
  # Perform the group-wise analysis
  temp <- df1 |>
    group_by(col_TRT, !!sym(var)) |>
    summarise(N = n()) |> 
    mutate(pct = N / sum(N) * 100,
           `N (%)` = format(N, big.mark = ",", trim = T) %+% " (" %+% 
             round(pct,2) %+% "%)") |>
    select(!c(N, pct)) |> 
    pivot_wider(names_from = col_TRT, values_from = `N (%)`,
                names_glue = "{.value}_{col_TRT}") |>
    rename(subgroup = 1) |> 
    mutate(variable = var)
  # Append the result to the final tibble
  tab4 <- bind_rows(tab4, temp)
}
tab2 <- tab4 |> 
  filter(subgroup != "z") |> 
  left_join(tab3) |> 
  bind_rows(tab2)
# Reorder columns
tab2 <- tab2[, c("variable", "subgroup",
                 "N (%)_Q1", "Mean (sd)_Q1", "Median [min-max]_Q1",
                 "N (%)_Q2", "Mean (sd)_Q2", "Median [min-max]_Q2", "p_Q2",
                 "N (%)_Q3", "Mean (sd)_Q3", "Median [min-max]_Q3", "p_Q3",
                 "N (%)_Q4", "Mean (sd)_Q4", "Median [min-max]_Q4", "p_Q4")
]
# merge
tab1 <- tab1 |> 
  left_join(tab2)
