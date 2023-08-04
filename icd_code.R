# this code creates a lookup table to translate ICD10 codes into disease names (sub chapter level).
devtools::install_github("jackwasey/icd")
library(icd)
sub_chap <- icd::icd10_sub_chapters
subchap_lookup <- tibble(
  subchapter = names(sub_chap),
  codes = sub_chap) |> 
  unnest_longer(codes, indices_include = FALSE) |> 
  separate(codes, into = c("letter", "number"), sep = 1, convert = TRUE) |>
  filter(letter == "I") |> 
  mutate(number = as.integer(number)) |> 
  group_by(subchapter, letter) |>
  complete(number = full_seq(number, 1)) |>
  ungroup() |>
  mutate(
    codes = str_c(letter, str_pad(number, 2, pad = "0")),
    .keep = "unused"
  )
