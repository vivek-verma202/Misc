# simple trick to build attrition tables
`%+%` <- paste0
# Step 0: patients with qualifying NMOSD criteria
id_count <- bind_rows(id_count, tibble(
  criteria = "Step 0: starting count",
  n_left = n_distinct(df$ENROLID)))
#' Step 1: Index date between start and end cut
df <- df |> filter(between(INDEX_DT, strt_cut, end_cut))
id_count <- bind_rows(id_count, tibble(
  criteria = "Step 1: Index date between " %+% strt_cut " and " %+% end_cut,
  n_left = n_distinct(df$ENROLID)))
id_count <- id_count |> 
  mutate(n_lost = lag(n_left) - n_left)
write_csv(id_count, path_out %+% "attr_table.csv")
