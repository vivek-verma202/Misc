# following code identifies continuously enrolled IDs in MarketScan data (T table, as tmp_df below)
# calculate and add index date column to tmp_df
# Identify patients with continuous enrollment before and after index
tmp_df <- tmp_df |> 
  filter(RX == 1) |>
  mutate(BLSTART = INDEX_DT - 365,
         BLEND = INDEX_DT + 365) |> 
  select(ENROLID, INDEX_DT, DTSTART, DTEND, BLSTART, BLEND) |> 
  distinct()
n_distinct(tmp_df$ENROLID) # 1122
cont_enrol_bfr <- tmp_df |> 
  filter(DTSTART <= INDEX_DT,
         DTEND >= BLSTART)  |>
  mutate(BLDAYS = as.numeric(pmin(DTEND, INDEX_DT) - 
                               pmax(DTSTART, BLSTART)) + 1) |> 
  group_by(ENROLID) |>
  summarise(BLDAYS = sum(BLDAYS))
# check BLDAYS
summary(cont_enrol_bfr$BLDAYS)
cont_enrol_bfr <- cont_enrol_bfr |> 
  filter(BLDAYS == 366) |> pull(ENROLID)
length(cont_enrol_bfr) # 1007

cont_enrol_aftr <- tmp_df |> 
  filter(DTSTART <= BLEND,
         DTEND >= INDEX_DT)  |>
  mutate(BLDAYS = as.numeric(pmin(DTEND, BLEND) - 
                               pmax(DTSTART, INDEX_DT)) + 1) |> 
  group_by(ENROLID) |>
  summarise(BLDAYS = sum(BLDAYS)) 
# check BLDAYS
summary(cont_enrol_aftr$BLDAYS)
cont_enrol_aftr <- cont_enrol_aftr |> 
  filter(BLDAYS == 366) |> pull(ENROLID)
length(cont_enrol_aftr) # 814
