food = read.csv("Food-Inspections-csv 4.27.csv")
library(tidyverse)

# Compute the total frequency for each category
total = food |>
  select(Facility.Type) |>
  group_by(Facility.Type) |> 
  summarise(type_n = n()) |>
  filter(type_n >= 30)

# Compute the passing frequency for each category
pass = food |>
  select(Facility.Type, Risk) |>
  filter(Risk == "Risk 3 (Low)") |>
  group_by(Facility.Type) |>
  summarise(pass_n = n())

# Combine 2 data frame and calculate the risk rate
final = left_join(total, pass) |>
  mutate(risk_rate = pass_n / type_n) |>
  select(Facility.Type, pass_n, type_n, risk_rate) |>  
  arrange(desc(risk_rate)) |>
  view()

write.csv(final, file = "pass rate_facility type_time.csv", row.names = FALSE)