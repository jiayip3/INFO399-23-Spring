# Pass rate --- Facility Type
food = read.csv("Food-Inspections-csv 4.27.csv")
library(tidyverse)

# Compute the total frequency for each category
total = food |>
  select(Facility.Type) |>
  group_by(Facility.Type) |> 
  summarise(type_n = n()) |>
  filter(type_n >= 30)

# Compute the passed frequency for each category
pass = food |>
  select(Facility.Type, Results) |>
  mutate(Results = ifelse(Results == "Pass w/ Conditions", "Pass", Results)) |>
  filter(Results == "Pass") |>
  group_by(Facility.Type) |>
  summarise(pass_n = n())

# Combine 2 data frame and calculate the pass rate
final = left_join(total, pass, by = "Facility.Type") |>
  mutate(pass_rate = pass_n / type_n) |>
  arrange(desc(pass_rate)) |>
  select(Facility.Type, pass_n, type_n, pass_rate) |>
  view()

write.csv(final, file = "pass rate_facility type.csv", row.names = FALSE)











