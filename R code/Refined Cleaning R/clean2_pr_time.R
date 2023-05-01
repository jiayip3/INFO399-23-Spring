food = read.csv("Food-Inspections-csv 4.27.csv")
library(tidyverse)

# Compute the total frequency for each category in different years
total = food |>
  select(Facility.Type, Inspection.Date) |> 
  filter(Inspection.Date != 2017) |>
  group_by(Facility.Type, Inspection.Date) |> 
  summarise(type_n = n()) |>
  filter(type_n >= 10)

# Compute the passing frequency for each category in different years
pass = food |>
  select(Facility.Type, Inspection.Date, Results)|> 
  filter(Inspection.Date != 2017) |>
  mutate(Results = ifelse(Results == "Pass w/ Conditions", "Pass", Results)) |>
  filter(Results == "Pass") |>
  group_by(Facility.Type, Inspection.Date) |>
  summarise(pass_n = n())

# Combine 2 data frame and calculate the pass rate in different years
final = left_join(total, pass) |>
  mutate(pass_rate = pass_n / type_n) |>
  select(Facility.Type, Inspection.Date, pass_rate) |>
  view()

write.csv(final, file = "pass rate_facility type_time.csv", row.names = FALSE)
