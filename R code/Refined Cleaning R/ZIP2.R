food = read.csv("Food-Inspections-csv 4.27.csv")
library(tidyverse)
##### ZIP
# Compute the total frequency for each category
total2 = food |>
  select(Zip) |>
  group_by(Zip) |> 
  summarise(type_n = n()) |>
  filter(type_n >= 30)

# Compute the passed frequency for each category
pass2 = food |>
  select(Zip, Results) |>
  mutate(Results = ifelse(Results == "Pass w/ Conditions", "Pass", Results)) |>
  filter(Results == "Pass") |>
  group_by(Zip) |>
  summarise(pass_n = n())

# Combine 2 data frame and calculate the pass rate
final2 = left_join(total2, pass2) |>
  mutate(pass_rate = pass_n / type_n) |>
  arrange(desc(pass_rate)) |>
  select(Zip, pass_n, type_n, pass_rate) |>
  view()

write.csv(final2, file = "pass rate_zip3.csv", row.names = FALSE)
