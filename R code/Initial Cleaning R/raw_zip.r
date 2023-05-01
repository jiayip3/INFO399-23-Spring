# Read CSV file
food = read.csv("Food.csv")

head(food)
library(tidyverse)

# Compute the total frequency for each category
total = food |>
  select(Zip) |>
  group_by(Zip) |> 
  summarise(type_n = n())

# Compute the passed frequency for each category
pass = food |>
  select(Zip, Results) |>
  mutate(Results = ifelse(Results == "Pass w/ Conditions", "Pass", Results)) |>
  filter(Results == "Pass") |>
  group_by(Zip) |>
  summarise(pass_n = n())

# Combine 2 data frame and calculate the pass rate
final = left_join(total, pass) |>
  mutate(pass_rate = pass_n / type_n) |>
  arrange(desc(pass_rate)) |>
  select(Zip, pass_n, type_n, pass_rate) |>
  view()

write.csv(final, file = "pass rate_raw zip.csv", row.names = FALSE)
