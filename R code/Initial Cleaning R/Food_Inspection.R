# Read CSV file
food = read.csv("Food_Inspections.csv")
head(food)
library(tidyverse)

# Compute the total frequency for each category
total = food |>
  select(Facility.Type) |>
  group_by(Facility.Type) |> 
  summarise(type_n = n()) 

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

write.csv(final, file = "pass rate.csv", row.names = FALSE)
        