# Read CSV file
food = read.csv("AKA.csv")
head(food)
library(tidyverse)

# Compute the total frequency for each category
total = food |>
  select(AKA.Name) |>
  group_by(AKA.Name) |> 
  filter(AKA.Name == "MCDONALD'S") |>
  summarise(type_n = n())

# Compute the total frequency for each category in different years
total = food |>
  mutate(year = substr(Inspection.Date, nchar(Inspection.Date) - 3, nchar(Inspection.Date))) |>
  select(AKA.Name, year) |> 
  mutate(year = as.numeric(year)) |>
  filter(year != 2017, AKA.Name == "MCDONALD'S") |>
  group_by(AKA.Name, year) |> 
  summarise(type_n = n())

# Compute the passing frequency for each category in different years
pass = food |>
  mutate(year = substr(Inspection.Date, nchar(Inspection.Date) - 3, nchar(Inspection.Date))) |>
  select(AKA.Name, year, Results) |> 
  mutate(year = as.numeric(year)) |>
  filter(year != 2017) |>
  mutate(Results = ifelse(Results == "Pass w/ Conditions", "Pass", Results)) |>
  filter(Results == "Pass", AKA.Name == "MCDONALD'S") |>
  group_by(AKA.Name, year) |>
  summarise(pass_n = n())

# Combine 2 data frame and calculate the pass rate in different years
final = left_join(total, pass) |>
  mutate(pass_rate = pass_n / type_n) |>
  select(AKA.Name, year, pass_rate) |>
  view()

# Visualization
plot(x = final$year, y = final$pass_rate, ylim = c(0.5, 1), type = "l", xlab = "Year", ylab = "Pass Rate")
text(x = final$year, y = final$pass_rate, labels = round(final$pass_rate, 4), pos = 3)
title("Pass Rate of McDonald's over Time (2010-2016)")
