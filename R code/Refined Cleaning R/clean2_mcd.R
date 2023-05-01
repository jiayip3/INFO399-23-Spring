food = read.csv("Food-Inspections-csv 4.27.csv")
library(tidyverse)

# Compute the total frequency for each category
total = food |>
  select(AKA.Name) |>
  group_by(AKA.Name) |> 
  filter(AKA.Name == "MCDONALDS") |>
  summarise(type_n = n())

# Compute the total frequency for each category in different years
total = food |>
  select(AKA.Name, Inspection.Date) |> 
  filter(Inspection.Date != 2017, AKA.Name == "MCDONALDS") |>
  group_by(AKA.Name, Inspection.Date) |> 
  summarise(type_n = n())

# Compute the passing frequency for each category in different years
pass = food |>
  select(AKA.Name, Inspection.Date, Results) |> 
  filter(Inspection.Date != 2017) |>
  mutate(Results = ifelse(Results == "Pass w/ Conditions", "Pass", Results)) |>
  filter(Results == "Pass", AKA.Name == "MCDONALDS") |>
  group_by(AKA.Name, Inspection.Date) |>
  summarise(pass_n = n())

# Combine 2 data frame and calculate the pass rate in different years
final = left_join(total, pass) |>
  mutate(pass_rate = pass_n / type_n) |>
  select(AKA.Name, Inspection.Date, pass_rate) |>
  view()

# Visualization
plot(x = final$Inspection.Date, y = final$pass_rate, ylim = c(0.5, 1), type = "l", xlab = "Year", ylab = "Pass Rate")
text(x = final$Inspection.Date, y = final$pass_rate, labels = round(final$pass_rate, 4), pos = 3)
title("Pass Rate of McDonald's over Time (2010-2016)")


food = read.csv("Food-Inspections-csv 4.27.csv")
library(tidyverse)

# Compute the total frequency for each category
total = food |>
  select(AKA.Name) |>
  group_by(AKA.Name) |> 
  filter(AKA.Name == "MCDONALDS") |>
  summarise(type_n = n())

# Compute the total frequency for each category in different years
total = food |>
  select(AKA.Name, Inspection.Date) |> 
  filter(Inspection.Date != 2017, AKA.Name == "SUBWAY") |>
  group_by(AKA.Name, Inspection.Date) |> 
  summarise(type_n = n())

# Compute the passing frequency for each category in different years
pass = food |>
  select(AKA.Name, Inspection.Date, Results) |> 
  filter(Inspection.Date != 2017) |>
  mutate(Results = ifelse(Results == "Pass w/ Conditions", "Pass", Results)) |>
  filter(Results == "Pass", AKA.Name == "SUBWAY") |>
  group_by(AKA.Name, Inspection.Date) |>
  summarise(pass_n = n())

# Combine 2 data frame and calculate the pass rate in different years
final = left_join(total, pass) |>
  mutate(pass_rate = pass_n / type_n) |>
  select(AKA.Name, Inspection.Date, pass_rate) |>
  view()

# Visualization
plot(x = final$Inspection.Date, y = final$pass_rate, ylim = c(0.5, 1), type = "l", xlab = "Year", ylab = "Pass Rate")
text(x = final$Inspection.Date, y = final$pass_rate, labels = round(final$pass_rate, 4), pos = 3)
title("Pass Rate of SUBWAY over Time (2010-2016)")