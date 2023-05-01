food = read.csv("refined_1.csv")
getwd()
head(food)
library(tidyverse)

total = food |>
  select(Facility.Type) |>
  group_by(Facility.Type) |> 
  summarise(type_n = n()) |>
  filter(type_n >= 30) |> 
  na.omit() |>
  arrange(desc(type_n)) |>
  view()

view(food)
