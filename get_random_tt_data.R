# choosing a dataset randomly

set.seed(1)

ttyears <- c(2018:2025)
ttweeks <- c(1:52)

# choose a year at random

chosen_year <- sample(ttyears, size = 1)

# choose at week at random

chosen_week <- sample(ttweeks, size = 1)


# read the data from that year/week

df <- tidytuesdayR::tt_load(chosen_year, chosen_week)


# print something about the data

print(df)