library(tidyverse)
library(datapasta)


# paste 2021 data from press release


b2021 <- tibble::tribble(
  ~bird, ~votes, ~rank,
  "Long-tailed bat/pekapeka-tou-roa", 2894, "1st",
  "Kākāpō", 3351, "2nd",
  "Rifleman/titipounamu", 1852, "3rd",
  "Kea", 1594, "4th",
  "Antipodean albatross/toroa", 1468, "5th",
  "Black robin/kakaruia", 1228, "6th",
  "Little penguin/kororā", 1302, "7th",
  "Morepork/ruru", 1260, "8th",
  "Blue duck/whio", 1184, "9th",
  "Rockhopper penguin", 1477, "10th"
) %>%
  mutate(year = 2021)

# paste 2022 data from wikipedia

b2022 <- tibble::tribble(
  ~bird, ~votes, ~rank,
  "Pīwauwau / rock wren", 2894, "1st",
  "Kororā / little penguin", 3351, "2nd",
  "Kea", 1852, "3rd",
  "Karure / kakaruia / Chatham Island black robin", 1594, "4th",
  "Tawaki piki toka / rockhopper penguin", 1468, "5th",
  "Pīwakawaka / Fantail", 1228, "6th",
  "Hihi / stitchbird", 1302, "7th",
  "Kārearea / New Zealand falcon", 1260, "8th",
  "Pūteketeke / Australasian crested grebe", 1184, "9th",
  "Titipounamu / rifleman", 1477, "10th"
) %>%
  mutate(year = 2022)

# paste 2023 data from wikipedia

b2023 <- tibble::tribble(
  ~bird, ~votes, ~rank,
  "Pūteketeke / Australasian crested grebe", 290374, "1st",
  "North Island Brown Kiwi", 12904, "2nd",
  "Kea", 12060, "3rd",
  "Kākāpō", 10889, "4th",
  "Pīwakawaka / Fantail", 7857, "5th",
  "Tawaki piki toka / Rockhopper penguin", 6763, "6th",
  "Karure / kakaruia / Chatham Island black robin", 6753, "7th",
  "Huia", 6467, "8th",
  "Tūī", 6457, "9th",
  "Takahē", 6292, "10th"
) %>%
  mutate(year = 2023)

# paste 2024 data from wikipedia

b2024 <- tibble::tribble(
  ~bird, ~votes, ~rank,
  "Hoiho / yellow-eyed penguin", 6328, "1st",
  "Karure / kakaruia / Chatham Island black robin", 5442, "2nd",
  "Kākāpō", 4548, "3rd",
  "Ruru / Morepork", 4467, "4th",
  "Kea", 4206, "5th",
  "Pīwakawaka / Fantail", 4205, "6th",
  "Takahē", 3892, "7th",
  "Tawaki piki toka / Rockhopper penguin", 3834, "8th",
  "Kōkako", 3445, "9th",
  "Toroa / Antipodean albatross", 3415, "10th"
) %>%
  mutate(year = 2024)

# combine data, fix inconsistencies in naming

top10_year <- rbind(b2021, b2022, b2023, b2024) %>%
  mutate(bird = as_factor(bird)) %>%
  mutate(rank_number = parse_number(rank)) %>%
  select(-rank) %>%
  arrange(year, rank_number) %>%
  mutate(bird = case_when(
    bird == "Long-tailed bat/pekapeka-tou-roa" ~ "Pekapeka-tou-roa / Long-tailed bat",
    bird == "Rockhopper penguin" ~ "Tawaki piki toka / Rockhopper penguin",
    bird == "Black robin/kakaruia" ~ "Karure / kakaruia / Chatham Island black robin",
    bird == "Rifleman/titipounamu" ~ "Titipounamu / rifleman",
    bird == "Antipodean albatross/toroa" ~ "Toroa / Antipodean albatross",
    bird == "Little penguin/kororā" ~ "Kororā / little penguin",
    bird == "Morepork/ruru" ~ "Ruru / Morepork",
    bird == "Tawaki piki toka / rockhopper penguin" ~ "Tawaki piki toka / Rockhopper penguin",
    bird == "Pīwakawaka / fantail" ~ "Pīwakawaka / Fantail",
    bird == "North Island Brown Kiwi" ~ "Brown Kiwi",
    bird == "Blue duck/whio" ~ "Whio / Blue duck",
    TRUE ~ bird
  )) %>%
  mutate(maori_name = str_trim(str_extract(bird, "^[^/]+")))

# write data to csv

top10_year %>%
  write_csv(here::here("charts", "2025-04-17_birds", "top10.csv"))
