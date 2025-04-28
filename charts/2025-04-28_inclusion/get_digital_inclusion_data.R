library(tidyverse)
library(here)


# read data
wellbeing <- read_csv(here("charts/2025-04-28_inclusion/raw/Wellbeing statistics 2023 - CSV (updated 26 March 2025).csv")) %>%
  select(CA_code, VA_code, estimate)

# filter digital inclusion variables
digital_inclusion <- wellbeing %>%
  filter(VA_code %in% c("V55A", "V55B", "V55C", "V55D", "V56A", "V56B", "V56C", "V56D"))

# filter by life stage 
lifestage <- digital_inclusion %>%
  filter(CA_code %in% c("C02A", "C02B", "C02C", "C02D"))

# filter by age_group
age_group <- digital_inclusion %>%
  filter(CA_code %in% c("C03A", "C03B", "C03C", "C03D", "C03E", "C03F", "C03G"))

# filter by gender
gender <- digital_inclusion %>%
  filter(CA_code %in% c("C04A", "C04B")) 

# LIFE STAGE 


labelled_lifestage <- lifestage %>%
  mutate(
    stage = case_when(
      CA_code == "C02A" ~ "Young people",
      CA_code == "C02B" ~ "Prime working age",
      CA_code == "C02C" ~ "Middle-age",
      CA_code == "C02D" ~ "Older people",
      TRUE ~ NA_character_
    ),
    category = case_when(
      str_detect(VA_code, "V55") ~ "Internet use",
      str_detect(VA_code, "V56") ~ "Internet satisfaction",
      TRUE ~ NA_character_
    ),
    response = case_when(
      VA_code == "V55A" ~ "Many times a day",
      VA_code == "V55B" ~ "Once or twice a day",
      VA_code == "V55C" ~ "A few times a week or less",
      VA_code == "V55D" ~ "Never use it",
      VA_code == "V56A" ~ "Very satisfied",
      VA_code == "V56B" ~ "Satisfied",
      VA_code == "V56C" ~ "No feeling either way", 
      VA_code == "V56D" ~ "Dissatisfied/very dissatisfied",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    response = factor(response, levels = c(
      # Internet use levels in descending frequency
      "Many times a day", 
      "Once or twice a day", 
      "A few times a week or less", 
      "Never use it",
      # Satisfaction levels from most to least satisfied
      "Very satisfied", 
      "Satisfied", 
      "No feeling either way", 
      "Dissatisfied/very dissatisfied"
    )),
    category = factor(category, levels = c("Internet use", "Internet satisfaction"))
  )

# write to csv

labelled_lifestage %>%
  write_csv(here::here("charts/2025-04-28_inclusion/lifestage.csv"))

# AGE GROUP


labelled_age_group <- age_group %>%
  mutate(
    age = case_when(
      CA_code == "C03A" ~ "15-24",
      CA_code == "C03B" ~ "25-34",
      CA_code == "C03C" ~ "35-44",
      CA_code == "C03D" ~ "45-54",
      CA_code == "C03E" ~ "55-64",
      CA_code == "C03F" ~ "65-74",
      CA_code == "C03G" ~ "75+",
      TRUE ~ NA_character_
    ),
    category = case_when(
      str_detect(VA_code, "V55") ~ "Internet use",
      str_detect(VA_code, "V56") ~ "Internet satisfaction",
      TRUE ~ NA_character_
    ),
    response = case_when(
      VA_code == "V55A" ~ "Many times a day",
      VA_code == "V55B" ~ "Once or twice a day",
      VA_code == "V55C" ~ "A few times a week or less",
      VA_code == "V55D" ~ "Never use it",
      VA_code == "V56A" ~ "Very satisfied",
      VA_code == "V56B" ~ "Satisfied",
      VA_code == "V56C" ~ "No feeling either way", 
      VA_code == "V56D" ~ "Dissatisfied/very dissatisfied",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    response = factor(response, levels = c(
      # Internet use levels in descending frequency
      "Many times a day", 
      "Once or twice a day", 
      "A few times a week or less", 
      "Never use it",
      # Satisfaction levels from most to least satisfied
      "Very satisfied", 
      "Satisfied", 
      "No feeling either way", 
      "Dissatisfied/very dissatisfied"
    )),
    category = factor(category, levels = c("Internet use", "Internet satisfaction"))
  )

# write to csv

labelled_age_group %>%
  write_csv(here::here("charts/2025-04-28_inclusion/age_group.csv"))


# GENDER 

labelled_gender <- gender %>%
  mutate(
    gender = case_when(
      CA_code == "C04A" ~ "Male",
      CA_code == "C04B" ~ "Female",
      TRUE ~ NA_character_
    ),
    category = case_when(
      str_detect(VA_code, "V55") ~ "Internet use",
      str_detect(VA_code, "V56") ~ "Internet satisfaction",
      TRUE ~ NA_character_
    ),
    response = case_when(
      VA_code == "V55A" ~ "Many times a day",
      VA_code == "V55B" ~ "Once or twice a day",
      VA_code == "V55C" ~ "A few times a week or less",
      VA_code == "V55D" ~ "Never use it",
      VA_code == "V56A" ~ "Very satisfied",
      VA_code == "V56B" ~ "Satisfied",
      VA_code == "V56C" ~ "No feeling either way", 
      VA_code == "V56D" ~ "Dissatisfied/very dissatisfied",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    response = factor(response, levels = c(
      # Internet use levels in descending frequency
      "Many times a day", 
      "Once or twice a day", 
      "A few times a week or less", 
      "Never use it",
      # Satisfaction levels from most to least satisfied
      "Very satisfied", 
      "Satisfied", 
      "No feeling either way", 
      "Dissatisfied/very dissatisfied"
    )),
    category = factor(category, levels = c("Internet use", "Internet satisfaction"))
  )

# write to csv

labelled_gender %>%
  write_csv(here::here("charts/2025-04-28_inclusion/gender.csv"))
