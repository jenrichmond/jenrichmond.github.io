# load packages

library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(ggeasy)
library(patchwork)
library(Hmisc) # for %nin%

# adjust year/week values here
year = 2025
week = 21



tt <- tt_load(year, week)

df <- tt[[1]]


colours <- c("Black Dragons", "Blue Dragons", "Gold Dragons", 
            "Green Dragons", "Red Dragons")   

labels <- c("Charisma", "Constitution", "Dexterity", "Intelligence", 
            "Strength", "Wisdom")

cols <- c("#272727", "#3375B4", "#E4BD38", "#435E1E", "#D0421D")

dragons <- df %>%
  filter(type == "Dragon") %>%
  filter(category %in% colours) %>%
  select(name, category, type, str:cha) %>%
  mutate(age = case_when(str_detect(name, "Wyrmling") ~ "Wyrmling", 
                            str_detect(name, "Young") ~ "Young", 
                         str_detect(name, "Adult") ~ "Adult", 
                         str_detect(name, "Ancient") ~ "Ancient")) %>%
    pivot_longer(names_to = "index", values_to = "score", str:cha)

glimpse(dragons)

dragons$age <- factor(dragons$age)
levels(dragons$age)
dragons$age <- fct_relevel(dragons$age, c("Wyrmling", "Young", "Adult", "Ancient"))

dragons$index <- factor(dragons$index, 
                        labels = c("Charisma","Constitution","Dexterity",
                                                  "Intelligence","Strength", "Wisdom")) 

dragons$index <- fct_relevel(dragons$index, c("Strength", "Charisma", "Constitution", "Intelligence", 
                                              "Wisdom", "Dexterity"))


dragons$category <- factor(dragons$category, labels = c("Black", "Blue", "Gold", "Green", 
                                                  "Red"))


dragons %>%
  ggplot(aes(x = age, y = score, group = category, colour = category)) +
  geom_jitter(width = 0.1, height = 0, size = 2) +
  geom_line() +
  facet_wrap(~index) +
  scale_colour_manual(values = cols) +
  facet_wrap(~ index) +
  theme_minimal() +
  easy_add_legend_title("Dragon type") +
  easy_move_legend(to = c("bottom")) +
  labs(x = "Age", y = "Score", title = "Dragon Development") +
  easy_text_size(c("axis.text.x", "axis.text.y"), 8)




ggsave("development.png", width = 6, height = 4, bg = "white")


