library(tidyverse)
library(patchwork)


# 1. make your custom ggplot theme apply to all plots in your document

theme_set(theme_dragons())


# 2. use plot_annotation() + theme() to control titles on patchwork plot 


plot_dragon_index(dragons_long, "wis") + plot_dragon_index(dragons_long, "cha") +
  plot_annotation(title = "My title",
                  theme = 
                    theme(plot.background = element_rect(fill = "lavender"),
                          plot.margin = margin(20, 20, 20, 20),  # T, R, B, L 
                          plot.title = element_text(hjust = 0, # align left
                                              size = 16,  color = "black",  
                                            margin = margin(b = 5))  # spacing below title
                    )
  )


# 3. reorder x axis levels by another variable 

df %>% 
  ggplot(aes(x = reorder(category, mean_score), y = score)) +
  geom_boxplot() +    
  coord_flip() 