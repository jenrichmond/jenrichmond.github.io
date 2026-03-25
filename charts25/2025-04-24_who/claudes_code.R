# Install required packages if needed
# install.packages(c("ggplot2", "treemapify", "dplyr", "patchwork"))

library(ggplot2)
library(treemapify)
library(dplyr)
library(patchwork)

# Create the data frame for Hepatitis B
hep_b_data <- data.frame(
  region = c("African Region", "South-East Asia Region", "Eastern Mediterranean Region", 
             "Western Pacific Region", "European Region", "Region of the Americas"),
  infections = c(771000, 266000, 86000, 83000, 18000, 8000),
  type = "Hepatitis B"
)

# Create the data frame for Hepatitis C
hep_c_data <- data.frame(
  region = c("South-East Asia Region", "Eastern Mediterranean Region", "Region of the Americas", 
             "African Region", "European Region", "Western Pacific Region"),
  infections = c(225000, 183000, 176000, 172000, 126000, 98000),
  type = "Hepatitis C"
)

# Combine both datasets
combined_data <- rbind(hep_b_data, hep_c_data)

# Define color palette similar to the original
region_colors <- c(
  "African Region" = "#5EB3D5",
  "South-East Asia Region" = "#A9D18E",
  "Eastern Mediterranean Region" = "#F6B18F",
  "Western Pacific Region" = "#FFD966",
  "European Region" = "#C293CE",
  "Region of the Americas" = "#A593C7"
)

# Create function to make treemap with consistent theme
create_treemap <- function(data, title) {
  ggplot(data, aes(area = infections, fill = region, label = paste0(region, "\n", format(infections, big.mark = " ")))) +
    geom_treemap() +
    geom_treemap_text(colour = "black", place = "centre", grow = FALSE, size = 11) +
    scale_fill_manual(values = region_colors) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
    labs(title = title)
}

# Create individual treemaps
p1 <- create_treemap(filter(combined_data, type == "Hepatitis B"), "Hepatitis B")
p2 <- create_treemap(filter(combined_data, type == "Hepatitis C"), "Hepatitis C")

# Combine plots side by side
combined_plot <- p1 + p2 + plot_layout(ncol = 2)

# Add overall title
title <- ggplot() + 
  labs(title = "Figure 2.4    Number of new hepatitis B and hepatitis C infections, by WHO region, 2022") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 14, face = "bold"))

# Combine title and plots
final_plot <- title / combined_plot +
  plot_layout(heights = c(0.1, 1))

# Display the plot
final_plot

# Add source note at the bottom
final_plot <- final_plot +
  plot_annotation(caption = "Source: WHO (12).") +
  theme(plot.caption = element_text(hjust = 0, size = 10))

# You can save the plot with:
# ggsave("hepatitis_treemap.png", final_plot, width = 10, height = 8)