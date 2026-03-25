library(datapasta)
library(tidyverse)


hep_data <- tibble::tribble(
                         ~WHO_region, ~new_hepB, ~new_hepC, ~deaths_hepB, ~deaths_hebC, 
              "African Region", 771000, 172000, 272000, 35000,
         "Region of the Americas", 8000, 176000, 20000, 38000,
      "South-East Asia Region", 266000, 225000, 218000, 42000,
               "European Region", 18000, 126000, 32000, 21000,
  "Eastern Mediterranean Region", 86000, 183000, 41000, 65000,
        "Western Pacific Region", 83000, 98000, 518000, 43000
  )

hep_data %>%
  write_csv(here::here("charts", "2025-04-25_who", "hepBC.csv"))
