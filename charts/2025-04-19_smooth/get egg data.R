library(datapasta)
library(readxl)
library(tidyverse)
library(janitor)

# NEWZEALAND  data pasted from ...
# https://figure.nz/chart/WNZOpEoBKRyz4hBh-a5aHFhuTKVojTjod

eggsNZ <- tibble::tribble(
      ~Month, ~NZD,
  "Jan 2015", 3.78,
  "Feb 2015", 3.79,
  "Mar 2015", 3.64,
  "Apr 2015", 3.71,
  "May 2015", 3.87,
  "Jun 2015", 3.83,
  "Jul 2015", 3.81,
  "Aug 2015", 3.77,
  "Sep 2015", 3.74,
  "Oct 2015", 3.76,
  "Nov 2015", 3.82,
  "Dec 2015", 3.64,
  "Jan 2016", 3.77,
  "Feb 2016", 3.72,
  "Mar 2016", 3.75,
  "Apr 2016",  3.8,
  "May 2016", 3.78,
  "Jun 2016",  3.8,
  "Jul 2016", 3.77,
  "Aug 2016", 3.82,
  "Sep 2016", 3.88,
  "Oct 2016", 3.94,
  "Nov 2016", 3.73,
  "Dec 2016",  3.8,
  "Jan 2017",  3.8,
  "Feb 2017", 3.85,
  "Mar 2017", 3.87,
  "Apr 2017",  3.9,
  "May 2017", 3.93,
  "Jun 2017", 3.83,
  "Jul 2017", 3.76,
  "Aug 2017", 3.74,
  "Sep 2017", 3.77,
  "Oct 2017",  3.9,
  "Nov 2017", 3.89,
  "Dec 2017", 3.81,
  "Jan 2018",  3.9,
  "Feb 2018", 3.91,
  "Mar 2018",  3.9,
  "Apr 2018", 3.89,
  "May 2018", 3.88,
  "Jun 2018",  3.8,
  "Jul 2018", 3.86,
  "Aug 2018",    4,
  "Sep 2018", 4.14,
  "Oct 2018", 4.18,
  "Nov 2018", 4.18,
  "Dec 2018", 4.25,
  "Jan 2019", 4.19,
  "Feb 2019",  4.3,
  "Mar 2019", 4.32,
  "Apr 2019", 4.43,
  "May 2019", 4.51,
  "Jun 2019", 4.63,
  "Jul 2019", 4.51,
  "Aug 2019", 4.61,
  "Sep 2019", 4.44,
  "Oct 2019", 4.42,
  "Nov 2019", 4.39,
  "Dec 2019", 4.39,
  "Jan 2020", 4.48,
  "Feb 2020", 4.53,
  "Mar 2020", 4.48,
  "Apr 2020", 4.78,
  "May 2020", 4.64,
  "Jun 2020", 4.31,
  "Jul 2020", 4.34,
  "Aug 2020", 4.57,
  "Sep 2020", 4.42,
  "Oct 2020", 4.46,
  "Nov 2020", 4.21,
  "Dec 2020", 4.32,
  "Jan 2021", 4.22,
  "Feb 2021",  4.3,
  "Mar 2021", 4.46,
  "Apr 2021", 4.48,
  "May 2021", 4.62,
  "Jun 2021", 4.66,
  "Jul 2021", 4.81,
  "Aug 2021", 4.65,
  "Sep 2021", 5.22,
  "Oct 2021", 4.98,
  "Nov 2021", 5.13,
  "Dec 2021", 5.07,
  "Jan 2022", 5.09,
  "Feb 2022", 5.15,
  "Mar 2022", 4.93,
  "Apr 2022", 5.01,
  "May 2022", 4.86,
  "Jun 2022", 5.13,
  "Jul 2022", 5.34,
  "Aug 2022", 5.85,
  "Sep 2022", 5.85,
  "Oct 2022", 6.28,
  "Nov 2022", 5.93,
  "Dec 2022", 6.68,
  "Jan 2023", 7.04,
  "Feb 2023", 7.84,
  "Mar 2023", 8.36,
  "Apr 2023", 8.44,
  "May 2023", 8.58,
  "Jun 2023", 8.58,
  "Jul 2023",  9.5,
  "Aug 2023", 9.31,
  "Sep 2023", 9.19,
  "Oct 2023", 9.08,
  "Nov 2023",  9.3,
  "Dec 2023", 9.28,
  "Jan 2024", 9.54,
  "Feb 2024", 9.33,
  "Mar 2024", 9.28,
  "Apr 2024", 9.17,
  "May 2024", 8.89,
  "Jun 2024", 9.41,
  "Jul 2024", 8.65,
  "Aug 2024", 8.92,
  "Sep 2024", 8.86,
  "Oct 2024", 8.34,
  "Nov 2024", 8.38,
  "Dec 2024", 8.37,
  "Jan 2025", 8.89
  )


eggsNZ <- eggsNZ %>%
mutate(year = parse_number(Month)) %>%
  mutate(Month = str_sub(Month, 1, -5)) %>%
  select(year, month = Month, price = NZD)  %>% 
  mutate(currency = "NZD")


#USA .xlsx downloaded from...
#https://data.bls.gov/timeseries/APU0000708111

eggsUSA <- read_xlsx(here::here("charts", "2025-04-19_smooth", "eggsUSA.xlsx")) %>%
  slice(9:n()) %>%
  row_to_names(row_number = 1) %>%
  pivot_longer(names_to = "month", values_to = "price", Jan:Dec) %>%
  select(year = Year, month, price) %>%
  mutate(currency = "USD")

# combine

eggsNZ_USA <- rbind(eggsNZ, eggsUSA)
  
eggsNZ_USA$year <- as.numeric(eggsNZ_USA$year)
eggsNZ_USA$price <- as.numeric(eggsNZ_USA$price)

glimpse(eggsNZ_USA)

eggsNZ_USA %>% write_csv(here::here("charts", "2025-04-19_smooth", "eggsNZUSA.csv"))
                     