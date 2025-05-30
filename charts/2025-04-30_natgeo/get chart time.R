library(datapasta)
library(tidyverse)
library(here)


chart_time <- data.frame(
        date = c("2025-03-27","2025-03-28","2025-03-29",
                 "2025-03-30","2025-03-31","2025-04-01","2025-04-02",
                 "2025-04-03","2025-04-04","2025-04-05","2025-04-06","2025-04-07",
                 "2025-04-08","2025-04-09","2025-04-10","2025-04-11",
                 "2025-04-12","2025-04-13","2025-04-14","2025-04-15","2025-04-16",
                 "2025-04-17","2025-04-18","2025-04-19","2025-04-20",
                 "2025-04-21","2025-04-22","2025-04-23","2025-04-24","2025-04-25",
                 "2025-04-26","2025-04-27","2025-04-28","2025-04-29",
                 "2025-04-30"),
        time = c("2:06","2:13","1:05","0:00","1:58",
                 "2:19","1:43","2:31","0:27","2:01","2:11","0:51","0:38",
                 "3:17","0:00","0:51","0:00","0:27","1:20","0:00","1:28",
                 "2:10","2:32","1:00","1:58","1:18","2:54","0:00","0:00",
                 "0:00","0:50","0:59","2:04","0:09","0:00")
)


chart_time %>% write_csv(here("chart_time.csv"))
