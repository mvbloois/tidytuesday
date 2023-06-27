library(tidyverse)
library(maps)

historical_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv')


skimr::skim(historical_markers)

historical_markers %>% 
  filter(longitude_minus_w > -130) %>% 
  ggplot(aes(x = longitude_minus_w, y = latitude_minus_s)) +
  geom_point(size = 0.3, alpha = 0.2) 
