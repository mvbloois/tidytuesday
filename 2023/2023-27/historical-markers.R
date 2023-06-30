library(tidyverse)
library(maps)
library(showtext)

font_add_google("Lora", "font")
showtext_auto()

`%notin%` <- Negate(`%in%`)

historical_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv')

usa_map <- map_data("state")

ggplot(usa_map, aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "white", colour = "#B31942", linewidth = 0.15) +
  geom_point(data = filter(historical_markers, state_or_prov %notin% c("Alaska", "Hawaii", "Puerto Rico")),
             aes(x = longitude_minus_w, y = latitude_minus_s),
             colour = "#0A3161",
             alpha = 0.2, shape = "+") +
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5) +
  labs(
    title = "It is the state",
    subtitle = "Geographical distribution of historical markers in the conterminous United States.",
    caption = "Data: Historical Marker Database USA Index"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "font",
                        colour = "#0A3161"),
    plot.title = element_text(size = 48),
    plot.subtitle = element_text(size = 22),
    plot.caption = element_text(size = 14),
    plot.margin = margin(10,10,10,10)
  )

ggsave(
  "./2023/2023-25/historical-markers.png",
  bg = "white",
  width = 5,
  height = 4
)
