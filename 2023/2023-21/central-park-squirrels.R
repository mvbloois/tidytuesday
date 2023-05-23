library(tidyverse)
library(janitor)
library(osmdata)
library(showtext)

font_add_google("Fira Code", "font")
showtext_auto()

cinnamon <- "#AB6F4C"
gray <- "#7B888E"
black <- "#2F1F1C"

subtitle <- "In October 2018, with the help of hundreds of volunteers and key NYC entities, the Squirrel Census tallied the squirrels in Central Park in New York, NY. There are 2,373 grays in Central Park."

# city
city <- "Central Park, New York"

city_bb <- c(-73.9902,40.7633,-73.9482,40.8021)

city_roads <- city_bb %>%
  opq() %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

city_water <- city_bb %>%
  opq() %>%
  add_osm_feature(key = "water") %>%
  osmdata_sf()


squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv') %>% 
  clean_names() %>% 
  mutate(date = mdy(date)) %>% 
  filter(!is.na(primary_fur_color))


ggplot() +
  geom_sf(data = city_water$osm_polygons, fill = "#7fc0ff") +
  geom_sf(data = city_roads$osm_lines,
          colour = "#6a5a00",
          linewidth = 0.2,
          alpha = 0.2) +
  geom_point(data = squirrel_data,
             aes(x = x, y = y, colour = primary_fur_color),
             size = 0.7) +
  scale_colour_manual(values = c(black, cinnamon, gray)) +
  guides(colour = guide_legend(title = "Primary Fur Colour",
                               keywidth = 1,
                               keyheight = 0.2,
                               default.unit = "cm"
                               )
         ) +
  labs(
    title = "THE SQUIRREL CENSUS",
    subtitle = str_wrap(subtitle, 55),
    caption = "DEPT. OF DATA - NYC"
  ) +
  coord_sf(xlim = c(-73.95, -73.982),
           ylim = c(40.765, 40.801)) +
  theme_void() +
  theme(
    text = element_text(family = "font"),
    plot.title = element_text(colour = "#ae1f23",
                              size = 70,
                              hjust = 0.5),
    plot.subtitle = element_text(colour = "grey20",
                                 size = 25,
                                 lineheight = 0.35,
                                 hjust = 0.5),
    plot.caption = element_text(colour = "#ae1f23",
                                size = 35,
                                hjust = 0.5),
    legend.position = c(.8, .3),
    legend.background = element_rect(fill = "white",
                                     colour = "white"),
    legend.text = element_text(size = 14,
                               lineheight = 25),
    legend.title = element_text(size = 16,
                                vjust = -3),
    legend.margin = margin(3,3,3,3),
    plot.margin = margin(20,20,20,20)
  )

ggsave("./2023/2023-21/central-park-squirrels.png",
       bg = "white", width = 4.1, height = 7)
system("open ./2023/2023-21/central-park-squirrels.png")

