library(tidyverse)
library(maps)
library(ggtext)
library(showtext)

font_add_google("Lora", "font")
showtext_auto()

`%notin%` <- Negate(`%in%`)

us_place_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv')
nl_places <- readr::read_csv2("2023/2023-26/WoonplaatsenCodes.csv") %>% select(nl_place = Title) %>% mutate(nl_place = str_to_lower(nl_place))

us_nl_places <- us_place_names %>% 
  mutate(us_place = str_to_lower(feature_name)) %>% 
  mutate(us_place = str_remove(us_place, "new ")) %>% 
  inner_join(
    nl_places,
    by = join_by(us_place == nl_place)
  ) %>% 
  filter(state_name %notin% c("Alaska", "Hawaii"))

st <- "<span style = 'color:#B31942;'>Zurich</span> is a small village in the north of the Netherlands. Its spelling is very similar to Zürich.<br>
Other common names are Linden (27 times) and (New) Amsterdam (13 times)."

ggplot(usa_map, aes(long, lat)) +
  geom_polygon(aes(group = group), fill = "white", colour = "grey30", linewidth = 0.15) +
  geom_point(data = us_nl_places,
             aes(x = prim_long_dec, y = prim_lat_dec,
                 colour = ifelse(us_place == "zurich", "#B31942",  "#0A3161")
                 ),
             size = 0.75
             ) +
  scale_colour_identity() +
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5) +
  labs(
    title = "Dutch place names in the United States",
    subtitle = st,
    caption = "Data: US Board of Geographic Names, Central Bureau of Statistics"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "font",
                        colour = "#0A3161"),
    plot.title = element_text(size = 48),
    plot.subtitle = element_markdown(size = 22,
                                     lineheight = 0.35),
    plot.caption = element_text(size = 14),
    plot.margin = margin(10,10,10,10)
  )

ggsave(
  "./2023/2023-26/us-populated-places.png",
  bg = "white",
  width = 5,
  height = 4
)

