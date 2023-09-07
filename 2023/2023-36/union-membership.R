library(tidyverse)
library(geofacet)
library(showtext)

font_add_google("Lora", "font")
showtext_auto()

`%notin%` <- Negate(`%in%`)

states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')


states %>% 
  filter(sector == "Private") %>% 
  ggplot(aes(x = year)) +
  geom_area(aes(y = employment), fill = alpha("#0A3161", 0.2)) +
  geom_area(aes(y = members), fill = "#0A3161") +
  facet_geo(~state, scale = "free_y", grid = "us_state_grid2") +
  labs(x = NULL, y = NULL,
       title = "Development of employment and union membership in the private sector 1983 - 2022",
       subtitle = "Higher but declining numbers of union members on the West Coast and in the North East",
       caption = "Data: Union Membership, Coverage, and Earnings from the CPS www.unionstats.com") +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        colour = "#0A3161"),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    strip.text = element_text(size = 4,
                              colour = "#0A3161"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.2),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(size = 10,
                              hjust = 0.5),
    plot.subtitle = element_text(size = 8,
                                 hjust = 0.5),
    plot.caption = element_text(size = 6,
                                vjust = -5),
    plot.margin = margin(30,10,30,10)
  )


ggsave(
  "./2023/2023-36/union-membership.png",
  bg = "white",
  width = 9,
  height = 6,
  dpi = 300
)

system("open ./2023/2023-36/union-membership.png")
