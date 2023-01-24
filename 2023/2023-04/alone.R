library(tidyverse)
library(showtext)
library(ggdist)
library(ggthemes)
library(ggtext)

font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "roboto condensed")
showtext_auto()
showtext_opts(dpi = 300)

darkblue <- "#203354"
background <- "#161C20"
linecolour <- "#424242"
textcolour <- "#666666"
lightgrey <- "#D3D3D3"

usa <- as.character(state.division)
names(usa) <- state.name
  
survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')

plot_data <- survivalists %>% 
  mutate(origin = case_when(country == "United States" ~ usa[state],
                            country == "Canada" ~ "Canada",
                            TRUE ~ "Rest of World")) %>% 
  group_by(origin) %>% 
  mutate(median_days = median(days_lasted),
         num = n())

st <- "Alone is a survival TV series where 10 survivalists are dropped in an extremely remote area and must fend for themselves.
"

plot_data %>% 
  ggplot(aes(x = fct_reorder(origin, median_days), y = days_lasted,
             fill = origin)) +
  geom_boxplot(width = 0.2,
               colour = linecolour,
               alpha = 0.7,
               outlier.colour = NA) +
  stat_dots(
    side = "right",
    justification = -0.2
  ) +
  geom_text(aes(y = -1, label = num),
            family = "roboto",
            size = 4,
            colour = textcolour,
            hjust = "right",
            vjust = -0.29) +
  geom_text(aes(x = "East South Central", 
                y = -0.3, label = "# of\nsurvivalists"),
            family = "roboto",
            size = 1.4,
            colour = textcolour,
            hjust = "right",
            vjust = 1.) +
  scale_fill_tableau(
    palette = "Tableau 20"
  ) +
  labs(x = "Origin of survivalist",
       y = "Days lasted",
       title = "Canadians the best in being Alone",
       subtitle = st,
       caption = "Data: {alone}") +
  guides(fill = "none") +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(family = "roboto",
                        colour = textcolour),
    axis.text = element_text(colour = textcolour,
                             size = 12,
                             vjust = -0.29),
    plot.title.position = "plot",
    plot.title = element_text(family = "roboto condensed",
                              face = "bold",
                              size = 22),
    plot.subtitle = element_text(family = "roboto condensed"),
    plot.caption = element_text(family = "roboto condensed"),
    plot.background = element_rect(fill = background),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dotted",
                                      colour = linecolour),
    panel.grid.minor.x = element_blank()
  )

ggsave("./2023/2023-04/alone.png", device = "png", dpi = 300,
      width = 8, height = 5, bg = background)  

system("open ./2023/2023-04/alone.png")
