
library(tidyverse)
library(ggtext)
library(showtext)

font_add_google("Noto Serif", "font")
showtext_auto()

births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/births.csv')
deaths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-27/deaths.csv')

# https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/

col1 <- "#BF1019"
col2 <- "#056517"

title <- glue::glue(
  "<span style = 'color:{col1}'>Births</span> and <span style = 'color:{col2}'>deaths</span> on February 29th<p>"
)


ggplot() +
  ggdist::stat_dots(
    data = filter(births, year_birth > 1899),
    aes(x = year_birth),
    side = "right",
    dotsize = 2.8,
    colour = col1,
    fill = alpha(col1, 0.5),
    binwidth = 1) +
  ggdist::stat_dots(
    data = filter(deaths, year_death > 1899),
    aes(x = year_death),
    side = "left",
    colour = col2,
    fill = alpha(col2, 0.5),
    dotsize = 2.8,
    binwidth = 1) +
  scale_x_continuous(breaks = seq(from = 1900, to = 2020, by = 12)) +
  labs(x = "Year",
       y = NULL,
       title = title,
       subtitle = "as mentioned on Wikipedia",
       caption = "Data: Wikipedia") +
  theme_minimal() +
  theme(
    text = element_text(
      family = "font",
      size = 18,
      colour = "grey20"
                        ),
    plot.title.position = "plot",
    plot.title = element_markdown(size = 36,
                                  hjust = 0.5),
    plot.subtitle = element_text(size = 22,
                                 hjust = 0.5,
                                 vjust = 6),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    
    plot.margin = margin(10,10,10,10)
  )

ggsave(
  "./2024/2024-02-27/2024-02-27.png",
  bg = "white",
  width = 5,
  height = 3
)

system("open ./2024/2024-02-27/2024-02-27.png")
