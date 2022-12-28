library(tidyverse)
library(showtext)

# load fonts
font_add_google("Orbitron", "orbitron")
font_add_google("Racing Sans One", "zen")
showtext::showtext_opts(dpi = 300)
showtext_auto()

# load data
tlBooks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlBooks.csv')
tlFootnotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlFootnotes.csv')

# prepare data
plot_data <- tlBooks %>% 
  filter(format == "book") %>% 
  count(series, sort = TRUE) %>% 
  drop_na()

# plot data
ggplot(data = plot_data) +
  geom_col(aes(x = n, y = fct_reorder(series, n)),
           fill = "#C00000",
           show.legend = FALSE) +
  geom_text(aes(label = n, x = n + 11, y = fct_reorder(series, n)),
            colour = "#323C34",
            family = "orbitron"
            ) +
  scale_x_continuous(expand =  expansion(mult = c(0, 0.15))) +
  labs(title = "READ ME UP, SCOTTY" ,
       subtitle = str_wrap("The Star Trek franchise has a history of tie-in fiction which began with the 1967 \
                           publication of James Blish's Star Trek 1. This chart shows the number of books per \
                           series.", 80), 
       caption = "Data: {rtrek}",
       x = "Number of books",
       y = "Series") +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 18, face = "bold"),
    plot.background = element_rect(fill = "#A8B5A9"),
    text = element_text(family = "orbitron",
                        colour = "#323C34"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "#7A6424"),
    panel.grid.minor.x = element_blank()
  )

ggsave(here::here("plots", "2022-52", "rtrek.png"),
       width = 7, height = 5, dpi = 300, bg = "#A8B5A9",
       device = "png"
)
