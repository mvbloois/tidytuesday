library(tidyverse)
library(tidytext)
library(showtext)

font_add_google("Fira Sans", "font")
showtext_auto()

ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')

clrs <- c("red", "redish", "orange", "orangish", "yellow", "yellowish", "gold", 
             "green", "greenish", "blue", "blueish", "violet", "purple", "pink", "pinkish", "brown",
             "white", "silver", "silvery", 
             "black"
)

colours <- c("red(ish)", "orange(ish)", "yellow(ish) / gold", 
          "green(ish)", "blue(ish)", "violet / purple", "pink(ish)", "brown",
          "white", "silver(y)", 
          "black"
)

hexes <- c("red",  "orange",  "yellow",
           "green", "blue", "violet", "pink", "brown",
           "white", "#C0C0C0",
           "black"
)

plot_data <- ufo_sightings %>% 
  select(summary) %>% 
  unnest_tokens(word, summary) %>%
  filter(word %in% clrs) %>% 
  mutate(word = case_match(
    word,
    c("green", "greenish") ~ "green(ish)",
    c("red", "redish") ~ "red(ish)",
    c("orange", "orangish") ~ "orange(ish)",
    c("blue", "blueish") ~ "blue(ish)",
    c("yellow", "yellowish", "gold") ~ "yellow(ish) / gold",
    c("pink", "pinkish") ~ "pink(ish)",
    c("violet", "purple") ~ "violet / purple",
    c("silver", "silvery") ~ "silver(y)",
    .default = word
    )) %>% 
  count(word) %>% 
  mutate(word = factor(word, colours))

plot_data %>% 
  ggplot(aes(x = n, y = fct_rev(word))) +
  geom_col(aes(fill = word)) +
  scale_fill_manual(values = hexes) +
  labs(x = "number of mentions", y = NULL,
       title = "What colours are most mentioned in UFO sightings?",
       subtitle = str_wrap("The National UFO Reporting Center maintains a collection of UFO sightings. This chart looks at the colours mentioned in the summary of almost 100,000 events.", 80),
       caption = "Data: National UFO Reporting Center") +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    text =  element_text(family = "font",
                         colour = "grey80",
                         size = 20),
    axis.text = element_text(colour = "grey80",
                             size = 24),
    plot.background = element_rect(fill = "#2F3B62"),
    plot.title.position = "plot",
    plot.title = element_text(size = 42),
    plot.subtitle = element_text(size = 24,
                                 face = "italic",
                                 lineheight = 0.3),
    plot.caption = element_text(size = 20),
    panel.grid.major.y = element_blank(),
    panel.grid = element_line(colour = "#202845",
                              linewidth = 0.2,
                              linetype = "dashed"),
    plot.margin = margin(10, 20, 10, 20)
  )

ggsave(
  "./2023/2023-24/ufo-sightings-redux.png",
  bg = "white",
  width = 5,
  height = 4
)
system("open ./2023/2023-24/ufo-sightings-redux.png")
           