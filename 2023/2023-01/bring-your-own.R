library(tidyverse)
library(showtext)
library(ggtext)
library(rvest)

font_add('fa-solid', './img/fa-solid-900.ttf')
font_add_google("Karla", "karla")
showtext_auto()
showtext_opts(dpi = 300)

webpage <-
  read_html("https://en.wikipedia.org/wiki/Marathon_world_record_progression")

tbls <- html_nodes(webpage, "table")

mens_tbl <- webpage %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>%
  select(
    time = Time,
    athlete = Name,
    nationality = Nationality,
    date = Date,
    event_place = `Event/Place`,
    source = Source
  ) %>%
  mutate(
    across(where(is.character), ~ str_remove_all(., "\\[\\d + \\]")),
    across(where(is.character), ~ str_remove_all(., "\\[nb \\d + \\]")),
    event_place = if_else(
      str_sub(event_place, 1, 11) == "Polytechnic",
      "Polytechnic Marathon, London",
      event_place
    )
  ) %>%
  mutate(
    time = strptime(time, format = "%H:%M:%S") %>% as.POSIXct(),
    seconds = time - as.POSIXct(strptime("0:0:0", format = "%H:%M:%S")),
    distance_covert = 42.195 * (min(as.numeric(seconds)) / as.numeric(seconds)),
    togo = glue::glue("{round((42.195 - distance_covert) * 1000, 0)} meters to go") %>% as.character()
  ) %>%
  slice_min(time, n = 10) %>%
  mutate(
    rank = row_number(),
    togo = if_else(rank == 1, "finished", togo),
    rank = rank / 10
  )

bg <- "grey45"
  
subtitle <-
  str_wrap(
    "How far behind are previous world record holders on the marathon from the current fastest man: Eliud Kipchoge?",
    70
  )

mens_tbl %>%
  ggplot() +
  geom_rect(aes(
    xmin = 40,
    xmax = 42.8,
    ymin = 0.5,
    ymax = 1.1
  ),
  fill = bg) +
  geom_segment(
    aes(
      x = 42.195,
      xend = distance_covert,
      y = rank - 0.03,
      yend = rank - 0.03
    ),
    colour = "#1A8AE5",
    size = 0.7
  ) +
  geom_segment(
    aes(
      x = 42.195,
      xend = 42.195,
      y = 0.05,
      yend = 1.1
    ),
    colour = "white",
    size = 1.3
  ) +
  geom_richtext(
    aes(
      x = distance_covert - 0.03,
      y = rank,
      label = "<span style='font-family:fa-solid;'>&#xf70c;</span>"
    ),
    size = 7,
    fill = NA,
    label.colour = NA,
    colour = "green"
  ) +
geom_text(
    aes(
      x = distance_covert - 0.09,
      y = rank + 0.025,
      
      label = athlete
    ),
    family = "karla",
    fontface = "bold",
    colour = "white",
    size = 4.5,
    hjust = "right"
  ) +
  geom_text(
    aes(
      x = distance_covert - 0.09,
      y = rank - 0.02,
      label = date
    ),
    family = "karla",
    colour = "grey85",
    size = 3,
    hjust = "right"
  ) +
  geom_text(
    aes(x = 42.25, y = rank, label = togo),
    family = "karla",
    colour = "white",
    hjust = "left"
  ) +
  labs(title = "MARATHON WORLD RECORD HOLDERS",
       subtitle = subtitle,
       caption = "Data: Wikipedia") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg),
    text = element_text(family = "karla",
                        colour = "white"),
    plot.title = element_text(size = 24, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    plot.caption =  element_text(size = 12)
  )

ggsave("./2023/2023-01/bring-your-own.png", device = "png", dpi = 300,
       width = 8, height = 5)  
