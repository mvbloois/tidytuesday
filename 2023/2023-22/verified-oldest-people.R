library(tidyverse)
library(geomtextpath)
library(showtext)

font_add_google("Tangerine", "font")
font_add_google("Fira Code", "labs")
showtext_auto()

centenarians <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv'
  )

dates <- seq.Date(
  from = ymd("1985-01-01"),
  to = ymd("2023-05-30"),
  by = "day"
)

oldest_alive <- function(date) {
  centenarians %>%
    mutate(
      is_alive = death_date > date | is.na(death_date),
      age_at_date = ifelse(is_alive, as.numeric(interval(birth_date, date), 'years'), age),
      date = date
    ) %>%
    filter(is_alive) %>%
    filter(age_at_date == max(age_at_date))
}

df <- map(dates, oldest_alive) %>%
  reduce(bind_rows)

names <- c("Sarah Knauss", "Kane Tanaka")

subtitle <-
  "Jeanne Calment (1875-1997) is the oldest verified person we know. She was the oldest person in the world from December 27th, 1987, when Anna Eliza Williams died, to her own death on August 4th, 1997. Kane Tanaka and Sarah Strauss are the second and third oldest people we know."

df %>%
  ggplot(aes(x = date, y = age_at_date)) +
  geom_line(linewidth = 0.2,
            linetype = "dashed") +
  geom_textpath(
    data = filter(df, name == "Jeanne Calment"),
    aes(label = name,
        group = name),
    colour = "darkblue",
    #family = "labs",
    size = 4.,
    vjust = -.3
  ) +
  geom_textpath(
    data = filter(df, name %in% names),
    aes(label = name,
        group = name),
    colour = "darkblue",
    #family = "labs",
    size = 2.5,
    linewidth = 0.5,
    vjust = -.3
  ) +
  geom_line(
    data = filter(df,!name %in% c(names, "Jeanne Calment")),
    aes(colour = "darkred", group = name),
    linewidth = 0.5
  ) +
  scale_colour_identity() +
  scale_y_continuous(limits = c(111, 124),
                     breaks = 111:124) +
  guides(colour = "none") +
  labs(
    x = NULL,
    y = "Age at date",
    title = "The oldest person alive",
    subtitle = str_wrap(subtitle, 95),
    caption = "Source: WikiPedia via {centenarians} by Frank Hull"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        size = 75),
    panel.grid.minor.y = element_blank(),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 35,
                                 lineheight = 0.3),
    plot.caption = element_text(size = 25),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  "./2023/2023-22/verified-oldest-people.png",
  bg = "white",
  width = 5,
  height = 4
)
system("open ./2023/2023-22/verified-oldest-people.png")
