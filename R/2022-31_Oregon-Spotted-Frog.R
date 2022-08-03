
topic <- "Oregon-Spotted-Frog"
year <- 2022
week <- 31
yrwk <- glue::glue("{year}-{week}")
plots_dir <- paste0(year, "-", week)
fs::dir_create(here::here("plots", plots_dir))
png_file <- glue::glue("{yrwk}_{topic}.png")
pdf_file <- glue::glue("{yrwk}_{topic}.pdf")

# install.packages('rsvg')
# remotes::install_github('coolbutuseless/ggsvg')

## packages
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(ggsvg)

font_add_google(name = "Bilbo", family = "bilbo")
font_add_google(name = "Petrona", family = "petrona")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

## Data ----

frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv') %>% 
  janitor::clean_names() %>% 
  mutate(survey_date = as.Date(survey_date, format = "%m/%d/%Y"),
         frog_id = as.integer(factor(frequency)))

frogs_tbl <- frogs %>% 
  select(subsite, survey_date, interval, frog_id, x = utme_83, y = utmn_83, female)

frogs_prepared <- frogs_tbl %>% 
  group_by(frog_id) %>% 
  mutate(total_distance = sqrt((x - lag(x, 1L))^2 + (y - lag(y, 1L))^2)) %>% 
  mutate(distance_from_start = sqrt((first(x) - last(x))^2 + (first(y) - last(y))^2)) %>%
  mutate(total_distance = ifelse(is.na(total_distance), 0, total_distance)) %>% 
  group_by(frog_id, female) %>% 
  summarise(start = first(subsite),
            total_distance = sum(total_distance),
            distance_from_start = min(distance_from_start))

svg_url <- "./img/frog.svg"
svg_txt <- paste(readLines(svg_url), collapse = "\n")

## Plot ----

background <- "#f5fcef"

title <- "There And Back Again"
subtitle <- "Radio-telemetry has been used to study late-season movement and habitat use by Oregon spotted frogs (Rana pretiosa) at Crane Prairie Reservoir in Oregon. This dataset includes individual frog location data and habitat use during each tracking event that occurred roughly weekly between September and late November of 2018."
caption <- "Source: usgs.gov spotted frog data"

frogs_prepared %>% 
  mutate(frog_id = fct_reorder(factor(frog_id), start)) %>% 
  ggplot(aes(x = fct_reorder(factor(frog_id), start),
             y = total_distance,
             fill = start)) +
  geom_col(alpha = .5, width = 0.5) +
  geom_point_svg(aes(y = distance_from_start), svg = svg_txt,
                 size = 5, colour = "black") +
  geom_segment(aes(x = fct_reorder(factor(frog_id), start),
                   xend = fct_reorder(factor(frog_id), start),
                   y = 0, yend = distance_from_start - 15)) +
  geom_text(aes(x = 23.5, y = 1250),
            label = "total distance travelled",
            family = "petrona",
            colour = "gray40") +
  geom_text(aes(x = 23, y = 700),
            label = "distance from start point",
            family = "petrona",
            colour = "gray40") +
  geom_curve(aes(x = 26, xend = 28, y = 1270, yend = 1260),
             curvature = -1,
             colour = "gray40") +
  geom_curve(aes(x = 26, xend = 27.5, y = 690, yend = 670),
             colour = "gray40") +
  scale_y_continuous(
    limits = c(0, 1350),
    expand = c(0, 0),
    breaks = c(0, 250, 500, 750, 1000, 1250),
    labels = number_format(suffix = " m")
  ) +
  scale_fill_discrete(name = "Start Location") +
  labs(x = NULL, 
       y = NULL,
       title = title,
       subtitle = str_wrap(subtitle, 60),
       caption = caption) +
  theme_minimal() +
  theme(text = element_text(family = "petrona"),
        plot.title.position = "plot",
        plot.title = element_text(family = "bilbo",
                                  size = 32,
                                  face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = background))

## saving

ggsave(here::here("plots", yrwk, pdf_file),
      width = 10, height = 10, dpi = 300,
      device = cairo_pdf)

ggsave(here::here("plots", yrwk, png_file),
      width = 10, height = 10, dpi = 300,
      device = "png")
