
topic <- "Technology-Adoption"
year <- 2022
week <- 29
yrwk <- glue::glue("{year}-{week}")
plots_dir <- paste0(year, "-", week)
fs::dir_create(here::here("plots", plots_dir))
png_file <- glue::glue("{yrwk}_{topic}.png")
pdf_file <- glue::glue("{yrwk}_{topic}.pdf")

title <- "Vaccination rates in Mali"
subtitle <- "% of children who received an immunization (2019)"
caption <- "Source: data.nber.org"

## packages
library(tidyverse)
library(janitor)
library(scales)
library(ggtext)
library(showtext)

font_add_google(name = "Mali", family = "mali")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

## data
technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv') 

green <- "#14B53A"
yellow <- "#FCD116"
red <- "#CE1126"
blue <- "#68b0e5"

mali <- technology %>% 
  filter(iso3c == "MLI") %>% 
  filter(category == "Vaccines") %>% 
  filter(year == 2019) %>% 
  filter(variable %in% c("BCG", "Pol3", "pctimmunizmeas")) %>% 
  mutate(rank = row_number() + 4,
         value = value / 100) %>% 
  mutate(variable = case_when(variable == "BCG" ~ "BCG",
                                 variable == "Pol3" ~ "Polio",
                                 variable == "pctimmunizmeas" ~ "Measles",
                                 TRUE ~ variable))

ggplot(data = mali, 
       aes(fill = variable)) +
  geom_rect(aes(xmin = rank, xmax = rank + 1, ymin = 0, ymax = value), 
            color = 'white', show.legend = FALSE) +
  geom_text(aes(x = (rank + rank + 2) / 2, y = value,
                label = percent_format()(value)),
            size = 5.5, nudge_y = 0.015, nudge_x = -0.4,
            family = "mali" ,colour = "white") +
  geom_text(aes(x = (rank + rank + 1) / 2, y = .965,
                label = variable),
            size = 5.5, nudge_y = 0.01, nudge_x = -0.01,
            family = "mali" ,colour = "white") +
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = c(red, yellow, green)) +
  xlab('') + ylab('') +
  labs(title = title, subtitle = subtitle, caption = caption) +
  theme_void() +
  theme(text = element_text(family = "mali",
                            colour = "white"),
        plot.background = element_rect(fill = blue,
                                       colour = NA),
        plot.title.position = "plot",
        plot.title = element_text(size = 32, hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(size = 24, hjust = 0.5),
        plot.caption = element_text(size = 14),
        plot.margin = unit(c(1, 2, 1, 2), "cm")
        )

## saving

ggsave(here::here("plots", yrwk, pdf_file),
      width = 10, height = 10, dpi = 300,
      device = cairo_pdf)

ggsave(here::here("plots", yrwk, png_file),
      width = 10, height = 10, dpi = 300,
      device = "png")

