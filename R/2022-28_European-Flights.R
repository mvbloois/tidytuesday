
topic <- "European-Flights"
year <- 2022
week <- 28
yrwk <- glue::glue("{year}-{week}")
plots_dir <- paste0(year, "-", week)
fs::dir_create(here::here("plots", plots_dir))
png_file <- glue::glue("{yrwk}_{topic}.png")
pdf_file <- glue::glue("{yrwk}_{topic}.pdf")

title <- "Europe's largest airports are approaching pre-covid flight numbers"
subtitle <- "number of monthly commercial flights: <span style='color:#ff7f52;'>actuals</span> versus <span style='color:#208eb1;'>forecasts</span> based on pre-covid numbers"
caption <- "Source: Eurocontrol"

## packages
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(ggtext)
library(tsibble)
library(fable)
library(showtext)
font_add_google(name = "Rubik Mono One", family = "rubik-mono-one")
font_add_google(name = "Heebo", family = "heebo")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

## data
flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

flights_tbl <- flights %>% 
  clean_names() %>% 
  rename(airport_label = pivot_label) %>% 
  select(-ends_with("_2")) 

top_12_icao <- flights_tbl %>% 
  filter(year == 2019) %>% 
  group_by(apt_icao) %>% 
  summarise(flt_tot_1 = sum(flt_tot_1)) %>% 
  arrange(desc(flt_tot_1)) %>% 
  head(12) %>% 
  pull(apt_icao)

top_12_icao <- c(top_12_icao, "LTBA")

flights_top12_tbl <- flights_tbl %>% 
  filter(apt_icao %in% top_12_icao) %>% 
  mutate(apt_icao = ifelse(apt_icao == "LTBA", "LTFM", apt_icao),
         apt_name = case_when(apt_icao == "LTFM" ~ "Istanbul",
                              TRUE ~ apt_name),
         month = floor_date(flt_date, unit = "month"),
         month = yearmonth(month)
         ) %>% 
  group_by(apt_name, month) %>% 
  summarise(flights = sum(flt_tot_1),
            .groups = "drop") %>% 
  as_tsibble(key = apt_name, index = month)

flights_pre_covid <- flights_top12_tbl %>% filter(month <= yearmonth("2020-02-01"))

forecast <- flights_pre_covid %>% 
  model(
    ets = ETS(flights)
  ) %>% 
  forecast(h = "3 years")

flights_combined_tbl <- flights_top12_tbl %>% 
  left_join(
    forecast, by = c("apt_name", "month")
  ) %>% 
  rename(flights_forecast = .mean,
         flights_actual = flights.x) %>% 
  mutate(flights_forecast = ifelse(month == yearmonth("2020 Feb"),
                                   flights_actual, flights_forecast))

blue <- "#026996"
coral <- "#ff7f52"
coralred <- "#ff4141"
yelloworange <- "#ffa443"
cyancornflowerblue = "#208eb1"

## plotting

flights_combined_tbl %>% 
  filter(year(month) > 2017) %>% 
  ggplot(aes(x = month, y = flights_actual)) +
  geom_area(colour = coral,
            fill = coral, alpha =.2) +
  geom_ribbon(aes(ymin = flights_actual, ymax = flights_forecast),
              fill = cyancornflowerblue, alpha = .2) +
  geom_line(aes(y = flights_forecast),
            colour = cyancornflowerblue,
            linetype = "dotted") +
  scale_x_yearmonth(date_labels = "%Y") +
  scale_y_continuous(labels = number_format(scale = 1/1e3, suffix = "k")) +
  labs(title =title,
       subtitle = subtitle,
       caption = caption, 
       x = NULL, y = NULL) +
  facet_wrap(~apt_name) +
  theme_minimal() +
  theme(text = element_text(family = "heebo"),
        axis.text = element_text(colour = blue, size = 10),
        plot.title.position = "plot",
        plot.title = element_markdown(family = "rubik-mono-one",
                                      colour = blue,
                                      face = "bold",
                                      size = 12,
                                      hjust = 0.5,
                                      vjust = 3.5),
        plot.subtitle = element_markdown(colour = blue,
                                         size = 10,
                                         hjust = 0.5,
                                         vjust = 3.5),
        plot.caption = element_text(colour = blue,
                                    size = 12,
                                    vjust = -5),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(colour = blue),
        plot.margin = unit(c(1, 2, 1, 2), "cm"),
        plot.background = element_rect(fill = "#fcf3ea"))

## saving

ggsave(here::here("plots", yrwk, pdf_file),
      width = 10, height = 8, dpi = 300,
      device = cairo_pdf)

ggsave(here::here("plots", yrwk, png_file),
      width = 10, height = 8, dpi = 300,
      device = "png")
