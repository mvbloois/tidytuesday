
topic <- "European-Flights"
year <- lubridate::year(Sys.Date())
week <- lubridate::week(Sys.Date())
yrwk <- glue::glue("{year}-{week}")
plots_dir <- paste0(year, "-", week)
fs::dir_create(here::here("plots", plots_dir))
png_file <- glue::glue("{yrwk}_{topic}.png")
pdf_file <- glue::glue("{yrwk}_{topic}.pdf")


## packages
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(ggtext)
library(sf)
library(glue)
library(patchwork)
library(showtext)
font_add_google(name = "Libre Baskerville", family = "baskerville")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

airports <- readr::read_csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
                            col_names = c("airport_id", "name", "city", "country", "iata", "icao",
                                          "latitude", "longitude", "altitude", "timezone", "dst",
                                          "tz_db_tz", "type", "source"))

flights_tbl <- flights %>% 
  clean_names() %>% 
  rename(airport_label = pivot_label) %>% 
  select(-ends_with("_2")) %>% 
  left_join(airports, by = c("apt_icao" = "icao")) %>% 
  filter(is.na(name))
 
unique(flights_tbl$apt_icao)

airports %>% 
  filter(country == "Spain") %>% View()


flights_ctry_mth <- flights_tbl %>%  
  group_by(country, year, month_num) %>% 
  summarise(total_movements = sum(flt_tot_1),
            .groups = "drop")

flights_ctry_mth %>% 
  filter(year >= 2019) %>% 
  group_by(country, month_num) %>% 
  mutate(date = paste(year, month_num),
         movement_index = total_movements / total_movements[year == 2019] * 100) %>% 
  filter(year > 2019) %>% 
  ggplot(aes(x = date, y = movement_index, fill = movement_index)) +
  geom_hline(yintercept = 100, colour = "red", linetype = 2) +
  geom_col(position = "dodge", alpha = .6) +
  facet_wrap(~country) +
  theme_minimal()

flights_ctry_mth %>% 
  filter(year >= 2019) %>% 
  group_by(country, month_num) %>% 
  mutate(date = paste(year, month_num),
         movement_index = total_movements - total_movements[year == 2019])  %>% 
  filter(year > 2019) %>% 
  ggplot(aes(x = date, y = movement_index, fill = movement_index)) +
  geom_hline(yintercept = 100, colour = "red", linetype = 2) +
  geom_col(position = "dodge", alpha = .6) +
  facet_wrap(~country) +
  theme_minimal()

skimr::skim(flights)

library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-25, 54.12), ylim = c(35.5, 73.97), expand = FALSE) +
  theme_minimal()

#crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ",

colours <- c(`2022` = "#C8102E",
   `2018` = "#f57d90")

blue <- "#012169"
green <- "#00B140"
# darkblue: #0c1454
# yellow: #f0dd90
# darkyellow: #e3c139

title <- "GENDER   PAY   GAP   WIDENS"
subtitle <- "in most regions between <span style='color:#f57d90;'>2018</span> and <span style='color:#C8102E;'>2022</span>"
caption <- "Source: gender-pay-gap.service.gov.uk"

plt_1 <- ggplot(paygap_regional, aes(x = median, y = region_rnk)) +
  geom_vline(data = paygap_national,
             aes(xintercept = median, colour = factor(year)),
             linetype = 2,
             show.legend = FALSE) +
  geom_vline(aes(xintercept = 0), colour = green, linetype = 2, size = 1.2) +
  geom_label(aes(x = 0, y = 12, label = "Equal Pay"),
             colour = green,
             family = "baskerville",
             fontface = "italic",
             fill = "#eff2f7") +
  geom_label(aes(x = minimum - 3.7, y = 0, label = lbl_nat_min),
             colour = "#f57d90",
             family = "baskerville",
             fontface = "italic",
             fill = "#eff2f7") +
  geom_label(aes(x = maximum + 3.7, y = 0, label = lbl_nat_max),
             colour = "#C8102E",
             family = "baskerville",
             fontface = "italic",
             fill = "#eff2f7") +
  geom_line(aes(group = region_rnk, colour = line_col), size = 1.5,
            show.legend = FALSE) + 
  geom_point(aes(colour = factor(year)), size = 4,
             show.legend = FALSE) +
  geom_label(data = paygap_regional %>% group_by(region_name) %>%
              filter(median == min(median)) %>%
              filter(median < minimum),
            aes(x = median - 0.5, y = region_rnk,
            label = region_name), hjust = "right",
            family = "baskerville", colour = blue, size = 8,
            fill = "#eff2f7", label.size = NA) +
  geom_label(data = paygap_regional %>% group_by(region_name) %>%
              filter(median == max(median)) %>%
              filter(median > maximum + 1),
            aes(x = median + 0.5, y = region_rnk,
            label = region_name), hjust = "left",
            family = "baskerville", colour = blue, size = 8,
            fill = "#eff2f7", label.size = NA) +
  scale_x_continuous(
    limits = c(-5, NA),
    labels = number_format(suffix = "%", style_positive = "plus"),
    expand = expansion(add = c(0,5))
    ) +
  scale_colour_manual(values = colours) +
  labs(title = title,
       subtitle = subtitle,
       caption = caption,
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "baskerville"),
        axis.text = element_text(colour = blue, size = 15),
        plot.title.position = "plot",
        plot.title = element_markdown(colour = blue,
                                      face = "bold",
                                      size = 28,
                                      hjust = 0.5,
                                      vjust = 3.5),
        plot.subtitle = element_markdown(colour = blue,
                                         size = 24,
                                         hjust = 0.5,
                                         vjust = 3.5),
        plot.caption = element_text(colour = blue,
                                    size = 12,
                                    vjust = -5),
        axis.text.y = element_blank(),
        axis.text.x = element_text(vjust = -3),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin = unit(c(1, 2, 1, 2), "cm"),
        plot.background = element_rect(fill = "#eff2f7"))


plt_1 + plot_layout(ncol = 1)

ggsave(here::here("plots", yrwk, pdf_file),
      width = 10, height = 8, dpi = 300,
      device = cairo_pdf)

ggsave(here::here("plots", yrwk, png_file),
      width = 10, height = 8, dpi = 300,
      device = "png")
