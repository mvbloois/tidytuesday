library(tidyverse)
library(terra)
library(tidyterra)
library(lubridate)
library(patchwork)
library(cropcircles)
library(ggimage)
library(magick)
library(showtext)

font_add_google("Inter", "font")
showtext_auto()
showtext_opts(dpi = 300)

### Logo ----

logo <- image_read(hex_crop("./img/numbat.jpg", border_size = 0))


# get dims of the logo
logo_width <- magick::image_info(center)$width
logo_height <- magick::image_info(center)$height

### Data ----

numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')

numbats_points <- numbats %>% 
  count(decimalLongitude, decimalLatitude) %>% 
  drop_na()

numbats_months <- numbats %>% 
  count(month) %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  drop_na()

numbats_days <- numbats %>% 
  count(day = wday) %>% 
  mutate(day = factor(day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% 
  drop_na()

## Map courtesy of Benjamin Nowak ----
## https://twitter.com/BjnNowak/status/1633010135965597698

ndvi <- rast("https://t.co/o4oUcFrOqK")

pal_ndvi <- c(
  "#6E462C", "#CCCC66",
  "#25A244", "#208B3A",
  "#1A7431", "#10451D"
)

map <- ggplot() +
  geom_spatraster(
    data = ndvi %>% filter(NDVI > 0),
    aes(fill = NDVI),
    alpha = 0.6,
    na.rm = TRUE
  ) +
  geom_jitter(data = numbats_points,
             aes(x = decimalLongitude, y = decimalLatitude,
                 size = n), colour = "red", alpha = 0.6) +
  scale_fill_gradientn(
    colours = pal_ndvi,
    na.value = NA
  ) +
  guides(fill = "none",
         size = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(fil = "#FFFFFF",
                                   colour =  NA)
  )

## Charts ----

plt_1 <- ggplot(numbats_months) +
  geom_col(aes(x = month, y = n), fill = "#846050") +
  labs(x = NULL,
       y = NULL,
       title = "Most sightings in November and December") +
  theme_minimal() +
  theme(
    text = element_text(family = "font"),
    plot.title.position = "plot",
    plot.title = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
        )

plt_2 <- ggplot(numbats_days) +
  geom_col(aes(x = day, y = n), fill = "#846050") +
  labs(x = NULL,
       y = NULL,
       title = "All days are good for sightings, but Monday's best") +
  theme_minimal() +
  theme(
    text = element_text(family = "font"),
    plot.title.position = "plot",
    plot.title = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

pw <- ((plt_1 / plt_2) | map) +
  plot_annotation(
    title = "Numbat sightings in Australia",
    subtitle = str_wrap("The numbat (Myrmecobius fasciatus) is an insectivorous marsupial. It is diurnal and its diet consists almost exclusively of termites. The species was once widespread across southern Australia, but is now restricted to several small colonies in Western Australia.", 100),
    caption = "Data: Atlas of Living Australia",
    theme = theme(
      plot.title = element_text(size = 20,
                                vjust = 7,
                                face = "bold"),
      plot.subtitle = element_text(size = 8,
                                   vjust = 3)
    )
  ) &
  theme(
    text = element_text(family = "font")
  )

cowplot::ggdraw(pw) +
  cowplot::draw_image(logo, x = 1, y = 1, hjust = 1, vjust = 1, width = 0.13, height = 0.2)

ggsave("./2023/2023-10/numbats.png", device = "png", dpi = 300,
       width = 7, height = 5, bg = "white")

