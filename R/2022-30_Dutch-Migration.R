
topic <- "Dutch-Migration"
year <- 2022
week <- 30
yrwk <- glue::glue("{year}-{week}")
plots_dir <- paste0(year, "-", week)
fs::dir_create(here::here("plots", plots_dir))
png_file <- glue::glue("{yrwk}_{topic}.png")
pdf_file <- glue::glue("{yrwk}_{topic}.pdf")

## packages
library(tidyverse)
library(scales)
library(ggtext)
library(showtext)
library(cbsodataR)
library(glue)
library(patchwork)

font_add_google(name = "Tajawal", family = "domine")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

#results <- cbs_search("immmigration", language = "nl")
#cbs_get_meta("85287NED")

## DATA ----

### * World ----

continents_raw <-
  cbs_get_data(
    id = "85287NED",
    Geslacht = "T001038",
    LeeftijdOp31December = "10000", 
    BurgerlijkeStaat = "T001019",
    Geboorteland = c("G008519", "G008520", "G008524", "G008525", "G008531", "G008691"),
    select = c("Geslacht", "LeeftijdOp31December",
               "BurgerlijkeStaat", "Geboorteland", "Perioden", "Immigratie_1", 
               "EmigratieInclusiefAdministratieveC_2")
    )

continents_tbl <- continents_raw %>% 
  cbs_add_label_columns() %>% 
  select(year = Perioden_label,
         continent = Geboorteland_label,
         immigration = Immigratie_1,
         emigration = EmigratieInclusiefAdministratieveC_2) %>% 
  pivot_longer(
    cols = immigration:emigration,
    names_to = "type",
    values_to = "number"
  ) %>% 
  pivot_wider(
    names_from = "continent",
    values_from = "number"
  ) %>% 
  rename(Africa = 3,
         Americas = 4,
         Asia = 5,
         Europe = 6,
         Oceania = 7) %>% 
  mutate(Europe = Europe - Nederland) %>% 
  select(-Nederland) %>% 
  pivot_longer(
    cols = Africa:Oceania,
    names_to = "continent",
    values_to = "number"
  ) %>% 
  pivot_wider(
    names_from = type,
    values_from = number
  ) %>% 
  mutate(year = as.numeric(as.character(year)),
         continent = factor(continent,
                            levels = c("Oceania", "Americas", "Africa", "Asia", "Europe"))
         ) %>% 
  filter(year >= 2000)

world_tbl <- continents_tbl %>% 
  group_by(year) %>% 
  summarise(across(immigration:emigration, sum)) %>% 
  mutate(net_migration = immigration-emigration)
 
### * Countries ----

countries_raw <-
  cbs_get_data(
    id = "85287NED",
    Geslacht = "T001038",
    LeeftijdOp31December = "10000", 
    BurgerlijkeStaat = "T001019",
    #Geboorteland = c("G008519", "G008520", "G008524", "G008525", "G008531"),
    select = c("Geslacht", "LeeftijdOp31December",
               "BurgerlijkeStaat", "Geboorteland", "Perioden", "Immigratie_1", 
               "EmigratieInclusiefAdministratieveC_2")
  )

countries_tbl <- countries_raw %>% 
  filter(Geboorteland >= "G008533", Geboorteland < "T001175") %>% 
  cbs_add_label_columns() %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  select(year = Perioden_label,
         country = Geboorteland_label,
         immigration = Immigratie_1,
         emigration = EmigratieInclusiefAdministratieveC_2) %>% 
  filter(year >= 2000)

top_ten_tbl <- countries_tbl %>% 
  group_by(country) %>% 
  summarise(across(immigration:emigration, sum)) %>% 
  mutate(net_migration = immigration-emigration)

top_tens_tbl <- 
  bind_rows(
top_immigration <- top_ten_tbl %>% 
  filter(country != "Nederland") %>% 
  arrange(desc(immigration)) %>% 
  head(10) %>% 
  mutate(type = "immigration") %>% 
  select(country, type, number = immigration)
,
top_emigration <- top_ten_tbl %>% 
  filter(country != "Nederland") %>% 
  arrange(desc(emigration)) %>% 
  head(10) %>% 
  mutate(type = "emigration") %>% 
  select(country, type, number = emigration)
,
top_net <- top_ten_tbl %>% 
  filter(country != "Nederland") %>% 
  arrange(desc(abs(net_migration))) %>% 
  head(10) %>% 
  mutate(type = "net migration") %>% 
  select(country, type, number = net_migration)
) %>% 
  mutate(country = case_when(country == "Polen" ~ "Poland",
                             country == "Duitsland" ~ "Germany",
                             country == "Syrië" ~ "Syria",
                             country == "Verenigd Koninkrijk" ~ "UK",
                             country == "Bulgarije" ~ "Bulgaria",
                             country == "Turkije" ~ "Turkey",
                             country == "Verenigde Staten van Amerika" ~ "USA",
                             country == "Roemenië" ~ "Romania",
                             country == "Spanje" ~ "Spain",
                             country == "Frankrijk" ~ "France",
                             country == "Italië" ~ "Italy",
                             country == "België" ~ "Belgium",
                             country == "Marokko" ~ "Morocco",
                             TRUE ~ country))

## Colours ----

background <- "#474B4F"
col <- "#648381"
line <- "#648381"
text <- "#E4FDE1"
h1 <- "#8ACB88"
h2 <- "#FFBF46"
## Plots ----
 
plt_1 <- world_tbl %>% 
  ggplot(aes(x = year, y = net_migration)) +
  geom_col(fill = col) +
  geom_line(aes(y = immigration), size = 2, colour = h1) +
  geom_line(aes(y = emigration), size = 2, colour = h2) +
  geom_text(aes(x = 2013, y = 200000, label = "immigration"),
            family = "domine", colour = h1,
            size = 8) +
  geom_text(aes(x = 2017, y = 135000, label = "emigration"),
            family = "domine", colour = h2,
            size = 8) +
  geom_text(aes(x = 2012, y = 60000, label = "net migration"),
            family = "domine", colour = col,
            size = 6) +
  scale_x_continuous(breaks = c(1995,2000,2005,2010, 2010, 2015, 2020)) +
  scale_y_continuous(labels = number_format(accuracy = 1L)) +
  labs(x = NULL, y = NULL,) +
  theme_minimal() +
  theme(text = element_text(family = "domine",
                            size = 14),
        plot.title.position = "plot",
        axis.line = element_line(colour = line),
        panel.grid = element_blank(),
        axis.text = element_text(family = "domine",
                                 colour = text)
        
  )

plot_top_ten <- function(df) {
  
  type <- unique(df$type)
  title = glue("Contributors to {type}")
  
  ggplot(data = df,
         aes(x = fct_reorder(country, number), y = number)) +
  geom_col(fill = col) +
  scale_y_continuous(limits = c(0, 4e5),
                     labels = number_format(scale = 1/1e3, suffix = "k")) +
  labs(x = NULL, y = NULL, title = title) +
  coord_flip() +
  theme_minimal() +
    theme(text = element_text(family = "domine",
                              colour = text,
                              size = 14),
          plot.title.position = "plot",
          axis.line = element_line(colour = line),
          axis.text = element_text(family = "domine",
                                   colour = text),
          panel.grid = element_blank())
}

plt_2 <- top_tens_tbl %>% filter(type == "immigration") %>% plot_top_ten()
plt_3 <- top_tens_tbl %>% filter(type == "emigration") %>% plot_top_ten()
plt_4 <- top_tens_tbl %>% filter(type == "net migration") %>% plot_top_ten()

## saving
plt_1 / (plt_2 + plt_3 + plt_4) + plot_layout(ncol = 1, widths = c(1, 2)) +
  plot_annotation(title = "Immigration to and emigration from the Netherlands",
                  subtitle = "by country of birth, excluding Dutch nationals",
                  caption = "Source: dataset 85287NED; CBS.nl",
                  theme = theme(text = element_text(family = "domine",
                                                    colour = text),
                                plot.title = element_text(size = 26,
                                                          hjust = 0.5),
                                plot.subtitle = element_text(size = 22,
                                                             hjust = 0.5),
                                plot.background = element_rect(fill = background,
                                                               colour = NA)
                                )
                  )
  


ggsave(here::here("plots", yrwk, pdf_file),
      width = 10, height = 10, dpi = 300,
      device = cairo_pdf)

ggsave(here::here("plots", yrwk, png_file),
      width = 10, height = 10, dpi = 300,
      device = "png")
