
topic <- "Dutch-Migration"
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
library(cbsodataR)
font_add_google(name = "Mali", family = "mali")
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

results <- cbs_search("immmigration", language = "en")
cbs_get_meta("84978NED")
cbs_get_meta("83518NED")
## data

anthor <- cbs_get_data(id = "83518NED", Geslacht = "T001038", 
             Generatie = c("1012600", "2012605", "2013356", "2013357", 
                           "T001040"), Perioden = c("2021MM05", "2021MM06", "2021MM07", 
                                                    "2021MM08", "2021MM09", "2021MM10", "2021MM11", "2021MM12", 
                                                    "2021JJ00", "2022MM01", "2022MM02", "2022MM03", "2022MM04", 
                                                    "2022MM05"), select = c("Geslacht", "Migratieachtergrond", 
                                                                            "Generatie", "Perioden", "Immigratie_1", "EmigratieInclusiefAdministratieveC_2"
                                                    ))
anthor %>% 
  cbs_add_label_columns() %>% View()

data_raw <-
  cbs_get_data(
    id = "84978NED",
    # Geslacht = "T001038",
    RegioS = "NL01  ",
    Perioden = c(
      "2010JJ00",
      "2011JJ00",
      "2012JJ00",
      "2013JJ00",
      "2014JJ00",
      "2015JJ00",
      "2016JJ00",
      "2017JJ00",
      "2018JJ00",
      "2019JJ00",
      "2020JJ00"
    ),
    select = c(
      "Geslacht",
      "Leeftijd",
      "Geboorteland",
      "Perioden",
      "RegioS",
      "Immigratie_1",
      "EmigratieInclusiefAdministratieveC_2"
    )
  )

data_tbl <- data_raw %>%
  cbs_add_label_columns() %>% 
  select(sex = Geslacht_label,
         age = Leeftijd_label,
         country_of_birth = Geboorteland_label,
         year = Perioden_label,
         region = RegioS_label,
         immigration = Immigratie_1,
         emigration = EmigratieInclusiefAdministratieveC_2) 

View(data_tbl)

c_o_b_exclude <- c("Totaal", "GIPS landen in de EU", "Europese Unie (exclusief Nederland)",
                   "Midden- en Oost-Europese landen in de EU")

data_tbl %>% 
  filter(country_of_birth == "Eritrea") %>% 
  filter(year %in% 2012:2020) %>% 
  filter(!age == "Totaal") %>% 
  filter(!sex == "Totaal mannen en vrouwen") %>% 
  group_by(sex, age, country_of_birth, year) %>% 
  summarise(immigration = sum(immigration, na.rm = TRUE)) %>% 
  pivot_wider(names_from = sex, values_from = immigration) %>% 
  ggplot(aes(x = age, y = -Mannen)) +
  geom_col(fill = "blue") +
  geom_col(aes(y = Vrouwen), fill = "pink") +
  scale_y_continuous(labels = abs) +
  labs(x = "", y = "") +
  coord_flip() +
  facet_wrap(~year) +
  theme_minimal()


data_tbl %>% 
  filter(region == "Nederland") %>% 
  filter(!country_of_birth %in% continents) %>% 
  filter(!country_of_birth %in% c_o_b_exclude) %>% 
  group_by(country_of_birth) %>% 
  mutate(m_i = max(immigration),
         m_e = max(emigration)) %>% 
  View()


unique(data_tbl$country_of_birth)

green <- "#14B53A"
yellow <- "#FCD116"
red <- "#CE1126"
blue <- "#68b0e5"

continents <- c("Afrika", "Amerika", "Azië", "Europa (inclusief Nederland)", "Oceanië")

data_tbl %>% 
  filter(country_of_birth == "Ethiopië") %>% 
  filter(str_detect(region, "\\(PV\\)")) %>% 
  group_by(year, country_of_birth, region) %>% 
  summarise(immigration = sum(immigration),
            emigration = sum(emigration)) %>% 
  ggplot(aes(x = year, y = immigration, fill = country_of_birth)) +
  geom_col() +
  geom_col(aes(y = -emigration)) +
  facet_wrap(~region) +
  theme_minimal()


## saving

ggsave(here::here("plots", yrwk, pdf_file),
      width = 10, height = 10, dpi = 300,
      device = cairo_pdf)

ggsave(here::here("plots", yrwk, png_file),
      width = 10, height = 10, dpi = 300,
      device = "png")

