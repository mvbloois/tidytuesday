library(tidyverse)
library(ggtext)
library(glue)
library(showtext)

font_add_google("Fira Sans", "firasans")
sysfonts::font_add(family = "fa-brands", regular = "./img/Font Awesome 6 Brands-Regular-400.otf")
sysfonts::font_add(family = "fa-solid", regular = "./img/fa-solid-900.ttf")
showtext_auto()
showtext_opts(dpi = 300)

txt <- "grey15"

table <- glue("<span style=\"font-family:fa-solid;color:{txt}\">&#xf0ce;</span>")
mastodon <- glue("<span style=\"font-family:fa-brands;color:{txt}\">&#xf4f6;</span>")
twitter <- glue("<span style=\"font-family:fa-brands;color:{txt}\">&#xf099;</span>")
github <- as.character(glue("<span style=\"font-family:fa-brands;color:{txt}\">&#xf09b;</span>"))
caption <- glue("{table} AfriSenti   {mastodon} @martijnvanbloois @fosstodon.org   {twitter} @prancke   {github} mvbloois")

tuesdata <- tidytuesdayR::tt_load(2023, week = 9)

afrisenti <- tuesdata$afrisenti
languages <- tuesdata$languages
language_scripts <- tuesdata$language_scripts
language_countries <- tuesdata$language_countries
country_regions <- tuesdata$country_regions

plotdata <- afrisenti %>% 
  count(language_iso_code, label) %>%
  group_by(language_iso_code) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(pos = max(ifelse(label == "positive", p, 0)),
         lab_pos = case_when(label == "negative" ~ 1 - (p/2),
                             label == "neutral" ~ pos + (p/2),
                             label == "positive" ~ p/2)) %>% 
  inner_join(
    languages,
    by = join_by(language_iso_code)
  ) 

subt <- paste0(
  "Sentiment analysis based on 111.000+ tweets in 14 African languages shows<br>",
  "some interesting differences in ",
  "<span style = 'color:#319400'>positive</span>, ",
  "<span style = 'color:#F7C608'>neutral</span> and ",
  "<span style = 'color:#952038'>negative</span> sentiments."
)

plotdata %>% 
  ggplot(aes(x = fct_reorder(language, pos),
             y = n,
             fill = label)) +
  geom_bar(position = "fill", stat = "identity",
           show.legend = FALSE) +
  geom_text(aes(y = lab_pos,
                label = scales::percent_format(accuracy = 1L)(p)),
            family = "firasans",
            size = 3,
            colour = "grey95") +
  scale_fill_manual(name = "sentiment",
                    values = c("#952038", "#F7C608", "#319400"),
                    guide = guide_legend(reverse = TRUE) ) +
  coord_flip() + 
  labs(x = NULL,
       y = NULL,
       title = "Twitter Sentiment Analysis",
       subtitle = str_wrap(subt, 80),
       caption = caption) +
  theme_minimal() +
  theme(
    text = element_text(family = "firasans",
                        colour = txt),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "firasans",
                               colour = txt,
                               margin = margin(l = 10, r = -5)),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 22,
                              face = "bold",
                              colour = txt,
                              hjust = 0.5),
    plot.subtitle = element_markdown(size = 10,
                                 hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.5)
  )

ggsave("./2023/2023-09/african-language-sentiment.png", device = "png", dpi = 300,
        width = 6, height = 4, bg = "white")

