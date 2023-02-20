library(tidyverse)
library(tidytext)
library(ggtext)
library(glue)
library(showtext)

devtools::install_github("frankiethull/BobRossColors")
library(BobRossColors)

brc <- unique_bob_ross_colors 

font_add(family = "korinna", regular = "./img/Korinna-Regular.otf")
sysfonts::font_add(family = "fa-brands", regular = "./img/Font Awesome 6 Brands-Regular-400.otf")
sysfonts::font_add(family = "fa-solid", regular = "./img/fa-solid-900.ttf")
showtext_auto()
showtext_opts(dpi = 300)

bg <-  "#0e1111"
tw <- "#F3F4F7"
txt <- "#565858"

table <- glue("<span style=\"font-family:fa-solid;color:{txt}\">&#xf0ce;</span>")
mastodon <- glue("<span style=\"font-family:fa-brands;color:{txt}\">&#xf4f6;</span>")
twitter <- glue("<span style=\"font-family:fa-brands;color:{txt}\">&#xf099;</span>")
github <- as.character(glue("<span style=\"font-family:fa-brands;color:{txt}\">&#xf09b;</span>"))
caption <- glue("{table} jwilber/Bob_Ross_Paintings {mastodon} @martijnvanbloois @fosstodon.org {twitter} @prancke {github} mvbloois")


bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')

colors <- bob_ross %>% 
  summarise(across(where(is.logical), sum)) %>% 
  pivot_longer(cols = everything(), names_to = "color", values_to = "frequency") %>% 
  mutate(color_name = str_replace_all(color, "_", " "),
         color = str_remove_all(color, "_")) %>% 
  inner_join(brc, by = join_by(color)) %>% 
  filter(!color_hex %in% c("#000000", "#FFFFFF")) %>% 
  arrange(desc(frequency))

topics <- bob_ross %>% 
  select(painting_title) %>% 
  unnest_tokens(word, painting_title) %>% 
  anti_join(stop_words,
            by = join_by(word)) %>% 
  count(word, sort = TRUE) %>% 
  head(13) %>% 
  bind_cols(colors)

theme_set(theme_minimal())
theme_update(text = element_text(family = "korinna",
                                 colour = bg),
             plot.title.position = "plot",
             plot.title = element_text(size = 24,
                                       hjust = 0.5),
             plot.subtitle = element_text(size = 16,
                                          hjust = 0.5),
             plot.background = element_rect(fill = tw,
                                            colour = tw),
             plot.caption.position = "plot",
             plot.caption = element_markdown(hjust = 0.5),
             axis.text = element_text(colour = bg),
             panel.grid.major.y = element_blank(),
             panel.grid.major.x = element_line(colour = bg),
             panel.grid.minor.x = element_blank()
)

ggplot(data = topics) +
  geom_col(aes(x = n, y = fct_reorder(word, n),
               fill = color_hex)) +
  geom_text(aes(x = n - 0.5,
                y = as.numeric(fct_reorder(word, n)) - 0.2,
                label = color_name),hjust = "right",
            size = 1,
            colour = tw,
            fontface = "bold",
            family = "korinna") +
  geom_textbox(aes(x = 30,
                   y = 6,
                   label = "Mountains are Bob Ross' favorite subject based on text analysis of the titles of 403 paintings made for the tv series The Joy of Painting."),
               fill = tw,
               colour = bg,
               size = 3.5,
               family = "korinna") +
  scale_fill_identity() +
  labs(y = NULL,
       x = "frequency",
       title = "THE TOPICS OF PAINTING",
       subtitle = "by BOB ROSS",
       caption = caption)

ggsave("./2023/2023-08/bob-ross-paintings.png", device = "png", dpi = 300,
        width = 6, height = 4, bg = tw)

