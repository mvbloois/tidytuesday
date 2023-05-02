
library(tidyverse)
library(ggridges)
library(ggthemes)
library(ggtext)
library(glue)
library(janitor)
library(showtext)

font_add_google("Fira Sans", "font")
showtext_auto()

bg <- "#EFEFEF"
fg <- "#123456"

## plot ------
plots <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/plots.csv')
species <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/species.csv')
surveys <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/surveys.csv')

df <- surveys %>% 
  filter(! is.na(species)) %>% 
  inner_join(
    species,
    by = join_by(species)
  ) %>% 
  mutate(
    colour = fg,
    label = glue("<b style='color:{colour}'>{commonname}</b><br/><i style='color:{colour};font-size:20px;'>{scientificname}</i>")) %>% 
  select(species, label, censusdate)
  
df %>% 
  ggplot(aes(x = censusdate, y = fct_rev(label), fill = species)) +
  geom_density_ridges(scale = 2.5, size = 0.25, rel_min_height = .01,
                      alpha = 0.5) +
  #scale_x_date(date_breaks = c("1980","1990","2000","2010","2020")) +
  scale_fill_tableau(palette = "Tableau 20") +
  labs(
    x = NULL,
    y = NULL,
    title = "Of mice and rats",
    subtitle = str_wrap("The Portal Project is a long-term ecological study being conducted near Portal, AZ. Since 1977, the site has been used to study the interactions among rodents, ants and plants and their respective responses to climate. To study the interactions among organisms, they experimentally manipulate access to 24 study plots.", 85),
    caption = "Data: Portal Project"
  ) +
  guides(
    fill = "none"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "font",
                        colour = fg,
                        size = 30),
    axis.text.y = element_markdown(lineheight = 0.2,
                                   vjust = 0.1),
    axis.text.x = element_text(size = 40,
                               colour = fg),
    panel.grid.major.y = element_line(colour = "grey90"),
    plot.title.position = "plot",
    plot.title = element_text(size = 80),
    plot.subtitle = element_text(face = "italic",
                                 size = 30,
                                 lineheight = 0.3,
                                 margin = margin(5,10,15,10)),
    plot.margin = margin(20,20,20,20)
  )


ggsave("./2023/2023-18/portal-project.png",
       height = 8,
       width = 6,
       bg = bg)
system("open ./2023/2023-18/portal-project.png")


  
