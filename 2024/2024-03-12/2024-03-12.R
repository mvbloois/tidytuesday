
library(tidyverse)
library(treemapify)
library(showtext)

font_add_google("Truculenta", "truculenta")
font_add_google("Roboto Slab", "robotoslab")
showtext_auto()

fiscal_sponsor_directory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-12/fiscal_sponsor_directory.csv')
cols <- PrettyCols::prettycols("Sea")

bg <- cols[5]
txt <- cols[4]
ln <- cols[2]

data <- fiscal_sponsor_directory |>  
  select(project_types) |> 
  separate_rows(project_types, sep = '\\|') |> 
  mutate(project_types = str_remove(project_types, ":.+"),
         project_types = str_trim(project_types),
         project_types = str_to_lower(project_types),
         project_types = str_replace(project_types, "/|and", " & ")) |>
  drop_na() |>
  count(project_types, sort = TRUE) |> 
  mutate(project_types = if_else(n < 10, 'other', project_types)) |> 
  group_by(project_types) |>
  summarise(n = sum(n)) 

data |> 
  ggplot() +
  geom_treemap(aes(area = n),
               fill = cols[2],
               colour = cols[4],
               show.legend = FALSE) +
  geom_treemap_text(aes(area = n, label = project_types),
                    family = "robotoslab", grow = TRUE, 
                    colour = txt, place = "bottomleft") +
  labs(title = "Fiscal Sponsors",
       subtitle = "Project types supported by 370 fiscal sponsors",
       caption = "Source: Fiscal Sponsor Directory",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    text = element_text(colour = cols[1], lineheight = 0.3),
    plot.title = element_text(family = "truculenta", size = 55),
    plot.subtitle = element_text(family = "robotoslab", size = 25),
    plot.caption = element_text(family = "robotoslab", size = 15),
    plot.background = element_rect(fill = bg),
    plot.margin = margin(15, 15, 15, 15)
  )

ggsave(
  "./2024/2024-03-12/2024-03-12.png",
  bg = "white",
  width = 5,
  height = 3
)
