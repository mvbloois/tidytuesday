# https://github.com/doehm/ggbrick
devtools::install_github("doehm/ggbrick")

library(tidyverse)
library(ggbrick)
library(showtext)

font_add_google("Open Sans", "font")
font_add_google("Bebas Neue", "title")
showtext_auto()

trashwheel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-05/trashwheel.csv')

cols <- c("#4dbd05", "#127cb1", "#9b45a3", "#f18a07")

trashwheel |> 
  mutate(Date = mdy(Date)) |> 
  select(Name, Date, Weight) |> 
  drop_na() |> 
  group_by(Wheel = Name, Year = year(Date)) |> 
  summarise(Weight = sum(Weight/10),
            .groups = "drop") |> 
  arrange(Wheel = factor(Wheel,
                         levels = c("Mister Trash Wheel",
                                    "Gwynnda Trash Wheel",
                                    "Professor Trash Wheel",
                                    "Captain Trash Wheel")
                         )
  ) |> 
  
  ggplot(aes(Year, Weight, fill = Wheel)) +
  geom_brick(gap = 0,
             bricks_per_layer = 4,
             linewidth = 0.1,
             colour = "white") +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(labels = scales::number_format(scale = 10)) +
  coord_brick() +
  labs(x = NULL,
       y = "Tons of trash",
       title = "MR. TRASH WHEEL",
       subtitle = str_wrap("The Baltimore Healthy Harbor initiative has four Trash Wheels collecting trash. Mr. Trash Wheel was the first to start, and since then three more have joined the family. The Trash Wheel Family has collected more than 2,945 tons of trash over the years.", 95),
       caption = "Source: Healthy Harbor Trash Wheel Collection Data") +
  theme_minimal() +
  theme(
    text = element_text(family = "font"),
    plot.title.position = "plot",
    plot.title = element_text(family = "title",
                              size = 52),
    plot.subtitle = element_text(size = 22,
                                 colour = "grey15",
                                 lineheight = 0.35,
                                 margin = margin(0, 0, 15, 0)),
    plot.caption = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.1),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14,
                               vjust = -0.2),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    legend.key.height = unit(8, "pt"),
    plot.margin = margin(10,10,10,10)
    )

ggsave(
  "./2024/2024-03-05/2024-03-05.png",
  bg = "white",
  width = 5,
  height = 3
)

system("open ./2024/2024-03-05/2024-03-05.png")
