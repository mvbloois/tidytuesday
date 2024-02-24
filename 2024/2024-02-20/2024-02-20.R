
library(tidyverse)
library(ggtext)
library(showtext)

font_add_google("Fira Sans", "font")
showtext_auto()

cols <- c("#0056B9", "#FFD800")
dark <- "#3E3E42"

isc_grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-20/isc_grants.csv')

plot_data <- isc_grants %>% 
  group_by(year, group) %>% 
  summarise(funded = sum(funded)) %>% 
  mutate(round = if_else(group == 1, "spring", "autumn"))

subtitle <- paste0(
  "The amount funded between 2016 and 2023 split by <p>the ",
  "<span style = 'color:#FFD800'>spring</span> and the ",
  "<span style = 'color:#0056B9'>autumn</span> cycle.<p>"
)


plot_data %>% 
  ggplot(aes(x = year, y = funded, fill = round)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = 2016:2023) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_manual(values = cols) +
  labs(x = NULL, y = NULL,
       title = "Awarded ISC Grants",
       subtitle = subtitle,
       caption = "Data: R Consortium Infrastructure Steering Committee") +
  theme_minimal() +
  theme(text = element_text(family = "font", size = 30,
                            colour = "grey85"),
        plot.background = element_rect(fill = dark),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(vjust = 6),
        plot.caption = element_text(vjust = 5),
        axis.text.x = element_text(colour = dark,
                                 vjust = 12),
        axis.text.y = element_text(colour = "grey75",
                                   vjust = -0.005),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.1),
        plot.margin = margin(10,10,10,10)
        )

ggsave(
  "./2024/2024-02-20/2024-02-20.png",
  bg = dark,
  width = 4,
  height = 4
)
