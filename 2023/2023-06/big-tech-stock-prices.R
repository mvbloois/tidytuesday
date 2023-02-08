library(tidyverse)
library(gghighlight)
library(showtext)

font_add_google("Fira Code", "fira code")
showtext_auto()
showtext_opts(dpi = 300)

red <- "#bf0000"

big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

big_tech_stock_prices %>% 
  filter(!stock_symbol %in% c("IBM", "ORCL")) %>% 
  inner_join(big_tech_companies,
             by = "stock_symbol") %>% 
  ggplot() +
  geom_line(aes(x = date, y = close, group = stock_symbol),
            linewidth = 0.8,
            colour = red) +
  gghighlight(unhighlighted_params = list(colour = "grey25",
                                          linewidth = 0.4),
              use_direct_label = FALSE) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$ ")) +
  facet_wrap(~company) +
  labs(x = NULL,
       y = "closing price",
       title = "BIG TECH STOCK PRICES",
       subtitle = "of various big tech companies",
       caption = "Data: Kaggle") +
  theme_minimal() +
  theme(
    text = element_text(family = "fira code",
                        colour = red),
    plot.background = element_rect(fill = "#293133"),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5,
                              face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour = red,
                                      linewidth = 0.1,
                                      linetype = "dotted"),
    strip.text = element_text(colour = red,
                              face = "bold",
                              size = 10),
    axis.text = element_text(colour = red),
    axis.text.y = element_text(vjust = -0.1),
  )

ggsave("./2023/2023-06/big-tech-stock-prices.png", device = "png", dpi = 300,
      width = 8, height = 5, bg = "#293133")
