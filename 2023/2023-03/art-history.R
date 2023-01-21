library(tidyverse)
library(showtext)
library(glue)
library(ggrepel)
library(ggtext)

font_add_google("Playfair Display", "font")
font_add_google("Homemade Apple", "labels")
showtext_auto()
showtext_opts(dpi = 300)

artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

plot_data <- artists %>% 
  filter(book == "Gardner")

impressionists <- plot_data %>% 
  filter(artist_name %in% c("Berthe Morisot",
                            "Édouard Manet",
                            "Claude Monet", 
                            "Auguste Renoir", 
                            "Paul Cézanne",
                            "Edgar Degas",
                            "Mary Cassatt",
                            "Marie Bracquemond")
  )

pal <- c("#1c1e1b","#e48158","#a35666","#4a8fb0","#3b7449","#e4bd3e","#8184b1")

subtitle <- str_wrap("
Berthe Morisot and Mary Cassatt, along with Marie Bracquemont, are known as les trois grandes dames of Impressionism. Cassatt first appeared in the 1980 edition, and Morisot in 1996. Bracquemont is not mentioned.
", 80)

plot_data %>% 
  ggplot(aes(
    x = year, y = space_ratio_per_page_total, 
    group = artist_name)
       ) +
  geom_line(colour = "grey95") +
  geom_line(data = impressionists,
            aes(colour = artist_name),
            show.legend = FALSE) +
   geom_text(data = filter(impressionists, year == max(year) & artist_name != "Mary Cassatt"),
                   aes(x = year + 0.9, y = space_ratio_per_page_total,
                       label = artist_name, colour = artist_name),
                   family = "labels",
                   hjust = "left",
                   size = 3,
                   show.legend = FALSE) +
  geom_text(data = filter(impressionists, year == max(year) & artist_name == "Mary Cassatt"),
            aes(x = year + 0.9, y = space_ratio_per_page_total - 0.15,
                label = artist_name, colour = artist_name),
            family = "labels",
            hjust = "left",
            size = 3,
            show.legend = FALSE) +
   scale_x_continuous(limits = c(1925, 2035),
                      breaks = c(1920,1940,1960,1980,2000,2020),
                      labels = c(1920,1940,1960,1980,2000,2020)) +
   scale_colour_manual(values = pal) +
   labs(x = "year of publication",
        y = "space ratio per page",
        title = "Impressionists in Gardner's Art Through the Ages",
        subtitle = subtitle,
        caption = "Data: {arthistory}: Art History Textbook Data") +
  theme_minimal() +
   theme(
     text = element_text(family = "font"),
     plot.title.position = "plot",
     panel.grid.minor.x = element_blank(),
     plot.title = element_text(hjust = 0.5,
                               size = 24),
     plot.subtitle = element_text(hjust = 0.5)
     )

ggsave("./2023/2023-03/art-history.png", device = "png", dpi = 300,
      width = 8, height = 5, bg = "white")  
