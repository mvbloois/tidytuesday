library(tidyverse)
library(showtext)

font_add_google("Karla", "font")
showtext_auto()
showtext_opts(dpi = 300)

pal <- c("darkgreen", "darkred", "darkblue")

drugs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')

atc_tbl <- tribble(~atc, ~atc_desc, 
                   "A","ALIMENTARY TRACT AND METABOLISM",
                   "B","BLOOD AND BLOOD FORMING ORGANS",
                   "C","CARDIOVASCULAR SYSTEM",
                   "D","DERMATOLOGICALS",
                   "G","GENITO URINARY SYSTEM AND SEX HORMONES",
                   "H","SYSTEMIC HORMONAL PREPARATIONS",
                   "J","ANTIINFECTIVES FOR SYSTEMIC USE",
                   "L","ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS",
                   "M","MUSCULO-SKELETAL SYSTEM",
                   "N","NERVOUS SYSTEM",
                   "P","ANTIPARASITIC PRODUCTS",
                   "R","RESPIRATORY SYSTEM",
                   "S","SENSORY ORGANS",
                   "V","VARIOUS")

plot_data <- drugs %>% 
  filter(category == "human") %>% 
  mutate(atc = str_sub(atc_code, 1, 1))  %>% 
  group_by(atc, authorisation_status) %>%
  summarise(count = n()) %>% 
  inner_join(atc_tbl, by = join_by(atc))
  
  
plot_data %>% 
  ggplot(aes(x = authorisation_status, y = fct_rev(atc_desc))) +
  geom_point(aes(size = count, colour = authorisation_status),
             show.legend = FALSE) +
  geom_text(aes(label = count),
            family = "font",
            colour = "white",
            size = 3) +
  scale_size_continuous(range = c(5, 12)) +
  scale_colour_manual(values = pal) +
  scale_x_discrete(position = "top") +
  labs(title = "EUROPEAN DRUG DEVELOPMENT",
       subtitle = str_wrap("Number of centrally registered marketing authorisations of human medicines between 1995 and 2023, split by therapeutic area and authorisation status.", 80),
       caption = "Data: European Medicines Agency",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    text = element_text(family = "font"),
    plot.title.position = "plot",
    plot.title = element_text(family = "font",
                              face = "bold",
                              size = 24)
  )

ggsave("./2023/2023-11/european-drug-development.png", device = "png", dpi = 300,
       width = 6, height = 7, bg = "white")  

system("open ./2023/2023-11/european-drug-development.png")

