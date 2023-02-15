library(tidyverse)
library(patchwork)
library(showtext)

font_add_google("Karla", "karla")
showtext_auto()
showtext_opts(dpi = 300)

bg <- "#dadada"


theme_set(theme_minimal())
theme_update(text = element_text(family = "karla"),
             plot.title.position = "plot",
             plot.title = element_text(size = 24,
                                       hjust = 0.5),
             plot.subtitle = element_text(size = 16,
                                          hjust = 0.5),
             plot.background = element_rect(fill = bg,
                                            colour = bg)
             )


source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R")

age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv') %>% 
  filter(character_1_gender != character_2_gender) %>% 
  mutate(age_man = ifelse(character_1_gender == "man", actor_1_age, actor_2_age),
         age_woman = ifelse(character_1_gender == "woman", actor_1_age, actor_2_age))

age_gaps %>% 
  summarise(ff = sum(age_woman >= 40) / n(),
            mm = sum(age_man >= 40) / n())

plt_1 <- ggplot(age_gaps) +
  geom_density(aes(x = age_man), 
               colour = "darkgreen",
               fill = "darkgreen",
               alpha = 0.4) +
  geom_density(aes(x = age_woman), 
               colour = "orange",
               fill = "orange",
               alpha = 0.4) +
  geom_text(aes(x = 41, y = 0.048, label = "Female characters"),
            family = "karla",
            size = 3) +
  geom_text(aes(x = 57, y = 0.022, label = "Male characters"),
            family = "karla",
            size = 3) +
  labs(x = "Age",
       y = "Density",
       title = "Only 13% of the female characters is over 40 years old",
       subtitle = "Age distribution of all male and female movie characters") +
  scale_fill_manual(name = "Gender",
                      labels = c("Female", "Male"),
                      values = c("orange", "darkgreen")) +
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8))

age_40_60 <- age_gaps %>%
  filter(age_man >= 40 & age_man <= 60) %>% 
  mutate(col = case_when(age_man < age_woman ~ "orange",
                         age_man - age_woman > 15 ~ "darkgreen",
                         TRUE ~ "black"),
         diff = age_man - age_woman)

plt_2 <- age_40_60 %>%
  ggplot(aes(x = age_man, y =  age_woman)) +
  geom_abline(aes(intercept = 0, slope = 1),
              linetype = "dotted") +
  geom_smooth(size = 1.5, se = FALSE, colour = "orange") +
  geom_jitter(colour = "darkgreen", size = 2,
              height = 0, width = 0.2, alpha = 0.5) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "As male movie characters get older,",
       subtitle = "their female love interests do not",
       x = "Age of male movie character",
       y = "Age of female love interest") +
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 10))

patchwork <- plt_1 + plt_2
patchwork + 
  plot_annotation(
  title = "HOLLYWOOD AGE GAP",
  subtitle = "The Hollywood Age Gap looks at age differences between movie love couples.",
  caption = 'Data: Data Is Plural'
)

ggsave("./2023/2023-07/hollywood-age-gap.png", device = "png", dpi = 300,
       width = 9, height = 5, bg = bg)
