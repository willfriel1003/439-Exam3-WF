library(tidyverse)

fish <- read_csv("https://raw.githubusercontent.com/rachelss/439_data/main/Copy%20of%20fish_nutrient_excretion_students%20-%20Data%20for%20students.csv")

mt_st_helens_2 <- read_csv("https://raw.githubusercontent.com/rachelss/439_data/main/mt_st_helens_veg_structure_plot_year%20-%20mt_st_helens_veg_structure_plot_year.csv")

mt_st_helens_1 <- read_csv("https://raw.githubusercontent.com/rachelss/439_data/main/mt_st_helens_veg_plots%20-%20mt_st_helens_veg_plots.csv")

library(ggplot2)

ggplot(data = fish, mapping = aes(x = Temperature_C, y = `Per capita N excretion rate`)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(data = fish, mapping = aes(x = `Body mass_g`, y = `Per capita N excretion rate`)) +
  geom_point() +
  geom_smooth(method = lm)

ggplot(data = fish, mapping = aes(x = `Body mass_g`)) +
  geom_histogram(bins = 30)

ggplot(data = fish, mapping = aes(x = factor(Temperature_C), y = `Per capita N excretion rate`)) +
  geom_boxplot()

fish_20.1 <- fish %>%
  filter(Temperature_C == 20.1)

fish_16.4 <- fish %>%
  filter(Temperature_C == 16.4)

ggplot(data = fish_16.4, mapping = aes(x = `Body mass_g`, y = `Per capita N excretion rate`)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Fish at 16.4 degrees celsius")

mean_rate_by_temp <- fish %>%
  group_by(Temperature_C) %>%
  summarize(mean_n_rate = mean(`Per capita N excretion rate`))

ggplot(data = mean_rate_by_temp, mapping = aes(x = Temperature_C, y = mean_n_rate)) +
         geom_point() + 
         geom_smooth(method = "lm")

    
