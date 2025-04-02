st_helens <- mt_st_helens_2

ggplot(data = st_helens, mapping = aes(x = year, y = richness)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~ plot_name)

ggplot(data = st_helens, mapping = aes(x = richness)) +
  geom_histogram(bins = 15) +
  facet_wrap(~ plot_name)

ggplot(data = st_helens, mapping = aes(x = factor(year), y = richness)) +
  geom_boxplot()

#Yo

mt_st_helens_1 <- mutate(mt_st_helens_1,
                         plot_name = substr(plot_code, 1, 4),
                         plot_number = substr(plot_code, 5, 6)
                         )

mt_st_helens_1$plot_number <- as.numeric(mt_st_helens_1$plot_number)

sthelens_joined <- mt_st_helens_2 %>%
  inner_join(mt_st_helens_1, by = c("plot_name", "plot_number"))

ggplot(data = sthelens_joined, mapping = aes(x = year, y = richness)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~succession_type)

sthelens_joined_avg <- sthelens_joined %>%
  group_by(plot_name, year, succession_type) %>%
  summarize(mean_richness = mean(richness))

ggplot(sthelens_joined_avg, aes(year, mean_richness)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~succession_type) +
  labs(
    title = "Richness recovery overtime by succession type",
    subtitle = "",
    caption = "The Primary and Secondary plots were more devastated at first, but Primary plots had a quicker regeneration compared           
    to the Secondary plots. The Disturbance plots did not see as much destruction, so they did not have much to regenerate.          "
  )

sthelens_avg_1980 <- sthelens_joined_avg %>%
  filter(year == 1980) %>%
  filter(!is.na(mean_richness)) %>%
  filter(!is.na(succession_type))

summary(lm(mean_richness ~ succession_type * year, data = sthelens_avg_1980))

summary(lm(mean_richness ~ succession_type*year, data = sthelens_joined_avg))

lm_dist_yr_rich <- lm(mean_richness ~ succession_type*year, data = sthelens_joined_avg)

library(tidyverse)
library(moderndive)

get_regression_table(lm_dist_yr_rich)
get_regression_summaries(lm_dist_yr_rich)
