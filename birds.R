library(tidyverse)
library(ggplot2)
library(covidcast)

recent_bird_subset <- read_csv("https://tinyurl.com/recent-bird-subset")
bird_info <- read_csv("https://tinyurl.com/bird-info")
sites_info <- read_csv("https://tinyurl.com/observation-site-info")


hummingbirds_from_subset <- filter(recent_bird_subset, species_code == "rthhum")

process_chunk <- function(df, pos) {
  filter(df, SPECIES_CODE == "rthhum")
}

hummers <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_1988_1995_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

hummers2 <- read_csv_chunked(
    file = "../../shared/439539/birds/full/PFW_1996_2000_public.csv",
    callback = DataFrameCallback$new(process_chunk),
    chunk_size = 10000
  )

all_hummers <- bind_rows(hummers, hummers2)  


process_chunk <- function(df, pos) {
  filter(df, SPECIES_CODE %in% c("blujay", "norcar"))
}

blue_red1 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2001_2005_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

blue_red2 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2006_2010_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

blue_red3 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2011_2015_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

blue_red4 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2016_2020_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

blue_red5 <- recent_bird_subset %>%
  filter(species_code %in% c("blujay", "norcar"), subnational1_code == "US-RI") %>%
  rename_with(toupper)

blue_red <- bind_rows(blue_red1, blue_red2, blue_red3, blue_red4) %>%
  rename_with(toupper)

blue_redx <- bind_rows(blue_red, blue_red5)

blue_red_ri <- blue_redx %>%
  filter(SUBNATIONAL1_CODE == "US-RI", VALID == 1, SPECIES_CODE %in% c("blujay", "norcar"), YEAR > 2000, YEAR < 2021) %>%
  group_by(YEAR, SPECIES_CODE) %>%
  summarize(COUNT = n(), .groups = "drop") %>%
  arrange(YEAR, SPECIES_CODE)

ggplot(data = blue_red_ri, aes(x = YEAR, y = COUNT, color = SPECIES_CODE)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(values = c("blujay" = "blue", "norcar" = "red"),
                     labels = c("Blue Jay", "Northern Cardinal")) +  geom_smooth(method = lm, se = FALSE) +
  labs(x = "Year", y = "Count", color = "Species")

summary(lm(COUNT ~ SPECIES_CODE*YEAR, data = blue_red_ri))

## =====================================================================

process_chunk <- function(df, pos) {
  filter(df, SPECIES_CODE == "rethaw")
}

hawks1 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_1988_1995_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

hawks2 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_1996_2000_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

hawks3 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2001_2005_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

hawks4 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2006_2010_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)
  
hawks5 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2011_2015_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

hawks6 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2016_2020_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

hawks7 <- recent_bird_subset %>%
  filter(species_code == "rethaw") %>%
  rename_with(toupper)

hawks <- bind_rows(hawks1, hawks2, hawks3, hawks4, hawks5, hawks6) %>%
  rename_with(toupper)

hawks <-  bind_rows(hawks, hawks7)

sites_info <- sites_info %>%
  rename_with(toupper)

hawks_joined <- hawks %>%
  left_join(sites_info, by = "LOC_ID")

hawks_joined_new <- hawks_joined %>%
  filter(VALID == 1, !is.na(SQUIRRELS)) %>%
  group_by(YEAR) %>%
  summarize(
    percent_squirrels = mean(SQUIRRELS == 1, na.rm = TRUE) * 100,
    inverse_squirrels = mean(SQUIRRELS == 0, na.rm = TRUE) * 100
  )

ggplot(data = hawks_joined_new, aes(YEAR, percent_squirrels)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  labs(x = "Year", y = "Percentage of Red-Tailed Hawk spottings Near Squirrels")

summary(lm(percent_squirrels ~ YEAR, data = hawks_joined_new))

## ===============================================================================

process_chunk <- function(df, pos) {
  filter(df, SPECIES_CODE == "eucdov")
}

doves1 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_1988_1995_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

doves2 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_1996_2000_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

doves3 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2001_2005_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

doves4 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2006_2010_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

doves5 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2011_2015_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

doves6 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2016_2020_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

doves7 <- recent_bird_subset %>%
  filter(species_code == "eucdov") %>%
  rename_with(toupper)

doves <- bind_rows(doves1, doves2, doves3, doves4, doves5, doves6) %>%
  rename_with(toupper)

doves <-  bind_rows(doves, doves7)

doves_grouped <- doves %>%
  filter(VALID == 1, YEAR < 2021) %>%
  group_by(YEAR, SPECIES_CODE)

ggplot(data = doves_grouped, aes(YEAR)) +
  geom_bar(fill = 'blue') +
  theme_bw() +
  labs(x = "Year", y = "Count")

doves_summarized <- doves_grouped %>%
  count(YEAR, name = "COUNT")

summary(lm(COUNT ~ YEAR, data = doves_summarized))


## ==========================================================================

process_chunk <- function(df, pos) {
  filter(df, SPECIES_CODE == "baleag")
}

eagles1 <- read_csv_chunked(
  file = "../../shared/439539/birds/full/PFW_2016_2020_public.csv",
  callback = DataFrameCallback$new(process_chunk),
  chunk_size = 10000
)

eagles2 <- recent_bird_subset %>%
  filter(species_code == "baleag") %>%
  rename_with(toupper)

eagles <- eagles1 %>%
  rename_with(toupper)

eagles <-  bind_rows(eagles, eagles2)

eagles_mutated <- eagles %>%
  mutate(
    COUNTRY = substr(SUBNATIONAL1_CODE, 1, 2),
    STATE = substr(SUBNATIONAL1_CODE, nchar(SUBNATIONAL1_CODE) - 1, nchar(SUBNATIONAL1_CODE))
  ) %>%
  group_by(COUNTRY, STATE) %>%
  summarize(COUNT = sum(HOW_MANY, na.rm = TRUE))

eagles_us <- eagles_mutated %>%
  filter(COUNTRY == "US")

eagles_us <- eagles_us %>%
  arrange(desc(COUNT)) %>%
  mutate(FILL_COLOR = rep(c("red", "white", "blue"), length.out = n()))

ggplot(data = eagles_us, aes(x = COUNT, y = reorder(STATE, COUNT), fill = FILL_COLOR)) +
  geom_col(color = "black") +
  scale_fill_identity() +
  theme_minimal() +
  labs(x = "Count", y = "State")

gc(
  
)
