library(tidyverse)
coffee <- read_csv("coffeequality.csv")

# manipulating and cleaning the data
coffee <- coffee %>%
  mutate(altitude2 = str_replace(Altitude, "-.*", "")) %>%
  mutate(altitude3 = str_replace(altitude2, "~.*", "")) %>%
  mutate(altitude4 = str_replace(altitude3, " .*", "")) %>%
  mutate(Altitude_num = as.numeric(altitude4)) %>%
  filter(Altitude_num < 4000) %>%
  mutate(Altitude_cat = case_when(
    Altitude_num < 900 ~ '150-899',
    Altitude_num >=900 & Altitude_num < 1650 ~ '900-1649',
    Altitude_num >= 1650 ~ '1650-2361'))

coffee$Altitude_cat <- factor(coffee$Altitude_cat, levels = c('150-899', '900-1649','1650-2361'))

max(coffee$Altitude_num)
# creating the histogram

cuppointshistogram <- coffee %>% ggplot() + 
  geom_histogram(aes(x = `Total Cup Points`), bins = 25, fill = "burlywood4", col = "chocolate4", alpha = 0.6) +
  labs(title = 'Most Lots of Coffee were Rated in the Low-Mid 80s', subtitle = 'Arabica Coffee, 2023', y = 'Lots of Coffee', caption = 'Cup Points is a measure of overall coffee quality based on a variety of tasting attributes.\nsource: Coffee Quality Institute') +
  theme_minimal()

cuppointshistogram

saveRDS(cuppointshistogram, 'cuppointshistogram.rds')

# creating the bar plot

barplotaltitude <- as.data.frame(table(coffee$Altitude_cat)) %>%
  ggplot(aes(x = Var1, y = Freq)) + 
  geom_bar(stat = 'identity', col = "chocolate4", fill = "burlywood4", alpha = 0.6) +
  labs(title = 'Most Coffee was Grown Between 900 and 1,649 Meters', subtitle = 'Arabica Coffee, 2023', y = 'Lots of Coffee', x = 'Grow Altitude (meters)', caption = 'source: Coffee Quality Institute') +
  theme_minimal() +
  coord_flip()
barplotaltitude 

saveRDS(barplotaltitude, 'barplotaltitude.rds')


meds <- coffee %>% group_by(Altitude_cat) %>% summarise(median = round(median(`Total Cup Points`),1))
maxs <- coffee %>% group_by(Altitude_cat) %>% summarise(max = round(max(`Total Cup Points`),1))
mins <- coffee %>% group_by(Altitude_cat) %>% summarise(min = round(min(`Total Cup Points`),1))

# creating the violin plot

violinplotaltitude <- coffee %>% filter(Altitude_cat != 'NA') %>%
  ggplot(aes(y = `Total Cup Points`, x = `Altitude_cat`)) + 
  geom_violin(col = "chocolate4", fill = "burlywood4", alpha = 0.6) +
  geom_text(data = meds, aes(x = `Altitude_cat`, y = `median`, label = paste0('Median = ',`median`)), col = 'white', size = 3, vjust = .5, hjust = .5) +
  geom_label(data = maxs, aes(x = `Altitude_cat`, y = `max`, label = paste0('Max = ',`max`)), col = 'black', size = 3, vjust = .5, hjust = -.2) +
  geom_label(data = mins, aes(x = `Altitude_cat`, y = `min`, label = paste0('Min = ', `min`)), col = 'black', size = 3, vjust = .5, hjust = 1.2) +
  scale_y_continuous(limits = c(75,92)) +
  coord_flip() +
  labs(title = 'Distribution of Total Cup Points by Altitude', subtitle = 'Arabica Coffee, 2023',
       x = 'Grow Altitude (meters)', y = "Total Cup Points", caption = 'Cup Points is a measure of overall coffee quality based on a variety of tasting attributes.\nsource: Coffee Quality Institute') +
  theme_minimal()
violinplotaltitude



saveRDS(violinplotaltitude, 'violinplotaltitude.rds')


# Creating the map
library(sf)
link = "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"  
map = read_sf(link)
map %>%
  ggplot +
  geom_sf()

coffee <- coffee %>%
  mutate(name = case_when(
    `Country of Origin` == "United States (Hawaii)" ~ "United States of America",
    TRUE ~ `Country of Origin`
  ))

countries <- coffee %>%
  group_by(name) %>%
  summarise(median_cup_points = median(`Total Cup Points`),
            count = n(),
            median_altitude = median(Altitude_num))

mapandcountries <- left_join(map, countries, by = 'name')


labels <- data.frame(name = c("Taiwan: 61", "Colombia: 18", "Guatemala: 18", 
                                        "Honduras: 13", "Thailand: 12"),
                                        x = c(120, -74, -90, -74, 100),
                                        y = c(19, 4.5, 21, 16, 8)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) 

asiaafrica <- mapandcountries %>%
  ggplot +
  geom_sf(aes(fill = count)) +
  geom_sf_label(data = labels, aes(label = name), size = 1.5) +
  scale_fill_gradient2(
    low = 'ivory',
    mid = 'chocolate',
    high = 'green',
    na.value = 'grey90',
    midpoint = 30,
    breaks = c(0,10,20,61)
  ) +
  xlim(30,135) +
  ylim(-30,35) +
  theme_void() +
  guides(fill = 'none') +
  labs(title = '', subtitle = '',caption = 'source: Coffee Quality Institute')

asiaafrica

americas <- mapandcountries %>%
  ggplot +
  geom_sf(aes(fill = count)) +
  geom_sf_label(data = labels, aes(label = name), size = 1.5) +
  scale_fill_gradient2(
    low = 'ivory',
    mid = 'chocolate',
    high = 'green',
    na.value = 'grey90',
    midpoint = 30,
    breaks = c(0,10,20,61)
  ) +
  xlim(-135,-35) +
  ylim(-35,35) +
  theme_void() +
  theme(legend.position = c(0.3,0.3), legend.key.size = unit(0.4, 'cm')) +
  labs(fill = 'Lots') +
  labs(title = 'Taiwan Contributed the Most Lots of Coffee by Far', subtitle = 'Arabica Coffee, 2023', caption = '')

americas

library(cowplot)

lotsmap <- plot_grid(americas,asiaafrica)

lotsmap

saveRDS(lotsmap, 'lotsmap.rds')
