# install.packages("gapminder")

library(tidyverse)
library(dslabs)
data(gapminder)

df <- gapminder

## continent of interest (coi)
coi <- "Europe"

## study df
summary(df)

## study unique values
df %>% 
  summarise(nb_country = n_distinct(country),
            nb_continent = n_distinct(continent),
            nb_year= n_distinct(year))

## examine life expectancy in continent
df %>%
  filter(continent == coi) %>% 
  select(country, life_expectancy) %>%
  arrange(life_expectancy) %>% 
  head(10)

## sort countries by lowest life expectancy
lowest <- df %>%
  filter(continent == coi) %>%
  select(country, year, life_expectancy) %>%
  arrange(life_expectancy)

## create data table of life expectancy
library(data.table)
lle <- data.table(lowest)

## arrange countries by lowest life expectancy seen
lle <- lle[ , .SD[which.min(life_expectancy)], by = country]
lle

## worst 5 life expectancy countries
bot4 <- as.array(lle$country[1:4])
bot4




## plot bot4 over time
df %>% 
  filter(country == bot4) %>% 
  ggplot(aes(x = year,y = life_expectancy,color = country)) +
  geom_line( size = 0.5)+
  ggtitle(coi)

## rate
df <- df %>% 
  filter(country == "Bosnia and Herzegovina") %>%
  select(year, life_expectancy) %>%
  arrange(year)
  


## plot change of rate or deviation or similar

 df <- mutate(df, rate = (100 * (life_expectancy - lag(life_expectancy))/lag(life_expectancy)))


## largest change in rate
 n_rate <- min(df$rate, na.rm=TRUE)


## year corresponding to worst rate

 df$year[which.min(df$rate)]

## plotting rate

df

