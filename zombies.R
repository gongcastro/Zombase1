library(tidyverse)
library(magrittr)
library(car)

# import data
zombies <-
  read.delim("zombies.txt",
             sep = "\t",
             na.strings = c("NA", ""),
             header = TRUE,
             dec = ".",
             colClasses = c("character", "factor", "numeric", "character", "factor", "factor"))

zombies %>%
  ggplot(aes(x = year,
             y = factor(country,
                        levels = rev(levels(country))))) +
  geom_jitter(aes(color = country),
              alpha = 0.5,
              height = 0.3) +
  guides(color = FALSE) +
  xlab("Year") +
  ylab("Country") +
  ggtitle("Zombie material over the years by country") +
  theme_minimal()

zombies %>%
  filter(year > 1975) %>%
  ggplot(aes(x = year,
             y = factor(country,
                        levels = rev(levels(country))))) +
  geom_tile(aes(fill = country),
            alpha = 0.5,
            height = 0.3) +
  guides(fill = FALSE) +
  xlab("Year") +
  ylab("Country") +
  ggtitle("Zombie material over the years by country") +
  theme_minimal()

zombies %>%
  filter(year > 1975) %>%
  ggplot(aes(x = year, y = country, color = type)) +
  geom_count(alpha = 0.5) +
  theme_minimal()

zombies %>%
  ggplot(aes(x = type, y = country, color = type)) +
  geom_count() +
  theme_minimal()


  
