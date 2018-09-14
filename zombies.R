library(tidyverse)
library(magrittr)
library(car)

zombies <-
  read.delim("zombies.txt",
             sep = "\t",
             na.strings = c("NA", ""),
             header = TRUE,
             dec = ".",
             colClasses = c("character", "factor", "numeric", "character", "factor"))

countsCountry <- zombies %$% country %>% table %>% as.vector

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
  ggplot(aes(x = year, y = country, fill = type)) +
  geom_tile()

  
