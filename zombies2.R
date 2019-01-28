library(magrittr)
library(ggplot2)
library(plyr)
library(dplyr)
library(car)

zombies <-
  read.delim("zombies.txt",
             sep = "\t",
             col.names = c("title", "type", "year", "director", "country"),
             na.strings = c("NA", ""),
             header = FALSE,
             dec = ".",
             colClasses = c("character", "character", "numeric", "character", "factor"))

countsCountry <- as.vector(table(zombies$country))

  
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

