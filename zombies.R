# zombies: Production of zombie material over the years
# Gonzalo García-Castro, gonzaloggc95@gmail.com

# ------------------------------------------------------------------------------


# set up -----------------------------------------------------------------------

# load packages
library(magrittr)  # for pipes
library(readxl)    # for importing Excel files
library(dplyr)     # for wrangling data
library(ggplot2)   # for visualising data
library(viridis)   # for plot colours
library(extrafont) # for text fonts
library(gridExtra) # for arranging plots together

# extra -------------------------------------------------------------------

# group countries by region
europe <- c("portugal", "spain", "france", "andorra", "italy", "greece", "belgium", "ireland",
            "uk", "netherlands", "germany", "austria", "switzerland", "czech_republic", "hungary",
            "poland", "norway", "sweden", "denmark", "finland", "iceland", "san_marino", "liechtenstein",
            "albania", "serbia", "bosnia_herzegovin", "montenegro", "croatia", "slovenia", "slovakia",
            "romania", "bulgary", "ukraine", "belarus", "stonia", "latvia", "lithuania", "macedonia",
            "malta", "cyprus", "luxembourg", "monaco", "vatican_city", "moldova")
america <- c("canada", "usa", "mexico", "guatemala", "honduras", "panama", "el_salvador", "cuba",
             "haiti", "dominican_republic", "puerto_rico", "venezuela", "colombia", "peru", "ecuador",
             "bolivia", "chile")


# import data -------------------------------------------------------------------
data <-
  read_xlsx("~/projects/zombies/zombies.xlsx", sheet = "zombies") %>%
  mutate(region = case_when(country = ))

# overal distribution by year ---------------------------------------------------
ggplot(data, aes(year)) +
  geom_density(aes(colour = country)) +
  scale_colour_viridis(discrete = TRUE, option = "inferno") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "black",
                                        colour = "white",
                                        size = 0.5),
        legend.position = "bottom",
        plot.background = element_rect(fill = "black"),
        text = element_text(colour = "white", family = "Arial Black"),
        axis.text.x = element_text(colour = "white", family = "Arial"),
        axis.text.y = element_text(colour = "white", family = "Arial"),
        legend.background = element_rect(fill = "black")) +
  ggsave("~/projects/zombies/year.png" , width = 15)



# country against year -----------------------------------------------------------
country_year <-
  data %>%
  filter(country != "NA", year != "NA") %>% arrange(country) %>%
  ggplot(., aes(year, country)) +
  geom_point(aes(color = type), show.legend = TRUE, alpha = 0.5) +
  labs(title = "Zombie-related material over the years by country",
       x = "Year",
       y = "Country") +
  scale_color_viridis(discrete = TRUE, option = "magma") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "black",
                                        colour = "white",
                                        size = 0.5),
        plot.background = element_rect(fill = "black"),
        text = element_text(colour = "white", family = "Arial Black"),
        axis.text.x = element_text(colour = "white", family = "Arial"),
        axis.text.y = element_text(colour = "white", family = "Arial"),
        legend.position = "top",
        legend.background = element_rect(fill = "black")) +
  ggsave("~/projects/zombies/country_year.png", height = , width = 10)

# country counts -----------------------------------------------------------------
country_counts <-
  data %>%
  filter(country != "NA", year != "NA") %>% group_by(country, type) %>%
  ggplot(., aes(x = as.factor(country), fill = type)) +
  geom_bar(color = "white", size = 0.1) +
  labs(title = "Zombie-related material by country",
       fill = "Type",
       x = "Country",
       y = "Counts (log scale)") +
  scale_fill_viridis(discrete = TRUE, option = "magma") +
  scale_y_log10() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "black",
                                        colour = "white",
                                        size = 0.5),
        plot.background = element_rect(fill = "black"),
        text = element_text(colour = "white", family = "Arial Black"),
        axis.text.x = element_text(colour = "white", angle = 90, family = "Arial"),
        axis.text.y = element_text(colour = "white", family = "Arial"),
        legend.text = element_text(family = "Arial"),
        legend.position = "right",
        legend.background = element_rect(fill = "black")) +
  ggsave("~/projects/zombies/country_counts.png", height = 7, width = 10)

# arrange plots -----------------------------------------------------------------
grid.arrange(country_year, country_counts, nrow = 1) 



# map ---------------------------------------------------------------------------
map <- map_data("world") %>% as.tibble()
counts <- data %>% filter(country != "NA", year != "NA") %>% count(country) %>% select(region = country)
ggplot(map, aes(long, lat, group = group, fill = region)) +
  geom_polygon(colour = "grey50", show.legend = FALSE) +
  labs(title = "Map of World") +
  scale_fill_viridis(option = "magma", discrete = TRUE) +
  theme_minimal() +
  theme(panel.grid = element_blank())

