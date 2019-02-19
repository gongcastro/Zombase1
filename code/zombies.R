# zombies: Production of zombie material over the years
# Gonzalo García-Castro, gonzaloggc95@gmail.com

# set up -----------------------------------------------------------------------

# load packages
library(magrittr)  # for pipes
library(readxl)    # for importing Excel files
library(dplyr)     # for wrangling data
library(tibble)
library(ggplot2)   # for visualising data
library(viridis)   # for plot colours
library(extrafont) # for text fonts
library(gridExtra) # for arranging plots together
library(forcats)   # for working with categorical variables
library(patchwork)

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
             "bolivia", "chile", "argentina", "uruguay", "paraguay", "brazil", "costa_rica", "nicaragua",
             "belize", "jamaica", "bahamas", "guyana", "aruba", "suriname", "barbados", "curaçao",
             "french_guiana", "guadelupe", "bermuda")
asia <- c("japan", "india", "china", "indonesia", "malaysia", "thailand", "hong_kong", "vietnam",
          "myanmar", "singapore", "philippines", "south_korea", "north_korea", "iran", "pakistan",
          "saudi_arabia", "sri_lanka", "israel", "qatar", "cambodia", "bangladesh", "iraq", "taiwan",
          "maldives", "syria", "nepal", "laos", "afghanistan", "united_arab_emirates", "mongolia", 
          "north_korea", "uzbekistan", "yemen", "macau", "lebanon", "oman", "brunei", "armenia",
          "jordan", "kuwait", "bhutan", "bahrain", "kyrgyzstan", "turkmenistan", "palestine",
          "tajikistan", "turkey")
africa <- c("morocco", "algeria", "tunisia", "south_africa", "nigeria", "democratic_republic_of_congo",
            "ghana", "ethiopia", "tanzania", "uganda", "senegal", "zimbabwue", "cameroon", "mali", "sudan",
            "south_sudan", "madagascar", "cote_divoire", "rwanda", "namibia", "angola", "mauritius",
            "somalia", "cape_verde", "zambia", "lybia", "mozambique", "guinea", "botswana", "benin", "niger",
            "nigeria", "burkina_faso", "seychelles", "malawi", "chad", "togo", "gabon", "liberia", "eswatini",
            "central_african_republic", "gambia", "eritrea", "mauritania", "djibuti", "sierran_leone",
            "burundi", "lesotho", "togo", "reunion", "comoros", "congo", "egypt")
oceania <- c("australia", "new_zealand", "fiji")


# import data -------------------------------------------------------------------
data <-
  read_xlsx("~/projects/zombies/data/zombies.xlsx", sheet = "zombies") %>%
  mutate(region = case_when(country %in% europe ~ "europe",
                            country %in% america ~ "america",
                            country %in% asia ~ "asia",
                            country %in% africa ~"africa",
                            country %in% oceania ~ "oceania"))

# plot data ---------------------------------------------------------------------
# overall distribution by region
region_type <-
ggplot(data, aes(year, fill = type)) +
  geom_histogram(colour = "white", size = 0.25, binwidth = 5) +
  labs(title = "Zombie-related material by region and material type", x = "Year", y = "Counts", fill = "Type of material") +
  scale_fill_viridis(discrete = TRUE, option = "inferno") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "gray13",
                                        colour = "white",
                                        size = 0.5),
        plot.background = element_rect(fill = "grey13"),
        text = element_text(colour = "white", family = "Arial Black", size = 15),
        axis.text.x = element_text(colour = "white", angle = 0, family = "Arial"),
        axis.text.y = element_text(colour = "white", family = "Arial"),
        legend.text = element_text(family = "Arial"),
        legend.position = "top",
        legend.key = element_rect(fill = "gray13", colour = "gray13"),
        legend.background = element_rect(fill = "gray13")) +
  facet_wrap(.~region, nrow = 1) +
  ggsave("~/projects/zombies/figures/region_type.png" , width = 15)

# overal distribution by year ---------------------------------------------------
year_type <-
  ggplot(data, aes(year, colour = type)) +
  geom_density() +
  labs(title = "Evolution of zombie-related material production over the years by type",
       x = "Year", y = "Density", colour = "Type of material") +
  scale_colour_viridis(discrete = TRUE, option = "inferno") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = "gray13",
                                          colour = "white",
                                          size = 0.5),
          plot.background = element_rect(fill = "grey13"),
          text = element_text(colour = "white", family = "Arial Black", size = 15),
          axis.text.x = element_text(colour = "white", angle = 0, family = "Arial"),
          axis.text.y = element_text(colour = "white", family = "Arial"),
          legend.text = element_text(family = "Arial"),
          legend.position = "right",
          legend.key = element_rect(fill = "gray13", colour = "gray13"),
          legend.background = element_rect(fill = "gray13")) +
  ggsave("~/projects/zombies/figures/year_type.png" , width = 15)

# country against year -----------------------------------------------------------
country_year <-
  data %>%
  filter(country != "NA", year != "NA") %>% arrange(country) %>%
  ggplot(., aes(year, fct_infreq(country))) +
  geom_point(aes(color = type), show.legend = TRUE, alpha = 0.5) +
  labs(title = "Zombie-related material over the years by country",
       x = "Year",
       y = "Country") +
  scale_color_viridis(discrete = TRUE, option = "magma") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey30", size = 0.5),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = "gray13",
                                          colour = "white",
                                          size = 0.5),
          plot.background = element_rect(fill = "grey13"),
          text = element_text(colour = "white", family = "Arial Black", size = 15),
          axis.text.x = element_text(colour = "white", angle = 0, family = "Arial"),
          axis.text.y = element_text(colour = "white", family = "Arial"),
          legend.text = element_text(family = "Arial"),
          legend.position = "right",
          legend.key = element_rect(fill = "gray13", colour = "gray13"),
          legend.background = element_rect(fill = "gray13")) +
  ggsave("~/projects/zombies/figures/country_year.png", height = , width = 10)

# country counts -----------------------------------------------------------------
country_counts <-
  data %>%
  filter(country != "NA", year != "NA") %>%
  group_by(country, type) %>%
  ggplot(., aes(x = fct_infreq(country), fill = type)) +
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
          panel.background = element_rect(fill = "gray13",
                                          colour = "white",
                                          size = 0.5),
          plot.background = element_rect(fill = "grey13"),
          text = element_text(colour = "white", family = "Arial Black", size = 15),
          axis.text.x = element_text(colour = "white", angle = 90, family = "Arial"),
          axis.text.y = element_text(colour = "white", family = "Arial"),
          legend.text = element_text(family = "Arial"),
          legend.position = "top",
          legend.key = element_rect(fill = "gray13", colour = "gray13"),
          legend.background = element_rect(fill = "gray13")) +
  ggsave("~/projects/zombies/figures/country_counts.png", height = 7, width = 10)

  
# budget-box --------------------------------------------------------------------
budget_box <-
    data %>%
    filter(!is.na(budget), !is.na(box), type == "film") %>%
    ggplot(., aes(colour = country)) +
    geom_segment(aes(x = budget, xend = box, y = title, yend = title), size = 1.5, linejoin = "mitre") +
    geom_point(aes(x = budget, y = title), size = 2) +
    geom_point(aes(x = box, y = title)) +
    labs(x = "Budget and box (US$)", y = "Title") +
    scale_colour_viridis(discrete = TRUE, option = "inferno") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey30", size = 0.5),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = "gray13",
                                          colour = "white",
                                          size = 0.5),
          plot.background = element_rect(fill = "grey13"),
          text = element_text(colour = "white", family = "Arial Black", size = 15),
          axis.text.x = element_text(colour = "white", angle = 0, family = "Arial"),
          axis.text.y = element_text(colour = "white", family = "Arial", size = 9),
          legend.text = element_text(family = "Arial"),
          legend.position = "top",
          legend.key = element_rect(fill = "gray13", colour = "gray13"),
          legend.background = element_rect(fill = "gray13")) +
    facet_wrap(.~region, nrow = 1) +
    ggsave("~/projects/zombies/figures/budget_box.png", height = 15, width = 15)
  

# map ---------------------------------------------------------------------------
map <-
  map_data("world") %>%
  mutate_at(vars(1:6), funs(tolower(.))) %>%
  rename(country = region) %>%
  full_join(., data) %>%
  group_by(country, lat, long, group, order) %>%
  summarise(log_counts = log(n())) %>%
  ungroup() %>%
  mutate(lat = as.numeric(lat),
         long = as.numeric(long),
         order = as.numeric(order)) %>%
  arrange(order) %>%
  as_tibble() %>%
  ggplot(., aes(long, lat, group = group, fill = log_counts)) +
  geom_polygon(colour = "grey13") +
  labs(title = "Counts by country", fill = "Log Frequency") +
  scale_fill_viridis(option = "inferno") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "gray13",
                                        colour = "white",
                                        size = 0.5),
        plot.background = element_rect(fill = "grey13"),
        text = element_text(colour = "white", family = "Arial Black", size = 15),
        axis.text = element_blank(),
        legend.text = element_text(family = "Arial"),
        legend.position = "right",
        legend.key = element_rect(fill = "gray13", colour = "gray13"),
        legend.background = element_rect(fill = "gray13")) +
  ggsave("~/projects/zombies/figures/map_counts.png", height = 15, width = 15)


# arrange plots -----------------------------------------------------------------
grid.arrange(year_type, map, region_type, country_year, country_counts, budget_box, ncol = 1) +
  ggsave("~/projects/zombies/figures/poster.png", height = 35, width = 15)
