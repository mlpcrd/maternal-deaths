---
  title: "Evolution of maternal deaths between 2000 and 2020 around the World"
subtitle: ""
author: "Manon Lepicard"
date: "2024-04-11"
format: 
  html:
  theme: Solarized
toc: true #table of content
execute: 
  echo: false
warning: false
message: false
lightbox: auto #overview
backgroundcolor: "#FAF3F1"
---

install.packages("plotly")
install.packages("tidyverse")

library(tidyverse)
library(plotly)
library(ggplot2)
library(viridisLite)

#defining data
localisation <- read_delim("Documents/dataanalyse/Ok/localisation.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
imp_data <- read_delim("Documents/dataanalyse/Ok/all-data.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
maternal_mortality_2000 <- read_delim("Documents/dataanalyse/Ok/maternal_mortality_2000.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
maternal_mortality_2020 <- read_delim("Documents/dataanalyse/Ok/maternal_mortality_2020.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

#my colours
my_colours <- c("#faf7f1","#98C2D1","#5D9693","#B74A63","#3D668C", "#E1F8FF", "#FAF3F1")
my_colours_map <- map(my_colours[4],function(x) colorRampPalette(c(my_colours[2],x))(5))
my_colours_map2 <- c(my_colours_map)
my_gradient <- c("#98C2D1", "#94a6f7", "#db4fa3", "#d62b60", "#B74A63")

#joining my data
data_join <- imp_data %>%
  full_join(localisation, by=join_by(country,code))%>%
  full_join(maternal_mortality_2020, by=join_by(country, code, year))%>%
  full_join(maternal_mortality_2000, by=join_by(country, code, year))

#map 
map_world <- map_data("world")
map_world$region <- as.character(map_world$region)
map_world$region[map_world$region == "USA"] <- "United States of America"
map_world$region[map_world$region == "UK"] <- "United Kingdom"
map_world$region[map_world$region == "Republic of Congo"] <- "Republic of the Congo"
map_world$region[map_world$region == "Democratic Republic of the Congo"] <- "Congo"

map_data_join <- full_join(data_join, map_world, by = c("country"="region"))

#data for graph
data_2000 <- filter(map_data_join, year == 2000)
data_2020 <- filter(map_data_join, year == 2020)
data_2000_2020 <- full_join(data_2020, data_2000)
health_exp <- full_join(select(imp_data, country, GDP_per_capita, Current_health_exp, year), localisation)

#map 2000
ggplot(data_2000) +
  aes(x = long, y = lat, group = group, fill = Maternal_mortality_2000) +
  geom_polygon() +
  scale_fill_gradientn(colours = viridis(256, option = "C"), limits = c(0, 1687), na.value = "light grey") +
  labs(
    title = "Maternal mortality in 2000",
    fill = "Number of maternal deaths"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    title = element_text(face = "bold", size = 12, family = "Georgia"),
    plot.background = element_rect(fill = my_colours[7]),
    panel.background = element_rect(fill = my_colours[6]),
    panel.grid = element_line(color = "white"), 
    legend.box.background = element_rect(),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.title = element_text(face = "bold", size = 10, family = "Arial"),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()  
  )

#plot graph about health exp
m_health_exp <- health_exp %>%
  group_by(Region, year) %>%
  summarise(m_health_Exp = mean(Current_health_exp*100, na.rm = TRUE))

m_world_exp <- health_exp %>%
  group_by(year) %>%
  summarise(m_health_Exp = mean(Current_health_exp*100, na.rm = TRUE)) %>%
  mutate(Region = "World")

m_health_exp <- m_health_exp %>%
  bind_rows(m_world_exp)

# Extract unique regions (except "World")
other_regions <- unique(m_health_exp$Region[-which(m_health_exp$Region == "World")])

# Create a named vector of colors
color_values <- c("World" = "black", setNames(my_gradient, other_regions))

# Plot with manual color scale
ggplot(m_health_exp, aes(x = year, y = m_health_Exp, color = Region)) +  
  geom_line(size = 1) +
  xlab("Year") +
  ylab("Health expenditure (%)") +
  labs(
    title = "Evolution of the health expenditure around the world between 2000 and 2020"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = my_colours[7]),
    panel.background = element_rect(fill = "white"),
    legend.box.background = element_rect(),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.title = element_text(face = "bold", size = 10)
  ) +
  scale_color_manual(values = color_values) +
  guides(color = guide_legend(title = "Region"))

#bar chart
data_2000_2020 %>%
  group_by(Region, year) %>%
  summarise(m_lifeExp=mean(Life_exp, na.rm = TRUE)) %>% #na.rm = true to use only region with data
  ggplot()+
  aes(Region, m_lifeExp, fill = Region)+
  geom_col()+
  facet_wrap(~ year)+
  labs(
    x = "Continent",
    y = "Average life expectancy",
    title = "Evolution of the life expectancy between 2000 and 2020 in the World"
  )+
  theme_minimal(base_size = 12)+
  theme(plot.title = element_text(size = rel(1)),
        strip.text = element_text(face="bold", size=9,lineheight=5.0), #title by column
        strip.background  = element_blank(), #background of title by column
        axis.text.x = element_blank(), #remove x-axis legend here
        plot.background = element_rect(fill = my_colours[7]),
        panel.background = element_rect(fill = "white"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.title = element_text(face = "bold", size=10),
  )+
  scale_fill_manual(values = my_gradient)


#map 2020
ggplot(data_2020) +
  aes(x = long, y = lat, group = group, fill = Maternal_mortality_2020) +
  geom_polygon() +
  scale_fill_gradientn(colours = viridis(256, option = "C"), limits = c(0, 1687), na.value = "light grey") +
  labs(
    title = "Maternal mortality in 2020",
    fill = "Number of maternal deaths"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    title = element_text(face = "bold", size = 12, family = "Georgia"),
    plot.background = element_rect(fill = my_colours[7]),
    panel.background = element_rect(fill = my_colours[6]),
    panel.grid = element_line(color = "white"), 
    legend.box.background = element_rect(),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.title = element_text(face = "bold", size = 10, family = "Arial"),
    axis.title.x = element_blank(),  
    axis.title.y = element_blank()
  )


 