# maternal-deaths

---
title: "Tackling the maternal mortality crisis between 2000 and 2020"
subtitle: ""
author: "Manon Lepicard"
date: "2024-04-11"
format: 
  html:
    theme: minty
    smooth-scroll: true
    toc: true #table of content
execute: 
  echo: false
  warning: false
  message: false
lightbox: auto #overview
backgroundcolor: "#FAF3F1"
---

  The fight against **Maternal Mortality** has been a major concern in recent decades. *Between 2000 and 2020*, despite significant progress in many countries, nearly **800 women died** as a result of complications related to pregnancy and childbirth, *according to the WHO*. This underlines **the urgent need for action to ensure equitable access to quality maternal health care worldwide**.

```{r}
#| label: setup
#| include: false

#Libraries
library(tidyverse)
library(plotly)
library(ggplot2)
library(viridisLite)

#my data
localisation <- read_delim("Documents/dataanalyse/Ok/localisation.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
imp_data <- read_delim("Documents/dataanalyse/Ok/all-data.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
maternal_mortality_2000 <- read_delim("Documents/dataanalyse/Ok/maternal_mortality_2000.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
maternal_mortality_2020 <- read_delim("Documents/dataanalyse/Ok/maternal_mortality_2020.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

#my colours
my_colours <- c("#FDFBFA","#98C2D1","#5D9693","#B74A63","#3D668C", "#E1F8FF", "#FAF3F1")
my_colours_map <- map(my_colours[4],function(x) colorRampPalette(c(my_colours[2],x))(5))
my_colours_map2 <- c(my_colours_map)
my_gradient <- c("#98C2D1", "#94a6f7", "#db4fa3", "#d62b60", "#B74A63")

#joining my data
data_join <- imp_data %>%
  full_join(localisation, by=join_by(country,code))%>%
  full_join(maternal_mortality_2020, by=join_by(country, code, year))%>%
  full_join(maternal_mortality_2000, by=join_by(country, code, year))

#data for maps 
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
```

  This report aims to ***analyze maternal mortality trends from 2000 to 2020***. It will correlate this data with countries' health investment as a percentage of GDP and demonstrate its impact on life expectancy through two comparative graphs. The *goal is to inform actionable strategies for further reducing maternal mortality and enhancing global maternal health*.
  
  
  
## 2000: developed countries are in trouble

```{r}
#| fig-cap: Here, the map shows the distribution of maternal deaths in 2000. Underdeveloped countries are most affected by this problem.
#| fig-width: 10
#| fig-height: 5

#map 2000
ggplot(data_2000) +
  aes(x = long, y = lat, group = group, fill = Maternal_mortality_2000) +
  geom_polygon() +
  scale_fill_gradientn(colours = viridis(256, option = "C"), limits = c(0, 1687), na.value = "light grey") +
  labs(
    title = "Maternal mortality in 2000",
    subtitle = "Countries in grey have no data or their names mismatch.",
    fill = "Number of maternal deaths"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    title = element_text(face = "bold", size = 12, family = "Georgia"),
    plot.subtitle = element_text (face ="italic", color = "grey", family = "Arial"),
    plot.background = element_rect(fill = my_colours[1]),
    panel.background = element_rect(fill = my_colours[6]),
    panel.grid = element_line(color = "white"), 
    legend.box.background = element_rect(),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.title = element_text(face = "bold", size = 10, family = "Arial"),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()  
  )
```
## Health data that reassure, or how health is being taken more seriously 


```{r}
#| fig-cap: This graph illustrates that investment in health has increased significantly over the two decades.
#| fig-width: 10
#| fig-height: 5

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
    title = "Evolution of the health expenditure around the world between 2000 and 2020",
    subtitle = "On the graph, the black line represents the global average."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    title = element_text(face = "bold", size = 12, family = "Georgia"),
    plot.background = element_rect(fill = my_colours[1]),
    panel.background = element_rect(fill = my_colours[7]),
    panel.grid = element_line(color = "white"),
    legend.box.background = element_rect(),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.title = element_text(face = "bold", size = 10, family = "Arial")
  ) +
  scale_color_manual(values = color_values) +
  guides(color = guide_legend(title = "Region"))
```

  We can therefore assume that the resources allocated to maternal health have evolved in line with the maternal mortality rate.


## An investment that pays off

```{r}
#| fig-cap: Life expectancy has also increased overall over the last 20 years.
#| fig-width: 10
#| fig-height: 5


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
        title = element_text(face = "bold", size = 12, family = "Georgia"),
        strip.text = element_text(face="bold", size=9,lineheight=5.0), #title by column
        strip.background  = element_blank(), #background of title by column
        plot.background = element_rect(fill = my_colours[1]),
        panel.background = element_rect(fill = my_colours[7]),
        axis.text.x = element_blank(), #remove x-axis legend here
        panel.grid = element_line(color = "white"),        
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.title = element_text(face = "bold", size = 10, family = "Arial")
  )+
  scale_fill_manual(values = my_gradient)
```

  These factors are *encouraging* when it comes to understanding how investing in health can support a better quality of life, and therefore *improve the life expectancy of its inhabitants*. 
On a smaller scale, on a subject such as infant mortality, the schematisation remains the same. Consistently, greater investment in these areas would alleviate the number of deaths caused by the lack of resources for women at risk. 


## 2020: hope exists (and is up for grabs)

```{r}
#| fig-cap: As we can see, in 2020 the data have improved significantly - the number of cases has been greatly reduced in underdeveloped countries, and the new data are encouraging.
#| fig-width: 10
#| fig-height: 5

#map 2020
ggplot(data_2020) +
  aes(x = long, y = lat, group = group, fill = Maternal_mortality_2020) +
  geom_polygon() +
  scale_fill_gradientn(colours = viridis(256, option = "C"), limits = c(0, 1687), na.value = "light grey") +
  labs(
    title = "Maternal mortality in 2020",    
    subtitle = "Countries in grey have no data or their names mismatch.",
    fill = "Number of maternal deaths"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    title = element_text(face = "bold", size = 12, family = "Georgia"),
    plot.subtitle = element_text (face ="italic", color = "grey", family = "Arial"),
    plot.background = element_rect(fill = my_colours[1]),
    panel.background = element_rect(fill = my_colours[6]),
    panel.grid = element_line(color = "white"), 
    legend.box.background = element_rect(),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.title = element_text(face = "bold", size = 10, family = "Arial"),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()  
  )
```
  This shows that *perseverance* and *taking medical needs into account* pay off handsomely when the issues are studied. 


## What you need to learn and remember 

  With this information, we can understand why it is important that resources are well and better allocated to health. Particularly on maternal health issues, as we can see that the burden has evolved in the right way in the most impacted countries, giving **considerable hope for the future**. 

These data provide a visual perspective on the *effectiveness of maternal health policies and programmes*, and their contribution to the overall *improvement in women's health*.

In conclusion, it is ***vital*** that we continue to talk about these issues, so that our politicians continue to look at the subject and try to reduce the major inequalities that affect women's lives. 

                            Let's support the women of the world!
