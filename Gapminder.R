
library(gapminder)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

gapminder

#calculate average lifeExp for each country
avg_life_expPerCountry <- gapminder %>% select(-continent,-pop,-gdpPercap)%>%
  group_by(country)%>%summarise(avg_lifeExp = mean(lifeExp))%>%
  arrange(desc(avg_lifeExp))

unique(gapminder$year)



avg_life_expPerCountry

#top10
top10 <- head(avg_life_expPerCountry$country,10)
#bottom10
bot10 <- tail(avg_life_expPerCountry$country,10)
#filter using the vectors above
avg_life_expPerCountry <- avg_life_expPerCountry %>% filter(country %in% c(top10,bot10))%>%
  arrange((avg_lifeExp))

avg_life_expPerCountry 
avg_life_expPerCountry$country <- factor(avg_life_expPerCountry$country , levels = avg_life_expPerCountry$country)
# Set the color scale
palette <- brewer.pal(5, "RdYlBu")[-(2:4)]

ggplot(avg_life_expPerCountry, aes(x = avg_lifeExp, y = country, color = avg_lifeExp)) +
  geom_point(size = 5)+
  geom_segment(aes(xend = 30, yend = country), size = 2)+
  geom_text(aes(label = round(avg_lifeExp,1)), color = "white", size = 2)+
  scale_x_continuous("LifeExp", expand =  c(0, 0), limits = c(30, 90), position = "top")+
  scale_color_gradientn(colors = palette)+
  theme_classic()+
  theme(axis.line.y = element_blank(),
        axis.ticks.y =element_blank(),
        legend.position = "none")+
  geom_vline(xintercept =mean(avg_life_expPerCountry$avg_lifeExp) , color = "grey40", linetype = 3,size = 1) +
  annotate(
    "text",
    x = mean(avg_life_expPerCountry$avg_lifeExp) + 4, y = 6.8,
    label = "The\nglobal\naverage",
    vjust = 1, size = 3, color = "grey40"
  )+annotate(
    "curve",
    x = mean(avg_life_expPerCountry$avg_lifeExp) + 4, y = 6.8,
    xend = mean(avg_life_expPerCountry$avg_lifeExp), yend = 8.9,
    arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
    color = "grey40"
  )+labs(title ="Highest and lowest life expectancies, Average",caption="Source: gapminder")
  
        
