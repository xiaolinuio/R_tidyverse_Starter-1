library("tidyverse")

?gather
gapminder <- read_csv(file ="data/gapminder-FiveYearData.csv")

gapminder
ggplot(data = gapminder) +
  geom_point(mapping = aes( x = gdpPercap, y = lifeExp))

ggplot(data = gapminder) +
  geom_jitter(mapping = aes( x = gdpPercap, y = lifeExp, color= continent))

ggplot(data = gapminder) +
  geom_point(mapping = aes( x = log(gdpPercap), y = lifeExp, color= continent, size =pop))


ggplot(data = gapminder) +
  geom_line(mapping = aes( x = year, y = lifeExp, group=country,color=continent))

 
ggplot(data = gapminder) +
  geom_boxplot(mapping = aes( x = continent, y = lifeExp))


ggplot(data = gapminder) +
  geom_jitter(mapping = aes(x= continent,y= lifeExp, color=continent))+
  geom_boxplot(mapping = aes( x = continent, y = lifeExp))


ggplot(data = gapminder) +
  geom_boxplot(mapping = aes( x = continent, y = lifeExp, color=continent))+
  geom_jitter(mapping = aes(x= continent,y= lifeExp, color=continent))


ggplot(data = gapminder,mapping = aes( x = continent, y = lifeExp,color=continent))+
  geom_boxplot()+
  geom_jitter()


ggplot(data = gapminder,mapping = aes( x = log(gdpPercap), y = lifeExp, color=continent)) +
  geom_jitter(alpha= 0.5) +
  geom_smooth(method ="lm")

ggplot(data = gapminder,mapping = aes( x = log(gdpPercap), y = lifeExp, color=continent)) +
  geom_jitter(alpha= 0.1) +
  geom_smooth(method ="lm") 

ggplot(data = gapminder,mapping = aes( x = log(gdpPercap), y = lifeExp)) +
  geom_jitter(mapping= aes(color=continent),alpha= 0.1) +
  geom_smooth(method ="lm")

ggplot(data = gapminder)+
  geom_boxplot(mapping = aes( x = year, y = lifeExp))
  

ggplot(data = gapminder)+
  geom_boxplot(mapping = aes( x = as.factor(year), y = lifeExp))

ggplot(data = gapminder)+
  geom_boxplot(mapping = aes( x = as.factor(year), y = log(gdpPercap)))

ggplot(data = gapminder)+
  geom_density2d(mapping = aes( x = lifeExp, y = log(gdpPercap)))

ggplot(data = gapminder,mapping = aes( x = gdpPercap, y = lifeExp))+
  geom_point()+
  geom_smooth()+
  scale_x_log10()+
  facet_wrap(~ continent)


ggplot(data = gapminder,mapping = aes( x = gdpPercap, y = lifeExp))+
  geom_point()+
  geom_smooth()+
  scale_x_log10()+
  facet_wrap(~ year)

ggplot(data = gapminder,mapping = aes( x = gdpPercap, y = lifeExp))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_x_log10()+
  facet_wrap(~ year)

ggplot(data = gapminder,mapping = aes( x = gdpPercap, y = lifeExp))+
  geom_point()+
  geom_smooth(method="lm")+
  scale_x_log10()+
  facet_wrap(~ continent)

filter(gapminder, year==2007)

ggplot(data=filter(gapminder, year==2007))+
  geom_bar(mapping = aes(x=continent))

filter(gapminder, year==2007, continent="Oceania")

ggplot(data=filter(gapminder, year==2007,continent=="Oceania"))+
  geom_bar(mapping = aes(x=continent,y=pop),stat = "identity")

ggplot(data=filter(gapminder, year==2007,continent=="Asia"))+
  geom_bar(mapping = aes(x=country,y=pop))


ggplot(data=filter(gapminder, year==2007,continent=="Asia"))+
  geom_col(mapping = aes(x=country,y=pop))+
  coord_flip()


ggplot(data=gapminder,mapping = aes(x=gdpPercap,y=lifeExp,color=continent, size=pop/10^6))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~ year)
 

ggplot(data=gapminder,mapping = aes(x=gdpPercap,y=lifeExp,color=continent, size=pop/10^6))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~ year)
  labs(title="Life Expectancy vs GDP per capita over time")
  
  
ggplot(data=gapminder,mapping = aes(x=gdpPercap,y=lifeExp,color=continent, size=pop/10^6))+
    geom_point()+
    scale_x_log10()+
    facet_wrap(~ year)+
    labs(title="Life Expectancy vs GDP per capita over time",
       subtitle="In the last 50 years, life expectancyis...",
       caption="Source: Gapiming",
       x="gdp per capita, in 000USD", 
       y= "Life Expec",
       color="Continet",
       size="Poplulation, in million")

ggsave("my_fancy_plot.png)
  
  ?ggsave
  
  
gapminder

# dplyr
