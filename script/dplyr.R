gapminder <- read_csv(file ="data/gapminder-FiveYearData.csv")
dplyr
gapminder
rep("this is an example", times=3)
"this is an example%" %>% rep(time=3)

year_contry_gdp <-select(gapminder,year,country,gdpPercap)
year_contry_gdp
?select

year_contry_gdp <-gapminder %>%  select(year,country,gdpPercap)
head(year_contry_gdp)

gapminder %>%
  filter(year==2002) %>%
  ggplot(mapping = aes(x= continent,y=pop))+
  geom_boxplot()

year_contry_gdp_euro <- gapminder %>%
  filter(continent=="Europe") %>%
  select(year,country,gdpPercap)
year_contry_gdp_euro

year_gdpPercap_lifeExp <-gapminder %>%
  filter (country=="Norway") %>%
  select(year,gdpPercap,lifeExp)
year_gdpPercap_lifeExp

country_gdpPercap_lifeExp_Norway <-gapminder %>%
  filter (country=="Norway") %>%
  select(year,gdpPercap,lifeExp)
country_gdpPercap_lifeExp_Norway

gapminder %>% 
  group_by(continent)

gapminder %>% 
  group_by(continent) %>% 
  summarise(mean_gdpPercap=mean(gdpPercap)) %>% 
  ggplot(mapping = aes(x= continent,y=mean_gdpPercap))+
  geom_point()

gapminder %>% 
  group_by(continent) %>% 
  filter(continent=="asia") %>% 
  summarise(mean_lifeExp_asia=mean(lifeExp)) %>% 
  ggplot(mapping = aes(x= country,y=mean_lifeExp_asia))+
  geom_point()

gapminder %>% 
  filter(continent=="Asia") %>% 
  group_by(country) %>% 
  summarise(mean_lifeExp_asia=mean(lifeExp)) %>% 
  ggplot(mapping = aes(x=country,y=mean_lifeExp_asia))+
  geom_point()+
  coord_flip()

gapminder %>% 
  mutate(gdp_billion=gdpPercap*pop/10^9) %>% 
  head()

gapminder %>% 
  mutate(gdp_billion=gdpPercap*pop/10^9) %>% 
  group_by(continent,year) %>% 
  summarise(mean_gdp_billion=mean(gdp_billion))

library(maps)
map_data("world") %>% 
  head()


gapminder_country_summary <-gapminder %>% 
  group_by(country) %>% 
  summarise(mean_lifeExp=mean(lifeExp))

library(maps)
map_data("world") %>% 
  rename(country=region) %>% 
  left_join(gapminder_country_summary,by="country") %>% 
  ggplot()+
  geom_polygon(aes(x=long,y=lat,group=group,fill=mean_lifeExp))+
  scale_fill_gradient(low="blue",high="red")+
  coord_equal()