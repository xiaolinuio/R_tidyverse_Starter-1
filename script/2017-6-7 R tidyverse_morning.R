download.file(url = "https://raw.githubusercontent.com/dmi3kno/SWC-tidyverse/master/data/gapminder_plus.csv", 
              destfile = "Data/gapminder_plus.csv")
library("tidyverse")

gapminder_plus <- read_csv(file ="Data/gapminder_plus.csv")
gapminder_plus


gapminder_plus %>% 
  filter(year==2007,continent=="Africa") %>% 
  mutate(babydeath_per1000=infantMort*pop/10^3) %>% 
  filter(babydeath_per1000>2e6) %>% 
  select(country,babydeath_per1000) %>% 
  left_join(gapminder_plus) %>% 
  mutate(babydeath_per1000=infantMort*pop/10^3,gdp_bin=gdpPercap*pop/1e9,pop_min=pop/1e6) %>% 
  select(-c(continent, babydeath_per1000,pop)) %>% 
  gather(key = variables,value = values, -c(country,year)) %>% 
  ggplot() + 
  geom_text(data =. %>% filter(year==2007) %>%  group_by(variables) %>%
              mutate(max_value=max(values)) %>%   
              filter(values==max_value),mapping=aes(x=year,y=values,label=country, color = country))+
  geom_line(mapping=aes(x=year,y=values,color = country))+
  facet_wrap(~variables,scales="free_y") +
  labs(title="jgjgjg",
       subtitle="2sss",
       caption="334455",
       y=NULL,
       x="Year")+
  theme_bw()+
  theme(legend.position = "none")

  
  