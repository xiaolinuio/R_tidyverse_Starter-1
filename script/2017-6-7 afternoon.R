download.file(url = "https://raw.githubusercontent.com/dmi3kno/SWC-tidyverse/master/data/gapminder_plus.csv", 
              destfile = "Data/gapminder_plus.csv")
library("tidyverse")
library(purrr)
gapminder_plus <- read_csv(file ="Data/gapminder_plus.csv")
gapminder_plus


mod <- lm(lifeExp ~ gdpPercap,data=gapminder_plus)
str(mod)

str(mod[8]) # 
str(mod[["df.residual"]]) #SAME WITH LINE 12,12,14
str(mod$df.residual)

mod$qr$qr[1,1]# find out the 1st vector of qr
mod$qr$qr[1]
str(mod$qr$qr)

gapminder_plus %>% group_by(continent) %>% summarise(mean_le=mean(lifeExp),
                                                     min_le=min(lifeExp),
                                                     max_le=max(lifeExp))
gapminder_plus %>% ggplot()+
  geom_line(mapping=aes(x=year,y=lifeExp,color=continent,group=country))+
  geom_smooth(mapping=aes(x=year,y=lifeExp),method=lm)+
  facet_wrap(~continent)
  
by_country<-gapminder_plus %>% group_by(continent,country) %>% 
  nest()

by_country$data
str(by_country$data)
by_country$data[1]
by_country$data[[1]]#$只对name有效

#map(list,function)

map(1:3,sqrt)
map_dbl(1:3,sqrt)#变横行

by_country %>% 
  mutate(model=purrr::map(data,~lm(lifeExp~year,data=.))) #function=~:purrr::map purrr中的map功能

model_by_country <- by_country %>% 
  mutate(model=purrr::map(data,~lm(lifeExp~year,data=.x))) %>% 
  mutate(summr=map(model,broom::glance)) %>% 
  unnest(summr) #打开内部表格
model_by_country 

model_by_country$summr[[1]]

by_country %>% 
  mutate(model=purrr::map(data,~lm(lifeExp~year,data=.))) %>% 
  mutate(summr=map(model,broom::glance)) %>% 
  unnest(summr) %>% arrange(r.squared) %>% 
  ggplot()+
  geom_jitter(mapping = aes(x=continent,y=r.squared))


 by_country %>% 
  mutate(model=purrr::map(data,~lm(lifeExp~year,data=.))) %>% 
  mutate(summr=map(model,broom::glance)) %>% 
  unnest(summr) %>% arrange(r.squared) %>% filter(r.squared<0.6)
 
 by_country %>% 
   mutate(model=purrr::map(data,~lm(lifeExp~year,data=.))) %>% 
   mutate(summr=map(model,broom::glance)) %>% 
   unnest(summr) %>% arrange(r.squared) %>% filter(r.squared<0.3) %>% 
   select(country) %>% left_join(gapminder_plus) %>% 
   ggplot()+
   geom_line(mapping = aes(x=year,y=lifeExp,color=country,group=country))
 

 
 gapminder_plus %>% ggplot()+
   geom_line(mapping=aes(x=log(gdpPercap),y=lifeExp,color=continent,group=country))+
   facet_wrap(~continent)
 
 gapminder_plus%>% 
   mutate(model=purrr::map(data,~lm(lifeExp~log(gdpPercap),data=.))) %>% 
   mutate(summr=map(model,broom::glance)) %>% 
   unnest(summr) %>% arrange(r.squared) %>% filter(r.squared<0.3) %>% 
   select(country) %>% left_join(gapminder_plus) %>% 
   ggplot()+
   geom_line(mapping = aes(x=log(gdpPercap),y=lifeExp,color=continent,group=country))+
   facet_wrap(~continent)
 ####### lifeExp-gdpPercap
by_country%>% 
   mutate(model=purrr::map(data,~lm(lifeExp~log(gdpPercap),data=.x))) %>% 
   mutate(summr=map(model,broom::glance)) %>% 
   unnest(summr) %>% arrange(r.squared) %>% filter(r.squared<0.1) %>% 
   select(country) %>% left_join(gapminder_plus) %>% 
   ggplot()+
   geom_point(mapping = aes(x=log(gdpPercap),y=lifeExp,color=country,group=country))+
   facet_wrap(~continent)
 
saveRDS(by_country,"by_country_tibble.rds") #保存某项具体的计算结果saveRDS
my_fresh_by_country<-readRDS("by_country_tibble.rds")
my_fresh_by_country

write_csv(gapminder_plus,"gapminder_plus_for_professor.csv")
