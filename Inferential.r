install.packages("gapminder")
library(gapminder)
data("gapminder")
View(gapminder)
install.packages("dplyr")
library(dplyr)  #used to manipulate data

#pipe operator LtoR
view(gapminder)
df1 <- gapminder %>%  
  select(country, lifeExp) %>%
  filter(country == "South Africa" | country == "Ireland")
  t.test(data=df1,lifeExp~country)

  install.packages("ggplot2")
  library(ggplot2)
  gapminder %>%
    filter(gdpPercap < 50000) %>%
    ggplot(aes(x=gdpPercap,y=lifeExp))+
    geom_point()
  gapminder %>%
    filter(gdpPercap < 50000) %>%
    ggplot(aes(x=log(gdpPercap),y=lifeExp))+
    geom_point()
  gapminder %>%
    filter(gdpPercap < 50000) %>%
    ggplot(aes(x=gdpPercap,y=lifeExp,col=continent,size=pop))+
    geom_point(aplha=0.6)
  gapminder %>%
    filter(gdpPercap < 50000) %>%
    ggplot(aes(x=gdpPercap,y=lifeExp,col=continent,size=pop))+
    geom_point(aplha=0.3)+
    geom_smooth(method=lm)+
    facet_wrap(~continent)
  gapminder %>%
    filter(gdpPercap < 50000) %>%
    ggplot(aes(x=gdpPercap,y=lifeExp,col=year,size=pop))+
    geom_point(aplha=0.3)+
    geom_smooth(method=lm)+
    facet_wrap(~continent)
  