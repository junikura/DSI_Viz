#March 13th####

install.packages("tidyverse")
install.packages("socviz")
install.packages("ggplot2")

library(tidyverse)
library(socviz)
library(ggplot2)


install.packages("gapminder")
library(gapminder)


gapminder

p <- ggplot(data = gapminder, 
            
            mapping = aes(x = gdpPercap, y = lifeExp))

p + geom_point()


#March 16th####
install.packages("tidyverse")
install.packages("socviz")
install.packages("ggplot2")

library(tidyverse)
library(socviz)
library(ggplot2)

install.packages("gapminder")
library(gapminder)





#https://r-graph-gallery.com/all-graphs.html#google_vignette


p <- ggplot(data = gapminder, 
            
            mapping = aes(x = gdpPercap, y = lifeExp))

#geom_point allows for scatter plot
p + geom_point()


#geom_smooth allows for a line plot
p + geom_smooth()

#lm allows for linear model
p + geom_smooth(method = "lm")

#Combine 
p + geom_point() + geom_smooth(method = "lm")


#log scale
p + geom_point() + geom_smooth(method = "lm") +
  scale_x_log10()

#Dollar signs on x-axis labels
p + geom_point() + geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::dollar)


#http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations#use-scale_xx-functions
#https://ggplot2-book.org/scale-position.html

# Adding colour
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp,
                          colour = continent))

p + geom_smooth(method = "lm") + geom_point() +
  scale_x_log10(labels = scales::dollar)


# Grayscale
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_smooth(method = "lm") + geom_point(alpha = 0.3) +
  scale_x_log10(labels = scales::dollar)

#____________________________________________________________________________

# Data
data <- data.frame(
  name = c("DD","with himself","with DC","with Silur" ,"DC","with himself","with DD","with Silur" ,"Silur","with himself","with DD","with DC" ),
  average = sample(seq(1,10) , 12 , replace=T),
  number = sample(seq(4,39) , 12 , replace=T)
)

# Increase bottom margin
par(mar=c(6,4,4,4))


# Basic Barplot
my_bar <- barplot(data$average , border=F , names.arg=data$name , 
                  las=2 , 
                  col=c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
                  ylim=c(0,13) , 
                  main="" )

# Add abline
abline(v=c(4.9 , 9.7) , col="grey")

# Add the text 
text(my_bar, data$average+0.4 , paste("n: ", data$number, sep="") ,cex=1) 

#Legende
legend("topleft", legend = c("Alone","with Himself","With other genotype" ) , 
       col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.3,0.5,0.4,0.6) , rgb(0.3,0.9,0.4,0.6) ,  rgb(0.3,0.9,0.4,0.6)) , 
       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, horiz = FALSE, inset = c(0.05, 0.05))

#____________________________________________________________________________

ggsave(filename = "Viz_class_Mar_16.png", height = 8, width = 10)
ggsave(filename = "Viz_class_Mar_16.pdf", height = 8, width = 10)


# Cheatsheet https://github.com/rstudio/cheatsheets/blob/main/data-visualization-2.1.pdf 
# R graphics cookbook: https://r-graphics.org/



#March 18th####

library(tidyverse)
library(socviz)
library(ggplot2)
library(gapminder)

gapminder

#GFP over time per country
p <- ggplot(data = gapminder, 
       
       mapping = aes(x = year, y = gdpPercap))

p + geom_line(aes(group = country))

#Faceting data
p <- ggplot(data = gapminder, 
            
            mapping = aes(x = year, y = gdpPercap))

p + geom_line(aes(group = country)) + facet_wrap(~continent)

#Make country trend light grey
#Adding trend line 
# Making y axis log and show values in dollars 
# 
p <- ggplot(data = gapminder, 
            
            mapping = aes(x = year, y = gdpPercap))

p + geom_line(color = "grey70", aes(group = country)) +
          geom_smooth(size=1.1, method = "loess", se= FALSE)+
    facet_wrap(~continent, ncol=5) + 
    labs (x = "Year", 
          y = "GDP per capita",
          Title = "GDP per capita pn Five Continents")
  


p + geom_line(aes(group = country)) + facet_wrap(~continent)



gss_sm

#facet_grid()

p <- ggplot(data = gss_sm, mapping = aes(x = age, y= childs))

p + geom_point(alpha = 0.2) + geom_smooth() + facet_grid(sex~race)







