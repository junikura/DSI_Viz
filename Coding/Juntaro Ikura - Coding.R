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


#stat_functions 

p <- ggplot(data=gss_sm, mapping = aes(x = bigregion))

p + geom_bar()


#non-default stat

p <-ggplot(data=gss_sm, mapping = aes(x = bigregion))

p + geom_bar(mapping = aes(y = ..prop.., group = 1))
             

# Setting up a fill
p <- ggplot(data = gss_sm, mapping = aes(x = bigregion, fill = religion))

p + geom_bar()

#Breaking down faceted by frequency 

p <- ggplot(data = gss_sm, mapping = aes(x = religion))

p + geom_bar(position = “dodge”, mapping = aes(y = ..prop.., group = bigregion)) + 
  facet_wrap(~bigregion, ncol = 1)


#Histograp with default bin
p <- ggplot(data = midwest, mapping = aes(x = area))

p + geom_histogram()

#Histogram with manual number of bins (10)
p <- ggplot(data = midwest, mapping = aes(x = area))

p + geom_histogram(bins = 10)

#Histogram - comparing distributions
oh_wi <- c("OH", "WI")

p <- ggplot(data = subset(midwest, subset = state %in% oh_wi),
         mapping = aes(x = percollege, fill = state))

p + geom_histogram(alpha = 0.4, bins = 20)

#Density plots 

p <- ggplot(data = midwest,mapping = aes(x = area))

p + geom_density()

#Modify density plots 
oh_wi <- c("OH", "WI")

p <- ggplot(data = subset(midwest, subset = state %in% oh_wi),
         mapping = aes(x = area, fill = state, color = state))

p + geom_density(alpha = 0.3)


#https://datavizcatalogue.com/search.html


#Continuoous variables by group or category 

#Piping
organdata |> select(1:6) |> sample_n(size = 5)

#Groups and facets using geom_line and plotting

p <- ggplot(data = organdata, mapping = aes(x = year, y = donors))

p + geom_line(aes(group = country)) + facet_wrap(~country)


# Using geom_boxplot

p <- ggplot(data = organdata, mapping = aes(x = country , y =
                                           donors))
                                           
p + geom_boxplot()

#Improving above boxplot (coord_flip)

p <- ggplot(data = organdata, mapping = aes(x = country, y =
                                           donors))
p + geom_boxplot() + coord_flip()


#Reordering based on high number of donors to low number of donoros
p <- ggplot(data = organdata, mapping = aes(x = reorder(country,
                                                     donors, na.rm = TRUE), 
                                                      y = donors)) 
p + geom_boxplot() + coord_flip()


#Colour points based on variable "world"
p <- ggplot(data = organdata, mapping = aes(x = reorder(country,
                                                         donors, na.rm = TRUE), 
                                                         y = donors,
                                                          color = world))
         p + geom_point() + coord_flip()
         

#Using geom_jitter 
         
p <- ggplot(data = organdata, mapping = aes(x = reorder(country,
                                                        donors, na.rm = TRUE), 
                                                        y = donors, 
                                                        color=world))
p + geom_jitter() + coord_flip()



#Piping our data / summarizing
by_country <-
  organdata |> group_by(consent_law, country) |>
  summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) |>
  ungroup()  



#Cleveland dotplot
p <- ggplot(data = by_country, mapping = aes(x = donors_mean,
                                          y = reorder(country, donors_mean), 
                                          color = consent_law))

p + geom_point(size=3) + labs(x = "Donor Procurement Rate", 
                              y = "", color = "Consent Law") + 
                              theme(legend.position = "top")



#Add text labels to points 

p <- ggplot(data = by_country, mapping = aes(x = roads_mean, y = donors_mean))

p + geom_point() + geom_text(mapping = aes(label = country))


#Changing position of text 
p <- ggplot(data = by_country, mapping = aes(x = roads_mean, y = donors_mean))

p + geom_point() + geom_text(mapping = aes(label = country), hjust = 0)



install.packages("ggrepel")
library(ggrepel)


p <- ggplot(elections_historic, aes(x = popular_pct, y = ec_pct,
                                 label = winner_label))
p + geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +

  labs(x = "Winner’s share of Popular Vote", y = "Winner’s share of Electoral
  College Votes", title = "Presidential Elections: Popular & Electoral
  College Margins", subtitle = "1824 - 2016")












#March 20th####


by_country <-
  organdata |> group_by(consent_law, country) |>
  summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) |>
  ungroup()

##try using subset 

p <- ggplot(data = by_country, mapping = aes(x = gdp_mean, y =
                                            health_mean))
p + geom_point() +

geom_text_repel(data = subset(by_country, gdp_mean > 25000),
                  mapping = aes(label = country))


#Annotating plots with text

p <- ggplot(data = organdata, mapping = aes(x = roads, y = donors))

p + geom_point() + annotate(geom = "text", x = 91, y = 33, label =
"A surprisingly high \n recovery rate.", hjust = 0)

#Annotating plots with shapes

p <- ggplot(data = organdata, mapping = aes(x = roads, y = donors))

p + geom_point() + 
  annotate(geom = "rect", xmin = 85, xmax = 135, ymin = 30, ymax = 35,
           fill = "red", alpha = 0.2) + 
          annotate(geom = "text", x = 91, y = 33, 
           label = "A surprisingly high \n recovery rate.", hjust = 0)


#New section

library(socviz)
head(asasec)


p <- ggplot(data = subset(asasec, Year ==2014), 
            mapping = aes(x = Members, y = Revenues))

p + geom_point() + geom_smooth()


# Modifying above

p <- ggplot(data = subset(asasec, Year == 2014), mapping =
           aes(x = Members,y = Revenues))

p + geom_point(mapping = aes(color = Journal)) +
    geom_smooth(method = "lm")

# Intermediate ggplot object

p0 <- ggplot(data = subset(asasec, Year == 2014), mapping =
           aes(x = Members,y = Revenues,label=Sname))

p1 <- p0 + geom_smooth(method = "lm", se = FALSE, color =
                     "gray80") + geom_point(mapping = 
                      aes(color = Journal))

p2 <- p1 + geom_text_repel(data = subset(asasec, Year == 2014 &
Revenues > 7000), size = 2)

p2



# Adding more intermediate variables 

p3 <- p2 + labs(x="Membership",
            y="Revenues",
            color = "Section has own Journal",
            title = "ASA Sections",
            subtitle = "2014 Calendar year.",
            caption = "Source: ASA annual report.")

p4 <- p3 + scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "top")

p4

# New section 

library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()


#Example using colour palettes 

p <- ggplot(data = organdata, mapping = aes(x = roads, y =
                                           donors, color = world))

p + geom_point(size = 2) + scale_color_brewer(palette = "Set2") +
    
    theme(legend.position = "top")

#HEX code palettes -> https://paletton.com/#uid=3030u0kllllaFw0g0qFqFg0w0aF
#Colour code cheat shet -> http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf



#Manually choosing colours 

cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p4 + scale_color_manual(values = cb_palette)

#Highlight data with colours

head(county_data)


#Defining US election part colours 

party_colors <- c("#2E74C0", "#CB454A")

p0 <- ggplot(data = subset(county_data, flipped == "No"),
         mapping = aes(x = pop,y = black/100))

p1 <- p0 + geom_point(alpha = 0.15, color = "gray50") +
  scale_x_log10(labels=scales::comma)

p1


#Including colour(county's that have flipped colours)
p2 <- p1 + geom_point(data = subset(county_data, flipped ==
                                  "Yes"), 
                      mapping = aes(x = pop, y = black/100, 
                                    color = partywinner16)) +
  
    scale_color_manual(values = party_colors)

p2

#Labeling axes, graph, and legend

p3 <- p2 + scale_y_continuous(labels=scales::percent) +
  
      labs(color = "County flipped to ... ",
       x = "County Population (log scale)",
       y = "Percent Black Population",
       title = "Flipped counties, 2016",
       caption = "Counties in gray did not flip.")

p3

# Hilighting flipped counties with high percentage of black residents

p4 <- p3 + geom_text_repel(data = subset(county_data, 
                            flipped == "Yes" & black > 25), 
                           mapping = aes(x = pop, y = black/100, 
                            label = state), size = 2)

p4 + theme_minimal() + theme(legend.position="top")


#Changing themes

theme_set(theme_dark())

p4

#ggthemes

install.packages("ggthemes")
library(ggthemes)
theme_set(theme_economist())
p4

#reset theme

theme_set(theme_minimal())
p4
p4 + theme_economist()