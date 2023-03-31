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
#March 23th####

#Activity (Alt-text)

#Two researcher'a in discussion 

#Activity 2 (Descriptove content)

#Level 1 - How brilliant people in past generations have organized their 24 hour day. 
#Categories are sleep, creative work, day job/admin, food/leisure, exercise, and other  



#Level 2- The average of how brilliant people in past generations have organized their day



#Level 3- How longer hours of creative work along with sleeping earlier and waking up earlier have shaped the brilliant minds of past generations  



#Level 4 -


#https://www.rgd.ca/database/files/library/RGD_AccessAbility2_Handbook_2021_09_28.pdf
#March 25th####

library(tidyverse)
library(socviz)
library(ggplot2)
library(gapminder)
library(ggrepel)

model_colors <- RColorBrewer::brewer.pal(3, "Set1")
model_colors

p0 <- ggplot(data = gapminder, mapping = aes(x = log(gdpPercap), y =
                                           lifeExp))
p1 <- p0 + geom_point(alpha = 0.2) +
    geom_smooth(method = "lm", aes(color = "OLS", fill = "OLS")) +

    geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3),
aes(color = "Cubic Spline", fill = "Cubic Spline")) +
  
  geom_smooth(method = "loess", aes(color = "LOESS", fill = "LOESS"))

#Colour
p1 + scale_color_manual(name = "Models", values =
model_colors) + scale_fill_manual(name = "Models",
values = model_colors) + theme(legend.position = "top")

#Show legend
p1 + scale_color_manual(name = "Models", values = model_colors) +
scale_fill_manual(name = "Models", values = model_colors) +
theme(legend.position = "top")

#Gapminder model object
out <- lm(formula = lifeExp ~ gdpPercap + pop +
       continent, data = gapminder)

str(out)                    


min_gdp <- min(gapminder$gdpPercap)
max_gdp <- max(gapminder$gdpPercap)
med_pop <- median(gapminder$pop)
pred_df <- expand.grid(
          gdpPercap = (seq(from = min_gdp, to = max_gdp, length.out = 100)), 
          pop = med_pop,
          continent = c("Africa", "Americas", "Asia", "Europe", "Oceania"))

#Making predictions
pred_out <- predict(object = out, newdata = pred_df, interval = "predict")
head(pred_out)

#fit is predicted value

#bind data and predictions 
pred_df <- cbind(pred_df, pred_out)
head(pred_df)

#How does per capita GDP affect life expectacy in Europe and Africa 

p <- ggplot(data = subset(pred_df, continent %in% c("Europe",
  "Africa")), aes(x = gdpPercap, y = fit, ymin = lwr, ymax = upr, color
  = continent, fill = continent, group = continent))
                                                                   
p + geom_point(data = subset(gapminder, continent %in% c("Europe",
      "Africa")), aes(x = gdpPercap, y = lifeExp, color = continent), alpha
      = 0.5, inherit.aes = FALSE) +
      
    geom_line() + geom_ribbon(alpha = 0.2, color = FALSE) +
      scale_x_log10(labels = scales::dollar)

#Activity - How does per capita GDP affect life expectancy in the America's? 

p <- ggplot(data = subset(pred_df, continent %in% c("Americas","Asia")), 
                  aes(x = gdpPercap, y = fit, ymin = lwr, ymax = upr, color
                  = continent, fill = continent, group = continent))

p + geom_point(data = subset(gapminder, continent %in% c("Americas", "Asia")), 
               aes(x = gdpPercap, y = lifeExp, color = continent), alpha
               = 0.5, inherit.aes = FALSE) +
  
  geom_line() + geom_ribbon(alpha = 0.2, color = FALSE) +
  scale_x_log10(labels = scales::dollar)

#Broom
install.packages("broom")
library(broom)
library(tidyverse)
library(ggplot2)

out_conf <- tidy(out)

#Component level statistics with tidy()


library(dplyr)

out_conf |> mutate_if(is.numeric, round, digits = 3)


p <- ggplot(out_conf, mapping = aes(x = term, y = estimate))
p + geom_point() + coord_flip()

out_conf <- subset(out_conf, term %nin% "(Intercept)")
out_conf$nicelabs <- prefix_strip(out_conf$term, "continent")

#Component level statistics with tidy()
p <- ggplot(out_conf, mapping = aes(x = reorder(nicelabs, estimate), y = estimate, ymin = conf.low, ymax = conf.high))

p +geom_pointrange() + coord_flip() + labs(x = "", y = "OLS Estimate")

#Component level statistics with tidy()

p <- ggplot(out_conf, mapping = aes(x = reorder(nicelabs, estimate), y =
                                   estimate, ymin = conf.low, ymax = conf.high))

p + geom_pointrange() + coord_flip() + labs(x = "", y = "OLS Estimate")

#Observation level statistics with augment()

out_aug <- augment(out)
head(out_aug) |> round_df()


#Observation level statistics with augment()

p <- ggplot(data = out_aug, mapping = aes(x = .fitted, y =
                                         .resid))

p + geom_point()

#Observation level statistics with glance()

glance(out) |> round_df()


#Grouped analysis and list columns 

eu77 <- gapminder |> filter(continent == "Europe", year == 1977)

fit <- lm(lifeExp ~ log(gdpPercap), data = eu77)

summary(fit)

#Nesting data 

out_le <- gapminder |>
  group_by(continent, year) |
  nest()
out_le


#Filtering and unnesting list columns 

out_le |> filter(continent == "Europe" & year == 1977) unnest()


#Nesting

fit_ols <- function(df) {lm(lifeExp ~ log(gdpPercap), data = df)}
out_le <- gapminder |>
group_by(continent, year) |>
  nest() |>
  mutate(model = map(data, fit_ols))
out_le


#Nested and tidied data

fit_ols <- function(df) { lm(lifeExp ~ log(gdpPercap), data = df)}

out_tidy <- gapminder |>  group_by(continent, year) nest() |>
  mutate(model = map(data, fit_ols), tidied = map(model, tidy)) |>
  unnest( tidied) |> filter(term %nin% "(Intercept)" & continent %nin% "Oceania")

#Plotting nested and tidied data 

p <- ggplot(data = out_tidy, mapping = aes(x = year, y = estimate,
                                        ymin = estimate - 2*std.error, ymax = estimate + 2*std.error, group
                                        = continent, color = continent))

p + geom_pointrange(position = position_dodge(width = 1)) +
  scale_x_continuous(breaks = unique(gapminder$year)) +
  theme(legend.position = "top") +
  labs(x = "Year", y = "Estimate", color = "Continent")


#Plotting nested and tidied data

p <- ggplot(data = out_tidy, mapping = aes(x = year, y = estimate, ymin = estimate - 2*std.error, ymax
                                        = estimate + 2*std.error, group = continent, color = continent))


p + geom_pointrange(position = position_dodge(width = 1)) +
                                          
  scale_x_continuous(breaks = unique(gapminder$year)) +
  theme(legend.position = "top") +
  labs(x = "Year", y = "Estimate", color = "Continent")

                            


#March 27th####

Talk Question: That was an amazing talk! Just wondering in the CP research space, is the non-hereditary CP still widely studied? Im guessing the genetic component is much more hot and exciting to study, but just wondering about the non-hereditary


#March 30th####

#loading packages 
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(tidyverse)
library(socviz)

#review data 
election |> select(state, total_vote,
                   r_points, pct_trump, party, census) |>
  sample_n(5)


#Denotes party colours
party_colors <- c("#2E74C0", "#CB454A") 


p0 <- ggplot(data = subset(election, st %nin% "DC"), mapping = aes(x = r_points, y = reorder(state, r_points), color = party))

p1 <- p0 + geom_vline(xintercept = 0, color = "gray30") +
  geom_point(size = 2)

p2 <- p1 + scale_color_manual(values = party_colors)

p3 <- p2 + scale_x_continuous(breaks = c( 30, 20, 10, 0, 10, 20, 30,
                                      40), labels = c("30 n (Clinton)", "20", "10", "0", "10", "20", "30","40 n(Trump)"))

p3 + facet_wrap(~ census, ncol=1, scales="free_y") +
  guides(color=FALSE) + labs(x = "Point Margin", y = "") +
  theme(axis.text=element_text(size=8))


install.packages("maps")
library(maps)
us_states <- map_data("state")
head(us_states)

# Black and white map
p <- ggplot(data = us_states, mapping = aes(x =long, y = lat, group = group))
p + geom_polygon(fill = "white", color = "black")

# Colouring map
p <- ggplot(data = us_states, aes(x = long, y = lat, group = group, fill = region))
p + geom_polygon(color = "gray90", linewidth = 0.1) + guides(fill = FALSE)

#Adding lines
p <- ggplot(data = us_states, mapping = aes(x = long, y = lat, group = group, fill = region))
p + geom_polygon(color = "gray90", size = 0.1) +
coord_map(projection = "albers", lat0 = 39, lat1 = 45) + guides(fill = FALSE)

#Adding election data to map

election$region <- tolower(election$state)
us_states_elec <- left_join(us_states, election, by = "region")

# Add data to map 

p0 <- ggplot(data = us_states_elec, mapping = aes(x = long, y = lat,
                                              group = group, fill = party))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

p2 <- p1 + scale_fill_manual(values = party_colors) +
  labs(title = "Election Results 2016", fill = NULL)

p2 + theme_map()


#County map 

county_map |>
  sample_n(5)

county_data |>
  select(id, name, state, pop_dens) |>
  sample_n(5)

#Plotting population density 

county_full <- left_join(county_map, county_data, by = "id")
head(county_full)

#Population density by county 

p <- ggplot(data = county_full, mapping = aes(x = long, y = lat, fill = pop_dens, group =
                         group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()

p2 <- p1 + scale_fill_brewer(palette="Blues",
            labels = c("0 10", "10 50", "50 100", "100 500", "500 1,000", "1,000 5,000", ">5,000"))

p2 + labs(fill = "Population per\nsquare mile") +
  
  theme_map() +
  
  guides(fill = guide_legend(nrow = 1)) +
  
  theme(legend.position = "bottom")

#new section
#https://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html
#https://kateto.net/netscix2016.html
#https://www.jessesadler.com/post/network-analysis-with-r/

install.packages("DiagrammeR")
library(DiagrammeR)

grViz(diagram = "digraph flowchart {
  node [fontname = arial, shape = square, color = purple]
  tab1 [label = '@@1']
  tab2 [label = '@@2']
  tab3 [label = '@@3']
  tab4 [label = '@@4']
  
  tabs1 -> tab2 -> tab3 -> tab4; }

  [1]: 'Artefact collection in field'
  [2]: 'Preliminary daying of artefacts (visual)'
  [3]: 'Artefacts sent to lab for dating'
  [4]: 'Jun'
  ") 



