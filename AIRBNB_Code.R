
# =========================================================================================================
# MASTER 2 TIDE - PROJET ETUDE_DISCRIPTIVE_AIRBNB 2017-2018

# =========================================================================================================
# 
#
## download packages and library

rm(list=ls(all=TRUE)) 
install.packages('tidyverse') 
install.packages("leaflet", dependencies = TRUE)
install.packages('knitr', dependencies = TRUE)
install.packages("rmarkdown")
library(leaflet)
library(ggplot2)
library (dplyr)
library(stats)
library(tidyr)
library(data.table)
library(scales)

# _____________Importing Data _____________________________________________#

setwd("C:/Users/tranm/Desktop/Master 2 TIDE/Stage SAS-R-Python/R")
Airbnb <- fread("./Projet R/Airbnb.txt", encoding="UTF-8")


#_____________Overview on the datasets_____________________________________#

glimpse(Airbnb)
head(Airbnb)

###########______________ Rename column ________________########

names(Airbnb)[2]<-"Adress"
summary(Airbnb)

##########___________NA Treatment and Data Selection____________######### 

Airbnb1 <- Airbnb %>% 
  select(- c(neighbourhood_group,last_review))
Airbnb1 %>% count (is.na (reviews_per_month))

########______________ Outliers Studies _______________################

summary(Airbnb1$price)
summary(Airbnb1$minimum_nights)

#########_________________Visualization of Outliers _(extremes)_ values by boxplots __________########

my_theme <-   theme_light() +                                    ## Define a Theme
  theme(axis.title = element_text(face = "bold"),
        axis.title.x = element_text(color = "coral2"),
        axis.title.y = element_text(color = "coral2"),
        plot.title = element_text(color = "firebrick", vjust=0.5, hjust=0.5, size = 15, face = "bold"),
        axis.text.x = element_text(face = "italic"),
        plot.caption = element_text( face = "italic", colour = "coral", size = 10, hjust = 1))

ggplot(data = Airbnb1, mapping = aes(x = "", y = price )) +
  geom_boxplot(color = "coral3")+
  ggtitle ("Price's boxplot")+
  labs (caption = "based on data from Airbnb ")+
  theme_minimal() +
  my_theme

ggplot(data = Airbnb1, mapping = aes(x = "", y = minimum_nights )) +
  geom_boxplot(color = "coral3")+
  ggtitle ("Minimum_nights's boxplot")+
  labs (caption = "based on data from Airbnb ")+
  theme_minimal() +
  my_theme


############_____________Outliers Suppression_________###########

Airbnb1 <- Airbnb1 %>% 
  filter(price >= 0 & price < 3000, minimum_nights <= 365 )

ggplot(data = Airbnb1, mapping = aes(x = "", y = price )) +
  geom_boxplot()+
  ggtitle ("Price's boxplot")+
  labs (caption = "based on data from Airbnb ")+
  theme_minimal() +
  my_theme

ggplot(data = Airbnb1, mapping = aes(x = "", y = minimum_nights )) +
  geom_boxplot() +
  ggtitle ("Minimum_nights's boxplot")+
  labs (caption = "based on data from Airbnb ")+
  theme_minimal() +
  my_theme

summary(Airbnb1$price)
summary(Airbnb1$minimum_nights)
###############__________________ GRAPHS _____________________#################

########__________ Price's Distribution___________######


ggplot(Airbnb1, mapping=aes(x=price))+
  geom_histogram(na.rm = TRUE, color = "brown1", fill = "brown1", binwidth=3)+
  ylim(0,2000)+
  ggtitle("Price Distribution Overview")+
  ylab("count price")+
  labs (caption = "based on data from Airbnb ")+
  theme_minimal() +
  my_theme+
  theme (panel.background = element_rect (fill = "gray81"),
         panel.grid.major = element_blank(),
         panel.grid.minor.y = element_line(size = 0.5))

ggplot(data = Airbnb1, mapping=aes(x=price))+
  geom_histogram(na.rm=TRUE, color = "brown1", fill = "brown1",binwidth=5)+
  xlim(1,2000)+
  ggtitle("Price Distribution (0 - 2000)")+
  ylab("count price")+
  labs (caption = "based on data from Airbnb ")+
  theme_minimal() +
  my_theme+
  theme (panel.background = element_rect (fill = "gray81"),
         panel.grid.major = element_blank(),
         panel.grid.minor.y = element_line(size = 0.5))

ggplot(Airbnb1)+
  geom_histogram(mapping=aes(x=price), na.rm=TRUE, color = "brown1", fill = "white", binwidth=5)+
  xlim(1,300)+
  ggtitle("Price Distribution (0 - 300)")+
  ylab("count price")+
  labs (caption = "based on data from Airbnb ")+
  theme_minimal() +
  my_theme+
  theme (panel.background = element_rect (fill = "gray81"),
         panel.grid.major = element_blank(),
         panel.grid.minor.y = element_line(size = 0.5))

############___________________ CORRELATION MATRIX __________________##############

### Design a table that contains only numeric variables interested

Airbnb_num<- Airbnb1 %>% select_if(is.numeric)%>%
  select (-c(id, host_id))


### Matrix of linear correlations 

cor_x <- cor(Airbnb_num %>%na.omit ()) %>%                      # Calculate the correlation between the set of variables in the Airbnb_num table
  as.data.frame() %>%                                           # transform the results into a dataframe
  mutate(variable_x = rownames(.)) %>% 
  gather(variable_y, correlation, colnames(Airbnb_num))



ggplot(cor_x, aes(x = variable_x, y = variable_y, fill = correlation)) +
  geom_tile(color = "white", show.legend = T) +
  geom_text(aes(label = correlation %>% round(2)), color = "black") +
  labs(x = NULL, y = NULL) +
  scale_fill_gradient(low = "white", high  = "firebrick2",
                      space = "Lab", name = "Correlation \n" ) +
  ggtitle ("CORRELATION MATRIX")+
  labs (caption = "based on data from Airbnb " )+
  theme_minimal() +
  my_theme+
  theme (axis.text.x = element_text (angle = 45, vjust = 1, hjust = 1, size = 12))



## ____________________Average price / geographical area___________________#

price_neighbour <- Airbnb1 %>% group_by(neighbourhood) %>%
  summarise_at(vars(price,latitude,longitude),mean)

# Histogram Representation

ggplot (price_neighbour %>% na.omit(), aes (x=neighbourhood, y = price, fill = neighbourhood))+
  geom_bar(stat = "identity", size = 2)+
  geom_text(aes (label = price %>% round (2)), color = "darkred", vjust = -0.5, hjust = 0.5)+
  ggtitle("Price Analysis By Neighbourhood")+
  labs (y = "Average Price")+
  theme_minimal()+
  my_theme+
  theme ( axis.text.x = element_text (angle = 45, vjust = 1, hjust = 1, size = 12),
          panel.background = element_rect (fill = "gray81"),
          panel.grid.major = element_blank(),
          panel.grid.minor.y = element_line(size = 0.5))+
  labs (caption = "based on data from Airbnb " )

# Map representation

vue<- leaflet(price_neighbour%>% na.omit()) %>% addTiles() %>% 
  setView(lng = 2.292551, lat = 48.858259, zoom = 12)%>%
  addCircles(lat = ~ latitude, lng = ~ longitude, radius = ~ sqrt(price) * 50, opacity = 0.02, fillOpacity = 0.48, fillColor = "red", popup = ~ paste(neighbourhood, price %>% round (2),"Euros"))
vue


############________________ Average _ Price / Room type _________________################

###############_________Room_Type Distribution_______________#########

Airbnb_room <- Airbnb1  %>% 
  count(room_type) %>% 
  mutate(percent= n/sum(n))

ggplot(Airbnb_room, aes(x=room_type, y = percent, fill = room_type))+
  geom_bar(stat = "identity", fill = c ("lightslateblue", "steelblue1", "steelblue4"))+
  geom_text(label = scales::percent(Airbnb_room$percent), vjust = -0.25) +
  labs(title = "Room Distribution", y = "Percent", x = "Room_Type")+
  labs (caption = "based on data from Airbnb ")+
  my_theme+
  theme ( axis.text.x = element_text ( vjust = 1, hjust = 1, size = 12),
          panel.background = element_rect (fill = "whitesmoke"),
          panel.grid.major = element_blank(),
          panel.grid.minor.y = element_line(size = 0.5))



######__________ Distribution _ Price / Room type ____________#########

price_room <- Airbnb1 %>% group_by(room_type) %>%
  summarise (moy_prix = mean (price))


ggplot (price_room %>% na.omit(), aes (x=room_type,y=moy_prix, fill = room_type))+
  geom_bar(stat = "identity", size = 0.5, fill = c ("magenta3", "maroon2", "orchid1"))+
  ggtitle("Price Analysis By Room Type")+
  labs (caption = "based on data from Airbnb ")+
  geom_text(aes (label = moy_prix %>% round (2)), color = "darkred", size = 6, vjust = -0.5, hjust = 0.5)+
  labs (y = "Average Price")+
  my_theme+
  theme ( axis.text.x = element_text ( vjust = 1, hjust = 1, size = 12),
          panel.background = element_rect (fill = "whitesmoke"),
          panel.grid.major = element_blank(),
          panel.grid.minor.y = element_line(size = 0.5))

######__________ Distribution _ Price / Room type / neighbourhood ____________#########

price_room1<- Airbnb1 %>% group_by(room_type,neighbourhood) %>%
  summarise (moy_prix = mean (price))


ggplot(price_room1, mapping = aes(x = neighbourhood, y = moy_prix, fill = room_type)) + 
  geom_bar(stat = "identity",size = 8, position = position_dodge())+
  ggtitle("Price Analysis By Room Type & By District")+
  labs (caption = "based on data from Airbnb ")+
  labs (y = "Average Price")+
  my_theme+
  theme ( axis.text.x = element_text ( angle = 45, vjust = 1, hjust = 1, size = 12),
          panel.background = element_rect (fill = "whitesmoke"),
          panel.grid.major = element_blank(),
          panel.grid.minor.y = element_line(size = 0.5))


#_______________________ Map of offer per District Distribution __________________________________#

district<- Airbnb1%>%group_by(neighbourhood)%>%summarise_at(vars(latitude,longitude),mean)
district
C_neighnrh<-Airbnb1 %>% 
  count(neighbourhood)%>% 
  mutate(percent= n/sum(n)*100)
C_neighnrh
offre_zone<- leaflet(C_neighnrh%>% na.omit()) %>% addTiles() %>% 
  setView(lng = 2.292551, lat = 48.858259, zoom = 12)%>%
  addCircles(lat = ~ district$latitude, lng = ~ district$longitude, radius = ~ sqrt(n)* 10, opacity = 0.02, fillOpacity = 0.48, fillColor = "red", popup = ~ paste(neighbourhood,percent %>% round(2),"% offer by region"))
offre_zone


#_____________________ Number of reviews and Price  _____________________________________#

ggplot(Airbnb1, aes(x=number_of_reviews, y = price))+
  geom_point(stat = "identity", size = 3, show.legend = F, fill = "firebrick", color = "firebrick")+
  facet_wrap(~ room_type, nrow = 2)+
  labs (caption = "based on data from Airbnb ")+
  my_theme+
  theme ( axis.text.x = element_text ( vjust = 1, hjust = 1, size = 12),
          panel.background = element_rect (fill = "whitesmoke"),
          panel.grid.major = element_blank(),
          panel.grid.minor.y = element_line(size = 0.5))






