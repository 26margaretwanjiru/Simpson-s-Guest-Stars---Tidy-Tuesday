
#Load packages

library(tidyverse)
library(ggthemes)


#Load the data

simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")

View(simpsons)

#A bit of filtering

guest_role <- simpsons%>%
  group_by(guest_star)%>%
  count(role)%>%
  filter(n>10)

str(guest_role)


#Load the downloaded simpson's images

rabbi <-jpeg::readJPEG("C:/Users/MARGRET/Downloads/rabbi.jpg") 
a <- grid::rasterGrob(rabbi, interpolate = T) 

ftony<-jpeg::readJPEG("C:/Users/MARGRET/Downloads/ftony.jpg") 
b <- grid::rasterGrob(ftony, interpolate = T) 

sidebob <-png::readPNG("C:/Users/MARGRET/Downloads/sidebob.png") 
c <- grid::rasterGrob(sidebob, interpolate = T) 

edna <-png::readPNG("C:/Users/MARGRET/Downloads/edna.png") 
d <- grid::rasterGrob(edna, interpolate = T) 

lionel <-jpeg::readJPEG("C:/Users/MARGRET/Downloads/lionel.jpg") 
e <- grid::rasterGrob(lionel, interpolate = T) 

troy <-png::readPNG("C:/Users/MARGRET/Downloads/troy.png") 
f <- grid::rasterGrob(troy, interpolate = T) 


#The graph

count_guest <- guest_role%>%
  ggplot()+
  geom_col(aes(x=reorder(role,n),y = n, fill=guest_star), color='#2a2a2a',
           show.legend = T)+
  labs(x= "Role", y = "Count(n)")+
  geom_text(aes(role, n, label = n),vjust = -0.5,
            color = "black", fontface = "bold")+
  annotation_custom(d, xmin = 4, xmax =8,
                    ymin = 0, ymax =70) +
  annotation_custom(b, xmin = 3.6, xmax =6.4,
                    ymin = 40, ymax =90) +
  annotation_custom(c, xmin = 3.7, xmax =4.2,
                    ymin = 22, ymax =90)+
  annotation_custom(f, xmin = 2.7, xmax = 3.2,
                    ymin = 19, ymax =90)+
  annotation_custom(e, xmin = 1.7, xmax =2.2,
                    ymin = 14.1, ymax =90)+
  annotation_custom(a, xmin = 0.7, xmax =1.2,
                    ymin = 11.5, ymax =90)+
  labs(title = 'Simpsons Guest Stars - Tidy Tuesday', subtitle = "(Guest stars that featured in a particular role more than 10 times)")+
  labs(caption  = "Data Source: Wikipedia|Plot by @magwanjiru - Twitter")+
  theme_classic()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 15)
  )+
  theme(axis.text.x = element_text(colour = "black", face = "bold"),
        axis.text.y = element_text(colour = "black", face = "bold"))

count_guest


ggsave("count_guest.png", count_guest, height=20, width=40, units="cm")
