library(gganimate)
library(dplyr)
library(tidyr)
library(readr)

data <- read.csv("downloads/exampleCodeData.csv") #update this path
C_data <- subset(data, select = -c(Deaths))
D_data <- subset(data, select = -c(Cases))

schools <- unique(data$ES_ID)

for(s in schools){
  print(s)
  school_data <- C_data[which(C_data$ES_ID == s),]
  school_data$Date2 <- school_data$Date
  p <- ggplot(data=C_data) +
    geom_line(data=C_data, aes(x=Date, y=Cases, group=ES_ID), color = "grey")+
    geom_line(data=school_data, aes(x=Date2, y=Cases, group=ES_ID), color = "red", size = 1.2)+
    geom_point(data=school_data, aes(x=Date2, y=Cases, group=ES_ID), color = "red", size = 5)+
    labs(title = "Date: {frame_along}")+
    theme_classic()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(size=22),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(face = "bold",size=18))+
    transition_reveal(Date2)
  animate(p, fps=20, height = 250, width =300)
  anim_save(paste0("GIFS/cases_", s, ".gif"))
  school_data <- D_data[which(D_data$ES_ID == s),]
  school_data$Date2 <- school_data$Date
  p <- ggplot(data=D_data) +
    geom_line(data=D_data, aes(x=Date, y=Deaths, group=ES_ID), color = "grey")+
    geom_line(data=school_data, aes(x=Date2, y=Deaths, group=ES_ID), color = "red", size = 1.2)+
    geom_point(data=school_data, aes(x=Date2, y=Deaths, group=ES_ID), color = "red", size = 5)+
    labs(title = "Date: {frame_along}")+
    theme_classic()+ 
    theme(axis.title.x=element_blank(), 
          axis.title.y=element_blank(), 
          plot.title = element_text(size=22),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(face = "bold",size=18))+
    transition_reveal(Date2) 
  animate(p, fps=20, height = 250, width =300)
  anim_save(paste0("GIFS/deaths_", s, ".gif"))
}