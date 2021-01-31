install.packages("gganimate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("readr")

library(gganimate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# The code below was used to create the GIFs on the COVID-19 web map I made for the School District of Philadelphia,
# which can be seen here: 
# https://tesladubois-d.maps.arcgis.com/apps/MapSeries/index.html?appid=29a0e036b0714963b1d31531c119b7fb 

# Set the working directory.
setwd("C:/Users/tdubois-adm/Downloads") #update this path as necessary.

# Unzip the file and read in the data.
unzip("AzaveaJobApp-main.zip")
data <- read.csv("AzaveaJobApp-main/SampleCodeData.csv")

# Make sure the date variable is formatted as such. 
data$Date <- as.Date(data$Date)

# Subset data into cases (C_data) and deaths (D_data).
C_data <- subset(data, select = -c(X, Deaths)) #taking out the column for deaths to make a cases data frame. 
D_data <- subset(data, select = -c(X, Cases)) #taking out the column for cases to make a deaths data frame.

# Create a lit of school IDs to loop through.
schools <- unique(data$ES_ID) #This list of 160 schools takes hours to run and is not necessary for you to see how this code works.
schools <- schools[1:3] #So we'll reduce it to 3 schools.

# Make a folder to put these in. 
dir.create("AzaveaJobApp-main/GIFs") #This will show up in the unzipped version of the 'AzaveaJobApp-main' folder. 

# Run through a for loop to generate a separate GIF for cases and deaths attributed to each catchment. 
# Each catchments GIF will have their data displayed by a red line, which is drawn over the gray lines of all other catchments. 

# Start the loop
for(s in schools){  
  # Print the school ID it's working on.
  print(s) 
  # Subset the data to highlight the school it's on.
  school_data <- C_data[which(C_data$ES_ID == s),]
  # We need a separate date variable for the school in focus to be able to reveal their data at a different pace than the rest. 
  school_data$Date2 <- school_data$Date
  # Start a ggplot.
  p <- ggplot(data=C_data) + # can't I do this instead? ggplot()
    # Draw everyone's lines in gray.
    geom_line(data=C_data, aes(x=Date, y=Cases, group=ES_ID), color = "grey")+
    # Draw school in focus in red. 
    geom_line(data=school_data, aes(x=Date2, y=Cases, group=ES_ID), color = "red", size = 1.2)+
    # Have a big read point lead the drawing of the focus school.
    geom_point(data=school_data, aes(x=Date2, y=Cases, group=ES_ID), color = "red", size = 5)+
    # Show the date changing as it adds each data point to the school in focus. 
    labs(title = "Date: {frame_along}")+
    # We're displaying this in a pop-up on a map, so it needs to be very basic. 
    theme_classic()+
    # Hardly any text and the text that is there needs to be big.
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title = element_text(size=22),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(face = "bold",size=18))+
    # The GIF is changing frames based on the Date2 variable we made for the school in focus. 
    transition_reveal(Date2)
  # This sets what should be animated, how fast should it change, and the size of the output. 
  animate(p, fps=20, height = 250, width =300)
  # Save the resulting GIF to the GIF folder. Name it based on the variable displayed and the elementary school ID (ES_ID). 
  anim_save(paste0("AzaveaJobApp-main/GIFs/cases_", s, ".gif"))
  # Start the exact same thing over again, but this time for deaths rather than cases. 
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
  anim_save(paste0("AzaveaJobApp-main/GIFs/deaths_", s, ".gif"))
}
