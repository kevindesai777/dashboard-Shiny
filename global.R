#Installing required packages
install.packages("shiny")
install.packages("jsonlite")
install.packages("ggplot2")
install.packages("googleVis")
#set options
options(scipen = 999)
#Load installed packages
library(shiny)
library(plyr)
library(googleVis)
library(ggplot2)
library(jsonlite)
library(plyr)
#run program
runApp("dashboard")


