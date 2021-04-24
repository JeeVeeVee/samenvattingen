#installing packages
install.packages("tidyverse")


#calling packages
library.package(tidyverse)

#calling functionality from another file
source("commands.R")

#see all objects from your session
objects()

#remove objects
rm(x, y, z)

#declaring variables
a <- 2
b <- tekst
vector <- c(1, 2, 4, 5)

#reading a dataset
aardbevingen <- read_csv("datasets/aardbevingen.csv")

names(aardbevingen) # returns names of the cols
glimpse(aardbevingen) # returns a preview of the data
View(aardbevingen) # shows the data

# a dataframe is a row from a dataset

# matrix 
a <- matrix(c(1,2,4,3,4,5,6), 
    nrow = 2, 
    ncol = 4, 
    byrow = TRUE)