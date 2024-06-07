
library(ggplot2)
library(dplyr)
library(datasets)

data(iris)

sepal_plot <- hist(iris$Sepal.Length)

#eureka I've got it!
