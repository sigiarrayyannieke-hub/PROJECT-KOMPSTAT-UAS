library(shiny)
library(bslib)
library(ggplot2)
library(moments)
library(readxl)

#Fungsi Modus
modus <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}
