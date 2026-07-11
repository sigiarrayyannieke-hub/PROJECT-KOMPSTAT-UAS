library(shiny)
library(bslib)
library(ggplot2)
library(moments)
library(readxl)

#Fungsi Modus
modus <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

#UI
ui <- navbarPage(title = "STATEX", 
                 theme = bs_theme(bootswatch = "flatly", primary = "#0B3C8C"),
  #Visualisasi
  tabPanel("Visualisasi", br(), 
           sidebarLayout(sidebarPanel(
             fileInput("file", "Upload CSV atau Excel",
                       accept = c(".csv", ".xlsx")),
             uiOutput("pilih_var"), 
             selectInput("grafik", "Pilih Grafik",
                         c("Histogram", "Boxplot", "Bar Chart", "Pie Chart"))),
             mainPanel(plotOutput("plot")))),
  #Statistika Deskriptif
  tabPanel("Statistika Deskriptif", br(),
           sidebarLayout(sidebarPanel(
             uiOutput("pilih_var2")),
             mainPanel(h3("Hasil Statistika Deskriptif"),
                       tableOutput("deskriptif"),
                       br(), h3("Interpretasi"),
                       verbatimTextOutput("interpretasi")))))
