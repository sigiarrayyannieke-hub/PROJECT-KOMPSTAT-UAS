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

#SERVER
server <- function(input, output, session){
  data_upload <- reactive({req(input$file)
    ext <- tools::file_ext(input$file$name)
    if(ext == "csv"){read.csv(input$file$datapath)
      } else{read_excel(input$file$datapath)}})
  output$pilih_var <- renderUI({req(data_upload())
    selectInput("variabel", "Pilih Variabel", choices = names(data_upload()))})
  output$pilih_var2 <- renderUI({req(data_upload())
    selectInput("variabel2", "Pilih Variabel", choices = names(data_upload()))})
  
  #Visualisasi
  output$plot <- renderPlot({req(input$variabel)
    x <- data_upload()[[input$variabel]]
    if(input$grafik == "Histogram"){
      hist(x, col = "skyblue", main = "Histogram", xlab = input$variabel)}
    if(input$grafik == "Boxplot"){
      boxplot(x, col = "lightgreen", main = "Boxplot")}
    if(input$grafik == "Bar Chart"){
      barplot(table(x), col = "orange", main = "Bar Chart")}
    if(input$grafik == "Pie Chart"){
      pie(table(x), main = "Pie Chart")}})
  
  #Statistika Deskriptif
  output$deskriptif <- renderTable({req(input$variabel2)
    x <- data_upload()[[input$variabel2]]
    cv <- sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
    data.frame(Statistik = c("Jumlah Data", "Minimum", "Q1", "Median", 
                             "Mean", "Q3", "Maksimum", "Modus", 
                             "Varians", "Standar Deviasi", "CV (%)", 
                             "IQR", "Skewness", "Kurtosis"),
               Nilai = round(c(length(x),
                               min(x, na.rm = TRUE),
                               quantile(x, 0.25, na.rm = TRUE),
                               median(x, na.rm = TRUE),
                               mean(x, na.rm = TRUE),
                               quantile(x, 0.75, na.rm = TRUE),
                               max(x, na.rm = TRUE),
                               modus(x),
                               var(x, na.rm = TRUE),
                               sd(x, na.rm = TRUE),
                               cv,
                               IQR(x, na.rm = TRUE),
                               skewness(x, na.rm = TRUE),
                               kurtosis(x, na.rm = TRUE)), 4))})
  output$interpretasi <- renderText({req(input$variabel2)
    x <- data_upload()[[input$variabel2]]
    cv <- sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
    sk <- skewness(x, na.rm = TRUE)
    hasil <- ""
    if(sk > 0)
      hasil <- paste(hasil, "Distribusi menceng ke kanan.\n")
    if(sk < 0)
      hasil <- paste(hasil, "Distribusi menceng ke kiri.\n")
    if(abs(sk) < 0.5)
      hasil <- paste(hasil, "Distribusi relatif simetris.\n")
    if(cv < 20){
      hasil <- paste(hasil, "Data relatif homogen.")
    } else{
      hasil <- paste(hasil, "Data relatif heterogen.")}
    hasil})}

