library(shiny)
library(bslib)
library(readxl)
library(ggplot2)
library(moments)
library(fitdistrplus)

#=========================
# FUNGSI MODUS
#=========================
modus <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#=========================
# UI
#=========================
ui <- navbarPage(
  title = "STATEX",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#0B3C8C"
  ),
  
  #=========================
  # HOME
  #=========================
  tabPanel(
    "Home",
    br(),
    fluidRow(
      column(
        12,
        wellPanel(
          h1("STATEX"),
          h3("Statistical Data Explorer"),
          p("Aplikasi eksplorasi data dan statistika deskriptif berbasis R Shiny."),
          h4("Fitur Utama"),
          tags$ul(
            tags$li("Upload CSV dan Excel"),
            tags$li("Visualisasi Data"),
            tags$li("Statistika Deskriptif"),
            tags$li("Deteksi Outlier"),
            tags$li("Interpretasi Otomatis")
          ),
          h4("Petunjuk"),
          tags$ol(
            tags$li("Upload data pada tab Upload Data"),
            tags$li("Pilih variabel numerik"),
            tags$li("Lihat visualisasi"),
            tags$li("Lihat hasil statistika deskriptif"),
            tags$li("Lihat deteksi outlier"),
            tags$li("Lihat identifikasi distribusi")
            
          )
        )
      )
    )
  ),
  
  #=========================
  # MATERI
  #=========================
  tabPanel(
    "Materi",
    br(),
    h3("Mean"),
    p("Rata-rata data."),
    h3("Median"),
    p("Nilai tengah data."),
    h3("Modus"),
    p("Nilai yang paling sering muncul."),
    h3("Varians"),
    p("Ukuran penyebaran data terhadap rata-rata."),
    h3("Standar Deviasi"),
    p("Akar dari varians."),
    h3("IQR"),
    p("Selisih antara Q3 dan Q1."),
    h3("Skewness"),
    p("Ukuran kemencengan distribusi."),
    h3("Kurtosis"),
    p("Ukuran keruncingan distribusi.")
  ),
  
  #=========================
  # UPLOAD DATA
  #=========================
  tabPanel(
    "Upload Data",
    br(),
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "file",
          "Upload CSV atau Excel (Maksimal 50 MB)",
          accept = c(".csv", ".xlsx")
        )
      ),
      mainPanel(
        h3("Preview Data"),
        tableOutput("preview")
      )
    )
  ),
  
  #=========================
  # VISUALISASI
  #=========================
  tabPanel(
    "Visualisasi",
    br(),
    sidebarLayout(
      sidebarPanel(
        uiOutput("pilih_var"),
        selectInput(
          "grafik",
          "Pilih Grafik",
          choices = c(
            "Histogram",
            "Boxplot",
            "Bar Chart",
            "Pie Chart"
          )
        )
      ),
      mainPanel(
        plotOutput("plot")
      )
    )
  ),
  
  #=========================
  # STATISTIKA DESKRIPTIF
  #=========================
  tabPanel(
    "Statistika Deskriptif",
    br(),
    sidebarLayout(
      sidebarPanel(
        uiOutput("pilih_var2")
      ),
      mainPanel(
        h3("Hasil Statistika Deskriptif"),
        tableOutput("deskriptif"),
        br(),
        h3("Interpretasi"),
        verbatimTextOutput("interpretasi")
      )
    )
  ),
  
  #=========================
  # OUTLIER
  #=========================
  tabPanel(
    "Deteksi Outlier",
    br(),
    sidebarLayout(
      sidebarPanel(
        uiOutput("pilih_var3")
      ),
      mainPanel(
        h3("Outlier"),
        verbatimTextOutput("outlier")
      )
    )
  ),
  
  #=========================
  # DISTRIBUSI
  #=========================
  tabPanel(
    "Identifikasi Distribusi",
    br(),
    sidebarLayout(
      sidebarPanel(
        uiOutput("pilih_var4")
      ),
      mainPanel(
        h3("Hasil Identifikasi Distribusi"),
        verbatimTextOutput("dist_info"),
        br(),
        plotOutput("dist_plot")
      )
    )
  )
)

#=========================
# SERVER
#=========================
server <- function(input, output, session) {
  data_upload <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if(ext == "csv"){
      read.csv(input$file$datapath)
    } else {
      read_excel(input$file$datapath)
    }
  })
  output$preview <- renderTable({
    head(data_upload())
  })
  output$pilih_var <- renderUI({
    req(data_upload())
    selectInput(
      "variabel",
      "Pilih Variabel",
      choices = names(data_upload())
    )
  })
  output$pilih_var2 <- renderUI({
    req(data_upload())
    selectInput(
      "variabel2",
      "Pilih Variabel",
      choices = names(data_upload())
    )
  })
  output$pilih_var3 <- renderUI({
    req(data_upload())
    selectInput(
      "variabel3",
      "Pilih Variabel",
      choices = names(data_upload())
    )
  })
  output$pilih_var4 <- renderUI({
    req(data_upload())
    selectInput(
      "variabel4",
      "Pilih Variabel",
      choices = names(data_upload())
    )
  })
  output$plot <- renderPlot({
    req(input$variabel)
    x <- data_upload()[[input$variabel]]
    if(input$grafik=="Histogram"){
      hist(
        x,
        col="skyblue",
        main="Histogram",
        xlab=input$variabel
      )
    }
    if(input$grafik=="Boxplot"){
      boxplot(
        x,
        col="lightgreen",
        main="Boxplot"
      )
    }
    if(input$grafik=="Bar Chart"){
      barplot(
        table(x),
        col="orange",
        main="Bar Chart"
      )
    }
    if(input$grafik=="Pie Chart"){
      pie(
        table(x),
        main="Pie Chart"
      )
    }
  })
  output$deskriptif <- renderTable({
    req(input$variabel2)
    x <- data_upload()[[input$variabel2]]
    cv <- sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100
    data.frame(
      Statistik = c(
        "Jumlah Data",
        "Minimum",
        "Q1",
        "Median",
        "Mean",
        "Q3",
        "Maksimum",
        "Modus",
        "Varians",
        "Standar Deviasi",
        "CV (%)",
        "IQR",
        "Skewness",
        "Kurtosis"
      ),
      Nilai = round(c(
        length(x),
        min(x,na.rm=TRUE),
        quantile(x,0.25,na.rm=TRUE),
        median(x,na.rm=TRUE),
        mean(x,na.rm=TRUE),
        quantile(x,0.75,na.rm=TRUE),
        max(x,na.rm=TRUE),
        modus(x),
        var(x,na.rm=TRUE),
        sd(x,na.rm=TRUE),
        cv,
        IQR(x,na.rm=TRUE),
        skewness(x,na.rm=TRUE),
        kurtosis(x,na.rm=TRUE)
      ),4)
    )
  })
  output$interpretasi <- renderText({
    req(input$variabel2)
    x <- data_upload()[[input$variabel2]]
    cv <- sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100
    sk <- skewness(x,na.rm=TRUE)
    hasil <- ""
    if(sk > 0){
      hasil <- paste(
        hasil,
        "Distribusi menceng ke kanan.\n"
      )
    }
    if(sk < 0){
      hasil <- paste(
        hasil,
        "Distribusi menceng ke kiri.\n"
      )
    }
    if(abs(sk) < 0.5){
      hasil <- paste(
        hasil,
        "Distribusi relatif simetris.\n"
      )
    }
    if(cv < 20){
      hasil <- paste(
        hasil,
        "Data relatif homogen."
      )
    } else {
      
      hasil <- paste(
        hasil,
        "Data relatif heterogen."
      )
    }
    hasil
  })
  output$outlier <- renderPrint({
    req(input$variabel3)
    x <- data_upload()[[input$variabel3]]
    Q1 <- quantile(x,0.25,na.rm=TRUE)
    Q3 <- quantile(x,0.75,na.rm=TRUE)
    IQRx <- IQR(x,na.rm=TRUE)
    bawah <- Q1 - 1.5*IQRx
    atas <- Q3 + 1.5*IQRx
    outlier <- x[x < bawah | x > atas]
    if(length(outlier)==0){
      cat("Tidak terdapat outlier")
    } else {
      cat("Outlier ditemukan:\n")
      print(outlier)
    }
  })
  output$dist_plot <- renderPlot({
    req(input$variabel4)
    x <- data_upload()[[input$variabel4]]
    validate(
      need(is.numeric(x),
           "Pilih variabel numerik")
    )
    hist(
      x,
      probability = TRUE,
      col = "skyblue",
      main = paste("Histogram", input$variabel4),
      xlab = input$variabel4
    )
    lines(
      density(x, na.rm = TRUE),
      lwd = 2
    )
  })
  output$dist_info <- renderPrint({
    req(input$variabel4)
    x <- data_upload()[[input$variabel4]]
    validate(
      need(is.numeric(x),
           "Pilih variabel numerik")
    )
    x <- na.omit(x)
    cat("===== IDENTIFIKASI DISTRIBUSI =====\n\n")
    cat("Jumlah Data :", length(x), "\n\n")
    sk <- skewness(x)
    kt <- kurtosis(x)
    cat("Skewness :", round(sk,4), "\n")
    cat("Kurtosis :", round(kt,4), "\n\n")
    if(length(x) <= 5000){
      hasil <- shapiro.test(x)
      cat("===== UJI SHAPIRO-WILK =====\n\n")
      print(hasil)
      cat("\n")
      if(hasil$p.value > 0.05){
        cat("KESIMPULAN:\n")
        cat("Data cenderung berdistribusi NORMAL\n")
      } else {
        cat("KESIMPULAN:\n")
        cat("Data tidak berdistribusi normal\n")
      }
    } else {
      cat("Jumlah data > 5000\n")
      cat("Uji Shapiro-Wilk tidak dijalankan.\n")
    }
    cat("\n===== INTERPRETASI =====\n")
    if(abs(sk) < 0.5){
      cat("Distribusi relatif simetris.\n")
    } else if(sk > 0){
      cat("Distribusi cenderung Lognormal/Gamma (menceng ke kanan).\n")
    } else {
      cat("Distribusi menceng ke kiri.\n")
    }
  })
}

#=========================
# JALANKAN APLIKASI
#=========================
shinyApp(ui, server)