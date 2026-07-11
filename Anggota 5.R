library(shiny)
library(bslib)
library(readxl)
library(moments)
library(fitdistrplus)

#Semua branch harus ada bagian ini untuk upload data
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
  # UPLOAD DATA
  #=========================
  tabPanel(
    "Upload Data",
    br(),
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "file",
          "Upload CSV atau Excel",
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
  # DETEKSI OUTLIER
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
  # IDENTIFIKASI DISTRIBUSI
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
server <- function(input, output, session){
  
  data_upload <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    if(ext == "csv"){
      read.csv(input$file$datapath)
    }else{
      read_excel(input$file$datapath)
    }
  })
  
  output$preview <- renderTable({
    head(data_upload())
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
  
  #=========================
  # DETEKSI OUTLIER
  #=========================
  output$outlier <- renderPrint({
    req(input$variabel3)
    
    x <- data_upload()[[input$variabel3]]
    
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQRx <- IQR(x, na.rm = TRUE)
    
    bawah <- Q1 - 1.5 * IQRx
    atas <- Q3 + 1.5 * IQRx
    
    outlier <- x[x < bawah | x > atas]
    
    if(length(outlier) == 0){
      cat("Tidak terdapat outlier")
    }else{
      cat("Outlier ditemukan:\n")
      print(outlier)
    }
  })
  
  #=========================
  # IDENTIFIKASI DISTRIBUSI
  #=========================
  output$dist_plot <- renderPlot({
    req(input$variabel4)
    
    x <- data_upload()[[input$variabel4]]
    
    validate(
      need(is.numeric(x), "Pilih variabel numerik")
    )
    
    hist(
      x,
      probability = TRUE,
      col = "skyblue",
      main = paste("Histogram", input$variabel4),
      xlab = input$variabel4
    )
    
    lines(density(x, na.rm = TRUE), lwd = 2)
  })
  
  output$dist_info <- renderPrint({
    req(input$variabel4)
    
    x <- data_upload()[[input$variabel4]]
    
    validate(
      need(is.numeric(x), "Pilih variabel numerik")
    )
    
    x <- na.omit(x)
    
    cat("===== IDENTIFIKASI DISTRIBUSI =====\n\n")
    cat("Jumlah Data :", length(x), "\n\n")
    
    sk <- skewness(x)
    kt <- kurtosis(x)
    
    cat("Skewness :", round(sk, 4), "\n")
    cat("Kurtosis :", round(kt, 4), "\n\n")
    
    if(length(x) <= 5000){
      
      hasil <- shapiro.test(x)
      
      cat("===== UJI SHAPIRO-WILK =====\n\n")
      print(hasil)
      cat("\n")
      
      if(hasil$p.value > 0.05){
        cat("KESIMPULAN:\n")
        cat("Data cenderung berdistribusi NORMAL\n")
      }else{
        cat("KESIMPULAN:\n")
        cat("Data tidak berdistribusi normal\n")
      }
      
    }else{
      
      cat("Jumlah data > 5000\n")
      cat("Uji Shapiro-Wilk tidak dijalankan.\n")
      
    }
    
    cat("\n===== INTERPRETASI =====\n")
    
    if(abs(sk) < 0.5){
      cat("Distribusi relatif simetris.\n")
    }else if(sk > 0){
      cat("Distribusi cenderung Lognormal/Gamma (menceng ke kanan).\n")
    }else{
      cat("Distribusi menceng ke kiri.\n")
    }
  })
}

#=========================
# JALANKAN APLIKASI
#=========================
shinyApp(ui, server)