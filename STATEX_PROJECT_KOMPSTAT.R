library(shiny)
library(bslib)

#UI-HOME
ui <- navbarPage(
  title = "STATEX",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#0B3C8C"
  ),

#isi tampilan home  
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
          h4("Petunjuk Penggunaan"),
          tags$ol(
            tags$li("Buka tab Upload Data."),
            tags$li("Unggah file CSV atau Excel."),
            tags$li("Pilih variabel yang akan dianalisis."),
            tags$li("Lihat hasil visualisasi data."),
            tags$li("Lihat statistika deskriptif."),
            tags$li("Lihat hasil deteksi outlier."),
            tags$li("Lihat identifikasi distribusi data.")
          )
        )
      )
    )
  )
)

#server
server <- function(input, output, session){}

library(shiny)
library(bslib)

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
  )
)

#=========================
# SERVER
#=========================
server <- function(input, output, session){}

#=========================
# JALANKAN APLIKASI
#=========================
shinyApp(ui, server)
