library(shiny)
library(bslib)
library(readxl)

#Semua bagian harus ada bagian ini agar bisa upload data
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
          "Upload CSV atau Excel (Maksimal 50 MB)",
          accept = c(".csv", ".xlsx")
        )
      ),
      mainPanel(
        h3("Preview Data"),
        tableOutput("preview")
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
}

#=========================
# JALANKAN APLIKASI
#=========================
shinyApp(ui, server)

