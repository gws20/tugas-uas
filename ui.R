library(shiny)
library(googleVis)
library(DT)
library(shinythemes)

navbarPage(
  "15.8637",
  tabPanel("Input",
             ####upload file####
             pageWithSidebar(
               headerPanel("Membaca data dari drive"),
               sidebarPanel(
                 tags$style(type='text/css', ".well { max-width: 20em; }"),
                 tags$head(
                   tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
                   tags$style(type="text/css", "select { width: 100%}"),
                   tags$style(type="text/css", "input { width: 19em; max-width:100%}")
                 ),
                 selectInput("readFunction", "Fungsi masukan:", c(
                   "read.table",
                   "read.csv",
                   "read.csv2",
                   "read.delim",
                   "read.delim2",
                   # foreign functions:
                   "read.spss",
                   "read.arff",
                   "read.dbf",
                   "read.dta",
                   "read.epiiinfo",
                   "read.mtp",
                   "read.octave",
                   "read.ssd",
                   "read.systat",
                   "read.xport",
                   # Advanced functions:
                   "scan",
                   "readLines"
                 )),
                 tags$hr(),
                 # Argument selecter:
                 htmlOutput("ArgSelect"),
                 # Argument field:
                 htmlOutput("ArgText"),
                 htmlOutput("ArgKet"),
                 tags$hr(),
                 # Upload data:
                 fileInput("file", "Upload data-file:"),
                 # Variable selection:
                 htmlOutput("varselect"),
                 textInput("name","disimpan dengan nama:",""),
                 actionButton("simpan","Simpan"),
                 div(textOutput("txtInputSukses"),style="color:blue")
               ),
               mainPanel(
                 DT::dataTableOutput("tabel_advance")
               )
             )
  ),
  tabPanel("Regresi",
           ####regresi####
           pageWithSidebar(
             headerPanel('Regresi'),
             sidebarPanel(htmlOutput("sideReg")),
             mainPanel(
               tabsetPanel(
                 tabPanel("Fitting", verbatimTextOutput("Fitting")),
                 tabPanel("Summary", verbatimTextOutput("summaryRegresi")),
                 tabPanel("Plot", plotOutput("plotRegresi")),
                 tabPanel("Prediksi",
                    fluidRow(
                      column(3, wellPanel(
                        selectInput("var_prediksi", "Prediksi nilai Variabel : ",c("y","x"))
                      )),
                      column(3, wellPanel(
                        numericInput("nilai_prediksi", "Nilai :", value = 0)
                      )),
                      column(3,
                             textOutput("lblHasil_prediksi"),
                             htmlOutput("hasil_prediksi")
                      )
                 )
               )
             )
           )
  )),
  tabPanel("ANOVA",
           ####ANOVA####
           pageWithSidebar(
             headerPanel("Analisis Of Varians"),
             sidebarPanel(
               selectInput("pilih_tipe_anova","Jenis ANOVA",c("satu arah","dua arah")),
               htmlOutput("sideAov"),
               numericInput("alpha_anova","Alpha :",value = 0.05,min = 0,max = 1,step = 0.01)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Tabel", 
                          verbatimTextOutput("tabel_anova")
                          ),
                 tabPanel("Asumsi", 
                          htmlOutput("asumsi_anova"),
                          tags$hr(),
                          "Uji Kenormalan :",
                          verbatimTextOutput("summary_asumsi_normal1"),
                          verbatimTextOutput("summary_asumsi_normal21"),
                          verbatimTextOutput("summary_asumsi_normal22"),
                          "Uji Kehomogenan Varian :",
                          verbatimTextOutput("summary_asumsi_homogen")
                 ),
                 tabPanel("Analisa",htmlOutput("analisa_anova"))
               )
             )
           )
  ),
  tabPanel("Summary",
           ####summary####
           verbatimTextOutput("summary_statistik")
  ),
  #####korelasi####
  tabPanel("Korelasi",htmlOutput("corr")),
  tabPanel("Residual",
           ####residual####
    pageWithSidebar(
      headerPanel("Analisis Residual"),
      sidebarPanel(
        htmlOutput("side_residual")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Histogram",plotOutput("residuals_hist")),
          tabPanel("Scatter",plotOutput("residuals_scatter")),
          tabPanel("QQ plot",plotOutput("residuals_qqline"))
        )
      )
    )
  ),
  navbarMenu("Visual",
    tabPanel("Histogram",
             ####histogram####
             # Sidebar with a slider input for the number of bins
             pageWithSidebar(
               headerPanel("Histogram"),
               sidebarPanel(
                 htmlOutput("var_histogram"),
                 sliderInput("bins_histogram",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("plot_histogram")
               )
             )
    ),
    tabPanel("Cluster",
             ####cluster####
             pageWithSidebar(
               headerPanel('clustering'),
               sidebarPanel(
                 htmlOutput("var"),
                 numericInput('clusters', 'Cluster count', 3,
                              min = 1, max = 9)
               ),
               mainPanel(
                 plotOutput('plot1')
               )
             )
             
             
             #------------------#
    ),
    
    
    tabPanel("Scatter Plot",
             ####scatter####
      pageWithSidebar(
        headerPanel("Scatter Plot"),
        sidebarPanel(htmlOutput("side_scatter")),
        mainPanel(plotOutput("scatter"))
      )
    )
  ),
  navbarMenu("About",
    tabPanel("About Me", htmlOutput("aboutme")),
    tabPanel("Dokumentasi", htmlOutput("aplikasi"))
  ),
  
  theme = shinytheme('flatly'),
  tags$head(tags$script(src = "script.js"))
)

