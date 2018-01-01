library(shiny)
library(googleVis)
library(DT)
library(shinythemes)
library(rsconnect)

function(input, output, session) {
  v <- reactiveValues(data = NULL)
  ### Argument names:
  ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  # Argument selector:
  output$ArgSelect <- renderUI({
    if (length(ArgNames())==0) return(NULL)
    selectInput("arg","Argument:",ArgNames())
  })
  ## Arg text field:
  output$ArgText <- renderUI({
    fun__arg <- paste0(input$readFunction,"__",input$arg)
    if (is.null(input$arg)) return(NULL)
    Defaults <- formals(input$readFunction)
    if (is.null(input[[fun__arg]]))
    {
      textInput(fun__arg, label = "Masukan value dari argumen terpilih:", value = deparse(Defaults[[input$arg]]))
    } else {
      textInput(fun__arg, label = "Masukan value dari argumen terpilih:", value = input[[fun__arg]])
    }
  })
  
  ### Data import:
  Dataset <- reactive({
    if (is.null(input$file)) {
      #jika belum ada yg di input
      return(data.frame())
    }
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    argList <- argList[names(argList) %in% ArgNames()]
    Dataset <- as.data.frame(do.call(input$readFunction,c(list(input$file$datapath),argList)))
    return(Dataset)
  })
  
  # Select variables:
  output$varselect <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    # Variable selection:    
    selectInput("vars", "Variables yang dipakai:",
                names(Dataset()), names(Dataset()), multiple =TRUE)
  })
  
  # button simpan
  observeEvent(input$simpan, {
    ifelse ((is.null(input$file)), 
       (v$suksesInput<-paste("file belum dipilih")),
       ifelse ((length(input$name)<1 | is.null(input$name) | input$name=="" ),
          (v$suksesInput<-"nama data belum diset"),
          {
            v$suksesInput <- paste0("Data '",input$name,"' sudah berhasil tersimpan")
            assign(input$name, data.frame(Dataset()[,input$vars,drop=FALSE]))
            v$dataSelect<- assign(input$name, data.frame(Dataset()[,input$vars,drop=FALSE]))
            }
        )
    )
  })  
  ####datanya####
  pilihData<-reactive(Dataset()[,input$vars,drop=FALSE])
  
  # Show table:
  output$table <- renderTable({
    if (is.null(input$vars) || length(input$vars)==0) return(NULL)
    return(Dataset()[,input$vars,drop=FALSE])
  })
  output$tabel_advance <- DT::renderDataTable(DT::datatable(
    cbind(State = rownames(pilihData()), pilihData()),
    options = list(pageLength = 10), rownames = FALSE)
  )
  # notif status
  output$txtInputSukses <- renderText({
    paste(v$suksesInput)
  })

  output$pilihData<- renderUI({
    # Variable selection:    
    selectInput("pilih", "Variables yang dipakai:",
                names(pilihData()), names(pilihData()))
  })
  
  ####simple input####
  ## masih crash....
  output$contents_simple_input <- renderTable({
    
    inFile <- input$file_simple
    
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header_simple, sep=input$sep_simple, 
                          quote=input$quote_simple)
    
    
  })
  ####input dari library####
  ## masih dalam pengembangan
  #datalib<-(get(input$dataLib))
  #output$table_datalib <- renderTable({
  #  return(datalib())
  #})
  
  ####cluster####
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    pilihData()[, c(input$xcol, input$ycol)]
  })  
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$var<- renderUI({
    # Variable selection: 
    if(is.null(input$file$datapath)){return()}
    else list (
    selectInput("ycol", "variabel y:",
                names(pilihData()), names(pilihData())[1]),
    selectInput("xcol", "variabel x:",
                names(pilihData()), names(pilihData())[2])
    )
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })

  ####regresi####
  
  rls<-function(x,y){
    b1<-(length(x)*sum(x*y) - sum(x)*sum(y))/(length(x)*sum(x^2)-sum(x)^2);
    b0<-mean(y)-b1*mean(x);
    cat("Fitting model regresi sederhananya adalah : \n");
    if (b1==0) {cat("bukan fungsi regresi")}
    else {
      if (b1>0) {cat("E(yh) = ", b0," + ",b1,"X")}
      else {cat("E(yh) = ", b0," - ",abs(b1),"X")};
    }
  }
  
  Ynew<-function(Xnew,x,y){
    b1<-(length(x)*sum(x*y) - sum(x)*sum(y))/(length(x)*sum(x^2)-sum(x)^2);
    b0<-mean(y)-b1*mean(x);
    return(b0+Xnew*b1);
  }
  
  Xnew<-function(Ynew,x,y){
    b1<-(length(x)*sum(x*y) - sum(x)*sum(y))/(length(x)*sum(x^2)-sum(x)^2);
    b0<-mean(y)-b1*mean(x);
    return((Ynew-b0)/b1);
  }
  
  output$sideReg<- renderUI({
    # Variable selection:  
    if(is.null(input$file$datapath)){return()}
    else list (
      selectInput("ycolReg", "variabel y:", names(pilihData()), names(pilihData())[1]),
      selectInput("xcolReg", "variabel x:", names(pilihData()), names(pilihData())[2]))
  })
  
  
  #summary
  output$summaryRegresi<-renderPrint({
    y<-pilihData()[,input$ycolReg]
    x<-pilihData()[,input$xcolReg]
    data<-pilihData()
    summary(lm(y~x,data))
  })
  output$Fitting<-renderPrint ({
    rls(pilihData()[,input$xcolReg],pilihData()[,input$ycolReg])
  })
  
  output$plotRegresi<-renderPlot({
    y<-pilihData()[,input$ycolReg]
    x<-pilihData()[,input$xcolReg]
    data<-pilihData()
    fit<-lm(y~x,data)
    par(mar = c(4, 4, 1, 1))
    plot(y~x, data, pch = 19, col = 'gray')
    abline(fit, col = 'red', lwd = 2)
  })
  
  #prediksi
  observe({
    c_var <- input$var_prediksi
    if (c_var == "x") {c_in<-"y"}
    else {c_in<-"x"}
    updateNumericInput(session, "nilai_prediksi", 
                       label = paste("Nilai variabel ", c_in," :"))
  })
  
  output$lblHasil_prediksi<-renderText(
    if(input$var_prediksi=="x") {"Hasil prediksi nilai x :"}
    else {"Hasil prediksi nilai y :"}
  )
  
  output$hasil_prediksi<-renderPrint({
    y<-pilihData()[,input$ycolReg]
    x<-pilihData()[,input$xcolReg]
    if(input$var_prediksi=="x") {
      Xnew(input$nilai_prediksi,x,y)
    }
    else {Ynew(input$nilai_prediksi,x,y)}
  })
  
  
  ####anova####
  anov1<-function(y,x){
    anov<-unlist(summary(aov(y~x)))
    atribut<-c("SoV","Derajat bebas","Sum of Square","Mean Suare","F-hitung","P-Value")
    Sumber<-c("Treatment","Error","Total")
    df<-c(anov[1],anov[2],anov[1]+anov[2])
    ss<-c(anov[3],anov[4],anov[3]+anov[4])
    ms<-c(anov[5],anov[6],NA)
    Fval<-c(anov[7],anov[8],NA)
    Pval<-c(anov[9],anov[10],NA)
    tabel<-data.frame(Sumber,df,ss,ms,Fval,Pval)
    colnames(tabel)<-atribut
    return(tabel)
  }
  anov2<-function(y,A,B){
    options(digits = 0)
    anov<-unlist(summary(aov(y~A+B)))
    atribut<-c("SoV","Derajat bebas","Sum of Square","Mean Suare","F-hitung","P-Value")
    Sumber<-c("Faktor A","Faktor B","Error","Total")
    df<-c(anov[1],anov[2],anov[3],anov[1]+anov[2]+anov[3])
    ss<-c(anov[4],anov[5],anov[6],anov[5]+anov[4]+anov[6])
    ms<-c(anov[7],anov[8],anov[9],NA)
    Fval<-c(anov[10],anov[11],anov[12],NA)
    Pval<-c(anov[13],anov[14],anov[15],NA)
    tabel<-data.frame(Sumber,df,ss,ms,Fval,Pval)
    colnames(tabel)<-atribut
    return(tabel)
  }
  
  output$sideAov<- renderUI({
    # Variable selection:  
    if(is.null(input$file$datapath)){return()}
    else list (
      selectInput("yAov", "variabel dependen:", names(pilihData()), names(pilihData())[1]),
      if (input$pilih_tipe_anova=="satu arah") {
        selectInput("xAov", "variabel independen:", names(pilihData()), names(pilihData())[2])}
      else {list(
        selectInput("Faktor_A", "Faktor A:", names(pilihData()), names(pilihData())[2]),
        
        selectInput("Faktor_B", "Faktor B:", names(pilihData()), names(pilihData())[3]))
      })
    
  })
  output$summary_asumsi_normal1<-renderPrint({
    data<-pilihData()
    y<-data[,input$yAov]
    if (input$pilih_tipe_anova=="satu arah"){
      x<-data[,input$xAov]
      by(data = y, INDICES = x,FUN = shapiro.test)
    }
  })
  output$summary_asumsi_normal21<-renderPrint({
    data<-pilihData()
    y<-data[,input$yAov]
    if (input$pilih_tipe_anova=="dua arah"){
      Faktor_A<-data[,input$Faktor_A]
      by(data = y, INDICES = Faktor_A,FUN = shapiro.test)
    }
  })
  output$summary_asumsi_normal22<-renderPrint({
    data<-pilihData()
    y<-data[,input$yAov]
    if (input$pilih_tipe_anova=="dua arah"){
      Faktor_B<-data[,input$Faktor_B]
      by(data = y, INDICES = Faktor_B,FUN = shapiro.test)
    }
  })
  output$summary_asumsi_homogen<-renderPrint({
    data<-pilihData()
    y<-data[,input$yAov]
    if (input$pilih_tipe_anova=="satu arah"){
      x<-data[,input$xAov]
      bartlett.test(y~x,data = data)
      
    }
    else {
      Faktor_A<-data[,input$Faktor_A]
      Faktor_B<-data[,input$Faktor_B]
      bartlett.test(y~c(Faktor_A+Faktor_B),data = data)
    }
  })
  
  normal_anova<-reactive({
    data<-pilihData()
    y<-data[,input$yAov]
    if (input$pilih_tipe_anova=="satu arah"){
      x<-data[,input$xAov]
      normal<-by(data = y, INDICES = x,FUN = shapiro.test)
    }
    else {
      Faktor_A<-data[,input$Faktor_A]
      Faktor_B<-data[,input$Faktor_B]
      normal<-c(by(data = y, INDICES = Faktor_A,FUN = shapiro.test),
                by(data = y, INDICES = Faktor_B,FUN = shapiro.test))
    }
    
    n<-length(normal)
    for(i in seq(1,n,4)){
      if(unlist(normal)[i+1]>input$alpha_anova) return("gagal tolak H0")
    }
    return("tolak H0")
  })
  homogen_anova<-reactive({
    data<-pilihData()
    y<-data[,input$yAov]
    if (input$pilih_tipe_anova=="satu arah"){
      x<-data[,input$xAov]
      homo<- bartlett.test(y~x,data = data)
      
    }
    else {
      Faktor_A<-data[,input$Faktor_A]
      Faktor_B<-data[,input$Faktor_B]
      homo<-bartlett.test(y~c(Faktor_A+Faktor_B),data = data)
    }
    if(unlist(homo)[3]>input$alpha_anova) return("gagal tolak H0")
    return("tolak H0")
    
  })
  output$asumsi_anova<-renderUI({
    list(
      if(normal_anova()=="gagal tolak H0"){
        p("Asumsi normalitas terpenuhi",style="color:green")}
      else {
        p("Asumsi normalitas tidak terpenuhi",style="color:red")
      },
      if(homogen_anova()=="gagal tolak H0"){
        p("Asumsi Homogenitas varian terpenuhi",style="color:green")}
      else {
        p("Asumsi Homogenitas varian tidak terpenuhi",style="color:red")
      }
    )
  })
  #output$tek<-renderText(normal_anova())
  anov<-reactive({
    data<-pilihData()
    y<-data[,input$yAov]
    if (input$pilih_tipe_anova=="satu arah"){
      x<-data[,input$xAov]
      aov<-aov(y~x,data)
      #anov1(y,x)
    }
    else {
      Faktor_A<-data[,input$Faktor_A]
      Faktor_B<-data[,input$Faktor_B]
      aov<-aov(y~Faktor_A+Faktor_B,data)
      #anov2(y,Faktor_A,Faktor_B)
    }
  })
  #output$tabel_anova1<-renderTable(unlist(summary(anov)))
  
  output$tabel_anova_satu<-renderTable({
      data<-pilihData()
      y<-data[,input$yAov]
      x<-data[,input$xAov]
      anov1(y,x)
  })
  output$tabel_anova<-renderPrint({
    
    if ((normal_anova()!="tolak H0")&(homogen_anova()!="tolak H0")) {summary(anov())}
    else {return(cat("Tabel tidak dapat ditampilkan karena asumsi tidak terpenuhi"))}
  })
  
  output$analisa_anova<-renderUI({
    if ((normal_anova()!="tolak H0")&(homogen_anova()!="tolak H0")) {
      if (input$pilih_tipe_anova=="satu arah"){
        list(
          tags$p("H0: rata-rata kelompok faktor adalah sama"),
          tags$p("H1: setidaknya terdapat sepasang rata-rata yang berbeda"),
          tags$hr(),
          tags$p("\nAlpha  : ", input$alpha_anova),
          tags$p("\nP-value: ", unlist(summary(anov()))[9]),
          if(unlist(summary(anov()))[9]>input$alpha_anova){list(
            tags$p("Kesimpulan  : Gagal tolak H0"),
            tags$p("Intepretasi : Dengan taraf signifikansi ",input$alpha_anova," cukup bukti bahwa rata-ratanya sama")
          )}
          else {list(
            tags$p("Kesimpulan  : Tolak H0"),
            tags$p("Intepretasi : Dengan taraf signifikansi ",input$alpha_anova," cukup bukti bahwa rata-ratanya berbeda")
          )}
        )}
      else {list(
        tags$p("H0(A): rata-rata kelompok faktor A adalah sama"),
        tags$p("H1(A): setidaknya terdapat sepasang rata-rata yang berbeda"),
        tags$p("H0(B): rata-rata kelompok faktor B adalah sama"),
        tags$p("H1(B): setidaknya terdapat sepasang rata-rata yang berbeda"),
        tags$hr(),
        tags$p("\nAlpha  : ", input$alpha_anova),
        tags$p("\nP-value A: ", unlist(summary(anov()))[13]),
        tags$p("\nP-value B: ", unlist(summary(anov()))[14]),
        if(unlist(summary(anov()))[13]>input$alpha_anova){
          if(unlist(summary(anov()))[14]>input$alpha_anova){list(
            tags$p("Kesimpulan  1 : Gagal tolak H0(A)"),
            tags$p("Kesimpulan  2 : Gagal tolak H0(B)"),
            tags$p("Intepretasi 1 : Dengan taraf signifikansi ",input$alpha_anova," cukup bukti bahwa rata kelompok Faktor A sama"),
            tags$p("Intepretasi 1 : Dengan taraf signifikansi ",input$alpha_anova," cukup bukti bahwa rata kelompok Faktor B sama")
          )}
          else {list(
            tags$p("Kesimpulan  1 : Gagal tolak H0(A)"),
            tags$p("Kesimpulan  2 : Tolak H0(B)"),
            tags$p("Intepretasi 1 : Dengan taraf signifikansi ",input$alpha_anova," cukup bukti bahwa rata kelompok Faktor A sama"),
            tags$p("Intepretasi 1 : Dengan taraf signifikansi ",input$alpha_anova," cukup bukti bahwa rata kelompok Faktor B berbeda")
          )}
        }
        else {
          if(unlist(summary(anov()))[14]>input$alpha_anova){list(
            tags$p("Kesimpulan  1 : Tolak H0(A)"),
            tags$p("Kesimpulan  2 : Gagal tolak H0(B)"),
            tags$p("Intepretasi 1 : Dengan taraf signifikansi ",input$alpha_anova," cukup bukti bahwa rata kelompok Faktor A berbeda"),
            tags$p("Intepretasi 1 : Dengan taraf signifikansi ",input$alpha_anova," cukup bukti bahwa rata kelompok Faktor B sama")
          )}
          else {list(
            tags$p("Kesimpulan  1 : Tolak H0(A)"),
            tags$p("Kesimpulan  2 : Tolak H0(B)"),
            tags$p("Intepretasi 1 : Dengan taraf signifikansi ",input$alpha_anova," cukup bukti bahwa rata kelompok Faktor A berbeda"),
            tags$p("Intepretasi 1 : Dengan taraf signifikansi ",input$alpha_anova," cukup bukti bahwa rata kelompok Faktor B berbeda")
          )}
        }
      )}
    }
    else {
      tags$p("Analisis tidak dapat ditampilkan, karena terdapat pelanggaran asumsi",syle="color:red")
    }
  })

  #### summary####
  output$summary_statistik<-renderPrint(
    summary(pilihData())
  )
  
  ####korelasi####
  
  output$corr <- renderGvis({
    d <- pilihData()[,sapply(pilihData(),is.integer)|sapply(pilihData(),is.numeric)] 
    cor <- as.data.frame(round(cor(d), 2))
    cor <- cbind(Variables = rownames(cor), cor)
    gvisTable(cor) 
  })
  
  #### residuals ####
  output$side_residual<-renderUI({
    # Variable selection:  
    if(is.null(input$file$datapath)){return()}
    else list (
      selectInput("y_res", "variabel y:", names(pilihData()), names(pilihData())[1]),
      selectInput("x_res", "variabel x:", names(pilihData()), names(pilihData())[2]))
  })
  output$residuals_hist <- renderPlot({
    x<-pilihData()[,input$x_res]
    y<-pilihData()[,input$y_res]
    model<-lm(y~x,data = pilihData())
    hist(model$residuals, main = paste(input$y_res, ' ~ ', input$x_res), xlab = 'Residuals') 
  })
  
  output$residuals_scatter <- renderPlot({
    x<-pilihData()[,input$x_res]
    y<-pilihData()[,input$y_res]
    model<-lm(y~x,data = pilihData())
    plot(model$residuals ~ x, xlab = x, ylab = 'Residuals')
    abline(h = 0, lty = 3) 
  })
  
  output$residuals_qqline <- renderPlot({
    x<-pilihData()[,input$x_res]
    y<-pilihData()[,input$y_res]
    model<-lm(y~x,data = pilihData())
    qqnorm(model$residuals)
    qqline(model$residuals) 
  })
  
  #### Visual ####
  #Histogram
  # Select variables:
  output$var_histogram <- renderUI({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    # Variable selection:    
    selectInput("var_histogram", "Variables yang dipakai:",
                names(pilihData()), names(pilihData())[1])
  })
  output$plot_histogram <- renderPlot({
    x    <- pilihData()[, input$var_histogram]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins_histogram + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', xlab = input$var_histogram)
  })
  
  
  ####SCatter####
  output$side_scatter<-renderUI({
    # Variable selection:  
    if(is.null(input$file$datapath)){return()}
    else list (
      selectInput("y_scatter", "variabel y:", names(pilihData()), names(pilihData())[1]),
      selectInput("x_scatter", "variabel x:", names(pilihData()), names(pilihData())[2]))
  })
  output$scatter<-renderPlot({
    x<-pilihData()[,input$x_scatter]
    y<-pilihData()[,input$y_scatter]
    plot(x,y,
         xlab = input$x_scatter, ylab = input$y_scatter,  main = paste("Scatter Plot ",input$name), pch = 16, 
         col = "black", cex = 1) 
  })
####about####
  ####abaut me####
  output$aboutme<-renderUI({list(
    tags$p("Nama  \t: Ginanjar Wahyu Subekti"),
    tags$p("NIM   \t: 15.8637"),
    tags$p("Kelas \t: 2KS3")
  )})
  ####aplikasi####
  output$aplikasi<-renderUI({list(
    tags$ul("INPUT"),
    tags$li("Fungsi masukkan = untuk menentukan file bertipe apa yang akan dipakai"),
    tags$li("Argumen = memilih argumen dari fungsi masukan, untuk mengubah pengaturan fungsi, jika dibiarkan, akan  menghasilkan argumen default."),
    tags$li("Masukan value dari argumen terpilih = nilai dari argumen, contoh yang perlu diubah, argumen header perlu diubah menjadi T atau TRUE"),
    tags$li("Upload data-file = upload data sesuai fungsi yg dipilih"),
    tags$li("disimpan dengan nama = data file yg diupload akan disimpan dengan nama yg diinputkan"),
    tags$hr(),
    
    tags$ul("Regresi"),
    tags$li("Data yang cocok untuk regresi ini adalah 'data_regresi.txt'"),
    tags$li("berisi tentang analisis regresi yang mencangkup fitting model, summary, plot dan menentukan prediksi y dan prediksi x"),
    tags$hr(),
    tags$ul("ANOVA"),
    tags$li("data yang cocok chick.txt dan data_anova.csv"),
    tags$li("variabel dependen merupakan variabel dependen yang berupa skala rasio"),
    tags$li("variabel independen/Faktor A/Faktor B merupakan variabel yang mempengaruhi dan berupa skala nominal atau interval"),
    tags$li("Alpha maksimal 1 dan minimal 0"),
    tags$li("apabila data tidak memenuhi asumsi normalitas dan homogenitas, maka tidak dapat ditampilkan, untuk mengeceknya dapat dilihat di tab asumsi"),
    tags$hr(),
    tags$ul("Summary"),
    tags$li("akan menampilkan statistik deskriptif dari tiap tiap variabel"),
    tags$hr(),
    tags$ul("korelasi"),
    tags$li("ini membutuhkan pakcage googleVis, dan yg ditampilkan adalah tabel korelasi dari seluruh variabel"),
    tags$hr(),
    tags$ul("Residual"),
    tags$li("akan menampilkan analisis residual, diantaranya: histogram, Scatter plot dan QQPlot"),
    tags$hr(),
    tags$ul("Visual"),
    tags$li("menampilkan tampilan visual berupa histogram, cluster dan Scatter plot"),
    tags$hr(),
    tags$ul("ABOUT"),
    tags$li("berisu dari data diri author dan dokumentasi aplikasi")
  )})
}