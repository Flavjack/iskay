library(shiny)
library(shinydashboard)
library(agricolae)
library(rhandsontable)
library(iskay)
library(dplyr)
library(exactRankTests)
library(broom)
library(clinfun)

server_iskay <- function(input, output, session) {    
  
  #---- Fiedlbook Import data ----------------------------------------------
  
  
  # Select sheets -----------------------------------------------------------
  output$ou_sheets <- renderUI({
    
    req(input$uin_fb_import)
    fb_temp <- input$uin_fb_import
    file.copy(fb_temp$datapath,paste(fb_temp$datapath, ".xlsx", sep=""))
    fb_sheet <- readxl::excel_sheets(paste(fb_temp$datapath, ".xlsx", sep=""))
    shiny::selectInput(inputId = "sel_input_sheet", label = "Select sheet", 
                       choices = fb_sheet, selected = 1,width = '30%')
    
  })
  #--------------------------------------------------------------------------------------------
  
  # Import Data Reactive Values
  importData <- reactive({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    sheet <- input$sel_input_sheet
    fb_temp <- input$uin_fb_import
    
    if(is.null(fb_temp)){return()}
    if(!is.null(fb_temp)){
      
      file.copy(fb_temp$datapath,paste(fb_temp$datapath, ".xlsx", sep=""))
      fb_temp <- readxl::read_excel(paste(fb_temp$datapath, ".xlsx", sep=""), sheet = sheet)
      
    }
  }) 
  #--------------------------------------------------------------------------------------------
  
  ######
  test_result <- reactive({
    
    #ToDO: 
    #Colocar un checkbox para hacer un display de tablas
    #Incluir resumen de datos de forma general
    #Incluir diferencias por pares A B PVALUE
    #
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    
    ctab <- input$tabs #get current tab
    fb <- importData()
    
    if(ctab=="twilcoxon2Tab"){
      
      x  <- input$sel_input_Xwilcox2
      x_col <- fb[, x] %>% pull()
      
      #select traits
      y <- input$sel_input_Ywilcox2
      y_col <- fb[, y] %>% pull()
      
      wilcoxHyp <- input$sel_input_wilcox2Hyp
      wilcoxMu <- input$sel_input_wilcox2Mu
      # outwilcox <- wilcox.exact(y_col ~ x_col, mu=wilcoxMu, alternative = wilcoxHyp, paired=TRUE)
      # dt <- broom::glance(outwilcox)
      out <- test_analysis(x= x_col, y = y_col, hyp = wilcoxHyp,param = wilcoxMu, test = "wilcoxon")
      #dt <- out$statistic
    }
      
    if(ctab=="tmanwithneyTab"){
      x  <- input$sel_input_Xmanw
      x_col <- fb[, x]
      str(x_col)
      #select traits
      y <- input$sel_input_Ymanw
      y_col <- fb[, y]
      
      manwHyp <- input$sel_input_manwHyp
      manwMu <- input$sel_input_manwMu
      
      out <- test_analysis(x= trt_col, y = trait_col, hyp = manwHyp,param = manwMu, test = "manwithney")
      #dt <- out$statistic
      
      
    }
  
    if(ctab =="tdurbinTab"){
      jud <- input$sel_input_juddurbin
      jug_col <- fb[, jud] %>% pull()
      #select treatments
      trt  <- input$sel_input_trtdurbin
      trt_col <- fb[, trt] %>% pull()
      #select traits
      trait <- input$sel_input_traitdurbin
      trait_col <- fb[, trait] %>% pull()
      #durbin test
      
      out <- test_analysis(x= trt_col, y = trait_col , jud = jug_col, test = "durbin")
      #dt <-  out$dt
      
        
    }
    
    if(ctab =="tfriedmanTab"){
      
      jud <- input$sel_input_judfrman
      jug_col <- fb[, jud]
      #select treatments
      trt  <- input$sel_input_trtfrman
      trt_col <- fb[, trt]
      #select traits
      trait <- input$sel_input_traitfrman
      trait_col <- fb[, trait]
      #friedman test
      
      out <- test_analysis(x= trt_col, y = trait_col , jud = jug_col, test = "friedman")
      #dt <-  out$dt
    }
    
    if(ctab=="tkruskalTab"){
      trt  <- input$sel_input_trtkru
      trt_col <- fb[, trt]
      #select traits
      trait <- input$sel_input_traitkru
      trait_col <- fb[, trait]
      
      #kruskall-wallis test
      #outkru <- kruskal(y = trait_col, trt = trt_col, group = TRUE,alpha = 0.05)
      out <- test_analysis(x= trt_col, y = trait_col, test = "kruskal")
      #dt <- out$dt 
    }
  
    if(ctab=="tjonckTab"){
      trt  <- input$sel_input_trtjonck
      trt_col <- fb[, trt] %>% pull()
      #select traits
      trait <- input$sel_input_traitjonck
      trait_col <- fb[, trait] %>% pull()
      #hypothesis
      jonckHyp <- input$sel_input_jonckHyp
      #J-T test
      out <- test_analysis(x= trt_col, y = trait_col, hyp = jonckHyp, test = "jonckheere")
      #dt <- out$statistic
    }
    
    out
  }) 
  
  
  
  
  #Help dialogue for Import Data ---------------------------
  
  observeEvent(input$show_dlgImport, {
    showModal(modalDialog(title = strong("Import Data"),
                          
                          includeMarkdown("www/help_text/friedman_help.rmd")
                          
                          
    ))
  })
  
  output$ou_fbImport = renderRHandsontable({
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    dt <- importData()
    rhandsontable::rhandsontable(dt)
  })
  
  #--------------------------------------------------------------------------------------------
  
  #############################################################################################
  #Wilconxon two samples paired Test ----------------------------------------------------
  #############################################################################################
  
  output$ou_Xwilcox2 <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())  
    shiny::selectizeInput(inputId = "sel_input_Xwilcox2", label = "Select variable X", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select variable',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  output$ou_Ywilcox2 <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_Ywilcox2", label = "Select variable Y", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select variable',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  # Wilcox two-paired sample table results 
  
  output$ou_dtwilcox2  <-  DT::renderDataTable({
  
    req(input$sel_input_Xwilcox2)
    req(input$sel_input_Ywilcox2)
    out <- test_result()  
    dt <- out$statistic
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results")
                          
                          var_sheet <- paste("Wilcoxon2Sample", "Test", sep="_")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                         options = list(scrollX = TRUE, 
                                                        scroller = TRUE,
                                                        dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )#,
                                                        
                                         )
                          )  
                          
                        }
    )
  })
  
  output$ou_dtwilcox3  <-  DT::renderDataTable({
    
    # req(input$sel_input_Xwilcox2)
    # req(input$sel_input_Ywilcox2)
    # out <- test_result()  
    # dt <- out$statistic
    dt <- iris
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results")
                          
                          var_sheet <- paste("Wilcoxon2Sample", "Test", sep="_")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                         options = list(scrollX = TRUE, 
                                                        scroller = TRUE,
                                                        dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )#,
                                                        
                                         )
                          )  
                          
                        }
    )
  })
  
  
  # Help dialogue for Wilcoxon two-paired Test
  
  observeEvent(input$show_dlgWilcox2, {
    showModal(modalDialog(title = strong("Wilcoxon Two-Paired Test"),
                          
                          includeMarkdown("www/help_text/wilcox2_help.rmd")
                          
                          
    ))
  })
  
  #--------------------------------------------------------------------------
  
  #############################################################################################
  # Mann-Whitney Test -------------------------------------------------------
  #############################################################################################
  
  output$ou_Xmanw <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_Xmanw", label = "Select variable X", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select variable',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  output$ou_Ymanw <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_Ymanw", label = "Select variable Y", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select variable',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  # Mann-Whitney table results ----------------------------------------------
  
  output$ou_dtmanw  <-  DT::renderDataTable({
    
    shiny::req(input$uin_fb_import)
    shiny::req(input$sel_input_Xmanw)
    shiny::req(input$sel_input_Ymanw)
    
    out <- test_result()  
    dt <- out$statistic
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results")
                          
                          var_sheet <- paste("Median","Test", sep="_")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                         options = list(scrollX = TRUE, 
                                                        scroller = TRUE,
                                                        dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )#,
                                                        
                                         )
                          )  
                          
                          
                        }
    )
  })
  
  # Help dialogue for Man-Whitney Test --------------------------------------
  
  observeEvent(input$show_dlgMWhitney, {
    showModal(modalDialog(title = strong("Mann-Whitney Test"),
                          
                          includeMarkdown("www/help_text/manwhitney_help.rmd")
                          
                          
    ))
  })
  #--------------------------------------------------------------------------
  
  #############################################################################################
  # Durbin Test -----------------------------------------------------------
  #############################################################################################
  
  output$ou_jugdurbin <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    

    
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_juddurbin", 
                          label = "Select judges",choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select judges',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  output$ou_trtdurbin <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_trtdurbin", label = "Select treatments",
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
    
    
  })
  
  output$ou_traitdurbin <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_traitdurbin", label = "Select trait", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  # Durbin table results ------------------------------------------------------------
  
  output$ou_dtdurbin  <-  DT::renderDataTable({
    
    shiny::req(input$uin_fb_import)
    shiny::req(input$sel_input_juddurbin)
    shiny::req(input$sel_input_trtdurbin)
    shiny::req(input$sel_input_traitdurbin)
    
    trait <- input$sel_input_traitdurbin
    out <- test_result()  
    dt <-  out$dt
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results...")
                          var_sheet <- paste("durbin",trait, sep="_")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                         options = list(scrollX = TRUE, 
                                                        scroller = TRUE,
                                                        dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )#,
                                                        
                                         )
                                         #selection = list( mode = "multiple")#, 
                                         #filter = 'bottom'#,
                          )  
                          
                        }
    )
  })
  
  # Help dialogue for Durbin Test ---------------------------------------------------
  
  observeEvent(input$show_dlgDurbin, {
    showModal(modalDialog(title = strong("Durbin Test"),
                          
                          includeMarkdown("www/help_text/durbin_help.rmd")
                          
                          
    ))
  })
  #--------------------------------------------------------------------------------------------
  
  #############################################################################################
  ############# Friedman Test -----------------------------------------------------------
  #############################################################################################
  
  output$ou_jugfrman <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_judfrman", 
                          label = "Select judges",choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select judges',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  output$ou_trtfrman <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_trtfrman", label = "Select treatments",
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
    
    
  })
  
  output$ou_traitfrman <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_traitfrman", label = "Select trait", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  # Friedman table results ------------------------------------------------------------
  
  output$ou_dtfrman  <-  DT::renderDataTable({
    
    shiny::req(input$uin_fb_import)
    shiny::req(input$sel_input_judfrman)
    shiny::req(input$sel_input_trtfrman)
    shiny::req(input$sel_input_traitfrman)
    
    trait <- input$sel_input_traitfrman
    out <- test_result()  
    dt <-  out$dt
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loadgin results")
                          var_sheet <- paste("friedman",trait, sep="")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                         options = list(scrollX = TRUE, 
                                                        scroller = TRUE,
                                                        dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )#,
                                                        
                                         )
                                         #selection = list( mode = "multiple")#, 
                                         #filter = 'bottom'#,
                          )  
                          
                        }
    )
  })
  
  # Help dialogue for Friedman Test ---------------------------------------------------
  
  observeEvent(input$show_dlgFriedman, {
    showModal(modalDialog(title = strong("Friedman Test"),
                          
                          includeMarkdown("www/help_text/friedman_help.rmd")
                          
                          
    ))
  })
  #--------------------------------------------------------------------------------------------
  
  #############################################################################################
  # Kruskal Wallis --------------------------------------------------------------------
  #############################################################################################
  
  output$ou_trtkru <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_trtkru", label = "Select treatments",
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
    
    
  })
  
  output$ou_traitkru <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_traitkru", label = "Select trait", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  # Kruskall-Wallis table results -----------------------------------------------------
  
  output$ou_dtkru  <-  DT::renderDataTable({
    
    shiny::req(input$uin_fb_import)
    shiny::req(input$sel_input_trtkru)
    shiny::req(input$sel_input_traitkru)
    
    trait <- input$sel_input_traitkru
    out <- test_result()  
    dt <- out$dt 
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loadgin results")
                          
                          var_sheet <- paste("kruskal",trait, sep="_")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                         options = list(scrollX = TRUE, 
                                                        scroller = TRUE,
                                                        dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )#,
                                                        
                                         )
                                         #selection = list( mode = "multiple")#, 
                                         #filter = 'bottom'#,
                          )  
                          
                        }
    )
  })
  
  # Help dialogue for Kruskal Test ----------------------------------------------------
  
  observeEvent(input$show_dlgKruskal, {
    showModal(modalDialog(title = strong("Kruskal Test"),
                          
                          includeMarkdown("www/help_text/kruskal_help.rmd")
                          
                          
    ))
  })
  
  #############################################################################################
  # Median Test --------------------------------------
  #############################################################################################
  
  # treatment input for median test
  output$ou_trtmed <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_trtmed", label = "Select treatments",
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
    
    
  })
  
  # trait input for median test
  output$ou_traitmed <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_traitmed", label = "Select trait", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  # Median table results ----------------------------
  
  output$ou_dtmed  <-  DT::renderDataTable({
    
    shiny::req(input$uin_fb_import)
    shiny::req(input$sel_input_trtmed)
    shiny::req(input$sel_input_traitmed)
    
    trait <- input$sel_input_traitmed
    out <- test_result()  
    dt <- out$dt
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          shiny::incProgress(amount = 1/2, "loading results")
                          
                          var_sheet <- paste("median",trait, sep="_")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                         options = list(scrollX = TRUE, 
                                                        scroller = TRUE,
                                                        dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )#,
                                                        
                                         )
                                 )  
                          
                          
                          
                        }
    )
  })
  
  # Help dialogue for Median Test --------------------
  
  observeEvent(input$show_dlgMedian, {
    showModal(modalDialog(title = strong("Median Test"),
                          
                          includeMarkdown("www/help_text/median_help.rmd")
                          
                          
    ))
  })
  
  #############################################################################################
  # Jonckheere-Tepstra -------------------------------
  #############################################################################################
  
  output$ou_trtjonck <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_trtjonck", label = "Select treatments",
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
    
    
  })
  
  output$ou_traitjonck <- renderUI({
    
    req(input$uin_fb_import)
    req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_traitjonck", label = "Select trait", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  # J-T table results 
  
  output$ou_dtjonck  <-  DT::renderDataTable({
    
    shiny::req(input$uin_fb_import)
    shiny::req(input$sel_input_trtjonck)
    shiny::req(input$sel_input_traitjonck)
    
    out <- test_result()  
    dt <- out$statistic
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          shiny::incProgress(amount = 1/2, "loading results...")
                          
                          
                          var_sheet <- paste("jonckheere", trait, sep="_")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                         options = list(scrollX = TRUE, 
                                                        scroller = TRUE,
                                                        dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )#,
                                                        
                                         )
                          )  
                          
                          
                          
                          
                        }
    )
  })
  
  # Help dialogue for J-T Test -----------------------------------------
  
  observeEvent(input$show_dlgjonck, {
    showModal(modalDialog(title = strong("Jonckheere Test"),
                          
                          includeMarkdown("www/help_text/jonck_help.rmd")
                          
                          
    ))
  })
  
  
} #end server_iskay
