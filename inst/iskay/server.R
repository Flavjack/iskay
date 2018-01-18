rm(list=ls(all=TRUE))
library(shiny)
library(shinydashboard)
library(agricolae)
library(rhandsontable)
library(iskay)
library(dplyr)
library(exactRankTests)
library(broom)
library(clinfun)
library(purrr)
library(htmlwidgets)
library(htmltools)
library(radarchart)
library(plotly)
library(V8)
library(shinyjs)
library(stringr)
data("skills")

server_iskay <- function(input, output, session) {    
  
  #---- Fiedlbook Import data ----------------------------------------------
  
  
  # Select sheets -----------------------------------------------------------
  output$ou_sheets <- renderUI({
    
    req(input$uin_fb_import)
    fb_temp <- input$uin_fb_import
    file.copy(fb_temp$datapath,paste(fb_temp$datapath, ".xlsx", sep=""))
    fb_sheet <- try(readxl::excel_sheets(paste(fb_temp$datapath, ".xlsx", sep="")))
    shiny::selectInput(inputId = "sel_input_sheet", label = "Select sheet", 
                       choices = fb_sheet, selected = 1,width = '30%')
    
  })
  #--------------------------------------------------------------------------------------------
  
  # Import Data Reactive Values
  importData <- reactive({
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
    sheet <- input$sel_input_sheet
    fb_temp <- input$uin_fb_import
    
    if(is.null(fb_temp)){return()}
    if(!is.null(fb_temp)){
      
      file.copy(fb_temp$datapath,paste(fb_temp$datapath, ".xlsx", sep=""))
      fb_temp <- try(readxl::read_excel(paste(fb_temp$datapath, ".xlsx", sep=""), sheet = sheet))
      
    }
  }) 
  #--------------------------------------------------------------------------------------------
  
  ######
  test_result <- reactive({
    
    #ToDo: 
    #Colocar un checkbox para hacer un display de tablas
    #Incluir resumen de datos de forma general
    #Incluir diferencias por pares A B PVALUE
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
    
    ctab <- input$tabs #get current tab
    fb <- importData()
    
    if(ctab=="tmanwithneyTab"){
      x  <- input$sel_input_Xmanw
      x_col <- fb[, x] %>% pull()
      str(x_col)
      #select traits
      y <- input$sel_input_Ymanw
      y_col <- fb[, y] %>% pull()
      #global summary
      glbdt <-  try(glb_summary(fb, y = y) %>% as.data.frame() %>% list(glbdt = .))
      #hypothesis and mean
      manwHyp <- input$sel_input_manwHyp
      manwMu <- input$sel_input_manwMu
      
      out_test <- try(test_analysis(x= x_col, y = y_col, hyp = manwHyp,param = manwMu, test = "manwithney"))
      out <- append(out_test, glbdt)
      
    }
    
    if(ctab=="twilcoxon2Tab"){
      
      x  <- input$sel_input_Xwilcox2
      x_col <- fb[, x] %>% pull()
      #select traits
      y <- input$sel_input_Ywilcox2
      y_col <- fb[, y] %>% pull()
      #global summary
      glbdt <-  try(glb_summary(fb, y = y) %>% as.data.frame() %>% list(glbdt = .))
      
      # hypothesis and mu (mean)
      wilcoxHyp <- input$sel_input_wilcox2Hyp
      wilcoxMu <- input$sel_input_wilcox2Mu
      out_test <- try(test_analysis(x= x_col, y = y_col, hyp = wilcoxHyp,param = wilcoxMu, test = "wilcoxon"))
      out <- append(out_test, glbdt) 
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
      #global summary
      glbdt <- try(glb_summary(fb, y = trait) %>% as.data.frame() %>% list(glbdt = .))
      
      comp <- FALSE
      if(is.element('pcom', input$cbTables_durbin)) {comp <-  TRUE}
      
      #durbin test
      out_test <- try(test_analysis(x= trt_col, y = trait_col , jud = jug_col, test = "durbin",comp = comp))
      #glbdt <- cbind(glbdt, out_test$statistics, out_test$parameters)
      out <- append(out_test, glbdt)
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
      #global summary
      #print(fb)
      glbdt <-  try(glb_summary(fb, y = trait) %>% as.data.frame() %>% list(glbdt = .))
      print(glbdt)
      comp <- TRUE #by default there is no pair comparison ultil pressing 
      if(is.element('pcom', input$cbTables_frman)) {comp <-  TRUE}
      
      #friedman test
      out_test <- try(test_analysis(x= trt_col, y = trait_col , jud = jug_col, test = "friedman", comp = comp))
      #glbdt <- cbind(glbdt, out_test$statistics, out_test$parameters)
      
      out <- append(out_test, glbdt)
    }
    
    if(ctab=="tkruskalTab"){
      trt  <- input$sel_input_trtkru
      trt_col <- fb[, trt]
      #select traits
      trait <- input$sel_input_traitkru
      trait_col <- fb[, trait]
      #global summary
      glbdt <-  try(glb_summary(fb, y = trait) %>% as.data.frame() %>% list(glbdt = .))
      
      comp <- FALSE #by default there is no pair comparison ultil pressing 
      if(is.element(el = 'pcom',set =  input$cbTables_kru)) {comp <-  TRUE}
      
      #kruskall-wallis test
      #outkru <- kruskal(y = trait_col, trt = trt_col, group = TRUE,alpha = 0.05)
      out_test <- try(test_analysis(x= trt_col, y = trait_col, test = "kruskal", comp = comp))
      glbdt <- cbind(glbdt, out_test$statistics, out_test$parameters)
      out <- append(out_test, glbdt) 
    }
    
    if(ctab=="tmedTab"){
      
      trt <- input$sel_input_trtmed
      trt_col <- fb[, trt]
      
      trait <- input$sel_input_traitmed
      trait_col <- fb[, trait] %>% pull() %>% as.numeric()
      
      #Global summary
      glbdt <-  try(glb_summary(fb, y = trait) %>% as.data.frame() %>% list(glbdt = .))
      hyp <- input$sel_input_medHyp #hypothesis
      
      comp <- FALSE #by default there is no pair comparison ultil pressing 
      if(is.element('pcom', input$cbTables_med)) {comp <-  TRUE}
      
      out_test <- try(test_analysis(x= trt_col, y = trait_col , hyp = hyp, test = "median", comp = comp))
      glbdt <- cbind(glbdt, out_test$statistics, out_test$parameters)
      out <- append(out_test, glbdt)
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
      #Global summary
      glbdt <-  try(glb_summary(fb, y = trait) %>% as.data.frame() %>% list(glbdt = .))
      
      out_test <- try(test_analysis(x= trt_col, y = trait_col, hyp = jonckHyp, test = "jonckheere"))
      out <- append(out_test,  glbdt) 
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
    print(importData())
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_Ywilcox2", label = "Select variable Y", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select variable',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  #dtwilcox for general summary
  output$ou_dtwilcox2_gsum  <-  DT::renderDataTable({
    
    req(input$sel_input_Xwilcox2)
    req(input$sel_input_Ywilcox2)
    
    out <- test_result()  
    #print(out)
    glbdt <- out$glbdt
    #print(input$cbTables_wilcox2)
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results")
                          var_sheet <- paste("Wilcoxon2Sample", "Test", sep="_")
                          DT::datatable(glbdt, rownames = FALSE,
                                        extensions = c('Buttons'),
                                        options = list( dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                           list(extend = 'csv',   filename = var_sheet),
                                                           list(extend = 'excel', filename = var_sheet)
                                                        )
                                                    )
                                        )
                        }
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
                          var_sheet <- paste("Wilcoxon2General", "Summary", sep="_")
                          DT::datatable( dt, rownames = FALSE,
                                         #filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)),
                                         options = list(pageLength = 5,
                                                        scrollX = TRUE,
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_Ymanw", label = "Select variable Y", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select variable',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })

  #dtmanw for general summary
  output$ou_dtmanw_gsum  <-  DT::renderDataTable({
    out <- test_result()  
    print(out)
    glbdt <- out$glbdt
    print(input$cbTables_wilcox2)
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results")
                          var_sheet <- paste("ManWithney", "Test", sep="_")
                          DT::datatable(glbdt, rownames = FALSE,
                                        extensions = c('Buttons'),
                                        options = list( dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )
                                        )
                          )
                        }
    )
  })
  
  # Mann-Whitney table results ----------------------------------------------
  output$ou_dtmanw  <-  DT::renderDataTable({
    
    #shiny::req(input$uin_fb_import)
    shiny::req(input$sel_input_Xmanw)
    shiny::req(input$sel_input_Ymanw)
    
    out <- test_result()  
    dt <- out$statistic
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results")
                          
                          var_sheet <- paste("MannW","Test", sep="_")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         #filter = 'top',
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
 
  observeEvent(input$show_dlgManW, {
    showModal(modalDialog(title = strong("Mann-Whitney Test"),
                          
                          includeMarkdown("www/help_text/manwhitney_help.rmd")
                          
                          
    ))
  })
  #--------------------------------------------------------------------------
  
  #############################################################################################
  # Durbin Test -----------------------------------------------------------
  #############################################################################################
  
  output$ou_jugdurbin <- renderUI({
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
    

    
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_traitdurbin", label = "Select trait", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  # dtdurbin for general summary
  output$ou_dtdurbin_gsum  <-  DT::renderDataTable({
    out <- test_result()  
    #print(out)
    #glbdt <- out$glbdt
    glbdt <- cbind(out$glbdt, out$statistic, out$parameter)
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results")
                          var_sheet <- paste("DurbinGeneral", "Summary", sep="_")
                          DT::datatable( glbdt, rownames = FALSE, 
                                         #filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                         options = list(scrollX = TRUE, 
                                                        scroller = TRUE,
                                                        dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )
                                        )
                          )
                        }
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
                                         #filter = 'top',
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
  
  # durbin paired comparison
  output$ou_dtdurbin_pcom  <-  DT::renderDataTable({
    
    #shiny::req(input$uin_fb_import)
    shiny::req(input$sel_input_juddurbin)
    shiny::req(input$sel_input_trtdurbin)
    shiny::req(input$sel_input_traitdurbin)
    
    trait <- input$sel_input_traitdurbin
    out <- test_result()  
    print(out)
    dt <-  out$comparison
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "Loading results")
                          var_sheet <- paste("friedman",trait, sep="")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         #filter = 'top',
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_traitfrman", label = "Select trait", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  #dtfrman for general summary
  output$ou_dtfrman_gsum  <-  DT::renderDataTable({
    out <- test_result()  
    #print(out)
    glbdt <- cbind(out$glbdt, out$statistic, out$parameter)
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results")
                          var_sheet <- paste("FriedmanGeneral", "Summary", sep="_")
                          DT::datatable( glbdt, rownames = FALSE, 
                                         #filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                         options = list(scrollX = TRUE, 
                                                        scroller = TRUE,
                                                        dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )
                                        )
                          )
                        }
    )
  })
  
  # Friedman table results ------------------------------------------------------------
  output$ou_dtfrman  <-  DT::renderDataTable({
    
    #shiny::req(input$uin_fb_import)
    #shiny::req(input$sel_input_judfrman)
    #shiny::req(input$sel_input_trtfrman)
    #shiny::req(input$sel_input_traitfrman)
    
    trait <- input$sel_input_traitfrman
    out <- test_result()  
    dt <-  out$dt
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results")
                          var_sheet <- paste("friedman",trait, sep="")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         #filter = 'top',
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
  
  # friedman paired comparison
  output$ou_dtfrman_pcom  <-  DT::renderDataTable({
    # 
    # shiny::req(input$uin_fb_import)
    # shiny::req(input$sel_input_judfrman)
    # shiny::req(input$sel_input_trtfrman)
    # shiny::req(input$sel_input_traitfrman)
    
    trait <- input$sel_input_traitfrman
    out <- test_result()  
    dt <-  out$comparison
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "Loading results")
                          var_sheet <- paste("friedman",trait, sep="")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         #filter = 'top',
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_traitkru", label = "Select trait", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  #dtkruskal for general summary
  output$ou_dtkru_gsum  <-  DT::renderDataTable({
    out <- test_result()  
    #print(out)
    #glbdt <- out$glbdt
    glbdt <- cbind(out$glbdt, out$statistic, out$parameter)
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results")
                          var_sheet <- paste("KruskalGeneral", "Summary", sep="_")
                          DT::datatable( glbdt, rownames = FALSE, 
                                         #filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                         options = list(scrollX = TRUE, 
                                                        scroller = TRUE,
                                                        dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )
                                        )
                          )
                        }
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
                                         #filter = 'top',
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

  # kruskal-wallis paired comparison
  output$ou_dtkru_pcom  <-  DT::renderDataTable({
    
    shiny::req(input$uin_fb_import)
    shiny::req(input$sel_input_trtkru)
    shiny::req(input$sel_input_traitkru)
    
    trait <- input$sel_input_traitkru
    out <- test_result()  
    dt <- out$comparison
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "Loading results")
                          var_sheet <- paste("friedman",trait, sep="")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         #filter = 'top',
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_traitmed", label = "Select trait", 
                          choices = fb_cols, selected = 1, width = NULL,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })
  
  #dtmed for general summary
  output$ou_dtmed_gsum  <-  DT::renderDataTable({
    out <- test_result()  
    #glbdt <- out$glbdt
    glbdt <- cbind(out$glbdt, out$statistic, out$parameter)
    
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results")
                          var_sheet <- paste("MedianGeneral", "Summary", sep="_")
                          DT::datatable( glbdt, rownames = FALSE, 
                                         #filter = 'top',
                                         extensions = c('Buttons', 'Scroller'),
                                         #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                         options = list(scrollX = TRUE, 
                                                        scroller = TRUE,
                                                        dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )
                                        )
                          )
                        }
    )
  })
  
  # Median table results ----------------------------
  
  output$ou_dtmed  <-  DT::renderDataTable({
    
    shiny::req(input$uin_fb_import)
    shiny::req(input$sel_input_trtmed)
    shiny::req(input$sel_input_traitmed)
    
    trait <- input$sel_input_traitmed
    out <- test_result()  
    #glbdt <- out$dt
    glbdt <- cbind(out$dt, out$statistic, out$parameter)
    
    
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          shiny::incProgress(amount = 1/2, "loading results")
                          
                          var_sheet <- paste("median",trait, sep="_")
                          
                          DT::datatable( glbdt, rownames = FALSE, 
                                         #filter = 'top',
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
  
  # median paired comparison
  output$ou_dtmed_pcom  <-  DT::renderDataTable({
    
    shiny::req(input$uin_fb_import)
    shiny::req(input$sel_input_trtmed)
    shiny::req(input$sel_input_traitmed)
    
    trait <- input$sel_input_traitmed
    out <- test_result()  
    dt <- out$comparison
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "Loading results")
                          var_sheet <- paste("median",trait, sep="_")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         #filter = 'top',
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
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
    
    #req(input$uin_fb_import)
    #req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_traitjonck", label = "Select trait", 
                          choices = fb_cols, selected = 1, width = NULL, 
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })

  #dtjonck for general summary
  output$ou_dtjonck_gsum  <-  DT::renderDataTable({
    out <- test_result()  
    glbdt <- out$glbdt
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          
                          shiny::incProgress(amount = 1/2, "loading results")
                          var_sheet <- paste("JonckheereGeneral", "Summary", sep="_")
                          DT::datatable(glbdt, rownames = FALSE,
                                        extensions = c('Buttons'),
                                        options = list( dom = 'Bfrtip',
                                                        buttons = list(
                                                          'copy',
                                                          list(extend = 'csv',   filename = var_sheet),
                                                          list(extend = 'excel', filename = var_sheet)
                                                        )
                                        )
                          )
                        }
    )
  })
  
  # J-T table results 
  output$ou_dtjonck  <-  DT::renderDataTable({
    
    shiny::req(input$uin_fb_import)
    shiny::req(input$sel_input_trtjonck)
    shiny::req(input$sel_input_traitjonck)
    
    trait <- input$sel_input_traitjonck
    out <- test_result()  
    dt <- out$statistic
    
    shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                        {
                          shiny::incProgress(amount = 1/2, "loading results...")
                          
                          
                          var_sheet <- paste("jonckheere", trait, sep="_")
                          
                          DT::datatable( dt, rownames = FALSE, 
                                         #filter = 'top',
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
  
  # Radar plot --------------------------------------------------------------

  output$ou_trtRadar <- renderUI({
    
    req(input$uin_fb_import)
    #req(input$sel_input_sheet)
    fb_cols <- names(importData())
    shiny::selectizeInput(inputId = "sel_input_trtradar", label = "Select treatment", 
                          choices = fb_cols,  width = NULL,multiple=TRUE,
                          options = list(
                            placeholder = 'Select treatments',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
    
  })

  # output$ou_lvlRadar <- renderUI({
  #   
  #   req(input$sel_input_trtradar)
  #   trt <- input$sel_input_trtradar
  #   trt_col <- importData() %>% select(trt) %>% pull()
  #   lvl_trt <- trt_col %>% unique()
  #   #fb_cols <- names(importData())
  #   shiny::selectizeInput(inputId = "sel_input_lvlradar", label = "Select levels of treatment",
  #                         choices = lvl_trt, selected = 1, width = NULL, multiple =TRUE,
  #                         options = list(
  #                           placeholder = 'Select levels',
  #                           onInitialize = I('function() { this.setValue(""); }')
  #                         )
  #   )
  #   
  # })
  # 
  
  output$ou_traitRadar <- renderUI({

    req(input$sel_input_trtradar)
    #trait <- input$sel_input_trtradar
    fb_cols <- names(importData())
    #fb_cols <- fb_cols[fb_cols!=input$sel_input_trtradar]
    shiny::selectizeInput(inputId = "sel_input_traitradar", label = "Select trait(s)",
                          choices = fb_cols, selected = 1, width = NULL, multiple =TRUE,
                          options = list(
                            placeholder = 'Select at least 3 traits',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
    )
  })
  
  radar_data <- reactive( {

    req(input$sel_input_trtradar)
    #req(input$sel_input_lvlradar)
    req(input$sel_input_traitradar)

    trt <- input$sel_input_trtradar
    fb <- importData()
    dt <- fb %>% group_by_(trt) %>%
                  summarise_all(funs(mean)) %>%
                  t() %>%
                  as.data.frame() %>%
                  tibble::rownames_to_column() %>%
                  purrr::map(as.vector) %>%
                  as.data.frame(stringsAsFactors = FALSE)

    dt_header <- dt[1,] %>% as.character()

    dt <- dt %>%  slice(-1) %>%
                  purrr::map(as.vector) %>%
                  map_at(.at = 2:4, as.numeric) %>%
                  as.data.frame()

    colnames(dt) <- dt_header
    print(dt)
    dt <- dt[-c(1:2),] 
    # if(length(input$sel_input_traitradar)>2){ 
    #   
    #   dt <- dt[ input$sel_input_traitradar,]
    # }
    dt
    
  })
  

  
  observeEvent(input$go,{
      req(input$sel_input_trtradar)
      #req(input$sel_input_lvlradar)
      req(input$sel_input_traitradar)
      input$uin_fb_import
      #print(input$uin_fb_import)
      #print(input$sel_input_traitradar)
      fp <- input$uin_fb_import
      if(is.null(fp))  aku <<- NULL
      else {
          aku <<- radar_data()
      }
      aku
   })
  
  output$omar <- DT::renderDataTable({
                  dt <- radar_data()
                  DT::datatable(dt)
                })  

  # 
  observe({

    #After all this conditions has been made, the submit button will appear to save the information
    toggleState("go",
                  !is.null(input$sel_input_trtradar) && str_trim(input$sel_input_trtradar, side = "both")!= "" &&
                  #!is.null(input$sel_input_lvlradar) && str_trim(input$fbsites_country, side = "both")!= "" &&
                  !is.null(input$sel_input_traitradar) && str_trim(input$sel_input_traitradar, side = "both") != "" &&
                  length(input$sel_input_traitradar)>2

    )
  })
  
    
  output$ui_radar <- renderChartJSRadar({
    
    if(input$go %% 2 == 0){
      p1 <- skills
      chartJSRadar(p1,  showToolTipLabel=TRUE)
    } 
    if(input$go %% 2 == 1)
    {
      as <- aku
      chartJSRadar(as,  showToolTipLabel=TRUE)
    }
    
  })  
  
  
  
} #end server_iskay


