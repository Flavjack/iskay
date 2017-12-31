library(shiny)
library(shinydashboard)
library(agricolae)
library(rhandsontable)
library(iskay)
library(dplyr)
library(exactRankTests)
library(broom)
library(clinfun)

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "HIDAP", titleWidth = "250px",
                  
      
                  # Twitter Sharing Link
                  tags$li(
                    class = "dropdown",
                    tags$a(
                      href = "http://twitter.com/share?url=https://foocheung.shinyapps.io/soma_stats/&text=Web Tool For Navigating and Plotting ADAT files",
                      target = "_blank",
                      tags$img(height = "18px",
                               src = "twitter.png")
                    )
                  ),
                  
                  # Facebook Sharing link
                  tags$li(
                    class = "dropdown",
                    tags$a(
                      href = "https://www.facebook.com/quipo.org/",
                      target = "_blank",
                      tags$img(height = "18px",
                               src = "facebook.png")
                    )
                  ),
                  
                  # LinkedIn Sharing link
                  tags$li(
                    class = "dropdown",
                    tags$a(
                      href = "https://www.linkedin.com/company/quipo-org/",
                      target = "_blank",
                      tags$img(height = "18px",
                               src = "linkedin.png")
                    )
                  )
  
                #tags$script(HTML("$('body').addClass('sidebar-mini');"))
  ),#end Header

  dashboardSidebar(width = "250px",
                   
                   br(),
                   #div(img(src="inst/app/www/quipoicon.png", width = "50px"), style="text-align: center;"),
                   
                   sidebarMenu(
                     id = "tabs",
                 
                         
                     menuItem("Import Data", icon = icon("table"),
                              menuSubItem("Import data", tabName = "timportData", icon = icon("table"))
                             
                     ),
                     
                     menuItem("Non-Parametric Test", icon = icon("th-list"),
                         
                              menuItem("One Sample",
                                       menuSubItem("Wilcoxon", tabName = "twilcoxon1Tab", icon = icon("table"))
                                       #menuSubItem("Clone list", tabName = "generateList", icon = icon("list")),
                                       #menuSubItem("Family list", tabName = "createList", icon = icon("list-alt")) ,
                                       #menuSubItem("Parental list", tabName = "parentList", icon = icon("list-alt")),
                                       #menuSubItem("Distribution Data", tabName = "distributionDB", icon = icon("database"))
                              ),
                              
                              menuItem("Two Sample",
                                       menuSubItem("Man-Whitney", tabName = "tmanwithneyTab", icon = icon("file")),
                                       menuSubItem("Wilcoxon", tabName = "twilcoxon2Tab", icon = icon("table"))
                                       
                                       #menuSubItem("Open fieldbook", tabName = "openFieldbook", icon = icon("file-o")),
                                       #menuSubItem("Check fieldbook", tabName = "checkFieldbook", icon = icon("eraser")),
                                       #menuSubItem("Data transformation", tabName = "singleAnalysisTrans", icon = icon("file-text-o"))
                              ),
                              
                              menuItem("One sample k-measures",
                                       menuSubItem("Durbin Test", tabName = "tdurbinTab", icon = icon("file")),
                                       menuSubItem("Friedman Test", tabName = "tfriedmanTab", icon = icon("file-o")),
                                       menuSubItem("Kendall Test", tabName = "tkendallTab", icon = icon("file-text-o"))
                                       #menuSubItem("Check fieldbook", tabName = "checkFieldbook", icon = icon("eraser")),
                                       #menuSubItem("", tabName = "", icon = icon("file-text-o"))
                              ),
                              
                              menuItem("Independent K-samples",
                                       menuSubItem("Kruskall-Waliis Test", tabName = "tkruskalTab", icon = icon("file")),
                                       menuSubItem("Median Test", tabName = "tmedTab", icon = icon("file-o")),
                                       menuSubItem("Jonckheere-T Test", tabName = "tjonckTab", icon = icon("file-o"))
                                       #menuSubItem("Check fieldbook", tabName = "checkFieldbook", icon = icon("eraser")),
                                       
                              )
                              
                     ),
                     
                     menuItem("Categorical Analysis", icon=icon("th-list"),
                              
                              
                              #menuItem("Correlation",
                                       menuSubItem("Contingency Table", tabName = "contingencyTab", icon = icon("file")),
                                       menuSubItem("Correlation", tabName = "correlationTab", icon = icon("file"))
                                      
                                       #menuSubItem("Open fieldbook", tabName = "openFieldbook", icon = icon("file-o")),
                                       #menuSubItem("Check fieldbook", tabName = "checkFieldbook", icon = icon("eraser")),
                                       #menuSubItem("Data transformation", tabName = "singleAnalysisTrans", icon = icon("file-text-o"))
                              #) 
                              
                              
                     ),
                     
                     
                     menuItem("Graph", icon=icon("th-list"),
                     
                     
                              menuItem("Graphics 2",
                                    menuSubItem("graphics 3", tabName = "tGraphics", icon = icon("file"))
                                    #menuSubItem("Open fieldbook", tabName = "openFieldbook", icon = icon("file-o")),
                                    #menuSubItem("Check fieldbook", tabName = "checkFieldbook", icon = icon("eraser")),
                                    #menuSubItem("Data transformation", tabName = "singleAnalysisTrans", icon = icon("file-text-o"))
                            ) 
                     
                      
                   )
                   )
  ),
  
  dashboardBody(
    #
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")
    # ),
    
    #includeCSS("www/custom.css"),
    
    tabItems(
      
      shinydashboard::tabItem(tabName = "timportData",
                              includeCSS("www/custom.css"),
                              tabItem(
                                tabName = "adat",
                                column(
                                  12,
                                  offset = 5,
                                  img(src = 'quipoicon.png', align = "center")
                                ),
                                column(
                                  12,
                                  offset = 3,
                                  h1("Non Parametric Data Analysis App", style = "font-family: 'Source Sans Pro';")
                                ),
                                
                                br(),
                                br(),
                                br(),
                                br(),
                                
                                # Upload ADAT UI
                                fluidRow( #begin fluidrow for box Import data
                                column(width =  12, offset = 1,
                                  box(title = "Upload your excel file", status = "success", solidHeader = TRUE,
                                      collapsible = FALSE,width = 10,
                                      
                                      
                                      shiny::fileInput(inputId = "uin_fb_import",label = "Import file",
                                                       accept = ".xlsx"),
                                      shiny::uiOutput("ou_sheets"),
                                      actionButton("show_dlgImport", "Help", icon("question-circle"),
                                                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                      br(),
                                      br(),
                                      rHandsontableOutput("ou_fbImport", height = 400)
                                  ),
                                  br(),
                                  br()
                                  
                                )
                              ),#end fluidrow for box Import data
                                
                              br(),
                              br()
                              )
                              
      ) , 

      
  #tab for Wilcoxon test   -------------------------------------------------------------     
      
      shinydashboard::tabItem(tabName = "twilcoxon2Tab",
                              #h2("wilcoxon Test"),
                              
                              fluidRow( #begin fluid row
                                column(width = 3, 
                                       
                                       box(#begin inputs box friedman
                                         title = "Wilcoxon Test", status = "primary", solidHeader = TRUE,
                                         collapsible = TRUE, width = NULL,
                                         
                                         uiOutput("ou_Xwilcox2"),
                                         uiOutput("ou_Ywilcox2"),
                                         shiny::selectInput(inputId = "sel_input_wilcox2Hyp", label = "Hypothesis",
                                                            choices = list(`Two-sided` = "t", `Greater than`= "g", `Less than` = "l"), 
                                                            selected = 1),
                                         
                                         shiny::numericInput(inputId = "sel_input_wilcox2Mu", label = "Enter mean value",
                                                             value = 0),
                                         
                                         actionButton("show_dlgWilcox2", "Help", icon("question-circle"),
                                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       ) #end box friedman
                                ),
                                column(width = 9,    
                                       box(#begin inputs box wilcox2
                                         title = "Results", status = "primary", solidHeader = TRUE,
                                         collapsible = TRUE, width = 12,
                                         
                                         DT::dataTableOutput("ou_dtwilcox2")
                                       )
                                )
                                
                              )    #end fluidrow 
      ), #end tab Wilcoxon Two-paired test  
      
      
  #---------------------------------------------------------------------------------------

      
  #tab for Mann-Whitney test   --------------------------------------------------------------     
  
  shinydashboard::tabItem(tabName = "tmanwithneyTab",
                          #h2("mann whitney Test"),
                          
                          fluidRow( #begin fluid row
                            column(width = 3, 
                                   
                                   box(#begin inputs box friedman
                                     title = "Mann-Whitney Test", status = "primary", solidHeader = TRUE,
                                     collapsible = TRUE, width = NULL,
                                     
                                     uiOutput("ou_Xmanw"),
                                     uiOutput("ou_Ymanw"),
                                     shiny::selectInput(inputId = "sel_input_manwHyp", label = "Hypothesis",
                                                        choices = list(`Two-sided` = "t", `Greater than`= "g", `Less than` = "l"), 
                                                        selected = 1),
                                     
                                     shiny::numericInput(inputId = "sel_input_manwMu", label = "Enter mean value",
                                                         value = 0),
                                    
                                     actionButton("show_dlgManW", "Help", icon("question-circle"),
                                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                   ) #end box friedman
                            ),
                            column(width = 9,    
                                   box(#begin inputs box mann-whitney
                                     title = "Results", status = "primary", solidHeader = TRUE,
                                     collapsible = TRUE, width = 12,
                                     
                                     DT::dataTableOutput("ou_dtmanw")
                                   )
                            )
                            
                          )    #end fluidrow 
  ), #end tab Mann-Whitney test
  #---------------------------------------------------------------------------------------    
    
  
  #tab for friedman test ----------------------------------------------------------------
    shinydashboard::tabItem(tabName = "tfriedmanTab",
                     #h2("Friedman Test"),
                    
                     fluidRow( #begin fluid row
                       column(width = 3, 
                        
                      box(#begin inputs box friedman
                      title = "Friedman Test", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE, width = NULL,

                      uiOutput("ou_jugfrman"),
                      uiOutput("ou_trtfrman"),
                      uiOutput("ou_traitfrman"),
                      
                      actionButton("show_dlgFriedman", "Help", icon("question-circle"),
                                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                          ) #end box friedman
                      ),
                      column(width = 9,    
                         box(#begin inputs box friedman
                           title = "Results", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,
                           
                            DT::dataTableOutput("ou_dtfrman")
                           )
                      )
                    
               )    #end fluidrow 
            ), #end tab Friedman test
            
  #---------------------------------------------------------------------------------------
  
  
  #tab for friedman test ----------------------------------------------------------------
  shinydashboard::tabItem(tabName = "tdurbinTab",
                          #h2("Friedman Test"),
                          
                          fluidRow( #begin fluid row
                            column(width = 3, 
                                   
                                   box(#begin inputs box friedman
                                     title = "Durbin Test", status = "primary", solidHeader = TRUE,
                                     collapsible = TRUE, width = NULL,
                                     
                                     uiOutput("ou_jugdurbin"),
                                     uiOutput("ou_trtdurbin"),
                                     uiOutput("ou_traitdurbin"),
                                     
                                     actionButton("show_dlgDurbin", "Help", icon("question-circle"),
                                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                   ) #end box friedman
                            ),
                            column(width = 9,    
                                   box(#begin inputs box friedman
                                     title = "Results", status = "primary", solidHeader = TRUE,
                                     collapsible = TRUE, width = 12,
                                     DT::dataTableOutput("ou_dtdurbin")
                                   )
                            )
                            
                          )    #end fluidrow 
  ), #end tab Durbin test
  
  #---------------------------------------------------------------------------------------
  
  
  
  #tab for Kruskal test -------------------------------------------------------------       
    shinydashboard::tabItem(tabName = "tkruskalTab",
                            #h2("Kruskall Test"),
                            
                            fluidRow( #begin fluid row
                              column(width = 3, 
                                     
                                     box(#begin inputs box kruskal
                                       title = "Kruskall-Wallis Test", status = "primary", solidHeader = TRUE,
                                       collapsible = TRUE, width = NULL,
                                       
                                       uiOutput("ou_trtkru"),
                                       uiOutput("ou_traitkru"),
                                       
                                       actionButton("show_dlgkruskal", "Help", icon("question-circle"),
                                                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                     ) #end box friedman
                              ),
                              column(width = 9,    
                                     box(#begin inputs box kruskal
                                       title = "Results", status = "primary", solidHeader = TRUE,
                                       collapsible = TRUE, width = 12,
                                       
                                       DT::dataTableOutput("ou_dtkru")
                                     )
                              )
                              
                            )    #end fluidrow 
    ), #end tab Kruskal wallis test
  
  #---------------------------------------------------------------------------------------
  
  
  #tab for Median test -----------------------------------       
  shinydashboard::tabItem(tabName = "tmedTab",
                          #h2("med"),
                          
                          fluidRow( #begin fluid row
                            column(width = 3, 
                                   
                                   box(#begin inputs box med
                                     title = "Median Test", status = "primary", solidHeader = TRUE,
                                     collapsible = TRUE, width = NULL,
                                     
                                     uiOutput("ou_trtmed"),
                                     uiOutput("ou_traitmed"),
                                     
                                     shiny::selectInput(inputId = "sel_input_medHyp", label = "Hypothesis",
                                                        choices = list(`Two-sided` = "two.sided", 
                                                                       `Increase than`= "increasing", 
                                                                       `Decrease than` = "decreasing"), 
                                                        selected = 1),
                                     
                                     actionButton("show_dlgmed", "Help", icon("question-circle"),
                                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                   ) #end box med
                            ),
                            column(width = 9,    
                                   box(#begin inputs box med
                                     title = "Results", status = "primary", solidHeader = TRUE,
                                     collapsible = TRUE, width = 12,
                                     
                                     DT::dataTableOutput("ou_dtmed")
                                   )
                            )
                            
                          )    #end fluidrow 
  ), #end tab med test

  #---------------------------------------------------------------------------------------
  
    
  #tab for Jonckherre-Tepstra test -----------------------------------       
  shinydashboard::tabItem(tabName = "tjonckTab",
                          #h2("JT Test"),
                          
                          fluidRow( #begin fluid row
                            column(width = 3, 
                                   
                                   box(#begin inputs box kruskal
                                     title = "Jonckheere-Tepstra Test", status = "primary", solidHeader = TRUE,
                                     collapsible = TRUE, width = NULL,
                                     
                                     uiOutput("ou_trtjonck"),
                                     uiOutput("ou_traitjonck"),
                                     
                                     shiny::selectInput(inputId = "sel_input_jonckHyp", label = "Hypothesis",
                                                        choices = list(`Two-sided` = "two.sided", 
                                                                       `Increase than`= "increasing", 
                                                                       `Decrease than` = "decreasing"), 
                                                        selected = 1),
                                     
                                     actionButton("show_dlgjonck", "Help", icon("question-circle"),
                                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                   ) #end box jonck
                            ),
                            column(width = 9,    
                                   box(#begin inputs box jonck
                                     title = "Results", status = "primary", solidHeader = TRUE,
                                     collapsible = TRUE, width = 12,
                                     
                                     DT::dataTableOutput("ou_dtjonck")
                                   )
                            )
                            
                          )    #end fluidrow 
  ) #end tab jonck test
  
  #--------------------------------------------------------------------------------------------
    
    ),
    br(),
    br(),
    br()
  )
  
)  
    
  server_iskay <- function(input, output, session) ({    
                
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
    
    

    #Wilconxon two samples paired Test ----------------------------------------------------
    
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
      
      shiny::req(input$uin_fb_import)
      shiny::req(input$sel_input_Xwilcox2)
      shiny::req(input$sel_input_Ywilcox2)
      
      fb <- importData()
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
      dt <- out$statistic
      
      shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                          {
                            
                            shiny::incProgress(amount = 1/2, "loading results")
                            
                            DT::datatable( dt, rownames = FALSE, 
                                           #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                           options = list(scrollX = TRUE, scroller = TRUE)
                                           #selection = list( mode = "multiple")#, 
                                           #filter = 'bottom'#,
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
    
    
    # Mann-Whitney Test -------------------------------------------------------
    
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
      
      fb <- importData()
      x  <- input$sel_input_Xmanw
      x_col <- fb[, x]
      str(x_col)
      #select traits
      y <- input$sel_input_Ymanw
      y_col <- fb[, y]
      
      manwHyp <- input$sel_input_manwHyp
      manwMu <- input$sel_input_manwMu
      
      out <- test_analysis(x= trt_col, y = trait_col, hyp = manwHyp,param = manwMu, test = "manwithney")
      dt <- out$statistic
    
      shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                          {
                            
                            shiny::incProgress(amount = 1/2, "loading results")
                            
                            DT::datatable( dt, rownames = FALSE, 
                                           #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                           options = list(scrollX = TRUE, scroller = TRUE)
                                           #selection = list( mode = "multiple")#, 
                                           #filter = 'bottom'#,
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
    
    # Friedman Test -----------------------------------------------------------
    
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
      
      fb <- importData()
      #select judges
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
      dt <-  out$dt
      
      shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                          {
                            
                            shiny::incProgress(amount = 1/2, "loading results...")
                            var_sheet <- paste("durbin",trait, sep="")
                            
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
    
    
    
    
    
    
    
    
    
    
    
      
    # Friedman Test -----------------------------------------------------------
  
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
      
       fb <- importData()
       #select judges
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
    
      
    # Kruskal Wallis --------------------------------------------------------------------
    
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
      
      fb <- importData()
      #select treatments
      trt  <- input$sel_input_trtkru
      trt_col <- fb[, trt]
      #select traits
      trait <- input$sel_input_traitkru
      trait_col <- fb[, trait]
      
      #kruskall-wallis test
      #outkru <- kruskal(y = trait_col, trt = trt_col, group = TRUE,alpha = 0.05)
      out <- test_analysis(x= trt_col, y = trait_col, test = "kruskal")
      dt <- out$dt 
       
      shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                          {
                            
                            shiny::incProgress(amount = 1/2, "loadgin results")
                            
                            DT::datatable( dt, rownames = FALSE, 
                                           #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                           options = list(scrollX = TRUE, scroller = TRUE)
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
    
    
    # Median Test --------------------------------------
    
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
      
      fb <- importData()
      #select treatments
      trt  <- input$sel_input_trtmed
      trt_col <- fb[, trt] %>% pull()
      #select traits
      trait <- input$sel_input_traitmed
      trait_col <- fb[, trait] %>% pull()
      
      print(trait_col)
      print(trt_col)
      #Test
      out <- test_analysis(x= trt_col, y = trait_col, test = "median")
      dt <- out$dt
  
      shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                          {
                             shiny::incProgress(amount = 1/2, "loading results")
                            
                            DT::datatable( dt, rownames = FALSE, 
                                             options = list(scrollX = TRUE, scroller = TRUE)
                                         
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
    
    #####
    
    # Jonckheere-Tepstra -------------------------------
    
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
    
    # J-T table results -----------------
    
    output$ou_dtjonck  <-  DT::renderDataTable({
      
      shiny::req(input$uin_fb_import)
      shiny::req(input$sel_input_trtjonck)
      shiny::req(input$sel_input_traitjonck)
      
      fb <- importData()

      #select treatments
      trt  <- input$sel_input_trtjonck
      trt_col <- fb[, trt] %>% pull()
      #select traits
      trait <- input$sel_input_traitjonck
      trait_col <- fb[, trait] %>% pull()
      #hypothesis
      jonckHyp <- input$sel_input_jonckHyp
      #J-T test
      out <- test_analysis(x= trt_col, y = trait_col, hyp = jonckHyp, test = "jonckheere")
      dt <- out$statistic
     
      shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                          {
                            shiny::incProgress(amount = 1/2, "loading results...")
                            DT::datatable( dt, rownames = FALSE, 
                                           options = list(scrollX = TRUE, scroller = TRUE)
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
    
      
  }) #end server_iskay
    
        
  shinyApp(ui, server_iskay )             
                              
  





  #---------------------------------------------------------------------------------------
  