library(shiny)
library(shinydashboard)
library(agricolae)
library(rhandsontable)
library(iskay)
library(dplyr)

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
                                       menuSubItem("Wilconxon", tabName = "twilconxonTab", icon = icon("table"))
                                       #menuSubItem("Clone list", tabName = "generateList", icon = icon("list")),
                                       #menuSubItem("Family list", tabName = "createList", icon = icon("list-alt")) ,
                                       #menuSubItem("Parental list", tabName = "parentList", icon = icon("list-alt")),
                                       #menuSubItem("Distribution Data", tabName = "distributionDB", icon = icon("database"))
                              ),
                              
                              menuItem("Two Sample",
                                       menuSubItem("Man-Whitney", tabName = "tmanwithneyTab", icon = icon("file"))
                                       #menuSubItem("Open fieldbook", tabName = "openFieldbook", icon = icon("file-o")),
                                       #menuSubItem("Check fieldbook", tabName = "checkFieldbook", icon = icon("eraser")),
                                       #menuSubItem("Data transformation", tabName = "singleAnalysisTrans", icon = icon("file-text-o"))
                              ),
                              
                              menuItem("One sample k-measures",
                                       menuSubItem("Q.de Coachran Test", tabName = "tqcoachranTab", icon = icon("file")),
                                       menuSubItem("Friedman Test", tabName = "tfriedmanTab", icon = icon("file-o")),
                                       menuSubItem("Kendall Test", tabName = "tkendallTab", icon = icon("file-text-o"))
                                       #menuSubItem("Check fieldbook", tabName = "checkFieldbook", icon = icon("eraser")),
                                       #menuSubItem("", tabName = "", icon = icon("file-text-o"))
                              ),
                              
                              menuItem("Independent K-samples ",
                                       menuSubItem("Kruskall-Waliis Test", tabName = "tkruskalTab", icon = icon("file")),
                                       menuSubItem("Median Test", tabName = "tmedianTab", icon = icon("file-o")),
                                       menuSubItem("Jonckheere Test", tabName = "tjonckTab", icon = icon("file-o"))
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

  #tab for Mann-Whitney test   -----------------------------     
  
  shinydashboard::tabItem(tabName = "tmanwithneyTab",
                          #h2("Friedman Test"),
                          
                          fluidRow( #begin fluid row
                            column(width = 3, 
                                   
                                   box(#begin inputs box friedman
                                     title = "Mann-Whitney Test", status = "primary", solidHeader = TRUE,
                                     collapsible = TRUE, width = NULL,
                                     
                                     uiOutput("ou_trtmanw"),
                                     uiOutput("ou_traitmanw"),
                                     shiny::selectInput(inputId = "sel_input_manwHyp", label = "Hypothesis",
                                                        choices = c("Two-sided", "Less than", "Greater than"), 
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
      
    
    # tab for friedman test ----------------------------
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
            
    # tab for Kruskal test -----------------------------------       
    shinydashboard::tabItem(tabName = "tkruskalTab",
                            #h2("Friedman Test"),
                            
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
    ) #end tab Kruskal wallis test
    
    
    
    
    ),
    br(),
    br(),
    br()
  )
  
)  
    
  server_iskay <- function(input, output, session) ({    
                
    ###### Fiedlbook Import data -------------------################3
    
    # Select sheets -----------------------------------------------------------
    output$ou_sheets <- renderUI({
      
      req(input$uin_fb_import)
      fb_temp <- input$uin_fb_import
      file.copy(fb_temp$datapath,paste(fb_temp$datapath, ".xlsx", sep=""))
      fb_sheet <- readxl::excel_sheets(paste(fb_temp$datapath, ".xlsx", sep=""))
      shiny::selectInput(inputId = "sel_input_sheet", label = "Select sheet", 
                         choices = fb_sheet, selected = 1,width = '30%')
      
    })
    
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
    
    # Help dialogue for Import Data
    
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
    
    
    # Mann-Whitney Test -------------------------------------------------------
    
    output$ou_trtmanw <- renderUI({
      
      req(input$uin_fb_import)
      req(input$sel_input_sheet)
      fb_cols <- names(importData())
      shiny::selectizeInput(inputId = "sel_input_trtmanw", label = "Select treatments",
                            choices = fb_cols, selected = 1, width = NULL,
                            options = list(
                              placeholder = 'Select treatments',
                              onInitialize = I('function() { this.setValue(""); }')
                            )
         )
      
    })
    
    output$ou_traitmanw <- renderUI({
      
      req(input$uin_fb_import)
      req(input$sel_input_sheet)
      fb_cols <- names(importData())
      shiny::selectizeInput(inputId = "sel_input_traitmanw", label = "Select trait", 
                            choices = fb_cols, selected = 1, width = NULL,
                            options = list(
                              placeholder = 'Select treatments',
                              onInitialize = I('function() { this.setValue(""); }')
                            )
      )
      
    })
    
    # Friedman table results ---------------------------
    
    output$ou_dtmanw  <-  DT::renderDataTable({
      
      shiny::req(input$uin_fb_import)
      
      shiny::req(input$sel_input_trtmanw)
      shiny::req(input$sel_input_traitmanw)
      
      
      fb <- importData()
      trt  <- input$sel_input_trtmanw
      trt_col <- fb[, trt]
      #select traits
      trait <- input$sel_input_traitmanw
      trait_col <- fb[, trait]
      #mann-whitney test
      outmanw <- wilcox.exact(trt_col , trait_col, alternative="t", mu=0)
      
      dtmanw  <- agr2df(outmanw$means)
      #print(dtmanw)
      groupmanw <- rename_tables(outmanw$groups, c("Treatment", "Rank", "Groups")) %>% 
        select_(-2) #remoe sum of ranks columns  
      #lef join by trt for merging groups column
      dt <- dplyr::left_join(dtmanw, groupmanw, by="Treatment")
      
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
    
    # Help dialogue for Man-Whitney Test -----------------------------------------
    
    observeEvent(input$show_dlgMWhitney, {
      showModal(modalDialog(title = strong("Mann-Whitney Test"),
                            
                            includeMarkdown("www/help_text/manwhitney_help.rmd")
                            
                            
      ))
    })
    
    
    
    
    
    
    
      
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
  
    # Friedman table results ---------------------------
    
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
       outfrim <- friedman( judge = jug_col, trt = trt_col, evaluation = trait_col)
       dtfrimeans  <- agr2df(outfrim$means)
       #print(dtfrmeans)
       groupfri <- rename_tables(outfrim$groups, c("Treatment", "Rank", "Groups")) %>% 
                   select_(-2) #remoe sum of ranks columns  
       #lef join by trt for merging groups column
       dt <- dplyr::left_join(dtfrimeans, groupfri, by="Treatment")
       
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
  
    # Help dialogue for Friedman Test -----------------------------------------
    
    observeEvent(input$show_dlgFriedman, {
      showModal(modalDialog(title = strong("Friedman Test"),
                            
                            includeMarkdown("www/help_text/friedman_help.rmd")
                            
                            
      ))
    })
    
      
    # Kruskal wallis -----------------------------------
    
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
    
    # Kruskall-Wallis table results -----------------
    
    output$ou_dtkru  <-  DT::renderDataTable({
      
      shiny::req(input$uin_fb_import)
      shiny::req(input$sel_input_trtkru)
      shiny::req(input$sel_input_traitkru)
      
      fb <- importData()
      #select judges
      #jud <- input$sel_input_judkru
      #jug_col <- fb[, jud]
      #select treatments
      trt  <- input$sel_input_trtkru
      trt_col <- fb[, trt]
      #select traits
      trait <- input$sel_input_traitkru
      trait_col <- fb[, trait]
      
      #kruskall-wallis test
      outkru <- kruskal(y = trait_col, trt = trt_col, group = TRUE,alpha = 0.05)
      
      dtkrumeans  <- agr2df(outkru$means,test = "kruskal")
      #print(dtfrmeans)
      groupkru <- rename_tables(outkru$groups, c("Treatment", "Means", "Groups")) %>% 
                  select_(-2) #remove sum of ranks columns  
      #lef join by trt for merging groups column
      dt <- dplyr::left_join(dtkrumeans, groupkru, by="Treatment")
      
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
    
    # Help dialogue for Kruskal Test -----------------------------------------
    
    observeEvent(input$show_dlgKruskal, {
      showModal(modalDialog(title = strong("Kruskal Test"),
                            
                            includeMarkdown("www/help_text/kruskal_help.rmd")
                            
                            
      ))
    })
      
  }) #end server_iskay
    
        
  shinyApp(ui, server_iskay )             
                              
  




