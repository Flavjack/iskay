library(shiny)
library(shinydashboard)
library(agricolae)

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
                                       menuSubItem("Kruskall-Waliis Test", tabName = "tkrusallTab", icon = icon("file")),
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
                     
                     
                              menuItem("Fieldbook management",
                                    menuSubItem("New fieldbook", tabName = "newFieldbook", icon = icon("file"))
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
                                column(
                                  12,
                                  offset = 1,
                                  box(title = "Upload your excel file", status = "success", solidHeader = TRUE,
                                      collapsible = FALSE,width = 10,
                                      shiny::fileInput(inputId = "uin_fb_import",label = "Import file",
                                                       accept = ".xlsx"),
                                      
                                      shiny::uiOutput("ou_sheets"),
                                      br(),
                                      br(),
                                      actionButton("show_dlgImport", "Help", icon("question-circle"),
                                                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")#,
                                      
                                  ),
                                  br(),
                                  br(),
                                  br(),
                                  br()
                                  
                                )
                                
                              )
                              
      ) , 
              
            
    shinydashboard::tabItem(tabName = "tfriedmanTab",
                     #h2("Friedman Test"),
                    
                     fluidRow( #begin fluid row
                       column(width = 12, 
                        
                      box(#begin inputs box friedman
                      title = "Friedman Test", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE, width = 3,

                      uiOutput("ou_jugfrman"),
                      uiOutput("ou_trtfrman"),
                      uiOutput("ou_traitfrman")#,
                      
                      ), #end box friedman
                     
                     box(#begin inputs box friedman
                       title = "Friedman Test", status = "primary", solidHeader = TRUE,
                       collapsible = TRUE, width = 12,
                       
                        DT::dataTableOutput("ou_dtfrman")
                     )
                    )
               )    #end fluidrow 
            )
            
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
    
    
    # Help dialogue Box Plots
    
    observeEvent(input$show_dlgImport, {
      showModal(modalDialog(title = "Import data",
                            
                            "Iskay accept excel files."
                            #easyClose = TRUE,
                            #footer = NULL
                              
                            # tags$iframe(
                            #   src = vidurl2,
                            #   width = 560,
                            #   height = 315
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
       dtfrman <- friedman( judge = jug_col, trt = trt_col, evaluation = trait_col)
       dtfrmeans  <- agr2df(dtfrman$means)
       #print(dtfrmeans)
       shiny::withProgress(message = "Visualizing Table...",value= 0,  #withProgress
                          {
                            
                            shiny::incProgress(amount = 1/2, "loadgin results")
                            
                            DT::datatable( dtfrmeans, rownames = FALSE, 
                                           #selection = list( mode= "multiple",  selected =  rownames(mtl_table)), 
                                           options = list(scrollX = TRUE, scroller = TRUE)
                                           #selection = list( mode = "multiple")#, 
                                           #filter = 'bottom'#,
                            )  
                            
            }
          )
    })
    

    
  }) #end server_iskay
    
        
  shinyApp(ui, server_iskay )             
                              
  



