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
                     
                     
                     menuItem("GraphLab", icon=icon("th-list"),
                              
                              
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
                                  h1("Iskay: NonParametric Data Analysis App", style = "font-family: 'Source Sans Pro';")
                                ),
                                
                                br(),
                                br(),
                                br(),
                                br(),
                                
                                # Upload ADAT UI
                                fluidRow( #begin fluidrow for box Import data
                                  column(width =  12, offset = 1,
                                      
                                            
                                         box(title = "Upload your excel file", status = "success", 
                                             solidHeader = TRUE, collapsible = FALSE,width = 10,
                                             
                                             
                                             shiny::fileInput(inputId = "uin_fb_import",label = "Import file",
                                                              accept = ".xlsx"),
                                             shiny::uiOutput("ou_sheets"),
                                             actionButton("show_dlgImport", "Help", icon("question-circle"),
                                                          style = "color: #fff; background-color: #337ab7; 
                                                          border-color: #2e6da4"),
                                             
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
      
      
      #tab for Wilcoxon Two sample test   -------------------------------------------------------------     
      
      shinydashboard::tabItem(tabName = "twilcoxon2Tab",
                              #h2("wilcoxon Test"),
                              
                              fluidRow( #begin fluid row
                                column(width = 3, 
                                       
                                     box(#begin inputs box friedman
                                       title = "Wilcoxon Test", status = "primary", solidHeader = TRUE,
                                       collapsible = TRUE, width = 12,
                                       
                                       uiOutput("ou_Xwilcox2"),
                                       uiOutput("ou_Ywilcox2"),
                                       shiny::selectInput(inputId = "sel_input_wilcox2Hyp", label = "Hypothesis",
                                                          choices = list(`Two-sided` = "t", `Greater than`= "g", `Less than` = "l"), 
                                                          selected = 1),
                                         
                                       shiny::numericInput(inputId = "sel_input_wilcox2Mu", label = "Enter mean value",
                                                             value = 0),
                                      
                                       checkboxGroupInput("cbTables_wilcox2", "Options",selected = 2,
                                                          choiceNames =
                                                            list("Global summary", "Multiple comparison",
                                                                  "Paired comparison" ),
                                                          choiceValues =
                                                            list("gsum", "mulcom", "pcom")
                                       ),
                                       actionButton("show_dlgWilcox2", "Help", icon("question-circle"),
                                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       ) #end box friedman
                                ),
                                column(width = 9, 
                                       
                                       conditionalPanel(
                                         condition = "input.cbTables_wilcox2.includes('gsum')",
                                         
                                       box(#begin inputs box wilcox2
                                           title = "Summary statistics", status = "primary", solidHeader = TRUE,
                                           collapsible = TRUE, width = 12,
                                           
                                           #DT::dataTableOutput("ou_dtwilcox2")#,
                                           div(DT::dataTableOutput("ou_dtwilcox2_gsum"))
                                         )
                                       
                                        ), #end conditionalPanel for wilcox2
                                      
                                       conditionalPanel(
                                         condition = "input.cbTables_wilcox2.includes('mulcom')",
                                         
                                        
                                       box(#begin inputs box wilcox2
                                         title = "Multiple comparison", status = "primary", solidHeader = TRUE,
                                         collapsible = TRUE, width = 12,
                                         
                                         DT::dataTableOutput("ou_dtwilcox2")#,
                                    
                                       )#,
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
                                         
                                         checkboxGroupInput("cbTables_manw", "Options",
                                                            choiceNames =
                                                              list("Global summary", "Multiple comparison",
                                                                   "Paired comparison" ),
                                                            choiceValues =
                                                              list("gsum", "mulcom", "pcom")
                                         ),
                                         
                                         actionButton("show_dlgManW", "Help", icon("question-circle"),
                                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       ) #end box friedman
                                ),
                                column(width = 9,   
                                       
                                       conditionalPanel(
                                         condition = "input.cbTables_manw == 'gsum'",
                                         
                                         box(#begin inputs box wilcox2
                                           title = "Results", status = "primary", solidHeader = TRUE,
                                           collapsible = TRUE, width = 12,
                                           
                                           #DT::dataTableOutput("ou_dtwilcox2")#,
                                           div(DT::dataTableOutput("ou_dtmanw_gsum"))
                                         )
                                         
                                       ), #end conditionalPanel for mann-whitney
                                       
                                       
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
                                         
                                         checkboxGroupInput("cbTables_frman", "Options",
                                                            choiceNames =
                                                              list("Global summary", "Multiple comparison",
                                                                   "Paired comparison" ),
                                                            choiceValues =
                                                              list("gsum", "mulcom", "pcom")
                                         ),
                                         
                                         actionButton("show_dlgFriedman", "Help", icon("question-circle"),
                                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       ) #end box friedman
                                ),
                                column(width = 9,  
                                       
                                       conditionalPanel(
                                         condition = "input.cbTables_frman == 'gsum'",
                                         
                                         box(#begin inputs box friedman
                                           title = "Results", status = "primary", solidHeader = TRUE,
                                           collapsible = TRUE, width = 12,

                                           div(DT::dataTableOutput("ou_dtfrman_gsum"))
                                         )
                                         
                                       ), #end conditionalPanel for friedman


                                       box(#begin inputs box friedman
                                         title = "Results", status = "primary", solidHeader = TRUE,
                                         collapsible = TRUE, width = 12,
                                         
                                         DT::dataTableOutput("ou_dtfrman")
                                       )
                                )
                                
                              )    #end fluidrow 
      ), #end tab Friedman test
      
      #---------------------------------------------------------------------------------------
      
      
      #tab for Durbin test ----------------------------------------------------------------
      shinydashboard::tabItem(tabName = "tdurbinTab",
                              #h2("Durbin Test"),
                              
                              fluidRow( #begin fluid row
                                column(width = 3, 
                                       
                                       box(#begin inputs box durbin
                                         title = "Durbin Test", status = "primary", solidHeader = TRUE,
                                         collapsible = TRUE, width = NULL,
                                         
                                         uiOutput("ou_jugdurbin"),
                                         uiOutput("ou_trtdurbin"),
                                         uiOutput("ou_traitdurbin"),
                                         
                                         checkboxGroupInput("cbTables_durbin", "Options",
                                                            choiceNames =
                                                              list("Global summary", "Multiple comparison",
                                                                   "Paired comparison" ),
                                                            choiceValues =
                                                              list("gsum", "mulcom", "pcom")
                                         ),
                                         
                                         actionButton("show_dlgDurbin", "Help", icon("question-circle"),
                                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       ) #end box durbin
                                ),
                                column(width = 9,
                                       
                                       conditionalPanel(
                                         condition = "input.cbTables_durbin == 'gsum'",
                                         
                                         box(#begin inputs box durbin
                                           title = "Results", status = "primary", solidHeader = TRUE,
                                           collapsible = TRUE, width = 12,
                                           
                                           div(DT::dataTableOutput("ou_dtdurbin_gsum"))
                                         )
                                         
                                       ), #end conditionalPanel for durbin
                                       
                                       box(#begin inputs box durbin
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
                                         
                                         checkboxGroupInput("cbTables_kru", "Options",
                                                            choiceNames =
                                                              list("Global summary", "Multiple comparison",
                                                                   "Paired comparison" ),
                                                            choiceValues =
                                                              list("gsum", "mulcom", "pcom")
                                         ),
                                         
                                         actionButton("show_dlgkruskal", "Help", icon("question-circle"),
                                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       ) #end box kruskal
                                ),
                                column(width = 9, 
                                       
                                       conditionalPanel(
                                         condition = "input.cbTables_kru == 'gsum'",
                                         
                                         box(#begin inputs box kruskal wallis
                                           title = "Results", status = "primary", solidHeader = TRUE,
                                           collapsible = TRUE, width = 12,
                                           
                                           div(DT::dataTableOutput("ou_dtkru_gsum"))
                                         )
                                         
                                       ), #end conditionalPanel for kruskal
                                       
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
                                         
                                         checkboxGroupInput("cbTables_med", "Options",
                                                            choiceNames =
                                                              list("Global summary", "Multiple comparison",
                                                                   "Paired comparison" ),
                                                            choiceValues =
                                                              list("gsum", "mulcom", "pcom")
                                         ),
                                         
                                         actionButton("show_dlgmed", "Help", icon("question-circle"),
                                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       ) #end box med
                                ),
                                column(width = 9,    

                                       conditionalPanel(
                                         condition = "input.cbTables_med == 'gsum'",
                                         
                                         box(#begin inputs box median
                                           title = "Results", status = "primary", solidHeader = TRUE,
                                           collapsible = TRUE, width = 12,
                                           
                                           div(DT::dataTableOutput("ou_dtmed_gsum"))
                                         )
                                         
                                       ), #end conditionalPanel for median
                                     
                                       
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
                                         
                                         
                                         checkboxGroupInput("cbTables_jonck", "Options",
                                                            choiceNames =
                                                              list("Global summary", "Multiple comparison",
                                                                   "Paired comparison" ),
                                                            choiceValues =
                                                              list("gsum", "mulcom", "pcom")
                                         ),
                                         
                                         
                                         actionButton("show_dlgjonck", "Help", icon("question-circle"),
                                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       ) #end box jonck
                                ),
                                column(width = 9,    
                                       
                                       conditionalPanel(
                                         condition = "input.cbTables_jonck == 'gsum'",
                                         
                                         box(#begin inputs box jonck
                                           title = "Results", status = "primary", solidHeader = TRUE,
                                           collapsible = TRUE, width = 12,
                                           
                                           #DT::dataTableOutput("ou_dtwilcox2")#,
                                           div(DT::dataTableOutput("ou_dtjonck_gsum"))
                                         )
                                         
                                       ), #end conditionalPanel for jonck
                                     
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
