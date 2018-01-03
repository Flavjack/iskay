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
  
  req(input$sel_input_Xwilcox2)
  req(input$sel_input_Ywilcox2)
  out <- test_result()  
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