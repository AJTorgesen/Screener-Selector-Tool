library(shiny)

shinyServer(function(input, output, session) {
    sever(
        opacity = 0.75
    )
    Sys.sleep(1) #  something that takes time
    
    waiter_hide()
    
    output$page_title <- renderUI({
        h1(input$nav)
    })
    
    
    #SIDEBAR POPOUT LOGIC----
    pop <- reactiveVal("in")
    
    popout <- function(direction)
    {
        if(direction == "out")
        {
            delay(200, jqui_switch_class("#mainpanel", removeClassName = "col-sm-12", addClassName = "col-sm-8", duration = 1000))
            delay(1250, jqui_show("#sidebar", effect = "blind", duration = 1000))
            pop("in")
            return()
        }
        else(direction == "in")
        {
            delay(200, jqui_hide("#sidebar", effect = "blind", duration = 1000))
            delay(1250, jqui_switch_class("#mainpanel", removeClassName = "col-sm-8", addClassName = "col-sm-12", duration = 1000))
            pop("out")
            return()
            
        }
    }
    
    shinyjs::onclick("sidebarToggle-lg", popout(pop()))
    
    
    #FILTER BUTTON LOGIC----
    filters <- reactiveValues(type = list(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
                              compare = list("Less than","Less than","Less than","Less than","Less than","Less than","Less than"),
                              value = list(0.5,0.5,0.5,0.5,5,5,5)
                              )
    
    dataModal <- function(predictive_validity = FALSE, concurrent_validity = FALSE, reliability = FALSE, classification_accuracy = FALSE, cost = FALSE, scoring_time = FALSE, admin_time = FALSE,
                          pv_01 = "Less than", cv_01 = "Less than", r_01 = "Less than", ca_01 = "Less than", cost_01 = "Less than", ST_01 = "Less than", AT_01 = "Less than",
                          pv_02 = 0.5, cv_02 = 0.5, r_02 = 0.5, ca_02 = 0.5, cost_02 = 5, ST_02 = 5, AT_02 = 5) {
        modalDialog(
            title = "Filter Criteria",
            div(
                tags$table(style = "width:100%",
                           tags$tr(style = "border-bottom: #e0e0e0 solid 1px;",
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           prettyCheckbox(
                                               inputId = "PV_FC",
                                               label = "Predictive Validity",
                                               value = predictive_validity,
                                               status = "primary",
                                               shape = "curve",
                                               outline = FALSE,
                                               fill = TRUE,
                                               icon = icon("check"),
                                               inline = TRUE,
                                               width = "100%"
                                           )),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           selectInput(
                                               inputId = "PV_FC_01",
                                               label = NULL,
                                               choices = c("Less than","Greater than","Equal to"),
                                               selected = pv_01,
                                               width = "100%"
                                           )),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           numericInput(
                                               inputId = "PV_FC_02",
                                               label = NULL,
                                               value = pv_02,
                                               min = 0,
                                               max = 1,
                                               step = 0.05,
                                               width = "100%"
                                           ))
                                   
                           ),
                           tags$tr(style = "border-bottom: #e0e0e0 solid 1px;",
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           prettyCheckbox(
                                               inputId = "CV_FC",
                                               label = "Concurrent Validity",
                                               value = concurrent_validity,
                                               status = "primary",
                                               shape = "curve",
                                               outline = FALSE,
                                               fill = TRUE,
                                               icon = icon("check"),
                                               inline = TRUE,
                                               width = "100%"
                                           )),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           selectInput(
                                               inputId = "CV_FC_01",
                                               label = NULL,
                                               choices = c("Less than","Greater than","Equal to"),
                                               selected = cv_01,
                                               width = "100%"
                                           )),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           numericInput(
                                               inputId = "CV_FC_02",
                                               label = NULL,
                                               value = cv_02,
                                               min = 0,
                                               max = 1,
                                               step = 0.05,
                                               width = "100%"
                                           ))
                                   
                           ),
                           tags$tr(style = "border-bottom: #e0e0e0 solid 1px;",
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           prettyCheckbox(
                                               inputId = "CA_FC",
                                               label = "Classification Accuracy",
                                               value = classification_accuracy,
                                               status = "primary",
                                               shape = "curve",
                                               outline = FALSE,
                                               fill = TRUE,
                                               icon = icon("check"),
                                               inline = TRUE,
                                               width = "100%"
                                           )),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           selectInput(
                                               inputId = "CA_FC_01",
                                               label = NULL,
                                               choices = c("Less than","Greater than","Equal to"),
                                               selected = ca_01,
                                               width = "100%"
                                           )
                                   ),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           numericInput(
                                               inputId = "CA_FC_02",
                                               label = NULL,
                                               value = ca_02,
                                               min = 0,
                                               max = 1,
                                               step = 0.05,
                                               width = "100%"
                                           )
                                   )
                                   
                           ),
                           tags$tr(style = "border-bottom: #e0e0e0 solid 1px;",
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           prettyCheckbox(
                                               inputId = "R_FC",
                                               label = "Reliability",
                                               value = reliability,
                                               status = "primary",
                                               shape = "curve",
                                               outline = FALSE,
                                               fill = TRUE,
                                               icon = icon("check"),
                                               inline = TRUE,
                                               width = "100%"
                                           )),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           selectInput(
                                               inputId = "R_FC_01",
                                               label = NULL,
                                               choices = c("Less than","Greater than","Equal to"),
                                               selected = r_01,
                                               width = "100%"
                                           )
                                   ),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           numericInput(
                                               inputId = "R_FC_02",
                                               label = NULL,
                                               value = r_02,
                                               min = 0,
                                               max = 1,
                                               step = 0.05,
                                               width = "100%"
                                           )
                                   )
                                   
                           ),
                           tags$tr(style = "border-bottom: #e0e0e0 solid 1px;",
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           prettyCheckbox(
                                               inputId = "Cost_FC",
                                               label = "Cost",
                                               value = cost,
                                               status = "primary",
                                               shape = "curve",
                                               outline = FALSE,
                                               fill = TRUE,
                                               icon = icon("check"),
                                               inline = TRUE,
                                               width = "100%"
                                           )),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           selectInput(
                                               inputId = "Cost_FC_01",
                                               label = NULL,
                                               choices = c("Less than","Greater than","Equal to"),
                                               selected = cost_01,
                                               width = "100%"
                                           )
                                   ),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           numericInput(
                                               inputId = "Cost_FC_02",
                                               label = NULL,
                                               value = cost_02,
                                               min = 0,
                                               max = 50,
                                               step = 1,
                                               width = "100%"
                                           )
                                   )
                                   
                           ),
                           tags$tr(style = "border-bottom: #e0e0e0 solid 1px;",
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           prettyCheckbox(
                                               inputId = "ST_FC",
                                               label = "Additional Scoring Time",
                                               value = scoring_time,
                                               status = "primary",
                                               shape = "curve",
                                               outline = FALSE,
                                               fill = TRUE,
                                               icon = icon("check"),
                                               inline = TRUE,
                                               width = "100%"
                                           )),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           selectInput(
                                               inputId = "ST_FC_01",
                                               label = NULL,
                                               choices = c("Less than","Greater than","Equal to"),
                                               selected = ST_01,
                                               width = "100%"
                                           )
                                   ),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           numericInput(
                                               inputId = "ST_FC_02",
                                               label = NULL,
                                               value = ST_02,
                                               min = 0,
                                               max = 60,
                                               step = 0.05,
                                               width = "100%"
                                           )
                                   )
                                   
                           ),
                           tags$tr(style = "border-bottom: #e0e0e0 solid 1px;",
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           prettyCheckbox(
                                               inputId = "AT_FC",
                                               label = "Administration Time",
                                               value = admin_time,
                                               status = "primary",
                                               shape = "curve",
                                               outline = FALSE,
                                               fill = TRUE,
                                               icon = icon("check"),
                                               inline = TRUE,
                                               width = "100%"
                                           )),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           selectInput(
                                               inputId = "AT_FC_01",
                                               label = NULL,
                                               choices = c("Less than","Greater than","Equal to"),
                                               selected = AT_01,
                                               width = "100%"
                                           )
                                   ),
                                   tags$td(style = "width:33.333%; padding: 15px;",
                                           numericInput(
                                               inputId = "AT_FC_02",
                                               label = NULL,
                                               value = AT_02,
                                               min = 0,
                                               max = 60,
                                               step = 0.05,
                                               width = "100%"
                                           )
                                   )
                                   
                           ),
                           
                           
                )
            ),
            footer = tagList(
                modalButton("Cancel"),
                actionButton(
                    inputId = "filter",
                    label = "Apply Filter(s)",
                    style = "background-color: #1d89ff; color: white;"
                )
            ),
            easyClose = TRUE
        )
        
        
        }
    
    observeEvent(input$FilterBttn, {
        showModal(dataModal(predictive_validity = filters$type[[1]], concurrent_validity = filters$type[[2]], reliability = filters$type[[3]], classification_accuracy = filters$type[[4]], cost = filters$type[[5]], scoring_time = filters$type[[6]], admin_time = filters$type[[7]],
                            pv_01 = filters$compare[[1]], cv_01 = filters$compare[[2]], r_01 = filters$compare[[3]], ca_01 = filters$compare[[4]], cost_01 = filters$compare[[5]], ST_01 = filters$compare[[6]], AT_01 = filters$compare[[7]],
                            pv_02 = filters$value[[1]], cv_02 = filters$value[[2]], r_02 = filters$value[[3]], ca_02 = filters$value[[4]], cost_02 = filters$value[[5]], ST_02 = filters$value[[6]], AT_02 = filters$value[[7]])
        )
        
    })
    
    
    observeEvent(input$filter, {
        filters$type[[1]] <- input$PV_FC
        filters$type[[2]] <- input$CV_FC
        filters$type[[3]] <- input$R_FC
        filters$type[[4]] <- input$CA_FC
        filters$type[[5]] <- input$Cost_FC
        filters$type[[6]] <- input$ST_FC
        filters$type[[7]] <- input$AT_FC
        if(input$PV_FC)
        {
            filters$compare[[1]] <- input$PV_FC_01
            filters$value[[1]] <- input$PV_FC_02
        }
        
        if(input$CV_FC)
        {
            filters$compare[[2]] <- input$CV_FC_01
            filters$value[[2]] <- input$CV_FC_02
        }
        
        if(input$R_FC)
        {
            filters$compare[[3]] <- input$R_FC_01
            filters$value[[3]] <- input$R_FC_02
        }
        
        if(input$CA_FC)
        {
            filters$compare[[4]] <- input$CA_FC_01
            filters$value[[4]] <- input$CA_FC_02
        }
        
        if(input$Cost_FC)
        {
            filters$compare[[5]] <- input$Cost_FC_01
            filters$value[[5]] <- input$Cost_FC_02
        }
        
        if(input$ST_FC)
        {
            filters$compare[[6]] <- input$ST_FC_01
            filters$value[[6]] <- input$ST_FC_02
        }
        
        if(input$AT_FC)
        {
            filters$compare[[7]] <- input$AT_FC_01
            filters$value[[7]] <- input$AT_FC_02
        }
        
        removeModal()
    })
    
    #Read in files containing webscraped data from NCII
    gradek_final <- readRDS("data/kindergarten.RData")
    grade1_final <- readRDS("data/grade1.RData")
    grade2_final <- readRDS("data/grade2.RData")
    grade3_final <- readRDS("data/grade3.RData")
    
    #MAIN TABLE LOGIC----
    maindata <- reactive({
        
        validate(need(!is.null(input$screener_type), "Choose a type."))
        validate(need(!is.null(input$criteria), "Choose a criteria."))
        
        df <- switch (input$grade,
                    "K" = gradek_final,
                    "1" = grade1_final,
                    "2" = grade2_final,
                    "3" = grade3_final
        )
        
        
        
        
        
        if("Math" %in% input$screener_type)
        {
            df_math <- df[df[10] == "Math",]
        }
        else
        {
            df_math <- NULL
        }
        
        if("Reading" %in% input$screener_type)
        {
            df_reading <- df[df[10] == "Reading",]
        }
        else
        {
            df_reading <- NULL
        }
        
        
        df <- rbind(df_reading,df_math)

        
        d <- data.frame(Names = paste0('<a href="',df[,2],'" target= "_blank" >',df[,1],"</a>"))
        
        if("Cost" %in% input$criteria)
        {
            d$Cost <- as.numeric(str_remove_all(df[,3], "\\$"))
            
            
        }
        if("Predictive Validity" %in% input$criteria)
        {
            d$`Predictive Validity` <- round(df[,7], 2)
            
            
        }
        if("Concurrent Validity" %in% input$criteria)
        {
            d$`Concurrent Validity` <- round(df[,8], 2)
            
            
        }
        if("Reliability" %in% input$criteria)
        {
            d$Reliability <- round(df[,9],2)
            
            
        }
        if("Classification Accuracy" %in% input$criteria)
        {
            d$Accuracy <- round(df[,6],2)
            
            
        }
        if("Additional Scoring Time" %in% input$criteria)
        {
            d$`Additional Scoring Time` <- df[,4]
            
            
        }
        if("Administration Time" %in% input$criteria)
        {
            d$`Administration Time` <- df[,5]
        }
        
        if(filters$type[[5]] && "Cost" %in% input$criteria)
        {
            d <- switch (filters$compare[[5]],
                         "Less than" = filter(d, Cost < filters$value[[5]]),
                         "Greater than" = filter(d, Cost > filters$value[[5]]),
                         "Equal to" = filter(d, Cost == filters$value[[5]])


            )
        }
        if(filters$type[[1]] && "Predictive Validity" %in% input$criteria)
        {
            d <- switch (filters$compare[[1]],
                         "Less than" = filter(d, `Predictive Validity` < filters$value[[1]]),
                         "Greater than" = filter(d, `Predictive Validity` > filters$value[[1]]),
                         "Equal to" = filter(d, `Predictive Validity` == filters$value[[1]])


            )
        }
        if(filters$type[[2]] && "Concurrent Validity" %in% input$criteria)
        {
            d <- switch (filters$compare[[2]],
                         "Less than" = filter(d, `Concurrent Validity` < filters$value[[2]]),
                         "Greater than" = filter(d, `Concurrent Validity` > filters$value[[2]]),
                         "Equal to" = filter(d, `Concurrent Validity` == filters$value[[2]])


            )
        }
        if(filters$type[[3]] && "Reliability" %in% input$criteria)
        {
            d <- switch (filters$compare[[3]],
                         "Less than" = filter(d, Reliability < filters$value[[3]]),
                         "Greater than" = filter(d, Reliability > filters$value[[3]]),
                         "Equal to" = filter(d, Reliability == filters$value[[3]])


            )
        }
        if(filters$type[[4]] && "Classification Accuracy" %in% input$criteria)
        {
            d <- switch (filters$compare[[4]],
                         "Less than" = filter(d, Accuracy < filters$value[[4]]),
                         "Greater than" = filter(d, Accuracy > filters$value[[4]]),
                         "Equal to" = filter(d, Accuracy == filters$value[[4]])


            )
        }
        if(filters$type[[6]] && "Additional Scoring Time" %in% input$criteria)
        {
            d <- switch (filters$compare[[6]],
                         "Less than" = filter(d, `Additional Scoring Time` < filters$value[[6]]),
                         "Greater than" = filter(d, `Additional Scoring Time` > filters$value[[6]]),
                         "Equal to" = filter(d, `Additional Scoring Time` == filters$value[[6]])


            )
        }
        if(filters$type[[7]] && "Administration Time" %in% input$criteria)
        {
            d <- switch (filters$compare[[7]],
                         "Less than" = filter(d, `Administration Time` < filters$value[[7]]),
                         "Greater than" = filter(d, `Administration Time` > filters$value[[7]]),
                         "Equal to" = filter(d, `Administration Time` == filters$value[[7]])


            )
        }
        
        if("Math" %in% input$screener_type)
        {
            
        }
        
        
        return(d)
        
        
    })
    
    #Same as maindata reactive without combining names into a clickable link. Used for creating a .csv file for download
    download_data <- reactive({
        df <- switch (input$grade,
                      "K" = gradek_final,
                      "1" = grade1_final,
                      "2" = grade2_final,
                      "3" = grade3_final
        )
        
        if("Math" %in% input$screener_type)
        {
            df_math <- df[df[10] == "Math",]
        }
        else
        {
            df_math <- NULL
        }
        
        if("Reading" %in% input$screener_type)
        {
            df_reading <- df[df[10] == "Reading",]
        }
        else
        {
            df_reading <- NULL
        }
        
        
        df <- rbind(df_reading,df_math)
        
        d <- data.frame(Names = df[,1], Links = df[,2])
        
        if("Cost" %in% input$criteria)
        {
            d$Cost <- as.numeric(str_remove_all(df[,3], "\\$"))
            
            
        }
        if("Predictive Validity" %in% input$criteria)
        {
            d$`Predictive Validity` <- round(df[,7], 2)
            
            
        }
        if("Concurrent Validity" %in% input$criteria)
        {
            d$`Concurrent Validity` <- round(df[,8], 2)
            
            
        }
        if("Reliability" %in% input$criteria)
        {
            d$Reliability <- round(df[,9],2)
            
-
        }
        if("Classification Accuracy" %in% input$criteria)
        {
            d$Accuracy <- round(df[,6],2)
            
            
        }
        if("Additional Scoring Time" %in% input$criteria)
        {
            d$`Additional Scoring Time` <- df[,4]
            
            
        }
        if("Administration Time" %in% input$criteria)
        {
            d$`Administration Time` <- df[,5]
            
            
        }
        
        if(filters$type[[5]] && "Cost" %in% input$criteria)
        {
            d <- switch (filters$compare[[5]],
                         "Less than" = filter(d, Cost < filters$value[[5]]),
                         "Greater than" = filter(d, Cost > filters$value[[5]]),
                         "Equal to" = filter(d, Cost == filters$value[[5]])
                         
                         
            )
        }
        if(filters$type[[1]] && "Predictive Validity" %in% input$criteria)
        {
            d <- switch (filters$compare[[1]],
                         "Less than" = filter(d, `Predictive Validity` < filters$value[[1]]),
                         "Greater than" = filter(d, `Predictive Validity` > filters$value[[1]]),
                         "Equal to" = filter(d, `Predictive Validity` == filters$value[[1]])
                         
                         
            )
        }
        if(filters$type[[2]] && "Concurrent Validity" %in% input$criteria)
        {
            d <- switch (filters$compare[[2]],
                         "Less than" = filter(d, `Concurrent Validity` < filters$value[[2]]),
                         "Greater than" = filter(d, `Concurrent Validity` > filters$value[[2]]),
                         "Equal to" = filter(d, `Concurrent Validity` == filters$value[[2]])
                         
                         
            )
        }
        if(filters$type[[3]] && "Reliability" %in% input$criteria)
        {
            d <- switch (filters$compare[[3]],
                         "Less than" = filter(d, Reliability < filters$value[[3]]),
                         "Greater than" = filter(d, Reliability > filters$value[[3]]),
                         "Equal to" = filter(d, Reliability == filters$value[[3]])
                         
                         
            )
        }
        if(filters$type[[4]] && "Classification Accuracy" %in% input$criteria)
        {
            d <- switch (filters$compare[[4]],
                         "Less than" = filter(d, Accuracy < filters$value[[4]]),
                         "Greater than" = filter(d, Accuracy > filters$value[[4]]),
                         "Equal to" = filter(d, Accuracy == filters$value[[4]])
                         
                         
            )
        }
        if(filters$type[[6]] && "Additional Scoring Time" %in% input$criteria)
        {
            d <- switch (filters$compare[[6]],
                         "Less than" = filter(d, `Additional Scoring Time` < filters$value[[6]]),
                         "Greater than" = filter(d, `Additional Scoring Time` > filters$value[[6]]),
                         "Equal to" = filter(d, `Additional Scoring Time` == filters$value[[6]])
                         
                         
            )
        }
        if(filters$type[[7]] && "Administration Time" %in% input$criteria)
        {
            d <- switch (filters$compare[[7]],
                         "Less than" = filter(d, `Administration Time` < filters$value[[7]]),
                         "Greater than" = filter(d, `Administration Time` > filters$value[[7]]),
                         "Equal to" = filter(d, `Administration Time` == filters$value[[7]])
                         
                         
            )
        }
        
        
        
        
        return(d)
    })
    
    #Create text for the type of screener (Reading or Math) to be used in the table title
    screener_type <- reactive({
        if("Math" %in% input$screener_type && "Reading" %in% input$screener_type)
        {
            return(paste0("Math & Reading"))
        }
        else
        {
            return(input$screener_type)
        }
    })
    
    #Create a reactive value of the table widget. Used in saving the widget in a seperate html and taking a screenshot for .pdf and .png download
    maintable <- reactiveVal()
    
    #Generate the table output
    output$screenertable <- renderDataTable({
        
        rowCallback <- c(
            "function(row, data){",
            "  for(var i=0; i<data.length; i++){",
            "    if(data[i] === null){",
            "      $('td:eq('+i+')', row).html('NA')",
            "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
            "    }",
            "  }",
            "}"  
        )
        dwnld <- datatable(maindata(),
                       caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black; font-size:200% ;', paste0('Screeners Table: Grade ', input$grade, ' ', screener_type())),
                       escape = FALSE, filter = 'none', selection = 'none',
                       rownames = FALSE, options = list(scrollx = TRUE, lengthChange = FALSE, pageLength = 20, dom ='tp', rowCallback = JS(rowCallback))) %>%
            formatRound(columns = 2:length(maindata()), digits = 2)
        
        
        if("Cost" %in% input$criteria)
        {
            
            dwnld <- formatCurrency(dwnld, c("Cost"), currency = "$")
        }
        if("Additional Scoring Time" %in% input$criteria)
        {
            dwnld <- formatString(dwnld, c("Additional Scoring Time"), suffix = " min(s)")
        }
        if("Administration Time" %in% input$criteria)
        {
            dwnld <- formatString(dwnld, c("Administration Time"), suffix = " min(s)")
        }
        
        maintable(dwnld)
        
        return(dwnld)
    })
    
    
    #Generate the filename for download from ui inputs
    download_filename <- reactive({
        paste0(input$download_name,input$download_type)
    })
    
    #Download Handler. Triggered when download button is pressed on UI
    output$download_btn <- downloadHandler(
        filename = function(){download_filename()},
        content = function(file){
            if(input$download_type == ".csv")
            {
                write.csv(download_data(), file, row.names = FALSE)
            }
            else if(input$download_type == ".pdf" || input$download_type == ".png")
            {
                saveWidget(maintable(), "maintable.html")
                webshot("maintable.html", file = file)
            }
            
        }
    )
    
})
