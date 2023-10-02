#Linear model app using modules
#Created by Keith Post on 10/1/23

#load packages
library(shiny)
library(tidyverse)
library(datasets)
library(DT)


# Linear model app without modules==================================================================
#create an app that pulls only dataframes containing at least two numeric variables and allows the
  #user to choose which variables, which automatically be used to run a linear model

## Background work
### Identify obj that are data frames
dfs_ds <- ls("package:datasets") %>%
  purrr::map(function(x) {
    get(x) %>%
      class() 
  }) %>%
  set_names(ls("package:datasets")) %>%
  .[.=="data.frame"] %>%
  names()

### Identify DFs that have at least two numeric variables
dfs_num_ds <- dfs_ds %>%
  purrr::map(function(x) {
    get(x) %>%
      apply(2, function(y) class(y)=="numeric") %>%
      sum()
  }) %>%
  set_names(dfs_ds) %>%
  .[. >= 2] %>%
  names()


# Functions
## Assess whether vars match dataset
check_vars <- function(var_list, data) {
  as.character(var_list) %in% names(data) %>%
    sum()==2
}



# Obj

  

## UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width=3,
      #inputs
      selectizeInput(inputId="sel_df", label="Choose a dataset", choices=dfs_num_ds, multiple=TRUE,
                     options=list(maxItems=1)),
      varSelectizeInput(inputId="sel_vars", label="Choose two variables", multiple=TRUE, data=NULL,
               options=list(maxItems=2)),
      radioButtons(inputId="rad_lm", label="Display linear model?", choices=c("Yes", "No"),
                   selected="No"),
      radioButtons(inputId="rad_se", label="Display confidence interval?", choices=c("Yes", "No"),
                   selected="No")
    ),
    mainPanel(width=9,
      #outputs
      # verbatimTextOutput("text")
      plotOutput(outputId="main_plot")
    )
  )
)

## Server
server <-function(input, output, session) {
  
  #create reactive data()
  data <- reactive({get(input$sel_df) %>%
      select(where(is.numeric))})

  
  #update inputs
  observeEvent(input$sel_df, {
    updateVarSelectizeInput(session, inputId="sel_vars", label="Choose two variables", data=data(),
                            options=list(maxItems=2))
  })


  #plot outputs
  # output$text <- renderPrint(as.character(input$sel_vars) %in% names(data()) %>%
  #                              sum)
  
  output$main_plot <- renderPlot({
    #reqs
    req(length(input$sel_vars)==2, #two vars selected
        check_vars(input$sel_vars, data())) #vars must match what's in data()
        # as.character(input$sel_vars) %in% names(data()) %>%
        #                        sum()==2)
    
    #dynamic lm_value
    lm_value <- reactive({if(input$rad_lm=="No") {
      FALSE
    } else if(input$rad_lm=="Yes") {TRUE}})
    
    
    #dynamic se_value
    se_value <- reactive({if(input$rad_se=="No") {
      FALSE
      } else if(input$rad_se=="Yes") {TRUE}})
    
    
    
    #plot data
    data() %>%
      ggplot(aes(x=!!input$sel_vars[[1]], y=!!input$sel_vars[[2]])) +
      geom_point() +
      {if(lm_value()) geom_smooth(method="lm", se=se_value())} +
      theme_bw()

  })
}


shinyApp(ui, server)















#later features....data transformations, test of normality of residuals, output table with model
  #parameters, output table of summary statsj






