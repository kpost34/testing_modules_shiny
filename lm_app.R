#Linear model app using modules
#Created by Keith Post on 10/1/23

#load packages
library(shiny)
library(tidyverse)
library(datasets)
library(DT)


# Basic Linear Model App without Modules============================================================
#create an app that pulls only dataframes containing at least two numeric variables and allows the
  #user to choose which variables, which are then plotted and the user can toggle a lm smoother
  #on/off and with/without CI bands

## Obj (background work)
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
  output$main_plot <- renderPlot({
    #reqs
    req(length(input$sel_vars)==2, #two vars selected
        check_vars(input$sel_vars, data())) #vars must match what's in data()
    
    
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



# Basic Linear Model App with Modules===============================================================
## Obj (background work)....same as above
### Identify objs (names) that are data frames
dfs_ds <- ls("package:datasets") %>%
  purrr::map(function(x) {
    get(x) %>%
      class() 
  }) %>%
  set_names(ls("package:datasets")) %>%
  .[.=="data.frame"] %>%
  names()

### Identify DFs (names) that have at least two numeric variables
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


## Generate a scatter plot with a smoother & CI bands
## UI
dataInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    #inputs
    selectizeInput(ns("sel_df"), label="Choose a dataset", choices=dfs_num_ds, multiple=TRUE,
                   options=list(maxItems=1)),
    varSelectizeInput(ns("sel_vars"), label="Choose two variables", multiple=TRUE, data=NULL,
                     options=list(maxItems=2)),
    radioButtons(ns("rad_lm"), label="Display linear model?", choices=c("Yes", "No"),
                 selected="No"),
    radioButtons(ns("rad_se"), label="Display confidence interval?", choices=c("Yes", "No"),
                 selected="No")
  )
}


scatterOutput <- function(id) {
  ns <- NS(id)

  tagList(
    #output
    plotOutput(ns("main_plot"))
  )
}



## Server
dataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
     #create reactive data()
    data <- reactive({get(input$sel_df) %>% 
        select(where(is.numeric))})

    #update inputs
    observeEvent(input$sel_df, {
      updateVarSelectizeInput(session, inputId="sel_vars", label="Choose two variables", data=data(),
                              options=list(maxItems=2))
    })
    
    list(
      #list of reactives to export to another server module
      data = reactive(data()), #note: need to re-wrap data() in reactive() before exporting
      vars = reactive(input$sel_vars),
      smooth = reactive(input$rad_lm),
      ci = reactive(input$rad_se)
    )
  })
}


scatterServer <- function(id, data, vars, smooth, ci) {
  moduleServer(id, function(input, output, session) {
    
    #plot output
    output$main_plot <- renderPlot({

      #reqs
      req(length(vars())==2, #two vars selected
          check_vars(vars(), data())) #vars must match what's in data()


      #dynamic se_value
      se_value <- reactive({if(ci()=="No") {
        FALSE
        } else if(ci()=="Yes") {TRUE}})



      #plot data
      data() %>%
        ggplot(aes(x=!!vars()[[1]], y=!!vars()[[2]])) +
        geom_point() +
        {if(smooth()=="Yes") geom_smooth(method="lm", se=se_value())} +
        theme_bw()
    })
  })
}


  
## App 
lmApp <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(width=3,
        dataInput("df")
      ),
      mainPanel(width=9,
        scatterOutput("scatter")
      )
    )
  )
  
  server <- function(input, output, session) {
    x <- dataServer("df")
    scatterServer("scatter", x$data, x$vars, x$smooth, x$ci)
  }
 
  shinyApp(ui, server)
  
}


lmApp()



# lm app with Added Features========================================================================
library(broom)
#features:
  #UI:


  #Server:
    #created plot_scatter() & removed extra reactive obj




## Obj (background work)....same as above
### Identify objs (names) that are data frames
dfs_ds <- ls("package:datasets") %>%
  purrr::map(function(x) {
    get(x) %>%
      class() 
  }) %>%
  set_names(ls("package:datasets")) %>%
  .[.=="data.frame"] %>%
  names()

### Identify DFs (names) that have at least two numeric variables
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


## Make scatter plot with SE (and CI) as options
plot_scatter <- function(data, var_list, reg, ci_val) {
  data %>%
    ggplot(aes(x=!!var_list[[1]], y=!!var_list[[2]])) +
    geom_point() +
    {if(reg=="Yes") geom_smooth(method="lm", se=ifelse(ci_val=="Yes", TRUE, FALSE))} +
    theme_bw()
}


## Calculate summary stats
calc_summ_stats <- function(data) {
  data %>%
    pivot_longer(cols=everything(), names_to="var", values_to="value") %>%
    group_by(var) %>%
    summarize(across(everything(),
              list(n=length,
                   min=min,
                   mean=mean,
                   median=median,
                   max=max,
                   sd=sd
              ), .names="{.fn}")) %>%
    ungroup() %>%
    mutate(across(where(is.numeric), ~signif(.x, 3)))
}


## Generate a scatter plot with a smoother & CI bands
## UI
dataInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    #inputs
    selectizeInput(ns("sel_df"), label="Choose a dataset", choices=dfs_num_ds, multiple=TRUE,
                   options=list(maxItems=1)),
    varSelectizeInput(ns("sel_vars"), label="Choose two variables", multiple=TRUE, data=NULL,
                     options=list(maxItems=2)),
    radioButtons(ns("rad_lm"), label="Display linear model?", choices=c("Yes", "No"),
                 selected="No"),
    radioButtons(ns("rad_se"), label="Display confidence interval?", choices=c("Yes", "No"),
                 selected="No")
  )
}


scatterTablesOutput <- function(id) {
  ns <- NS(id)

  tabsetPanel(id=ns("main_tabset"), type="tabs",
    #output
    tabPanel("table1",
      DTOutput(ns("raw_tab")),
      br(),
      DTOutput(ns("summ_tab")),
      br(),
      DTOutput(ns("summ_vars_tab"))
    ),
    tabPanel("plot",
      plotOutput(ns("main_plot"))
    )
  )
}



## Server
dataServer <- function(id) {
  moduleServer(id, function(input, output, session) {
     #create reactive data()
    data <- reactive({get(input$sel_df) %>% 
        select(where(is.numeric))})

    #update inputs
    observeEvent(input$sel_df, {
      updateVarSelectizeInput(session, inputId="sel_vars", label="Choose two variables", data=data(),
                              options=list(maxItems=2))
    })
    
    list(
      #list of reactives to export to another server module
      data = reactive(data()), #note: need to re-wrap data() in reactive() before exporting
      vars = reactive(input$sel_vars),
      smooth = reactive(input$rad_lm),
      ci = reactive(input$rad_se)
    )
  })
}


scatterTablesServer <- function(id, data, vars, smooth, ci) {
  moduleServer(id, function(input, output, session) {
    
    #plot output
    output$main_plot <- renderPlot({

      #reqs
      req(length(vars())==2, #two vars selected
          check_vars(vars(), data())) #vars must match what's in data()

      #plot data
      plot_scatter(data(), vars(), smooth(), ci())
    })
    
    
    #generate summary stats table
    output$raw_tab <- renderDT({
      data() %>%
        head(n=10) %>%
        tryCatch(error=function(e) NULL) %>%
        datatable(caption="Data Sample", rownames=FALSE, options=list(dom="t")) 
    })
    
    
    output$summ_tab <- renderDT({
      calc_summ_stats(data()) %>%
        tryCatch(error=function(e) NULL) %>%
        datatable(caption="Summary Stats", rownames=FALSE, options=list(dom="t"))
    })
    
    output$summ_vars_tab <- renderDT({
      #req
      req(length(vars())==2, #two vars selected
          check_vars(vars(), data())) #vars must match what's in data()
      
      data() %>%
        select(!!!vars()) %>%
        calc_summ_stats()
    })
  })
}


  
## App 
lmApp <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(width=3,
        dataInput("df")
      ),
      mainPanel(width=9,
        scatterTablesOutput("out")
      )
    )
  )
  
  server <- function(input, output, session) {
    x <- dataServer("df")
    scatterTablesServer("out", x$data, x$vars, x$smooth, x$ci)
  }
 
  shinyApp(ui, server)
  
}


lmApp()


#figure out a way to highlight cols and rows of two chosen variables & eliminate third table
#add titles, remove rownames, show x entries, search bar etc






#later features....data transformations, test of normality of residuals, output table with model
  #parameters, output table of summary stats






