#Example apps using modules
#Created by Keith Post on 9/24/23

#load packages
library(shiny)
library(tidyverse)
library(DT)

# t-test app========================================================================================
#create an app where a user generates two vectors using rnorm() by setting parameters in the UI then
  #a t-test is performed, the statistics are returned

## Without modules----------
### UI
ui <- fluidPage(
  #inputs
  "Sample 1",
  sliderInput("n1", "Sample Size", value=13, min=6, max=20, step=2),
  sliderInput("mean1", "Mean", value=3, min=1, max=5, step=1),
  sliderInput("sd1", "Standard Deviation", value=0.3, min=0.1, max=0.5, step=0.1),
  br(),
  "Sample 2",
  sliderInput("n2", "Sample Size", value=13, min=6, max=20, step=2),
  sliderInput("mean2", "Mean", value=3, min=1, max=5, step=1),
  sliderInput("sd2", "Standard Deviation", value=0.3, min=0.1, max=0.5, step=0.1),
  br(),
  
  #outputs
  textOutput("t_value"),
  textOutput("p_value")
)

### Server
server <- function(input, output, session){
  #create reactives from inputs
  samp1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
  samp2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
  
  #use reactives to run t-test
  t_test_result <- reactive(t.test(samp1(), samp2())) #note: assumes two-sided, unequal var, and alpha=0.05
  
  #extract outputs of t-test into renders
  output$t_value <- renderText(paste("t =", t_test_result()$statistic %>%
                                       signif(3)))
  output$p_value <- renderText(paste("p =", t_test_result()$p.value %>%
                                       signif(3)))
}

shinyApp(ui, server)


## With modules----------

#flow of the app: 1) create inputs (UI); 2) pull info from inputs and create reactive obj (server); 
  #3) take those objs, run test, and store into another reactive obj (server); 4) extract elements
  #4) of that obj into render (server), 5) print the outputs (UI)
#1-2 (same id), 3-4 (combined)-5 (same id)

### UI
sampleInput <- function(id) {
  #inputs
  tagList(
    "Sample 1",
    sliderInput(NS(id, "n1"), "Sample Size", value=13, min=6, max=20, step=2),
    sliderInput(NS(id, "mean1"), "Mean", value=3, min=1, max=5, step=1),
    sliderInput(NS(id, "sd1"), "Standard Deviation", value=0.3, min=0.1, max=0.5, step=0.1),
    br(),
    "Sample 2",
    sliderInput(NS(id, "n2"), "Sample Size", value=13, min=6, max=20, step=1),
    sliderInput(NS(id, "mean2"), "Mean", value=3, min=1, max=5, step=1),
    sliderInput(NS(id, "sd2"), "Standard Deviation", value=0.3, min=0.1, max=0.5, step=0.1),
    br()
  )
}


ttestOutput <- function(id) {
  tagList(
    textOutput(NS(id, "t_value")),
    textOutput(NS(id, "p_value"))
  )
}



### Server
sampleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    #create reactives from inputs
    samp1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
    samp2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
    
    #use reactives to run t-test
    reactive(t.test(samp1(), samp2()))
  })
}

ttestServer <- function(id, t_test_result) {
  moduleServer(id, function(input, output, session) {
    
    output$t_value <- renderText(paste("t =", t_test_result()[["statistic"]] %>% 
                                         signif(3)))

    output$p_value <- renderText(paste("p =", t_test_result()[["p.value"]] %>% 
                                         signif(3)))
  })
}
  
  
  
### App 
ttestApp <- function() {
  ui <- fluidPage(
    sampleInput("data"),
    ttestOutput("stats")
  )
  
 server <- function(input, output, session) {
    t_test_result <- sampleServer("data")
    ttestServer("stats", t_test_result)
 }
 
  shinyApp(ui, server)
}

ttestApp()



# t-test app with added features====================================================================
## Feature list
#boxplot output
#ui layout...row + columns


## Function
make_boxplot <- function(samp1, samp2, n1, n2) {
  names(samp1) <- rep("sample 1", n1)
  names(samp2) <- rep("sample 2", n2)
  
  enframe(c(samp1, samp2)) %>%
    ggplot() +
    geom_boxplot(aes(x=name, y=value, color=name)) +
    scale_color_viridis_d(end=0.6, guide="none") +
    theme_bw(base_size=16)
}



## UI
sampleInput <- function(id) {
  #inputs
  tagList(
    "Sample 1",
    sliderInput(NS(id, "n1"), "Sample Size", value=13, min=6, max=20, step=2),
    sliderInput(NS(id, "mean1"), "Population Mean", value=3, min=1, max=5, step=1),
    sliderInput(NS(id, "sd1"), "Population SD", value=0.3, min=0.1, max=0.5, step=0.1),
    br(),
    "Sample 2",
    sliderInput(NS(id, "n2"), "Sample Size", value=13, min=6, max=20, step=1),
    sliderInput(NS(id, "mean2"), "Population Mean", value=3, min=1, max=5, step=1),
    sliderInput(NS(id, "sd2"), "Population SD", value=0.3, min=0.1, max=0.5, step=0.1),
    br()
  )
}


ttestPlotOutput <- function(id) {
  tagList(
    plotOutput(NS(id, "plot")),
    br(),  #add space between text plot outputs
    h4("t-test Statistical Output"),
      h5(textOutput(NS(id, "t_value"))),
      h5(textOutput(NS(id, "p_value")))
  )
}



## Server
sampleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    #create reactives from inputs
    samp1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
    samp2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
    
    #use reactives to run t-test and generate boxplot
    list(
      t_test_out = reactive(t.test(samp1(), samp2())),
      box_out = reactive(make_boxplot(samp1(), 
                                      samp2(), 
                                      input$n1,
                                      input$n2))
    )
  })
}

ttestPlotServer <- function(id, tt_out, plot_out) {
  moduleServer(id, function(input, output, session) {
    #plot output
    output$plot <- renderPlot({plot_out()})
    
    #text (numerical) outputs
    output$t_value <- renderText(paste("t =", tt_out()[["statistic"]] %>%
                                         signif(3)))

    output$p_value <- renderText(paste("p =", tt_out()[["p.value"]] %>%
                                         signif(3)))
  })
}
  
  
  
### App 
ttestApp <- function() {
  ui <- fluidPage(
    #layout
    fluidRow(
      column(4,
        sampleInput("data"),
      ),
      column(8,
        ttestPlotOutput("stats")
      )
    )
  )
  
  server <- function(input, output, session) {
    x <- sampleServer("data")
    ttestPlotServer("stats", tt_out=x$t_test_out, plot_out=x$box_out)
  }
 
  shinyApp(ui, server)
  
}

ttestApp()



# t-test app with even more features================================================================
#features: 
  #UI: tabbed outputs--statistical output, boxplot, summary stats
  #Server: 



## Functions
### Create DF
create_df <- function(samp1, samp2, n1, n2) {
  names(samp1) <- rep("sample 1", n1)
  names(samp2) <- rep("sample 2", n2)
  
  enframe(c(samp1, samp2))
}



### Calculate summary stats
calc_summ_stats <- function(data) {
  data %>%
    group_by(name) %>%
    summarize(across(value,
              list(n=length,
                   min=min,
                   mean=mean,
                   median=median,
                   max=max,
                   sd=sd
              ), .names="{.fn}")) %>%
    ungroup() %>%
    mutate(across(where(is.double), ~signif(.x, 3)))
}


### Make boxplots
make_boxplot <- function(data) {
  data %>%
    ggplot() +
    geom_boxplot(aes(x=name, y=value, color=name)) +
    scale_color_viridis_d(end=0.6, guide="none") +
    theme_bw(base_size=12)
}


### Run ttest
run_ttest <- function(data) {
  data %>%
    t_test(value ~ name, detailed=TRUE) %>%
    select(type="alternative", group1, group2, df, t="statistic", conf.low, conf.high, p) %>%
    mutate(sig = p <= 0.05,
           across(where(is.numeric), ~signif(.x, 3)))
}



## UI
sampleInput <- function(id) {
  #inputs
  tagList(
    "Sample 1",
    sliderInput(NS(id, "n1"), "Sample Size", value=13, min=6, max=20, step=2),
    sliderInput(NS(id, "mean1"), "Population Mean", value=3, min=1, max=5, step=1),
    sliderInput(NS(id, "sd1"), "Population SD", value=0.3, min=0.1, max=0.5, step=0.1),
    br(),
    "Sample 2",
    sliderInput(NS(id, "n2"), "Sample Size", value=13, min=6, max=20, step=1),
    sliderInput(NS(id, "mean2"), "Population Mean", value=3, min=1, max=5, step=1),
    sliderInput(NS(id, "sd2"), "Population SD", value=0.3, min=0.1, max=0.5, step=0.1),
    br()
  )
}


ttestPlotOutput <- function(id) {
  tabsetPanel(id="tabs_ttest",
    tabPanel("Summary Stats",
      DTOutput(NS(id, "stats_table"))
    ),
    tabPanel("Boxplots",
      plotOutput(NS(id, "plot"))
    ),
    tabPanel("t-test output",
      h4("t-test Statistical Output"),
      DTOutput(NS(id, "ttest_table"))
    )
  )
}



## Server
sampleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    #create reactives from inputs
    samp1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
    samp2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
    
    
    #create DF
    df <- reactive(create_df(samp1(), samp2(), input$n1, input$n2))
    
    #use reactives to run t-test and generate boxplot
    list(
      stats_out = reactive(calc_summ_stats(df())),
      box_out = reactive(make_boxplot(df())),
      t_test_out = reactive(run_ttest(df()))
      
    )
  })
}

ttestPlotServer <- function(id, summ_stats, plot_out, tt_out) {
  moduleServer(id, function(input, output, session) {
    
    #stats table output
    output$stats_table <- renderDT({summ_stats()  %>% 
        DT::datatable()})
    
    #plot output
    output$plot <- renderPlot({plot_out()})
    
    #t-test output
    output$ttest_table <- renderDT(tt_out())
  })
}
  
  
  
### App 
ttestApp <- function() {
  ui <- fluidPage(
    #add more layout: sidebarLayout
    sidebarLayout(
      sidebarPanel(
        sampleInput("data"),
      ),
      mainPanel(
        ttestPlotOutput("stats")
      )
    )
  )
  
  server <- function(input, output, session) {
    x <- sampleServer("data")
    ttestPlotServer("stats", summ_stats=x$stats_out, plot_out=x$box_out, tt_out=x$t_test_out)
  }
 
  shinyApp(ui, server)
  
}

ttestApp()


# Add 1) test for equal variance and then run correct version based on results of test; 
  #2) toggle to add significance start onto boxplot; 
  #3) use rstatix::t_test off of df


# Add checkboxes to display: 1) raw data in a table, 2) summary data in a table, 3) boxplots (with
  #significance star if p< .05)


