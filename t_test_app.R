#Example apps using modules
#Created by Keith Post on 9/24/23

#load packages
library(shiny)
library(tidyverse)

# t-test app========================================================================================
#create an app where a user generates two vectors using rnorm() by setting parameters in the UI then
  #a t-test is performed, the statistics are returned

## Without modules----------
### UI
ui <- fluidPage(
  #inputs
  "Sample 1",
  sliderInput("n1", "Sample Size", value=6, min=6, max=20, step=2),
  sliderInput("mean1", "Mean", value=1, min=1, max=5, step=1),
  sliderInput("sd1", "Standard Deviation", value=0.3, min=0.1, max=0.5, step=0.1),
  br(),
  "Sample 2",
  sliderInput("n2", "Sample Size", value=6, min=6, max=20, step=2),
  sliderInput("mean2", "Mean", value=1, min=1, max=5, step=1),
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
    sliderInput(NS(id, "n1"), "Sample Size", value=6, min=6, max=20, step=2),
    sliderInput(NS(id, "mean1"), "Mean", value=1, min=1, max=5, step=1),
    sliderInput(NS(id, "sd1"), "Standard Deviation", value=0.3, min=0.1, max=0.5, step=0.1),
    br(),
    "Sample 2",
    sliderInput(NS(id, "n2"), "Sample Size", value=6, min=3, max=20, step=1),
    sliderInput(NS(id, "mean2"), "Mean", value=1, min=1, max=5, step=1),
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
    sliderInput(NS(id, "n1"), "Sample Size", value=6, min=6, max=20, step=2),
    sliderInput(NS(id, "mean1"), "Population Mean", value=1, min=1, max=5, step=1),
    sliderInput(NS(id, "sd1"), "Population SD", value=0.3, min=0.1, max=0.5, step=0.1),
    br(),
    "Sample 2",
    sliderInput(NS(id, "n2"), "Sample Size", value=6, min=3, max=20, step=1),
    sliderInput(NS(id, "mean2"), "Population Mean", value=1, min=1, max=5, step=1),
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






# Later will add a plot, descriptive stats, choice of plots, choice of distribution, etc, test
  #for equal variance, etc.

# Add checkboxes to display: 1) raw data in a table, 2) summary data in a table, 3) boxplots (with
  #significance star if p< .05)


