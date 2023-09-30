#Example apps using modules
#Created by Keith Post on 9/24/23

#load packages
library(shiny)
library(tidyverse)
library(DT)
library(rstatix)

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


## Function----------
make_boxplot <- function(samp1, samp2, n1, n2) {
  names(samp1) <- rep("sample 1", n1)
  names(samp2) <- rep("sample 2", n2)
  
  enframe(c(samp1, samp2)) %>%
    ggplot() +
    geom_boxplot(aes(x=name, y=value, color=name)) +
    scale_color_viridis_d(end=0.6, guide="none") +
    theme_bw(base_size=16)
}



## UI----------
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



## Server----------
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
  #UI: 
    #tabbed outputs--statistical output, boxplot, summary stats
    #conditionally displays * on boxplot (if significant) and accompanying caption
    #made ttest output sig cell bold and highlighted in yellow
  #Server: 
    #use {rstatix} to conduct t-test
    #using DTs from stat outputs
    #convert samples to reactive df, which is then used to generate outputs via functions
    #customized display of boxplots: size, color, overlaid points
    #test whether variances and equal and returns as text & logic used in running test



## Functions-----------
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
    mutate(across(where(is.double), ~signif(.x, 3))) %>%
    rename(group="name")
}


### Make boxplots
make_boxplot <- function(data, sig) {
  cap <- "The averages of samples 1 are significantly different at \u03b1 = 0.05"
  
  
  data %>%
    ggplot(aes(x=name, y=value)) +
    geom_boxplot(color="black", outlier.shape=NA) +
    geom_jitter(aes(color=name), size=2) +
    {if(sig) annotate(geom="text", label="*", x=1.5, y=.99*max(data[["value"]]), size=12)} +
    scale_color_viridis_d(end=0.6, guide="none") +
    xlab("") +
    {if(sig) labs(caption=cap)} +
    theme_bw(base_size=16) 
}


## Run Levene's test
run_levene_test <- function(data) {
  data %>%
    levene_test(value ~ name, center=mean) %>%
    mutate(sig = p <= 0.05) %>%
    pull(sig) 
}


### Run ttest
run_ttest <- function(data, var_equal) {
  data %>%
    t_test(value ~ name, detailed=TRUE, var.equal=var_equal) %>%
    select(type="alternative", group1, group2, df, t="statistic", conf.low, conf.high, p) %>%
    mutate(sig = p <= 0.05,
           across(where(is.numeric), ~signif(.x, 3)))
}



## UI----------
sampleInput <- function(id) {
  choices_radio <- c("Summary stats"="summ_stats",
                     "Boxplot"="box",
                     "t-test output"="tt_out")
  
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
    br(),
    radioButtons(NS(id, "rad_tab"), "Which output would you like to see?",
                 choices=choices_radio, selected=character(0))
  )
}


ttestPlotOutput <- function(id) {
  tabsetPanel(id="tabs_ttest", type="tabs",
    #blank tab
    # tabPanel("blank"),
    #summ stats panel
    tabPanel("summ_stats",
      h4(strong("Summary stats")),
      DTOutput(NS(id, "stats_table"))
    ),
    #boxplot panel
    tabPanel("box",
      fluidRow(
        h4(strong("Boxplots of samples 1 and 2")),
        column(8,
          plotOutput(NS(id, "plot"))
        ),
        column(4)
      )
    ),
    #ttest panel
    tabPanel("tt_out",
      h4(strong("t-test output")),
      textOutput(NS(id, "var_status_out")),
      br(),
      DTOutput(NS(id, "ttest_table"))
    )
  )
}



## Server----------
sampleServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    #create reactives from inputs
    samp1 <- reactive(rnorm(input$n1, input$mean1, input$sd1))
    samp2 <- reactive(rnorm(input$n2, input$mean2, input$sd2))
    
    
    #create DF
    reactive(create_df(samp1(), samp2(), input$n1, input$n2))
  })
}


ttestPlotServer <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    #ui
    # observeEvent(sel(), {
    #   updateTabsetPanel(session, "tabs_ttest", selected = sel())
    # })
    
    #equal_var
    equal_var <- reactive(run_levene_test(dat()))
    
    #significant ttest
    tt_sig <- reactive({run_ttest(dat(), var_equal=!equal_var()) %>%
                      pull(sig)})
    
    
    
    #stats table output
    output$stats_table <- renderDT(calc_summ_stats(dat()), rownames=FALSE, options=list(dom="t"))
    
    #plot output
    output$plot <- renderPlot({make_boxplot(dat(), sig=tt_sig())})
    
    
    output$var_status_out <- renderText(paste0("Variances are ", 
                                               if(equal_var()) {"unequal"} else{"equal"},
                                               "."))
    
    #t-test output
    output$ttest_table <- renderDT({datatable(run_ttest(dat(), var_equal=!equal_var()), 
                                              rownames=FALSE, 
                                              options=list(dom="t")) %>% 
                                     formatStyle("sig", fontWeight="bold", backgroundColor="yellow")
                          })
  })
}
  
  
  
### App 
ttestApp <- function() {
  ui <- fluidPage(
    #add more layout: sidebarLayout
    sidebarLayout(
      sidebarPanel(width=3,
        sampleInput("data"),
      ),
      mainPanel(width=9,
        ttestPlotOutput("stats")
      )
    )
  )
  
  server <- function(input, output, session) {
    # x <- sampleServer("data")
    df <- sampleServer("data")
    ttestPlotServer("stats", dat=df)
  }
 
  shinyApp(ui, server)
  
}

ttestApp()


# Add 
  #4) use radio buttons to navigate tabsetPanel (which will be made hidden)
  #5) make boxplot interactive 




