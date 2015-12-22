################ Phaethon Lifetime Prediction Explorer #########################
# Author : Richard Podkolinski
# Email  : devlar@gmail.com
# Purpose: Allows a user to explore lifetime data through plotting
################################################################################

library(dplyr)
library(ggplot2)
library(shiny)

# Dummy Data
load("Data/lifetime_dummy.RData")
df = tbl_df(df)

ui = fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Population Tools
      h4("Population Tools"),
      
      br(),
      # Individual Tools
      h4("Individual Lifetime Tools"),
      selectInput("id_input", "ID Select", 
                  choices = c(NA, unique(df$id))
                  )#/selectInput
      
    ), #/sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Tab 1",
          "next"
                 ),#/tabPanel
        tabPanel(
          "Tab 2",
          tableOutput("lifetime_descriptives"),
          plotOutput("energy_ts")
                 )#/tabPanel
      )#/tabsetPanel
    )#/mainPanel
  )#/sidebarLayout
)#/fluidPage

server = function(input, output, session) {
  
  ## Reactive Data Processing ##
  
  individual_select = reactive({
    output = df %>% filter(id == as.numeric(input$id_input))
    return(output)
  })
  
  
  ## Plots ##
  
  output$energy_ts = renderPlot({
    if(is.null(individual_select())) return()
    individual_select() %>% 
      ggplot(aes(x=epoch, y=energy)) +
      geom_line() + geom_vline(xintercept = last(individual_select()$censor_time))
  })
  
  ## Tables ##
  output$lifetime_descriptives = renderTable({
    individual_select() %>% slice(max(.$epoch))
  })
  
  
}

shinyApp(ui=ui, server=server)