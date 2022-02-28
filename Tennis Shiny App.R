library(tidyverse)

df


library(shiny)

var_choices <- names(df)[3:7]

library(shiny)

ui <- fluidPage(
  sidebarLayout(sidebarPanel(
    selectizeInput("playerchoice",
                   label = "Choose a Player", choices = levels(factor(df$player)),
                   selected = "Aryna Sabalenka"),
    radioButtons("varchoice", label = "Choose a Statistic",
                 choices = var_choices),
    sliderInput("binnumber", "Choose a Number of Bins", 
                min = 0, max = 50, value = 15, step = 5,
                animate = animationOptions(interval = 500, loop = TRUE))),
    mainPanel(plotOutput("histgraph"))
  )
)

server <- function(input, output, session) {
  
  df_sub <- reactive({
    df %>% filter(player == input$playerchoice)
  })
  
  hist_plot <- reactive({
    # ggplot(df_sub(), aes_string(x = input$varchoice)) +
    # geom_histogram(colour = "black", fill = "white", bins = 15)
    ggplot(df_sub(), aes(x = .data[[input$varchoice]])) +
      geom_histogram(colour = "black", fill = "white", bins = input$binnumber) +
      theme_grey(base_size = 22)
  })
  
  output$histgraph <- renderPlot({
    hist_plot()
  })
  
  
}

shinyApp(ui, server)

