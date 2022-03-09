library(tidyverse)

alcohol_df

library(shiny)

var_choices <- names(alcohol_df)[2:4]

ui <- fluidPage(
  sidebarLayout(sidebarPanel(
    selectizeInput("countrychoice",
                   label = "Choose a Country", choices = levels(factor(df$country)),
                   selected = "Australia"),
    radioButtons("varchoice", label = "Choose a Statistic",
                 choices = var_choices),
    sliderInput("binnumber", label = "Choose a Number of Bins", 
                min = 1, max = 50, value = 15, step = 1)),
    mainPanel(plotOutput("histgraph"),
              tableOutput("wintab"))
  )
)

server <- function(input, output, session) {
  
  df_sub <- reactive({
    df %>% filter(country == input$countrychoice)
  })
  
  hist_plot <- reactive({
    # ggplot(df_sub(), aes_string(x = input$varchoice)) +
    # geom_histogram(colour = "black", fill = "white", bins = 15)
    base_plot <-ggplot(df_sub, aes(x = .data[[input$varchoice]])) +
      geom_point() +
      geom_label_repel(data = onecountry_df, aes(label = country)) +
      geom_point(data = onecountry_df, size = 3, shape = 1) 
    
    if (is.numeric(df_sub()[[input$varchoice]]) == TRUE) {
      
      base_plot + geom_histogram(colour = "black", fill = "white",
                                 bins = input$binnumber) +
        theme_minimal(base_size = 22)
    } else if (is.character(df_sub()[[input$varchoice]])) {
      base_plot + geom_bar(colour = "black", fill = "white") +
        theme_minimal(base_size = 22) +
        coord_flip()
    }
  })
  
  output$histgraph <- renderPlot({
    hist_plot()
  })
  
  
  output$wintab <- renderTable({
    table(df_sub()$result)
  })
  
  
  
}

shinyApp(ui, server)

