library(tidyverse)
library(readxl)
df <- read_excel("data/slu_graduates_17_21.xlsx")

df
## fixes error in the data
df <- df %>% mutate(across(everything(),
                           .fns = ~replace(., . ==  "STATS" , "STAT")))

df_long <- df %>% pivot_longer(3:8, names_to = "type", values_to = "discipline")
df_major <- df_long %>% 
  filter(type == "major1" | type == "major2" | type == "major3")

df_stat <- df_major %>% filter(discipline == "STAT") 
df_statfull <- semi_join(df_long, df_stat, by = "adm_id") %>%
  filter(type == "major1" |
           type == "major2" | 
           type == "major3")

df_nostat <- df_statfull %>% filter(discipline != "STAT" &
                                      !is.na(discipline)) %>%
  group_by(discipline) %>%
  summarise(nstudent = n()) %>%
  mutate(discipline = fct_reorder(discipline, nstudent))
majorplot <- ggplot(data = df_nostat, aes(x = discipline, y = nstudent)) +
  geom_col() +
  coord_flip()

df$major1
var_choices <- names(df)[3:7]

library(shiny)

df_nomiss <- df_major %>% filter(!is.na(discipline)) %>%
  mutate(discipline = factor(discipline))
majors <- levels(df_nomiss$discipline)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(radioButtons("majorchoice", label = "Choose a Major",
                             choices = list("STAT", "MATH", "CS"))),
    mainPanel(plotOutput("m_plot"),
              plotOutput("gender_plot"))
  )
)

server <- function(input, output, session) {
  
  
  major_reactive_df <- reactive({
    
    df_stat <- df_major %>% filter(discipline == input$majorchoice) 
    df_statfull <- semi_join(df_long, df_stat, by = "adm_id") %>%
      filter(type == "major1" |
               type == "major2" | 
               type == "major3")
    
    df_nostat <- df_statfull %>% filter(discipline != input$majorchoice &
                                          !is.na(discipline)) %>%
      group_by(discipline) %>%
      summarise(nstudent = n()) %>%
      mutate(discipline = fct_reorder(discipline, nstudent))
  })
  
  gender_reactive_df <- reactive({
    
    df_stat <- df_major %>% filter(discipline == input$majorchoice) 
    df_statfull <- semi_join(df_long, df_stat, by = "adm_id") %>%
      filter(type == "major1" |
               type == "major2" | 
               type == "major3")
    
    df_nostat <- df_statfull %>% filter(discipline != input$majorchoice &
                                          !is.na(discipline)) %>%
      group_by(discipline, sex) %>%
      summarise(nstudent = n()) %>%
      mutate(discipline = fct_reorder(discipline, nstudent))
  })
  
  major_plot <- reactive({
    ggplot(data = major_reactive_df(), aes(x = discipline, y = nstudent)) +
      geom_col() +
      coord_flip()
  })
  major_plot_gender <- reactive({
    ggplot(data = gender_reactive_df(), aes(x = sex, y = nstudent)) +
      geom_col()
  })
  
  output$m_plot <- renderPlot({
    major_plot()
  })
  output$gender_plot <- renderPlot({
    major_plot_gender()
  })
}

shinyApp(ui, server)

