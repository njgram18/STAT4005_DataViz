3.3.6 Exercise 1:
library(tidyverse)

library(shiny)

ui <- fluidPage(
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server1 <- function(input, output, server) {
  output$greeting <- renderText(paste0("Hello ", input$name))
}

server2 <- function(input, output, server) {
  string <- reactive(paste0("Hello ", input$name))
  output$greeting <- renderText(string())
}

server3 <- function(input, output, server) {
  output$greeting <- paste0("Hello", input$name)
}


3.3.6 Exercise 2:
  
server1 <- function(input, output, session) {
    c <- reactive(input$a + input$b)
    e <- reactive(c() + input$d)
    output$f <- renderText(e())
}
reactive graph:
c + e --> f

server2 <- function(input, output, session) {
  x <- reactive(input$x1 + input$x2 + input$x3)
  y <- reactive(input$y1 + input$y2)
  output$z <- renderText(x() / y())
}
reactive graph:
x(x1, x2, x3) + y(y1, y2) --> z

server3 <- function(input, output, session) {
  d <- reactive(c() ^ input$d)
  a <- reactive(input$a * 10)
  c <- reactive(b() / input$c) 
  b <- reactive(a() + input$b)
}

reactive graph:
  c() --> ^input$d
a --> a *10
c --> b()/c
b --> a() + b
