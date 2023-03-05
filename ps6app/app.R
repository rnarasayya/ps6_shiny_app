#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

data <- read_delim("./Checkouts_by_Title.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Seattle Public Library Checkouts"),
    tabsetPanel(
      tabPanel("About", p("This dataset includes a", em("monthly"),"count of Seattle Public Library checkouts by title for physical and electronic items."),
               p("The dataset begins with checkouts that occurred in April 2005."),
               p("We have", strong(nrow(data)), "rows of data and", strong(ncol(data)), "columns."),
               p("Here are the first few rows of the dataset."),
               mainPanel(tableOutput("headData"))),
      tabPanel(
        "Plots", p("This is a scatterplot of the three most popular types of media."),
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("type",
              "Which material type do you want?",
              choices = list("BOOK", "VIDEODISC", "EBOOK"),
              selected = list("BOOK", "VIDEODISC", "EBOOK")
            ),
            checkboxInput("display", "Display line", FALSE)
          ),
          mainPanel(plotOutput("popular"))
        )
      ),
      tabPanel(
        "Tables", p("This table shows total checkouts either by month or year."),
        sidebarLayout(
          sidebarPanel(
            radioButtons("time",
              "Do you want to see total checkouts by month or year?",
              choices = list("CheckoutMonth", "CheckoutYear")
            )
          ),
          mainPanel(tableOutput("totals"), textOutput("tablemessage"))
        )
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$headData <- renderTable({head(data)})
    
    output$popular <- renderPlot({
      if(input$display) {
        data %>% filter(MaterialType %in% input$type, CheckoutYear != 2023) %>%
        group_by(CheckoutYear, MaterialType) %>%
        summarize(totalCheckouts = sum(Checkouts)) %>%
        ggplot(aes(x=CheckoutYear, y=totalCheckouts)) +
        geom_point(aes(color=MaterialType)) +
        geom_line(aes(color=MaterialType)) +
        labs(x = "Year", y = "Total Checkouts")
      }
      else {
        data %>% filter(MaterialType %in% input$type, CheckoutYear != 2023) %>%
          group_by(CheckoutYear, MaterialType) %>%
          summarize(totalCheckouts = sum(Checkouts)) %>%
          ggplot(aes(x=CheckoutYear, y=totalCheckouts)) +
          geom_point(aes(color=MaterialType)) +
          labs(x = "Year", y = "Total Checkouts")
      }
    })
    
    output$totals <- renderTable({
      if(input$time == "CheckoutMonth") {
        df <- data %>% group_by(CheckoutMonth) %>%
        summarize(totalCheckouts = sum(Checkouts)) %>%
        arrange(desc(totalCheckouts))
      }
      else {
        df <- data %>% group_by(CheckoutYear) %>%
          summarize(totalCheckouts = sum(Checkouts)) %>%
          arrange(desc(totalCheckouts))
      }
    })
    
    output$tablemessage <- renderText({
      if(input$time == "CheckoutMonth") {
        df <- data %>% group_by(CheckoutMonth) %>%
          summarize(totalCheckouts = sum(Checkouts)) %>%
          arrange(desc(totalCheckouts))
        paste("We have ", nrow(df), " rows in this table.")
      }
      else {
        df <- data %>% group_by(CheckoutYear) %>%
          summarize(totalCheckouts = sum(Checkouts)) %>%
          arrange(desc(totalCheckouts))
        paste("We have ", nrow(df), " rows in this table.")
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
