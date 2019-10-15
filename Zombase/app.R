---
output: html_document
runtime: shiny
---

##### load packages ###################################
library(shiny)           # for building the app
library(shinyWidgets)    # for fancy customisation
library(shinydashboard)  # for fancy dashboard
library(leaflet)         # for nice customisation
library(DT)              # for d
library(rlang)           # for lazy evaluation
library(readxl)          # for importing Excel files
library(dplyr)           # for manipulating data
library(ggplot2)         # for visualising data
library(knitr)           # for nice tables

#### parameters #######################################
options(knitr.kable.NA = '-')

#### import data ######################################
data <- read_xlsx("Data/zombies.xlsx") %>% arrange(-Year)

#### user interface ###################################
ui <- fluidPage(
    
    #### define UI ########################################
    includeCSS("Zombase/bootstrap.css"),
    setSliderColor(color = "black", sliderId = 1),
    setBackgroundColor(
        color    = c("#000000", "#591b1b"),
        gradient  = "radial",
        direction = c("bottom", "right")
    ),    
    # application title
    h1("Zombase"),
    br(),
    h2("A world wide databse on zombie-related material across years"),
    br(),
    
    # sidebar slider
    sidebarLayout(
        
        # slider for year with dynamic minimum and maximum
        sidebarPanel(
            sliderInput(inputId   = "year",
                        label     = "Year of publication:",
                        min       = min(data$Year, na.rm = TRUE),
                        max       =  max(data$Year, na.rm = TRUE),
                        step      = 1,
                        animate   = TRUE,
                        dragRange = TRUE,
                        value     = c(min(data$Year, ... = TRUE),
                                      max(data$Year, ... = TRUE))),
            checkboxGroupButtons(
                inputId  = "type",
                label    = "Material type",
                choices  = unique(data$Type),
                selected = unique(data$Type)
            ),
            pickerInput(
                inputId = "country", 
                label = "Country", 
                choices = unique(data$Country),
                selected = "United States",
                options = list(
                    `actions-box` = TRUE, 
                    size = 10,
                    `selected-text-format` = "count > 3"
                ), 
                multiple = TRUE
            )
        ),

        # show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel(title = "Database", dataTableOutput("table")),
                        tabPanel(title = "Type", plotOutput(outputId = "type")),
                        tabPanel(title = "Year", plotOutput(outputId = "year"))
            ),
        ),
    )
)

#### server #####################################################
server <- function(input, output) {

    # table
    output$table <- renderDataTable(expr = {
        
        data %>%
            filter(Year >= input$year[1],
                   Year <= input$year[2],
                   Type %in% input$type,
                   Country %in% input$country) %>%
            datatable(options = list(pageLength = 30),
                      rownames = FALSE,
                      filter = 'top') %>%
            formatCurrency(c("Budget", "Box"), currency = "$") %>%
            formatString("Duration", suffix = " min.") %>%
            formatStyle("Title", color = "black", backgroundColor = "white") %>%
            formatStyle(c("Type", "Year", "Author", "Country", "Budget", "Box", "Duration", "Producer"),
                        color = "white", backgroundColor = "black")
        
    })

    # type
    output$type <- renderPlot({
        
        data %>%
            arrange(input$order) %>%
            filter(Year >= input$year[1],
                   Year <= input$year[2],
                   Type %in% input$type,
                   Country %in% input$country) %>%
            ggplot(., aes(x = Year)) +
            facet_wrap(~input$type) +
            geom_histogram(binwidth = 1, fill = "white") +
            labs(x = "Year", y = "Count") +
            theme(panel.grid        = element_line(colour = 'grey30', linetype = "dotted"),
                  panel.background  = element_rect(fill = 'black', colour = 'white', size = 0.5),
                  plot.background   = element_rect(fill = 'black'),
                  text              = element_text(colour = 'white', size = 15),
                  axis.text         = element_text(size = 15, colour = 'white'),
                  legend.key        = element_rect(fill = 'black', colour = 'white'),
                  legend.background = element_rect(fill = 'white'))
    })
    
    # year
    output$year <- renderPlot({
        
        data %>%
            filter(Year >= input$year[1],
                   Year <= input$year[2],
                   Type %in% input$type,
                   Country %in% input$country
            ) %>%
            ggplot(., aes(x = Year, y = Country)) +
            geom_point(colour = "white") +
            labs(x = "Year", y = "Country") +
            theme(panel.grid        = element_line(colour = 'grey30', linetype = "dotted"),
                  panel.background  = element_rect(fill = 'black', colour = 'white', size = 0.5),
                  plot.background   = element_rect(fill = 'black'),
                  text              = element_text(colour = 'white', size = 15),
                  axis.text         = element_text(size = 15, colour = 'white'),
                  legend.key        = element_rect(fill = 'black', colour = 'white'),
                  legend.background = element_rect(fill = 'white'))
    })
    
    }
    
    


#### run ######################################################
shinyApp(ui = ui, server = server)
