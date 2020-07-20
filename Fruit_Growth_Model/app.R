# Fruit Growth Model: Shiny App
# author: Jimmy Larson
# created: 7.15.20
# last edited: 7.15.20
## load packages
library(shiny)
library(tidyverse)
## define UI for app----
ui <- fluidPage(
    # App Title
    titlePanel("Fruit Growth Model"),
    # sidebar layout with input and output definitions
    sidebarLayout(
        # sidebar panel for inputs
        sidebarPanel(
            ## download data template----
            downloadButton("downloadtemplate", "Download Measurement Template"),
            tags$hr(),
            ## input: file----
            fileInput("file1", "Choose CSV File:",
                      multiple = TRUE,
                      accept = c("csv", "comma-separated-values", ".csv" )
        ),
            checkboxInput("header", "Header", TRUE),
        ## input target crop load----
        tags$hr(),
            numericInput("target", "Target Fruit Set:", 0, min = 1, max = 100),
        ## measurement date inputs----
        tags$hr(),
            dateInput("meas1", "First Measurement Date:", format = "M/d/yyyy"),
            dateInput("meas2", "Second Measurement Date:", format = "M/d/yyyy"),
            dateInput("meas3", "Third Measurement Date:", format = "M/d/yyyy"),
            dateInput("meas4", "Fourth Measurement Date:", format = "M/d/yyyy"),
            dateInput("meas5", "Fifth Measurement Date:", format = "M/d/yyyy"),
            dateInput("meas6", "Sixth Measurement Date:", format = "M/d/yyyy"),
            dateInput("meas7", "Seventh Measurement Date:", format = "M/d/yyyy"),
        ## chemical thinner inputs----
        tags$hr(),
            dateInput("date", "Date of Thinner Application:", format = "M/d/yyyy"),
            textInput("thinner", "Chemical Thinner Applied:"),
            textInput("rate", "Rate Applied:")
    ),
    ## outputs----
    mainPanel(
        plotOutput("scatterplot"),
        tableOutput("contents")
        )
    )
)
## server----
server <- function(input, output) {
    ## reactive inputs----
    target_cropload <- reactive({input$target})
    meas1 <- reactive({input$meas1})
    meas2 <- reactive({input$meas2})
    meas3 <- reactive({input$meas3})
    meas4 <- reactive({input$meas4})
    meas5 <- reactive({input$meas5})
    meas6 <- reactive({input$meas6})
    meas7 <- reactive({input$meas7})
    
     ## render plot----
    output$scatterplot <- renderPlot({
        req(input$file1)
        
        ## load in dataframe----
        df <- read.csv(input$file1$datapath,
                       header = input$header)
        ## calculate growth rates----
        df %>%
            mutate(rate.1 = diam.2 - diam.1,
                   rate.2 = diam.3 - diam.2,
                   rate.3 = diam.4 - diam.3,
                   rate.4 = diam.5 - diam.4,
                   rate.5 = diam.6 - diam.5,
                   rate.6 = diam.7 - diam.6) -> df
        ## select 20 fastest growing fruit----
        df %>%
            arrange(desc(rate.1)) %>%
            head(20) -> date.2.fastest
        rate.1.fastest <- mean(date.2.fastest$rate.1)
        
        df %>%
            arrange(desc(rate.2)) %>%
            head(20) -> date.3.fastest
        rate.2.fastest <- mean(date.3.fastest$rate.2)
        
        df %>%
            arrange(desc(rate.3)) %>%
            head(20) -> date.4.fastest
        rate.3.fastest <- mean(date.4.fastest$rate.3)
        
        df %>%
            arrange(desc(rate.4)) %>%
            head(20) -> date.5.fastest
        rate.4.fastest <- mean(date.5.fastest$rate.4)
        
        df %>%
            arrange(desc(rate.5)) %>%
            head(20) -> date.6.fastest
        rate.5.fastest <- mean(date.6.fastest$rate.5)
        
        df %>%
            arrange(desc(rate.6)) %>%
            head(20) -> date.7.fastest
        rate.6.fastest <- mean(date.7.fastest$rate.6)
        
        ## determine cutoff point for persisting----
        rate.1.cutoff <- rate.1.fastest / 2
        rate.2.cutoff <- rate.2.fastest / 2
        rate.3.cutoff <- rate.3.fastest / 2
        rate.4.cutoff <- rate.4.fastest / 2
        rate.5.cutoff <- rate.5.fastest / 2
        rate.6.cutoff <- rate.6.fastest / 2
        ## determine which fruit will persist----
        df %>%
            mutate(date.2.outcome = case_when(rate.1 >= rate.1.cutoff ~ "persist",
                                              rate.1 < rate.1.cutoff ~ "abscise"),
                   date.3.outcome = case_when(rate.2 >= rate.2.cutoff ~ "persist",
                                              rate.2 < rate.2.cutoff ~ "abscise"),
                   date.4.outcome = case_when(rate.3 >= rate.3.cutoff ~ "persist",
                                              rate.3 < rate.3.cutoff ~ "abscise"),
                   date.5.outcome = case_when(rate.4 >= rate.4.cutoff ~ "persist",
                                              rate.4 < rate.4.cutoff ~ "abscise"),
                   date.6.outcome = case_when(rate.5 >= rate.5.cutoff ~ "persist",
                                              rate.5 < rate.5.cutoff ~ "abscise"),
                   date.7.outcome = case_when(rate.6 >= rate.6.cutoff ~ "persist",
                                              rate.6 < rate.6.cutoff ~ "abscise")) -> diam_data.outcome
        ## longform data----
        diam_data.outcome %>%
            select(cluster, fruit, rate.1:rate.6) %>%
            gather('rate.1', 'rate.2', 'rate.3', 'rate.4', 'rate.5', 'rate.6', key = "rate.date", value = "growth.rate") -> growth.rates
        diam_data.outcome %>%
            select(cluster, fruit, date.2.outcome:date.7.outcome) %>%
            gather(date.2.outcome:date.7.outcome, key = "date", value = "outcome") -> abscission.outcome
        growth.rates$outcome <- abscission.outcome$outcome
        ## plot data ----
        facet_labels <- c(paste("After", meas2()), paste("After", meas3()), paste("After", meas4()), paste("After", meas5()),
                          paste("After", meas6()), paste("After", meas7()))
        names(facet_labels) <- c("rate.1", "rate.2", "rate.3", "rate.4", "rate.5", "rate.6")
        growth.rates %>%
            drop_na() %>%
            ggplot(aes(x = growth.rate, y = outcome, color = outcome)) +
            geom_point(alpha = 0.5, size = 3) + 
            facet_wrap( ~ rate.date, labeller = labeller(rate.date = facet_labels)) +
            scale_color_brewer(palette = "Dark2") +
            labs(x = "Growth Rate (Current - Previous Diameter)",
                 y = "Persist or Abscise?",
                 color = "Outcome") +
            theme_bw()
    })
    ## render table----
    output$contents <- renderTable({
        req(input$file1)
        ## load in dataframe----
        df <- read.csv(input$file1$datapath,
                       header = input$header)
        ## calculate growth rates----
        df %>%
            mutate(rate.1 = diam.2 - diam.1,
                   rate.2 = diam.3 - diam.2,
                   rate.3 = diam.4 - diam.3,
                   rate.4 = diam.5 - diam.4,
                   rate.5 = diam.6 - diam.5,
                   rate.6 = diam.7 - diam.6) -> df
        ## select 20 fastest growing fruit----
        df %>%
            arrange(desc(rate.1)) %>%
            head(20) -> date.2.fastest
        rate.1.fastest <- mean(date.2.fastest$rate.1)
        
        df %>%
            arrange(desc(rate.2)) %>%
            head(20) -> date.3.fastest
        rate.2.fastest <- mean(date.3.fastest$rate.2)
        
        df %>%
            arrange(desc(rate.3)) %>%
            head(20) -> date.4.fastest
        rate.3.fastest <- mean(date.4.fastest$rate.3)
        
        df %>%
            arrange(desc(rate.4)) %>%
            head(20) -> date.5.fastest
        rate.4.fastest <- mean(date.5.fastest$rate.4)
        
        df %>%
            arrange(desc(rate.5)) %>%
            head(20) -> date.6.fastest
        rate.5.fastest <- mean(date.6.fastest$rate.5)
        
        df %>%
            arrange(desc(rate.6)) %>%
            head(20) -> date.7.fastest
        rate.6.fastest <- mean(date.7.fastest$rate.6)
        
        ## determine cutoff point for persisting----
        rate.1.cutoff <- rate.1.fastest / 2
        rate.2.cutoff <- rate.2.fastest / 2
        rate.3.cutoff <- rate.3.fastest / 2
        rate.4.cutoff <- rate.4.fastest / 2
        rate.5.cutoff <- rate.5.fastest / 2
        rate.6.cutoff <- rate.6.fastest / 2
        ## determine which fruit will persist----
        df %>%
            mutate(date.2.outcome = case_when(rate.1 >= rate.1.cutoff ~ "persist",
                                              rate.1 < rate.1.cutoff ~ "abscise"),
                   date.3.outcome = case_when(rate.2 >= rate.2.cutoff ~ "persist",
                                              rate.2 < rate.2.cutoff ~ "abscise"),
                   date.4.outcome = case_when(rate.3 >= rate.3.cutoff ~ "persist",
                                              rate.3 < rate.3.cutoff ~ "abscise"),
                   date.5.outcome = case_when(rate.4 >= rate.4.cutoff ~ "persist",
                                              rate.4 < rate.4.cutoff ~ "abscise"),
                   date.6.outcome = case_when(rate.5 >= rate.5.cutoff ~ "persist",
                                              rate.5 < rate.5.cutoff ~ "abscise"),
                   date.7.outcome = case_when(rate.6 >= rate.6.cutoff ~ "persist",
                                              rate.6 < rate.6.cutoff ~ "abscise")) -> diam_data.outcome
        ## longform data----
        diam_data.outcome %>%
            select(cluster, fruit, rate.1:rate.6) %>%
            gather('rate.1', 'rate.2', 'rate.3', 'rate.4', 'rate.5', 'rate.6', key = "rate.date", value = "growth.rate") -> growth.rates
        diam_data.outcome %>%
            select(cluster, fruit, date.2.outcome:date.7.outcome) %>%
            gather(date.2.outcome:date.7.outcome, key = "date", value = "outcome") -> abscission.outcome
        growth.rates$outcome <- abscission.outcome$outcome
        
        ## create table----
        growth.rates %>%
            drop_na() %>%
            mutate(outcome = case_when(
                outcome ==  "abscise" ~ "Abscise",
                outcome ==  "persist" ~ "Persist"),
                rate.date = case_when(
                    rate.date == "rate.1" ~ paste(meas2()),
                    rate.date == "rate.2" ~ paste(meas3()),
                    rate.date == "rate.3" ~ paste(meas4()),
                    rate.date == "rate.4" ~ paste(meas5()),
                    rate.date == "rate.5" ~ paste(meas6()),
                    rate.date == "rate.6" ~ paste(meas7())
                )) %>%
            group_by(rate.date, outcome) %>%
            count() %>%
            mutate(percent_drop = n/511 * 100,
                   percent_drop = round(percent_drop,0),
                   target_set = target_cropload(),
                   ) %>%
            #filter(outcome == "persist") %>%
            select(rate.date, n, percent_drop, target_set) %>%
            rename("Predicted Outcome" = outcome,
                   "Measurement Date" = rate.date,
                   "Number of Fruit" = n,
                   "Percent of Fruit Measured" = percent_drop,
                   "Target Number Fruit Set" = target_set)
    })
    ## download template----
    output$downloadtemplate <- downloadHandler(
        filename <- function(){
            "fgm_data_template.csv"
        },
        content <- function(file){
            file.copy("fgm_data_template.csv", file)
        }
    )
}

# Run the App----
shinyApp(ui, server)