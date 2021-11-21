library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(rsconnect)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

#Could show distribution of cocktails by number of ingredients
#Show distribution of each category
#Selectize
#Checkbox?


boston_names <- boston_cocktails %>% distinct(name, category)
boston_ingredients <- aggregate(.~boston_cocktails$name, data=boston_cocktails, FUN=length)

# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Boston Cocktails and their Ingredients"),
    
    # Sidebar 
    sidebarPanel(
        selectInput("select",
                    label="Visualization or Ingredient Search:",
                    choices= c("Visualization", "Search Ingredients", "Search Cocktails")),
        
        conditionalPanel(condition= "input.select == 'Visualization'",
                         selectInput("select_vis",
                                     label="Choose your visualization:",
                                     choices= c("Categories", "Total Ingredients"))),
        
        conditionalPanel(condition= "input.select == 'Search Ingredients'",
                         selectizeInput('select_ing',
                                        label='Search for Cocktail by Ingredient:',
                                        choices= boston_cocktails$ingredient,
                                        multiple= TRUE)),
        
        conditionalPanel(condition= "input.select == 'Search Cocktails'",
                         selectizeInput('select_cock',
                                        label='Search for Cocktails:',
                                        choices= boston_cocktails$name,
                                        multiple= TRUE))
        
    ),
    
    # Show a plot
    mainPanel(
        conditionalPanel(
            condition = "input.select == 'Visualization'",
            plotOutput("distPlot")
        ),
        
        conditionalPanel(
            condition = "input.select == 'Search Ingredients'",
            tableOutput("tablePlot")
        ),
        
        conditionalPanel(
            condition = "input.select == 'Search Cocktails'",
            tableOutput("tablePlot2")
        )
        
        
        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        if(input$select == "Visualization"){
            if(input$select_vis == "Categories"){
                dist <- ggplot(data=boston_names, aes(x=category)) + geom_bar(stat="count", fill="Red", color='black') + theme(axis.text.x = element_text(angle= 25, hjust=1, size=12)) + labs(x='Category', y='Count')}
            else{
                dist <- ggplot(data=boston_ingredients, aes(x=name)) + geom_bar(stat="count", fill="Green", color='black') + labs(x='# of Ingredients', y='Count') + theme(axis.line.x.bottom= element_line(color='black', size=0.5))}
            print(dist)
            
        }
    }) #This bracket closes first if statement
    output$tablePlot <- renderTable({
        if(input$select == "Search Ingredients"){
            tab <- boston_cocktails %>% filter(boston_cocktails$ingredient %in% input$select_ing) %>% select(-ingredient_number) %>% select(-row_id)}
        print(tab)
    })
    
    output$tablePlot2 <- renderTable({
        if(input$select == "Search Cocktails"){
            tab <- boston_cocktails %>% filter(boston_cocktails$name %in% input$select_cock) %>% select(-ingredient_number) %>% select(-row_id)}
        print(tab)
    })
}
#This bracket closes the server argument


# Run the application 
shinyApp(ui, server)
