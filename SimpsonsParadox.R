### Shiny App for teaching the Simpsons Paradox
### Author: Niklas Jung
### Date 01/10/2024
library(shiny)
library(dplyr)
library(ggplot2)

# Define the UI for the application
ui <- fluidPage(
  
  titlePanel("Simpson's Paradox: School Test Scores"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("viewType", "View:", 
                  choices = c("Grouped View (Schools A and B)", "Aggregated View")),
      helpText("This example shows two groups: School A (well-funded) and School B (less-funded). 
               School A students study fewer hours but perform better overall, 
               while School B students study more hours but score lower. 
               The combined data reverses the trend, illustrating Simpson's Paradox.")
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      p("In the grouped view, students in both schools perform better as they study more. 
        However, in the aggregated view, it seems like studying more leads to lower scores
        when both groups are combined, illustrating Simpson's Paradox!")
    )
  )
)

# Define the server logic for the application
server <- function(input, output) {
  
  # Create a reactive function to return the appropriate data
  dataset <- reactive({
    
    set.seed(72)  # Set a seed for reproducibility
    
    # Create data for School A (well-funded school, students study less but perform better)
    school_A <- data.frame(
      StudyHours = rnorm(50, mean = 2, sd = 1),   # Fewer hours of study
      TestScore = rnorm(50, mean = 75, sd = 5) + rnorm(50, mean = 5 * rnorm(50, mean = 2, sd = 1), sd = 2),
      School = "School A"
    )
    school_A$StudyHours <- pmax(0, school_A$StudyHours)  # Ensure no negative study hours
    
    # Create data for School B (less-funded school, students study more but perform worse)
    school_B <- data.frame(
      StudyHours = rnorm(50, mean = 6, sd = 1.5),  # More hours of study
      TestScore = rnorm(50, mean = 60, sd = 5) + rnorm(50, mean = 2 * rnorm(50, mean = 6, sd = 1.5), sd = 2),
      School = "School B"
    )
    school_B$StudyHours <- pmax(0, school_B$StudyHours)  # Ensure no negative study hours
    
    # Combine both datasets into one
    data <- rbind(school_A, school_B)
    
    return(data)
  })
  
  # Create the plot output based on the selected view
  output$scatterPlot <- renderPlot({
    
    data <- dataset()
    
    # Plot based on the selected view
    if (input$viewType == "Grouped View (Schools A and B)") {
      ggplot(data, aes(x = StudyHours, y = TestScore, color = School)) +
        geom_point(size = 3) +
        geom_smooth(method = "lm", se = FALSE) +
        theme_minimal() +
        labs(title = "Grouped View (Schools A and B)", 
             subtitle = "Both schools show improvement with more study hours", 
             x = "Study Hours", 
             y = "Test Score")
    } else {
      ggplot(data, aes(x = StudyHours, y = TestScore)) +
        geom_point(size = 3, aes(color = School)) +
        geom_smooth(method = "lm", se = FALSE) +
        theme_minimal() +
        labs(title = "Aggregated View", 
             subtitle = "Reversed trend when data is combined (Simpson's Paradox)", 
             x = "Study Hours", 
             y = "Test Score")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


### Generative AI was used to support this project. Source: https://chatgpt.com
