library(shiny)
library(ggplot2)
library(GGally)
library(dplyr)


data = read.csv("CaseStudy2-data.csv")

# Define the Shiny app UI
ui = fluidPage(
  titlePanel("Attrition Relationships"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "variables",
        "Select Variables:",
        choices = colnames(data),
        selected = colnames(data)[c(2, 4, 5, 6, 7)],
        multiple = TRUE
      )
    ),
    
    mainPanel(
      plotOutput("scatterPlotMatrix")
    )
  )
)

# Define the Shiny app server
server = function(input, output) {
  output$scatterPlotMatrix = renderPlot({
    # Your data should be in a variable called 'data'
    
    # Remove unnecessary columns for better visualization
    data_selected = data %>%
      dplyr::select(Attrition, dplyr::all_of(input$variables))
    
    # Filter out non-numeric columns for scatter plot matrix compatibility
    data_numeric = data_selected %>%
      dplyr::select_if(is.numeric)
    
    # Create an interaction factor for color aesthetics
    data_numeric$Attrition = data_selected$Attrition
    
    # Plot the scatter plot matrix
    plot = ggpairs(data_numeric, 
                    mapping = ggplot2::aes(color = Attrition), 
                    upper = list(continuous = wrap("points", alpha = 0.3)),
                    lower = list(continuous = wrap("smooth", method = "lm", se = FALSE, fullrange = TRUE))
    )
    
    print(plot)
  })
}



# Run the Shiny app
shinyApp(ui = ui, server = server)

