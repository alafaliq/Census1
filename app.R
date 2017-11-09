library(shiny)
library(ggplot2)
library(dplyr)
# user interface

ui = fluidPage(
  titlePanel("USA Census Visualization", windowTitle = "Diamonds"),
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with information from the 2010 census"),
      
#      textInput(inputId = "title", 
#                      label = "Chart Title",
#                      value = ""),
      selectInput(inputId = "var", 
                        label = "Choose a variable",
                        choices = list("Percent White", "Percent Black", "Percent Hispanic", "Percent Asian"),
                        selected = "Percent White")
      
    ),
    mainPanel(
      textOutput(outputId = "selected_var"),
      plotOutput(outputId = "map")
      
    )
  )
)


# Server

server = function(input, output){

## code for text
#output$selected_var =  renderText(
#  paste("You have selected", input$var))

## code for plot
output$map  = renderPlot({

counties = reactive({
race = readRDS("data/counties.rds")

counties_map = map_data("county")

counties_map = counties_map %>%
  mutate(name = paste(region, subregion, sep = ","))

left_join(counties_map, race, by = "name") # by = c("name1", "name2) possible two different names.
})

myrace = switch(input$var,
                "Percent White"= counties()$white,
                "Percent Black" = counties()$black,
                "Percent Hispanic" = counties()$hispanic,
                "Percent Asian" = counties()$asian)

 
  ggplot(counties(), aes(x = long, 
                       y = lat,
                       group = group,
                       fill = myrace)) +
    geom_polygon() +
    scale_fill_gradient(low = "white", high = "darkred") +
    theme_void()
  
})





}


# run the app
shinyApp(ui, server)