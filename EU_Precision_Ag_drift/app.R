#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
source("functions.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Precision Ag Drift refinement"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Spray Setup"),
      shiny::radioButtons(
        inputId = "field_option",
        choices = c("Full Field", "Single Band", "Multi Band", "Regular Spot"),
        label = "Choose a field option",
        selected = 3
      ),
      h3("Band Info"),
      fluidRow(
        column(
          width = 6,
          shiny::numericInput(
            inputId = "band_width",
            label = "Band width (Spray) [m]",
            value = 1,
            min = 0,
            max = 5
          )
        ),
        column(
          width = 6,
          shiny::numericInput(
            inputId = "inter_band_width",
            label = "Inter band width (Spray) [m]",
            min = 0,
            value = 1,
            max = 5
          )
        )
      ),
      h3("Field Info"),
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = "crop",
            label = "Crop",
            choices = c(
              "cereals (not maize)",
              "citrus",
              "Cotton",
              "Field beans",
              "hops",
              "Legumes",
              "maize",
              "oil seed rape",
              "olives",
              "pome/stone fruit, early applns",
              "pome/stone fruit, late applns",
              "potatoes",
              "soybean",
              "sugar beet",
              "sunflower",
              "tobacco",
              "vegetables",
              "vines, early applns",
              "vines, late applns"
            ),
            selected = 1
          )
        ),
        column(
          width = 6,
          selectInput(
            inputId = "water_body",
            label = "Water Body",
            choices = c("Ditch", "Stream", "Pond"),
            selected = 1
          )
        )
      ),
      h3("Mitigation info"),
      selectInput(
        inputId = "buffer_m",
        label = "Add a spray buffer",
        choices = c("No buffer (Step 3)", "5 m", "10 m"),
        selected = 1
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("field_plot"),
      verbatimTextOutput("drift_stats")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$field_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    plot_field()
  })
  output$drift_stats <- renderText({
    paste0(c(
      "Mean Drift deposition = 1.9 % of application rate\nPercent Reduction from Full Field application: 45 % reduction"
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
