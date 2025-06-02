#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(readr)
library(dplyr)
library(tibble)
library(magrittr)
source("drift_calc_functions.R")
source("plot_field.R")

# FOCUS SW Appendix B drift parameters
default_drift_reference_df <- read_csv(
  "data/focus_sw_drift_values.csv",
  show_col_types = FALSE
)

focus_sw_water_body_dim <- read_csv(
  "data/focus_sw_water_body_dimensions.csv",
  show_col_types = FALSE
)

focus_sw_crop_distance <- read_csv(
  "data/focus_sw_crop_distance_to_bank.csv",
  show_col_types = FALSE
)


focus_crop_combinations <-
  expand.grid(
    Crop = focus_sw_crop_distance$Crop,
    `water body` = focus_sw_water_body_dim$`water body`
  ) %>%
  as_tibble() %>%
  left_join(focus_sw_water_body_dim) %>%
  left_join(focus_sw_crop_distance) %>%
  mutate(
    z_1 = `distance from crop to top of bank (m)` +
      `distance from top of bank to water (m)`,
    z_2 = z_1 + `water width (m)`
  ) %>%
  left_join(
    default_drift_reference_df,
    by = "Crop grouping",
    relationship = "many-to-many"
  )


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Precision Ag Drift Refinement"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Spray Setup"),
      shiny::radioButtons(
        inputId = "field_option",
        choices = c("Full Field", "Single Band", "Multi Band", "Regular Spot"),
        label = "Choose a field option",
        selected = "Full Field"
      ),
      h3("Band Info"),
      fluidRow(
        column(
          width = 6,
          shiny::numericInput(
            inputId = "band_width",
            label = "Band width (Spray) [m]",
            value = 1,
            step = 0.1,
            min = 0,
            max = 5
          )
        ),
        column(
          width = 6,
          shiny::numericInput(
            inputId = "inter_band_width",
            label = "Inter band width (No Spray) [m]",
            min = 0,
            step = 0.1,
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
      plotOutput("field_plot", height = 500, width = 500),
      verbatimTextOutput("drift_stats"),
      "Drift calculations are done over an infinitly long and 1km deep field",
      "This is currently just an exploration tool developed by Michael Bird at Syngenta. Not to be used in a real RA ... yet"
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  selected_focus_combination <- reactive(
    focus_crop_combinations %>%
      filter(
        Crop == input$crop,
        `water body` == stringr::str_to_lower(input$water_body),
        NumApps == 1
      ) %>%
      mutate(
        z_1 = case_when(
          input$buffer_m == "No buffer (Step 3)" ~ z_1,
          input$buffer_m == "5 m" ~ 5,
          input$buffer_m == "10 m" ~ 10
        ),
        z_2 = z_1 + `water width (m)`
      )
  )

  output$field_plot <- renderPlot(
    {
      validate(
        need(
          nrow(selected_focus_combination()) > 0,
          "Crop and Water Body combinations not defined"
        )
      )
      # print(selected_focus_combination())
      # generate bins based on input$bins from ui.R
      if (input$field_option == c("Full Field")) {
        plot_full_field(
          z_1 = selected_focus_combination()$z_1,
          z_2 = selected_focus_combination()$z_2
        )
      } else if (input$field_option == "Single Band") {
        plot_single_band_field(
          band_width = input$band_width,
          z_1 = selected_focus_combination()$z_1,
          z_2 = selected_focus_combination()$z_2
        )
      } else if (input$field_option == "Multi Band") {
        plot_multi_band_field(
          band_width = input$band_width,
          inter_band_width = input$inter_band_width,
          z_1 = selected_focus_combination()$z_1,
          z_2 = selected_focus_combination()$z_2
        )
      } else if (input$field_option == "Regular Spot") {
        plot_regular_spot_field(
          band_width = input$band_width,
          inter_band_width = input$inter_band_width,
          z_1 = selected_focus_combination()$z_1,
          z_2 = selected_focus_combination()$z_2
        )
      }
    }
  )
  output$drift_stats <- renderText({
    validate(
      need(
        nrow(selected_focus_combination()) > 0,
        "Crop and Water Body combinations not defined"
      )
    )

    ffd <- full_field_drift(
      z_1 = selected_focus_combination()$z_1,
      z_2 = selected_focus_combination()$z_2,
      A = selected_focus_combination()$A,
      B = selected_focus_combination()$B,
      C = selected_focus_combination()$C,
      D = selected_focus_combination()$D,
      H = selected_focus_combination()$H
    )

    if (input$field_option == c("Full Field")) {
      drift_perc <- ffd
    } else if (input$field_option == "Single Band") {
      drift_perc <- single_band_drift(
        band_width = input$band_width,
        z_1 = selected_focus_combination()$z_1,
        z_2 = selected_focus_combination()$z_2,
        A = selected_focus_combination()$A,
        B = selected_focus_combination()$B,
        C = selected_focus_combination()$C,
        D = selected_focus_combination()$D,
        H = selected_focus_combination()$H
      )
    } else if (input$field_option == "Multi Band") {
      drift_perc <- multi_banded_drift(
        band_width = input$band_width,
        inter_band_width = input$inter_band_width,
        z_1 = selected_focus_combination()$z_1,
        z_2 = selected_focus_combination()$z_2,
        A = selected_focus_combination()$A,
        B = selected_focus_combination()$B,
        C = selected_focus_combination()$C,
        D = selected_focus_combination()$D,
        H = selected_focus_combination()$H
      )
    } else if (input$field_option == "Regular Spot") {
      drift_perc <- regular_spot_drift(
        band_width = input$band_width,
        inter_band_width = input$inter_band_width,
        z_1 = selected_focus_combination()$z_1,
        z_2 = selected_focus_combination()$z_2,
        A = selected_focus_combination()$A,
        B = selected_focus_combination()$B,
        C = selected_focus_combination()$C,
        D = selected_focus_combination()$D,
        H = selected_focus_combination()$H
      )
    }

    perc_reduc <- sprintf("%3.0f", 100 * (1 - drift_perc / ffd))
    glue::glue(
      "Mean drift deposition over water body: {sprintf('%.2f',drift_perc)} % of application rate\n",
      "Percent reduction over full field application: {perc_reduc} % reduction"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
