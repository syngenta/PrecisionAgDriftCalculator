# What ----

# Source code for a banded application drift calculator.
#
# This app implements the methods presented by Michael Bird and David Patterson
# at the inaugural GB Fate modelling forum. Some detail about the method are
# found in the github readme. Further details, are available on request.

# Packages ----
library(shiny) # Webapp creation
library(readr) # For reading csvs containing constants
library(dplyr) # Data manipulation
library(tibble) # Data manipulation
library(magrittr) # piping function

# Functions ----
#Functions for calculating spray drift as percentage of app rate. Contains
#functions for:
# - Full field
# - Single band
# - Multi band (parallel banded application covering field)
# - Regular spot application
source("drift_calc_functions.R")
#functions that create ggplot objects representing the above scenarios for
#visual plotting
source("plot_field.R")

# Data ----
# FOCUS SW Appendix B drift parameters
default_drift_reference_df <- read_csv(
  "data/focus_sw_drift_values.csv",
  show_col_types = FALSE
)

#Dimensions of FOCUS SW bodies
focus_sw_water_body_dim <- read_csv(
  "data/focus_sw_water_body_dimensions.csv",
  show_col_types = FALSE
)

# Distance from crop to bank according to FOCUS SW
focus_sw_crop_distance <- read_csv(
  "data/focus_sw_crop_distance_to_bank.csv",
  show_col_types = FALSE
)

# 1. Combine crop and SW body combinations
# 2. Calculate z_1 and z_2 for each crop and wb combinations
# 3. Join with drift parameters so that each row of `focus_crop_combinations`
#    uniquely represents a crop, water body, and number of applications
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

#UI ----
# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Precision Ag Drift Refinement"),

  # Sidebar
  sidebarLayout(
    sidebarPanel(
      #Options for field set up
      h3("Spray Setup"),
      shiny::radioButtons(
        inputId = "field_option",
        choices = c("Full Field", "Single Band", "Multi Band", "Regular Spot"),
        label = "Choose a field option",
        selected = "Full Field"
      ),

      #Information on Band geometry
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

      # Crop and Water body selection. Currently hard coded for FOCUS SW and
      # relies on these matching the rows in `focus_crop_combinations`: not good
      # for long term but fine for now.
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
        #Note that for UI, these are in Title case. Server side, these are modified
        #to lower case
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

      # Add in buffer lengths
      h3("Mitigation info"),
      selectInput(
        inputId = "buffer_m",
        label = "Add a spray buffer",
        choices = c("No buffer (Step 3)", "5 m", "10 m", "20 m", "30 m"),
        selected = 1
      )
    ),

    # main (right side) panel
    mainPanel(
      # Show a plot of the generated field
      plotOutput("field_plot", height = 500, width = 500),
      # Text below field plot
      verbatimTextOutput("drift_stats"),
      "Drift calculations are done over an infinitly long and 1km deep field",
      "This is currently just an exploration tool developed by Michael Bird at Syngenta. Not to be used in a real RA ... yet",
      br(),
      "Confused? Check out the Github for source code and documention:",
      tagList(a(
        "Github Link",
        href = "https://github.com/syngenta/PrecisionAgDriftCalculator/"
      ))
    )
  )
)

# Server ----
#Back end server function
server <- function(input, output) {
  #Take user inputs on crop, water body, and buffer: then filter the full
  #combinations of regression parameters down to a single line based off of
  #user selections. This reactive variable is recalculated every time a user
  #changes one of the mentioned inputs
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
          input$buffer_m == "10 m" ~ 10,
          input$buffer_m == "20 m" ~ 20,
          input$buffer_m == "30 m" ~ 30
        ),
        z_2 = z_1 + `water width (m)`
      )
  )

  # Plot the field visually
  output$field_plot <- renderPlot(
    {
      #first check inputs are valid
      validate(
        need(
          nrow(selected_focus_combination()) > 0,
          "Crop and Water Body combinations not defined"
        )
      )
      #for each field option, call a different plotting function.
      #This is not particularly flexible but it's very easy to read and understand

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
  #This text appears under the field picture and displays the drift percentage
  #as well as the reduction in drift from full field drift
  output$drift_stats <- renderText({
    #input validations
    validate(
      need(
        nrow(selected_focus_combination()) > 0,
        "Crop and Water Body combinations not defined"
      )
    )

    #full field drift calculation for this crop/water body/buffer
    ffd <- full_field_drift(
      z_1 = selected_focus_combination()$z_1,
      z_2 = selected_focus_combination()$z_2,
      A = selected_focus_combination()$A,
      B = selected_focus_combination()$B,
      C = selected_focus_combination()$C,
      D = selected_focus_combination()$D,
      H = selected_focus_combination()$H
    )

    #for any field option, calculate drift percentage
    if (input$field_option == c("Full Field")) {
      #already calculated
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

    # format percent reduction nicely
    perc_reduc <- sprintf("%3.0f", 100 * (1 - drift_perc / ffd))
    glue::glue(
      "Mean drift deposition over water body: {sprintf('%.2f',drift_perc)} % of application rate\n",
      "Percent reduction over full field application: {perc_reduc} % reduction"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
