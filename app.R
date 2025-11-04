library(shiny)
library(actranspr2)
library(DT)
library(rhandsontable)

# Definizione delle modalità di trasporto
modes <- sort(unique(actranspr2::fatal_road_inj_rate[["mode"]]))

ui <- fluidPage(
  titlePanel("Health impact of a modal shift in Italian cities"),
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "City",
                  choices = sort(unique(actranspr2::demo_data[["city"]]))),
      selectInput("mode_from", "Original travel mode", choices = modes),
      selectInput("mode_to", "New travel mode", choices = modes),
      numericInput("commuting_days", "Commuting days per week", value = 4, min = 1, max = 5),
      numericInput("working_weeks", "Working weeks per year", value = 48, min = 1, max = 52),

      h4("Shift share by distance range"),
      rHandsontableOutput("shift_table"),  # ✅ correttamente definito

      numericInput("sdr", "Social discount rate", value = 0.024, min = 0),
      numericInput("voly", "Value of a life year (€)", value = 70151, min = 0),
      numericInput("vsl", "Value of a statistical life (€)", value = 3607757, min = 0),
      numericInput("ref_year", "Reference year", value = 2021, min = 2021),
      actionButton("compute", "Compute health impact")
    ),
    mainPanel(
      DT::dataTableOutput("risultato")
    )
  )
)

server <- function(input, output) {

  # Tabella iniziale
  initial_table <- data.frame(
    Distance = c("< 3 km", "3–10 km", "10–15 km", "> 15 km"),
    Share = c(0.3, 0.2, 0.0, 0.0)
  )

  output$shift_table <- renderRHandsontable({
    rhandsontable(initial_table, rowHeaders = NULL)
  })

  observeEvent(input$compute, {
    if (!is.null(input$shift_table)) {
      table_data <- hot_to_r(input$shift_table)

      result <- actranspr2::morbidity_impact(
        city = input$city,
        min_age = 20,
        max_age = 64,
        mode_from = input$mode_from,
        mode_to = input$mode_to,
        share.less_3km = table_data$Share[1],
        share.3_10km = table_data$Share[2],
        share.10_15km = table_data$Share[3],
        share.more_15km = table_data$Share[4],
        commuting_days = input$commuting_days,
        experimental_data = FALSE,
        met_phy_act = 7,
        working_weeks = input$working_weeks,
        detail = "medium",
        sdr = input$sdr,
        ref_year = input$ref_year,
        voly = input$voly,
        vsl = input$vsl
      )

      output$risultato <- DT::renderDataTable({
        result
      })
    }
  })
}

shinyApp(ui = ui, server = server)
