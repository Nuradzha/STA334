library(shiny)
library(shinydashboard)
library(palmerpenguins)
library(ggplot2)

# Define the UI for the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Penguins Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Summary", tabName = "summary", icon = icon("table")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
              fluidRow(
                box(title = "Welcome", width = 12, status = "primary",
                    p("Name: NUR ADZHA MARHAMAH BINTI MOHAMAD ROSLAN"),
                    p("Student ID: 2022660968"),
                    p("This dashboard provides an overview of the Palmer Penguins dataset, including summary statistics and visualizations.")
                )
              )
      ),
      
      # Summary Tab
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Summary Statistics", width = 12, status = "primary",
                    tableOutput("summary_table")
                )
              )
      ),
      
      # Plots Tab
      tabItem(tabName = "plots",
              fluidRow(
                box(title = "Bar Chart", width = 6, status = "primary",
                    plotOutput("bar_chart")
                ),
                box(title = "Scatterplot", width = 6, status = "primary",
                    plotOutput("scatter_plot")
                )
              ),
              fluidRow(
                box(title = "Histogram", width = 12, status = "primary",
                    plotOutput("histogram")
                )
              )
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Summary table
  output$summary_table <- renderTable({
    penguins_summary <- penguins %>%
      dplyr::summarise(
        bill_length_mm_mean = mean(bill_length_mm, na.rm = TRUE),
        bill_length_mm_sd = sd(bill_length_mm, na.rm = TRUE),
        bill_depth_mm_mean = mean(bill_depth_mm, na.rm = TRUE),
        bill_depth_mm_sd = sd(bill_depth_mm, na.rm = TRUE),
        flipper_length_mm_mean = mean(flipper_length_mm, na.rm = TRUE),
        flipper_length_mm_sd = sd(flipper_length_mm, na.rm = TRUE),
        body_mass_g_mean = mean(body_mass_g, na.rm = TRUE),
        body_mass_g_sd = sd(body_mass_g, na.rm = TRUE),
        species_frequency = paste(table(penguins$species), collapse = ", ")
      )
    penguins_summary
  })
  
  # Bar chart
  output$bar_chart <- renderPlot({
    ggplot(penguins, aes(x = species, fill = species)) +
      geom_bar() +
      labs(title = "Penguin Species Count", x = "Species", y = "Count") +
      theme_minimal()
  })
  
  # Scatterplot
  output$scatter_plot <- renderPlot({
    ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
      geom_point() +
      labs(title = "Bill Length vs Bill Depth", x = "Bill Length (mm)", y = "Bill Depth (mm)") +
      theme_minimal()
  })
  
  # Histogram
  output$histogram <- renderPlot({
    ggplot(penguins, aes(x = body_mass_g, fill = species)) +
      geom_histogram(binwidth = 200, position = "dodge") +
      labs(title = "Body Mass Distribution", x = "Body Mass (g)", y = "Frequency") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
