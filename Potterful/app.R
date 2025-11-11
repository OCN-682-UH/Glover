# Load Libraries
library(rsconnect)
library(shiny)
library(tidyverse)
library(here) # Load the package as requested by the assignment
library(lubridate)
library(scales) # For formatting numbers

# 1. Data Loading and Path Structure

growth_path_doc <- here("Week_11", "Data", "pott_growth_data.csv")
spawn_path_doc <- here("Week_11", "Data", "pott_spawn_data.csv")


# Data Loading for Server Execution (Using direct file names for stability)
# We must use direct file names because the server uploads them to the root directory.
spawn_data_raw <- read_csv("pott_spawn_data.csv", show_col_types = FALSE)
growth_data_raw <- read_csv("pott_growth_data.csv", show_col_types = FALSE)


# Data Cleaning
# Spawning Data Cleaning
spawn_data_cleaned <- spawn_data_raw %>%
  select(Date, Viable, Unviable, Total, "Viability %...5") %>%
  mutate(
    Date = mdy(Date),
    # Critical Change: Use gsub to reliably remove commas before conversion,
    # as parse_number can be inconsistent on the server environment.
    Viable = as.numeric(gsub(",", "", Viable)),
    Unviable = as.numeric(gsub(",", "", Unviable)),
    Total = as.numeric(gsub(",", "", Total))
  ) %>%
  rename(Viability_Percent = `Viability %...5`) %>%
  filter(!is.na(Total) & Total > 0) %>%
  # Pivot to long format for the stacked bar plot
  pivot_longer(
    cols = c(Viable, Unviable),
    names_to = "Egg_Type",
    values_to = "Count"
  )

# Growth Data Cleaning
growth_data_cleaned <- growth_data_raw %>%
  # Ensure all necessary columns are present and clean up data types
  na.omit() %>%
  mutate(
    Protocol = as.factor(Protocol),
    Flexion = as.factor(`Flexion 0/1/2`)
  ) %>%
  select(id, dph, length, depth, Flexion, Protocol) %>%
  filter(!is.na(id))


# Define UI
ui <- navbarPage(
  "Potter's Angelfish Data Analyzer",
  
  # Tab 1: Larval Growth Analysis (Dynamic Figure 1)
  tabPanel(
    "Larval Growth Analysis",
    sidebarLayout(
      sidebarPanel(
        h4("Growth Plot Controls"),
        selectInput(
          "growth_protocol",
          "Select Protocol (Experimental Group):",
          choices = c("All", unique(growth_data_cleaned$Protocol)),
          selected = "All"
        ),
        selectInput(
          "growth_y_var",
          "Select Measurement Variable (Y-Axis):",
          choices = c("length", "depth"),
          selected = "length"
        ),
        p("Figure 1: Displays larval size vs. age, dynamically filtered by experimental protocol and measurement variable.")
      ),
      mainPanel(
        # Output 1: Dynamic Figure (Growth Plot)
        plotOutput("growth_plot", height = "500px")
      )
    )
  ),
  
  # Tab 2: Spawning Viability Trends (Dynamic Figure 2 & Secondary Output)
  tabPanel(
    "Spawning Viability Trends",
    sidebarLayout(
      sidebarPanel(
        h4("Spawning Data Controls"),
        dateRangeInput(
          "spawn_date_range",
          "Select Date Range:",
          start = min(spawn_data_cleaned$Date),
          end = max(spawn_data_cleaned$Date),
          min = min(spawn_data_cleaned$Date),
          max = max(spawn_data_cleaned$Date),
          format = "yyyy-mm-dd"
        ),
        p("Note: Plot and summary react to the selected date range.")
      ),
      mainPanel(
        h4("Daily Egg Production Stacked Bar Plot"),
        # Output 2: Dynamic Figure (Stacked Bar Plot)
        plotOutput("spawn_bar_plot", height = "400px"),
        hr(),
        h4("Reactive Spawning Summary (Secondary Output)"),
        # Output 3: Secondary Output (Reactive Summary Table)
        tableOutput("viability_summary_table")
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Reactive Expression 1: Filter growth data by Protocol
  growth_data_filtered <- reactive({
    req(input$growth_protocol)
    data <- growth_data_cleaned
    
    if (input$growth_protocol != "All") {
      data <- filter(data, Protocol == input$growth_protocol)
    }
    return(data)
  })
  
  # Reactive Expression 2: Filter spawning data by Date Range
  spawn_data_filtered <- reactive({
    req(input$spawn_date_range)
    
    start_date <- input$spawn_date_range[1]
    end_date <- input$spawn_date_range[2]
    
    # Filter the cleaned (long format) data
    spawn_data_cleaned %>%
      filter(Date >= start_date & Date <= end_date)
  })
  
  # Figure 1: Larval Growth Plot (Dynamic Figure Output 1)
  output$growth_plot <- renderPlot({
    data_to_plot <- growth_data_filtered()
    
    # Ensure there is data to plot before proceeding
    req(nrow(data_to_plot) > 0)
    
    # Use !!sym() to map the dynamically selected column
    ggplot(data_to_plot, aes(x = dph, y = !!sym(input$growth_y_var), color = Protocol)) +
      geom_point(alpha = 0.7) +
      # Adding a simple linear trend line for visual aid
      geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
      labs(
        title = paste("Larval Size (", str_to_title(input$growth_y_var), ") Growth Curve by Protocol"),
        x = "Days Post Hatch (dph)",
        y = paste(str_to_title(input$growth_y_var), " (mm)")
      ) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Figure 2: Daily Egg Production Stacked Bar Plot (Dynamic Figure Output 2)
  output$spawn_bar_plot <- renderPlot({
    data_to_plot <- spawn_data_filtered()
    
    req(nrow(data_to_plot) > 0)
    
    # Summarize back to the wide format's structure before plotting
    plot_data <- data_to_plot %>%
      group_by(Date, Egg_Type) %>%
      summarise(Total_Count = sum(Count), .groups = 'drop')
    
    # Generate the stacked bar plot
    ggplot(plot_data, aes(x = Date, y = Total_Count, fill = Egg_Type)) +
      geom_bar(stat = "identity", position = "stack", width = 0.9) +
      scale_fill_manual(values = c("Viable" = "#4e79a7", "Unviable" = "#f28e2b")) +
      scale_y_continuous(labels = comma) +
      labs(
        title = "Daily Viable vs. Unviable Egg Production",
        x = "Date",
        y = "Egg Count",
        fill = "Viability"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  
  # Secondary Output: Spawning Summary Table
  spawn_summary_data <- reactive({
    data_filtered <- spawn_data_filtered()
    
    # Recalculate wide-format aggregates for summary table from long data
    summary_calc <- data_filtered %>%
      pivot_wider(names_from = Egg_Type, values_from = Count, values_fn = sum) %>%
      # Ensure Total is calculated correctly if not present in the filtered set
      mutate(
        Total = Viable + Unviable,
        Viability_Percent = Viable / Total * 100
      )
    
    req(nrow(summary_calc) > 0)
    
    start_date <- format(min(summary_calc$Date), "%Y-%m-%d")
    end_date <- format(max(summary_calc$Date), "%Y-%m-%d")
    
    data.frame(
      Metric = c(
        "Date Range Analyzed",
        "Total Spawning Days (N)",
        "Cumulative Total Eggs Produced",
        "Cumulative Viable Eggs Produced",
        "Mean Daily Viability %"
      ),
      Value = c(
        paste(start_date, " to ", end_date),
        as.character(nrow(summary_calc)),
        format(sum(summary_calc$Total), big.mark = ","),
        format(sum(summary_calc$Viable), big.mark = ","),
        paste0(round(mean(summary_calc$Viability_Percent, na.rm = TRUE), 2), "%")
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$viability_summary_table <- renderTable({
    spawn_summary_data()
  }, striped = TRUE, bordered = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)