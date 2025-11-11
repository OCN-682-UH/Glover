# Load Libraries
library(rsconnect)
library(shiny)
library(tidyverse)
library(here)
library(lubridate)
library(scales) # For formatting numbers

# 1. Data Loading with 'here'

growth_path <- here("Week_11", "Data", "pott_growth_data.csv")
spawn_path <- here("Week_11", "Data", "pott_spawn_data.csv")

# Data Cleaning 

# Spawning Data Cleaning
spawn_data_raw <- read_csv(spawn_path, show_col_types = FALSE)
spawn_data_cleaned <- spawn_data_raw %>%
  # Note: The raw data contains multiple viability % columns; column 5 is used here, renamed.
  select(Date, Viable, Unviable, Total, "Viability %...5") %>%
  mutate(
    Date = mdy(Date),
    across(c(Viable, Unviable, Total), ~ parse_number(as.character(.)))
  ) %>%
  rename(Viability_Percent = `Viability %...5`) %>%
  filter(!is.na(Total) & Total > 0)

# Growth Data Cleaning
growth_data_raw <- read_csv(growth_path, show_col_types = FALSE)
growth_data_cleaned <- growth_data_raw %>%
  mutate(
    Protocol = as.factor(Protocol),
    Flexion = as.factor(`Flexion 0/1/2`)
  ) %>%
  select(id, dph, length, depth, Flexion, Protocol) %>%
  filter(!is.na(id))

# Define UI
ui <- navbarPage(
  "Potter's Angelfish Data Analyzer",
  
  # Tab 1: Larval Growth Analysis (Dynamic Figure Output)
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
        p("Note: Plot displays growth curve (size vs. age) dynamically filtered by protocol.")
      ),
      mainPanel(
        # Output 1:Dynamic Figure
        plotOutput("growth_plot", height = "500px")
      )
    )
  ),
  
  # Tab 2: Spawning Viability Trends (secondary output)
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
        p("Note: Date range is 12/12/22 to 05/31/24.")
      ),
      mainPanel(
        #DYNAMIC FIGURE OUTPUT: Daily Spawning Bar Plot
        h4("Daily Spawning Production and Viability Trend"),
        plotOutput("viability_bar_plot", height = "500px"),
        hr(),
        
        # SECONDARY OUTPUT: Summary metrics
        h4("Reactive Spawning Summary"),
        # Secondary Output 2: reactive summary table
        tableOutput("viability_summary_table"),
        hr(),
        h4("Cumulative Spawn Count Check"),
        #Secondary output 3
        verbatimTextOutput("total_spawn_text")
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
    
    if (input$growth_protocol!= "All") {
      data <- filter(data, Protocol == input$growth_protocol)
    }
    return(data)
  })
  
  # Reactive Expression 2: filter spawning data by Date Range
  spawn_data_filtered <- reactive({
    req(input$spawn_date_range)
    
    start_date <- input$spawn_date_range[1]
    end_date <- input$spawn_date_range[2]
    
    spawn_data_cleaned %>%
      filter(Date >= start_date & Date <= end_date)
  })
  
  # Reactive Expression 3: Reshape data for stacked bar plot (viable vs unviable)
  spawn_data_long <- reactive({
    spawn_data_filtered() %>%
      select(Date, Viable, Unviable) %>%
      pivot_longer(
        cols = c(Viable, Unviable),
        names_to = "Egg_Type",
        values_to = "Count"
      ) %>%
      #Order factors for plotting (Unviable on bottom, viable on top)
      mutate(Egg_Type = factor(Egg_Type, levels = c("Unviable", "Viable")))
  })
  
 
  #Dynamic figure output: Larval growth plot

  output$growth_plot <- renderPlot({
    data_to_plot <- growth_data_filtered()
    
    req(nrow(data_to_plot) > 0)
    
    # Use!!sym() to map the dynamically selected column
    ggplot(data_to_plot, aes(x = dph, y =!!sym(input$growth_y_var), color = Protocol)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 1.5) +
      labs(
        title = paste("Larval Size (", str_to_title(input$growth_y_var), ") Growth Curve"),
        x = "Days Post Hatch (dph)",
        y = paste(str_to_title(input$growth_y_var), " (mm)")
      ) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  

  #dynamic figure output: Daily spawning stacked Bar Plot
  output$viability_bar_plot <- renderPlot({
    data_to_plot <- spawn_data_long()
    req(nrow(data_to_plot) > 0)
    
    # Stacked bar plot for daily production (Viable + Unviable = Total)
    ggplot(data_to_plot, aes(x = Date, y = Count, fill = Egg_Type)) +
      geom_bar(stat = "identity", width = 1) + # width=1 makes bars "touch"
      scale_fill_manual(
        values = c("Viable" = "#00BFC4", "Unviable" = "#F8766D"),
        name = "Egg Status"
      ) +
      labs(
        title = "Daily Spawn Production (Viable vs. Unviable)",
        x = "Date",
        y = "Number of Eggs Produced (Total Stacked)"
      ) +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  #Secondary Output: Spawning Summary Table

  
  spawn_summary_data <- reactive({
    data_filtered <- spawn_data_filtered()
    
    req(nrow(data_filtered) > 0)
    
    start_date <- format(min(data_filtered$Date), "%Y-%m-%d")
    end_date <- format(max(data_filtered$Date), "%Y-%m-%d")
    
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
        as.character(nrow(data_filtered)),
        format(sum(data_filtered$Total), big.mark = ","),
        format(sum(data_filtered$Viable), big.mark = ","),
        paste0(round(mean(data_filtered$Viability_Percent), 2), "%")
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$viability_summary_table <- renderTable({
    spawn_summary_data()
  }, striped = TRUE, bordered = TRUE)
  

  # Secondary Output: Total Eggs Text
  
  output$total_spawn_text <- renderText({
    total_eggs <- sum(spawn_data_filtered()$Total, na.rm = TRUE)
    paste("Total Eggs Collected in Selected Range:", format(total_eggs, big.mark = ","))
  })
}

# Run the application
shinyApp(ui = ui, server = server)