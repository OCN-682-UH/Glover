#load libraries
library(rsconnect)
library(shiny)
library(tidyverse)
library(here)#mandatory for the assignment
library(lubridate)
library(scales)#for formatting numbers in the table


#data loading for server execution
#use direct file names because the server is stupid about 'here()'
spawn_data_raw <- read_csv("pott_spawn_data.csv", show_col_types = FALSE)
growth_data_raw <- read_csv("pott_growth_data.csv", show_col_types = FALSE)


#data cleaning
#spawning data cleaning: the data is messy, gotta fix it
spawn_data_cleaned <- spawn_data_raw %>%
  select(Date, Viable, Unviable, Total, "Viability %...5") %>%
  mutate(
    Date = mdy(Date),
    #critical: reliably remove commas before converting to a number
    Viable = as.numeric(gsub(",", "", Viable)),
    Unviable = as.numeric(gsub(",", "", Unviable)),
    Total = as.numeric(gsub(",", "", Total))
  ) %>%
  rename(Viability_Percent = `Viability %...5`) %>%
  filter(!is.na(Total) & Total > 0) %>%
  #pivot to long format for the stacked bar plot
  pivot_longer(
    cols = c(Viable, Unviable),
    names_to = "Egg_Type",
    values_to = "Count"
  )

#growth data cleaning
growth_data_cleaned <- growth_data_raw %>%
  #remove the na rows, no time for imputation
  na.omit() %>%
  mutate(
    #fix the protocol display issue: force it to be character, not a factor
    Protocol = as.character(Protocol),
    Flexion = as.factor(`Flexion 0/1/2`)
  ) %>%
  select(id, dph, length, depth, Flexion, Protocol) %>%
  filter(!is.na(id))


# define ui
ui <- navbarPage(
  "Potter's Angelfish Data Analyzer",
  
  #tab 1: larval growth analysis (dynamic figure 1)
  tabPanel(
    "Larval Growth Analysis",
    sidebarLayout(
      sidebarPanel(
        h4("Growth Plot Controls"),
        selectInput(
          "growth_protocol",
          "select protocol (experimental group):",
          # this should now correctly show the letters, not numbers
          choices = c("all", unique(growth_data_cleaned$Protocol)),
          selected = "all"
        ),
        selectInput(
          "growth_y_var",
          "select measurement variable (y-axis):",
          choices = c("length", "depth"),
          selected = "length"
        ),
        p("figure 1: larval size vs. age, filtered by protocol and measurement.")
      ),
      mainPanel(
        #output 1: dynamic figure (growth plot)
        plotOutput("growth_plot", height = "500px")
      )
    )
  ),
  
  #tab 2: spawning viability trends (dynamic figure 2 & secondary output)
  tabPanel(
    "Spawning Viability Trends",
    sidebarLayout(
      sidebarPanel(
        h4("spawning data controls"),
        dateRangeInput(
          "spawn_date_range",
          "select date range:",
          start = min(spawn_data_cleaned$Date),
          end = max(spawn_data_cleaned$Date),
          min = min(spawn_data_cleaned$Date),
          max = max(spawn_data_cleaned$Date),
          format = "yyyy-mm-dd"
        ),
        p("note: plot and summary react to the date range selection.")
      ),
      mainPanel(
        h4("daily egg production stacked bar plot"),
        # output 2: dynamic figure (stacked bar plot)
        plotOutput("spawn_bar_plot", height = "400px"),
        hr(),
        h4("reactive spawning summary (secondary output: must have two outputs!)"),
        # output 3: secondary output (reactive summary table)
        tableOutput("viability_summary_table")
      )
    )
  )
)

#define server logic
server <- function(input, output, session) {
  
  #reactive expression 1: filter growth data by protocol
  growth_data_filtered <- reactive({
    req(input$growth_protocol)#need this line or it crashes
    data <- growth_data_cleaned
    
    # filter the data based on the dropdown choice
    if (input$growth_protocol != "all") {
      data <- filter(data, Protocol == input$growth_protocol)
    }
    return(data)
  })
  
  #reactive expression 2: filter spawning data by date range
  spawn_data_filtered <- reactive({
    req(input$spawn_date_range)
    
    start_date <- input$spawn_date_range[1]
    end_date <- input$spawn_date_range[2]
    
    #filter the cleaned (long format) data
    spawn_data_cleaned %>%
      filter(Date >= start_date & Date <= end_date)
  })
  
  #figure 1: larval growth plot (dynamic figure output 1)
  output$growth_plot <- renderPlot({
    data_to_plot <- growth_data_filtered()
    
    #make sure there's data to plot
    req(nrow(data_to_plot) > 0)
    
    #use !!sym() to map the dynamically selected column
    ggplot(data_to_plot, aes(x = dph, y = !!sym(input$growth_y_var), color = Protocol)) +
      geom_point(alpha = 0.7) +
      #a simple linear model smooth line is safer for deployment
      geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
      labs(
        title = paste("larval size (", str_to_title(input$growth_y_var), ") growth curve by protocol"),
        x = "days post hatch (dph)",
        y = paste(str_to_title(input$growth_y_var), " (mm)")
      ) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # figure 2: daily egg production stacked bar plot (dynamic figure output 2)
  output$spawn_bar_plot <- renderPlot({
    data_to_plot <- spawn_data_filtered()
    
    req(nrow(data_to_plot) > 0)
    
    #summarize and plot the long data
    plot_data <- data_to_plot %>%
      group_by(Date, Egg_Type) %>%
      summarise(Total_Count = sum(Count), .groups = 'drop')
    
    #generate the stacked bar plot
    ggplot(plot_data, aes(x = Date, y = Total_Count, fill = Egg_Type)) +
      geom_bar(stat = "identity", position = "stack", width = 0.9) +
      scale_fill_manual(values = c("Viable" = "#4e79a7", "Unviable" = "#f28e2b")) +
      scale_y_continuous(labels = comma) +
      labs(
        title = "daily viable vs. unviable egg production",
        x = "date",
        y = "egg count",
        fill = "viability"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  
  #secondary output: spawning summary table
  spawn_summary_data <- reactive({
    data_filtered <- spawn_data_filtered()
    
    #need to pivot back to wide to calculate the summary stats
    summary_calc <- data_filtered %>%
      pivot_wider(names_from = Egg_Type, values_from = Count, values_fn = sum) %>%
      mutate(
        Total = Viable + Unviable,
        Viability_Percent = Viable / Total * 100
      )
    
    req(nrow(summary_calc) > 0)
    
    start_date <- format(min(summary_calc$Date), "%Y-%m-%d")
    end_date <- format(max(summary_calc$Date), "%Y-%m-%d")
    
    #creating the final table dataframe
    data.frame(
      Metric = c(
        "date range analyzed",
        "total spawning days (n)",
        "cumulative total eggs produced",
        "cumulative viable eggs produced",
        "mean daily viability %"
      ),
      Value = c(
        paste(start_date, " to ", end_date),
        as.character(nrow(summary_calc)),
        format(sum(summary_calc$Total, na.rm=TRUE), big.mark = ","),
        format(sum(summary_calc$Viable, na.rm=TRUE), big.mark = ","),
        paste0(round(mean(summary_calc$Viability_Percent, na.rm = TRUE), 2), "%")
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$viability_summary_table <- renderTable({
    spawn_summary_data()
  }, striped = TRUE, bordered = TRUE)
}

#run the application
shinyApp(ui = ui, server = server)