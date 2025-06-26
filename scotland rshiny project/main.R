library(ggplot2)
library(shiny)
library(dplyr)
library(plotly)
library(tidyverse)
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print(getwd())
df1 <- read_csv("https://raw.githubusercontent.com/letsplaywithstrings/curly-spoon/main/data_infant_death_rate.csv")
df2 <- read_csv("https://raw.githubusercontent.com/letsplaywithstrings/curly-spoon/main/waste_tonnes.csv")
df_locations <- read_csv("https://raw.githubusercontent.com/letsplaywithstrings/curly-spoon/main/long-lat.csv") 
#Read energy data
ec <- read.csv("ec.csv")
super <-  read.csv("super.csv")
#Filter the data
ec_final <- ec %>% filter(., Energy.Type != "All", Energy.Consuming.Sector == "All") %>% 
  arrange(FeatureName, Year)

unemployment_data <- read.csv("employment_data.csv")
allex_energy_data <- read.csv("allex_energy_data.csv")

# Sort the Council Areas and Years
sorted_areas <- sort(unique(allex_energy_data$FeatureName))
sorted_years <- sort(unique(allex_energy_data$DateCode))

#Read infant mortality rate data
idr <- read.csv("data_infant_death_rate.csv")

#Arrange the data
idr <- idr %>% arrange(FeatureName, Year)

# Filter the infant mortality rate between 2015-2020
filtered_data_idr <- idr %>%
  filter(Year >= 2015 & Year <= 2020)

# Calculate the mean mortality rate by city
mean_mortality <- filtered_data_idr %>%
  group_by(FeatureName) %>%
  summarize(mean_mortality_rate = mean(Mortality.Rate))

# Filter the data for the desired years (2015-2020)
filtered_data_ec <- ec_final %>%
  filter(Year >= 2015 & Year <= 2020, Energy.Type != "All")

# Calculate the mean of energy consumption by energy type and city
mean_energy <- filtered_data_ec %>%
  group_by(FeatureName, Energy.Type) %>%
  summarize(mean_consumption = round(mean(Value),2))

#Read coordinate data
coordinates <- read.csv("long_lat.csv")

# Load required libraries
library(ggplot2)
library(shiny)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(shiny)

#Read energy data
ec <- read.csv("ec.csv")

#Filter the data
ec_final <- ec %>% filter(., Energy.Type != "All", Energy.Consuming.Sector == "All") %>% 
  arrange(FeatureName, Year)

#Read infant mortality rate data
idr <- read.csv("data_infant_death_rate.csv")

#Arrange the data
idr <- idr %>% arrange(FeatureName, Year)
idr$Births <- as.numeric(gsub("[^0-9.]", "", idr$Births))

# Filter the infant mortality rate between 2015-2020
filtered_data_idr <- idr %>%
  filter(Year >= 2015 & Year <= 2020)

# Calculate the mean mortality rate by city
mean_mortality <- filtered_data_idr %>%
  group_by(FeatureName) %>%
  summarize(mean_mortality_rate = mean(Mortality.Rate))

# Filter the data for the desired years (2015-2020)
filtered_data_ec <- ec_final %>%
  filter(Year >= 2015 & Year <= 2020, Energy.Type != "All")

# Calculate the mean of energy consumption by energy type and city
mean_energy <- filtered_data_ec %>%
  group_by(FeatureName, Energy.Type) %>%
  summarize(mean_consumption = round(mean(Value),2))

#Read coordinate data
coordinates <- read.csv("long_lat.csv")

#Read waste_energy
newlist <- read.csv("waste_energy.csv")

#Read Employment
Employment_data <- read.csv("Employment_data.csv")
allex_energy_data <- read.csv("allex_energy_data.csv")

# Sort the Council Areas and Years
sorted_areas <- sort(unique(allex_energy_data$FeatureName))
sorted_years <- sort(unique(allex_energy_data$DateCode))



# User Interface ----------------------------------------------------------
# Define UI
ui <- navbarPage(
  title = "Relationship Between Energy Consumption and Some Development Indicators",
  
  tabsetPanel(
    tags$head(
      tags$style(
        HTML("
        .center-image {
          display: flex;
          justify-content: center;
        }
      ")
      )
    ),
    
    tabPanel(
      "About",
      fluidRow(
        column(
          width = 12,
          h3("About the App"),
          p("Welcome to the Energy Consumption app! This application allows you to explore energy consumption and its relationship between some development indicators by council area in Scotland."),
          p("The aim of this app is to provide users with an interactive tool to analyze and visualize energy consumption patterns and trends with the relationship of the indicators in different council areas. By selecting specific council areas, years,energy types, and other choices you can explore how energy consumption varies across regions with the other measurements over time."),
          p("In addition to visualizations, this app provides summary statistics and a correlation analysis between energy consumption with other development indicators. These features enable you to gain insights into potential relationships between energy consumption and health indicators."),
          p("Please note that the data used in this app is based on a specific dataset and the analysis and conclusions drawn may not be generalizable to all scenarios."),
          p("We hope this application serves as a valuable tool for understanding energy consumption patterns with development indicators in Scotland and their potential implications."),
          p("Powered By Python Sevenler Derneği (Kadir Şimşek)"),
          tags$div(
            style = "display: flex; justify-content: center;",
            imageOutput("gif_image")
          ),
        )
      )
    ),
    
    #  ----------------------------------------------------------------
    tabPanel(
      "Correlation Analysis for Insight",
      sidebarLayout(
        sidebarPanel(
          selectInput("x", "Select X-axis:", choices = colnames(super)),
          selectInput("y", "Select Y-axis:", choices = colnames(super))
        ),
        mainPanel(
          tabPanel("Correlation", verbatimTextOutput("cor_output")),
          tabPanel("Plot", plotOutput("scatter_plot_insight"))
        )
      )
    ),
    
    #  --------------------------------------------------------------
    tabPanel(
      "Energy Consumption",
      sidebarLayout(
        sidebarPanel(
          selectInput("council_area", "Select Council Area:",
                      choices = unique(ec_final$FeatureName)),
          selectInput("year", "Select Year:",
                      choices = unique(ec_final$Year)),
          
          selectInput("city", "Select Council Area:",
                      choices = unique(ec_final$FeatureName)),
          
          sliderInput("range", "Year Range:",
                      min = 2015, max = 2020,
                      value = c(2015, 2020),
                      sep = "", step = 1)
        ),
        
        mainPanel(
          plotOutput("energy_plot"),
          plotOutput("blank_tab_plot")
        )
      )
    ),
    
    tabPanel(
      "Data Explorer for Energy Consumption and Infant Mortality Rate",
      tabsetPanel(
        tabPanel(
          "Energy Consumption",
          DT::dataTableOutput("ec_final_table"),
          verbatimTextOutput("ec_final_summary")
        ),
        
        tabPanel(
          "Infant Mortality Rate",
          DT::dataTableOutput("idr_table"),
          verbatimTextOutput("idr_summary")
        )
      )
    ),
    
    tabPanel(
      "Energy Consumption vs Infant Mortality Rate",
      sidebarLayout(
        sidebarPanel(
          selectInput("energy_type", "Select Energy Type:",
                      choices = unique(ec_final$Energy.Type)),
          
          checkboxInput("linear_regression", "Include Linear Regression", value = FALSE),
          
          actionButton("calculate_button", "Calculate")
        ),
        
        mainPanel(
          plotOutput("scatter_plot"),
          verbatimTextOutput("correlation_output"),
          verbatimTextOutput("model_info")
        )
      )
    ),
    
    
    tabPanel(
      "Scotland Map",
      titlePanel("Map Visualization"),
      sidebarLayout(
        sidebarPanel(
          selectInput("input_var", "Select Variable:", choices = colnames(super[, 4:21]))
        ),
        mainPanel(
          leafletOutput("map",height = "700px", width = "100%")
        )
      )
    ),
    # Kadir - UI --------------------------------------------------------------
    tabPanel(
      "Employment",
      sidebarLayout(
        sidebarPanel(
          # Select Council Area
          selectInput("areaInput", "Select Council Area:",
                      choices = unique(unemployment_data$FeatureName),
                      selected = unique(unemployment_data$FeatureName)[1]),
          
          # Select Year
          selectInput("yearInput", "Select Year:",
                      choices = unique(unemployment_data$DateCode),
                      selected = unique(unemployment_data$DateCode)[1]),
          
          # Select graph type
          selectInput("graphTypeInput", "Select Graph Type:",
                      choices = c("Histogram", "Pie Chart", "Scatter Plot"),
                      selected = "Histogram")
        ),
        
        mainPanel(
          # Display the graph
          plotOutput("graphUnemployment_u")
        )
      )
    ),
    tabPanel(
      "Energy Consumption",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            # Select Council Area
            selectInput("council_area", "Council Area:", choices = sorted_areas),
            
            # Select Year
            selectInput("year", "Year:", choices = sorted_years),
            
            # Select Graph Type
            selectInput("graph_type", "Graph Type:", choices = c("Histogram", "Pie Chart")),
            
            # Submit button
            actionButton("submit", "Submit")
          ),
          
          mainPanel(
            # Output graph
            plotOutput("graphEnergy_u")
          )
        )
      )
    ),
    tabPanel(
      "Energy and Unemployment Correlation",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            # Council Area selection
            selectInput("council_area_corr", "Select Council Area:",
                        choices = unique(allex_energy_data$FeatureName)),
            
            # Year selection
            
          ),
          
          mainPanel(
            plotOutput("correlation_plot_u")
          )
        )
      )
    ),
    tabPanel(
      "Employment and Energy Analysis",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            # Council Area selection
            selectInput("council_area_analysis", "Select Council Area:",
                        choices = sort(unique(allex_energy_data$FeatureName))),
            
            # Year selection
            selectInput("year_analysis", "Select Year:",
                        choices = sort(unique(allex_energy_data$DateCode)))
          ),
          
          mainPanel(
            # Plots
            plotOutput("unemployment_plot_u"),
            plotOutput("energy_plot_u"),
            
            # Summary Analysis
            h4("Unemployment Summary"),
            verbatimTextOutput("unemployment_stats_u"),
            h4("Energy Consumption Summary"),
            verbatimTextOutput("energy_stats_u")
          )
        )
      )
    ),
    
    # UI --------------------------------------------------------------
    tabPanel(
      "Waste Management vs Energy Type",
      fluidPage(
        sidebarLayout(
          sidebarPanel(selectInput("factor1", "Choose waste management type:", choices = c("landfilled", "recycled", "waste_generated", "other_methods")),
                       selectInput("factor2", "Choose energy consumption type:", choices = c("electric", "petroleum", "gas", "Bioenergy_and_Wastes", "Manufactured_Fuels")),
                       actionButton("calculate", "Calculate Correlation")),
          mainPanel(
            plotOutput("scatterPlot"),
            verbatimTextOutput("correlationOutput")
          )
        )
      )
    ),
    # UI --------------------------------------------------------------
    tabPanel(
      title = "Line Graphs about Infant Mortality Rate and Waste",
      icon = icon("line-chart"),
      fluidRow(
        column(
          width = 3,
          selectInput("waste_filter", "Filter by Waste Category:", choices = unique(df2$waste)),
          selectInput("city_filter", "Filter by City:", choices = unique(df2$city)),
          sliderInput("date_filter", "Filter by Date Range:",
                      min = min(df1$date),
                      max = max(df1$date),
                      value = c(min(df1$date), max(df1$date)),
                      step = 1,
                      sep = ""
          ),
          br(),
        ),
        column(
          width = 7,
          plotlyOutput("lineplot_a")
        ),
        column(
          width=3,
        ),
        column(
          width=7,
          plotlyOutput("tonnes_plot")
        ),
      )
    ),
    
    # Data Explorer tab
    tabPanel(
      title = "Data Explorer Waste Management and Infant Mortality",
      icon = icon("table"),
      fluidRow(
        column(
          width = 2,
          selectInput("waste_filter_data", "Filter by Waste Category:", choices = unique(df2$waste)),
          selectInput("city_filter_data", "Filter by City:", choices = unique(df2$city)),
          sliderInput("date_filter_data", "Filter by Date Range:",
                      min = min(df1$date),
                      max = max(df1$date),
                      value = c(min(df1$date), max(df1$date)),
                      step = 1,
                      sep = ""
          ),
          br(),
        ),
        column(
          width = 10,
          dataTableOutput("data_table_a")
        )
      )
    ),
    
    tabPanel(
      title = "Correlation Analysis Infant Mortality ~ Waste Management",
      icon = icon("star"),
      selectInput("correlation_waste_filter", "Infant death rate ~ Waste in tonnes:",
                  choices = unique(df2$waste)),
      plotlyOutput("scatter_plot_a", height = "400px"),
      verbatimTextOutput("correlation_output_a")
    ),
    
    tabPanel(
      title = "Waste Map",
      icon = icon("map"),
      sidebarPanel(
        selectInput("waste_selector", "Select Waste Type:", choices = unique(df2$waste)),
        selectInput("year_selector", "Select Year:", choices = c("All",unique(df2$date[df2$date >= 2015 & df2$date <= 2020]))
        )),
      leafletOutput("waste_map", height = "600px"),
    ),
    # Waste Tonnes tab
    tabPanel("Waste Category by Tonnes",
             icon = icon("bar-chart"),
             sidebarPanel(width = 2,
                          selectInput("city_selector", "Select City:",
                                      choices = c("All", unique(df2$city)))
             ),
             sidebarPanel(width = 2,
                          selectInput("year_filter", "Select Year:", choices = c("All",unique(df2$date[df2$date >= 2015 & df2$date <= 2020])))
             ),
             column(
               width = 8,
               tableOutput("stats_table_a"),
             ),
             column(width = 6,
                    plotlyOutput("tonnes_bar_chart")
             ), 
             column(width = 6,
                    plotlyOutput("pie_chart_a")
             )
             
    )
  )
)

# Server ------------------------------------------------------------------
# Define server
server <- function(input, output) {
  
  output$energy_plot <- renderPlot({
    # Filter the data based on user inputs
    selected_area <- input$council_area
    selected_year <- input$year
    filtered_data <- ec_final %>%
      filter(FeatureName == selected_area,
             Year == selected_year,
             Energy.Type != "All", Energy.Consuming.Sector == "All")
    
    # Create the ggplot2 plot
    ggplot(filtered_data, aes(x = Energy.Type, y = Value)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste("Energy Consumption in", selected_area),
           x = "Energy Type",
           y = "Consumption (GWh)") +
      theme_minimal()
  })
  
  output$blank_tab_plot <- renderPlot({
    # Filter the data based on user inputs
    selected_city <- input$city
    selected_year_range <- input$range
    filtered_data <- ec_final %>%
      filter(FeatureName == selected_city,
             Year >= selected_year_range[1],
             Year <= selected_year_range[2],
             Energy.Type != "All", Energy.Consuming.Sector == "All")
    
    # Create the ggplot2 plot
    ggplot(filtered_data, aes(x = Year, y = Value, color = Energy.Type, 
                              group = Energy.Type)) +
      geom_line() +
      geom_point() +
      labs(title = paste("Change of Energy Consumption in", selected_city),
           x = "Year",
           y = "Consumption (GWh)",
           fill = "Energy Type") +
      theme_minimal()
  })
  
  output$ec_final_table <- DT::renderDataTable({
    DT::datatable(ec_final, options = list(scrollX = TRUE))
  })
  
  output$idr_table <- DT::renderDataTable({
    DT::datatable(idr, options = list(scrollX = TRUE))
  })
  
  output$ec_final_summary <- renderPrint({
    # Calculate and display summary statistics for energy consumption
    summary(ec_final$Value)
  })
  
  output$idr_summary <- renderPrint({
    # Calculate and display summary statistics for infant mortality rate, deaths, and births
    mortality_summary <- summary(idr$Mortality.Rate)
    deaths_summary <- summary(idr$Deaths)
    births_summary <- summary(idr$Births)
    
    cat("Infant Mortality Rate:\n")
    print(mortality_summary)
    
    cat("\nDeaths:\n")
    print(deaths_summary)
    
    cat("\nBirths:\n")
    print(births_summary)
  })
  
  observeEvent(input$calculate_button, {
    req(input$energy_type)
    
    # Filter the data based on user inputs and desired energy type
    filtered_data <- mean_energy %>%
      filter(Energy.Type == input$energy_type)
    
    # Calculate the correlation between consumption and mean infant mortality rate
    correlation <- cor(filtered_data$mean_consumption, mean_mortality$mean_mortality_rate)
    
    #Prepare data for plotting
    plot_data <- right_join(mean_mortality, filtered_data, by = "FeatureName")
    
    if (input$linear_regression) {
      # Perform linear regression
      lm_model <- lm(mean_mortality$mean_mortality_rate ~ filtered_data$mean_consumption)
      
      # Prepare data for plotting regression line
      regression_line <- data.frame(mean_consumption = filtered_data$mean_consumption,
                                    predicted_mortality = predict(lm_model))
      # Create the scatter plot with regression line
      scatter_plot <- ggplot() +
        geom_point(data = plot_data, aes(x = mean_consumption, y = mean_mortality_rate)) +
        geom_line(data = regression_line, aes(x = mean_consumption, y = predicted_mortality), color = "red") +
        labs(title = paste("Scatter Plot with Linear Regression: Consumption vs. Infant Mortality Rate"),
             x = "Mean Consumption (GWh)",
             y = "Mean Infant Mortality Rate") +
        theme_minimal()
    } else {
      # Create the scatter plot without regression line
      scatter_plot <- ggplot(plot_data, aes(x = mean_consumption, y = mean_mortality_rate)) +
        geom_point() +
        labs(title = paste("Scatter Plot: Consumption vs. Infant Mortality Rate"),
             x = "Mean Consumption (GWh)",
             y = "Mean Infant Mortality Rate") +
        theme_minimal()
    }
    
    # Output the scatter plot, correlation, and model information
    output$scatter_plot <- renderPlot({
      scatter_plot
    })
    
    output$correlation_output <- renderPrint({
      paste("Correlation:", correlation)
    })
    
    output$model_info <- renderPrint({
      if (input$linear_regression) {
        req(input$calculate_button)
        if (exists("lm_model")) {
          summary(lm_model)
        } else {
          "No linear regression model found. Please make sure to input valid data and push the 'Calculate' button."
        }
      } else {
        "No model information available. Linear regression is not selected."
      }
    })
  })
  
  # Code Merging Start ------------------------------------------------------
  ##Akgun
  output$cor_output <- renderPrint({
    cor_value <- cor(super[[input$x]], super[[input$y]])
    paste("Correlation between", input$x, "and", input$y, "is", round(cor_value,2))
  })
  
  output$scatter_plot_insight <- renderPlot({
    ggplot(super, aes(x = .data[[input$x]], y = .data[[input$y]])) +
      geom_point(color = "red") +
      labs(x = input$x, y = input$y, title = "Scatter Plot") + 
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_smooth(method = "lm", se = FALSE)
  })
  ##
  output$scatterPlot <- renderPlot({
    x<-input$factor1
    y<-input$factor2
    plot_data<-ggplot(data=newlist,aes_string(x=x,y=y))+geom_point()+geom_smooth(method = "lm", se = F,linewidth=1,color="black")
    print(plot_data)
  })
  observeEvent(input$calculate, {
    factor1 <- input$factor1
    factor2 <- input$factor2
    
    factor1_data <- newlist[, factor1]
    factor2_data <- newlist[, factor2]
    
    correlation <- cor(factor1_data, factor2_data)
    
    output$correlationOutput <- renderPrint({
      paste("Correlation: ", correlation)
    })
  })
  ##Kadir
  filtered_unemployment_data <- reactive({
    unemployment_data %>%
      filter(FeatureName == input$areaInput, DateCode == input$yearInput)
  })
  
  # Create the unemployment graph based on the selected type
  output$graphUnemployment_u <- renderPlot({
    if (input$graphTypeInput == "Histogram") {
      ggplot(filtered_unemployment_data(), aes(x = Age, y = Value)) +
        geom_bar(stat = "identity", fill = "red") +
        labs(x = "Age", y = "Unemployment Gap", title = "Unemployment Gap by Age (Histogram)") +
        theme_minimal()
    } else if (input$graphTypeInput == "Pie Chart") {
      ggplot(filtered_unemployment_data(), aes(x = "", y = Value, fill = Age)) +
        geom_bar(stat = "identity") +
        coord_polar("y") +
        labs(x = NULL, y = NULL, fill = "Age") +
        ggtitle("Unemployment Gap by Age (Pie Chart)") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set3")
    } else if (input$graphTypeInput == "Scatter Plot") {
      ggplot(filtered_unemployment_data(), aes(x = Age, y = Value)) +
        geom_point(color = "steelblue") +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(x = "Age", y = "Unemployment Gap", title = "Unemployment Gap by Age (Scatter Plot)") +
        theme_minimal()
    }
  })
  
  # Filter the energy consumption data based on user selections
  filtered_energy_data <- reactive({
    subset(allex_energy_data, FeatureName == input$council_area & DateCode == input$year)
  })
  
  # Render the energy consumption graph
  output$graphEnergy_u <- renderPlot({
    graph_title <- paste("Energy Consumption by Energy Type", input$year)
    
    if (input$graph_type == "Histogram") {
      ggplot(filtered_energy_data(), aes(x = Energy.Type, y = Value)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(x = "Energy Type", y = "Energy Consumption") +
        ggtitle(graph_title) +
        theme_minimal()
      
    } else if (input$graph_type == "Pie Chart") {
      ggplot(filtered_energy_data(), aes(x = "", y = Value, fill = Energy.Type)) +
        geom_bar(stat = "identity") +
        coord_polar("y") +
        labs(x = NULL, y = NULL, fill = "Energy Type") +
        ggtitle(graph_title) +
        theme_minimal()
    }
  })
  
  # Filter data based on user inputs and calculate correlation
  selected_data <- reactive({
    filtered_energy <- allex_energy_data %>%
      filter(FeatureName == input$council_area_corr) %>%
      group_by(DateCode) %>%
      summarise(mean_energy = mean(Value))
    
    filtered_unemployment <- unemployment_data %>%
      filter(FeatureName == input$council_area_corr) %>%
      group_by(DateCode) %>%
      summarise(mean_unemployment = mean(Value))
    
    correlation <- cor(filtered_unemployment$mean_unemployment, filtered_energy$mean_energy)
    
    list(mean_unemployment = filtered_unemployment, mean_energy = filtered_energy, correlation = correlation)
  })
  
  # Render the correlation plot
  output$correlation_plot_u <- renderPlot({
    data <- selected_data()
    
    plot(data$mean_unemployment$mean_unemployment, data$mean_energy$mean_energy,
         xlab = "Mean Unemployment Gap", ylab = "Mean Energy Consumed",
         main = "Correlation Plot")
  })
  
  # Filter the data based on user selections for analysis
  filtered_unemployment_analysis <- reactive({
    filter(unemployment_data, FeatureName == input$council_area_analysis, DateCode == input$year_analysis)
  })
  
  filtered_energy_analysis <- reactive({
    filter(allex_energy_data, FeatureName == input$council_area_analysis, DateCode == input$year_analysis)
  })
  
  # Perform statistical analysis on unemployment data
  output$unemployment_stats_u <- renderPrint({
    summary(filtered_unemployment_analysis()$Value)
  })
  
  # Perform statistical analysis on energy consumption data
  output$energy_stats_u <- renderPrint({
    summary(filtered_energy_analysis()$Value)
  })
  
  # Create a plot for unemployment gaps
  output$unemployment_plot_u <- renderPlot({
    ggplot(filtered_unemployment_analysis(), aes(x = Age, y = Value)) +
      geom_bar(stat = "identity", fill = "red") +
      labs(x = "Age", y = "Unemployment Gap", title = "Unemployment Gap by Age")
  })
  
  # Create a plot for energy consumption
  output$energy_plot_u <- renderPlot({
    ggplot(filtered_energy_analysis(), aes(x = Energy.Type, y = Value)) +
      geom_bar(stat = "identity", fill = "orange") +
      labs(x = "Energy Type", y = "Energy Consumption", title = "Energy Consumption by Type")
  })
  
  
  #-----------------------------------------------
  
  data_uu <- reactive({
    df_generated_waste <- df2 %>%
      filter(waste == "Waste Generated") %>%
      group_by(city, date) %>%
      summarise(generated_waste = sum(tonnes))
    df_filtered <- df2
    if (input$city_selector != "All") {
      df_filtered <- df_filtered %>% filter(city == input$city_selector)
    }
    
    if (input$year_filter != "All") {
      df_filtered <- df_filtered %>% filter(date == input$year_filter)
    }
    
    df_merged <- merge(df_filtered, df1)
    df_merged <- left_join(df_merged, df_generated_waste)
    df_merged <- df_merged %>%
      mutate(waste_ratio = tonnes / generated_waste) %>%
      group_by(waste) %>% filter(date >= 2015, date <= 2020) 
    df_merged
  })
  
  
  
  
  
  filtered_data_yy <- reactive({
    df_generated_waste <- df2 %>%
      filter(waste == "Waste Generated") %>%
      group_by(city, date) %>%
      summarise(generated_waste = sum(tonnes))
    
    if (input$city_filter != "All" && input$waste_filter != "All") {
      df_filtered <- df2 %>%
        filter(waste == input$waste_filter, city == input$city_filter) 
      
      df_merged <- merge(df_filtered, df1)
      df_merged <- left_join(df_merged, df_generated_waste)
      df_merged <- df_merged %>%
        mutate(waste_ratio = tonnes / generated_waste)
    } else if (input$city_filter != "All") {
      df_filtered <- df2 %>%
        filter(city == input$city_filter)
      df_merged <- merge(df_filtered, df1)
      df_merged <- left_join(df_merged, df_generated_waste)
      df_merged <- df_merged %>%
        mutate(waste_ratio = tonnes / generated_waste)
    } else if (input$waste_filter != "All") {
      df_filtered <- df2 %>%
        filter(waste == input$waste_filter)
      df_merged <- merge(df_filtered, df1)
      df_merged <- left_join(df_merged, df_generated_waste)
      df_merged <- df_merged %>%
        mutate(waste_ratio = tonnes / generated_waste)
    } else {
      df_merged <- merge(df_filtered, df1)
      df_merged <- left_join(df_merged, df_generated_waste)
      df_merged <- df_merged %>%
        mutate(waste_ratio = tonnes / generated_waste)
    }
    df_merged <- arrange(df_merged, date) 
    df_merged <- filter(df_merged, date >= input$date_filter[1] & date <= input$date_filter[2])
    df_merged
  })
  
  output$lineplot_a <- renderPlotly({
    data <- filtered_data_yy()
    mortality_trace <- plot_ly(data, x = ~date, y = ~rate, type = 'scatter', mode = 'lines', name = "Mortality Rate")
    birth_trace <- plot_ly(data = data, x = ~date, y = ~births, type = "scatter", mode = "lines", name = "Births")
    death_trace <- plot_ly(data = data, x = ~date, y = ~deaths, type = "scatter", mode = "lines", name = "Deaths")
    plot_data <- subplot(mortality_trace, birth_trace, death_trace, nrows = 3, shareX = TRUE) %>%  layout(
      title = paste("Infant Mortailty", "- City:", input$city_filter),
      xaxis = list(title = "Year"),
      yaxis = list(title = "Infant Mortality"))
  })
  
  output$tonnes_plot <- renderPlotly({
    data <- filtered_data_yy()
    tones_rate <- plot_ly(data, x = ~date, y = ~waste_ratio, type = 'scatter', mode = 'lines', name = "Ratio")
    tonnewaste <- plot_ly(data = data, x = ~date, y = ~generated_waste, type = "scatter", mode = "lines", name = "Generated Waste")
    tonnesa <- plot_ly(data = data, x = ~date, y = ~tonnes, type = "scatter", mode = "lines", name = "Waste amount in tonnes")
    plot_data <- subplot(tones_rate, tonnewaste, tonnesa, nrows = 3, shareX = TRUE) %>%  layout(
      title = paste("Waste in", "- City:", input$city_filter),
      xaxis = list(title = "Year"),
      yaxis = list(title = "Waste"))
  })
  
  output$data_plot_a <- renderPlot({
    data <- filtered_data_yy()
    
    lm_model <- lm(tonnes ~ rate, data = data)
    
    ggplot(data, aes(x = tonnes, y = rate)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      xlab("Tonnes") +
      ylab("Infant Mortality Rate") +
      ggtitle(paste("Waste Category:", input$waste_filter, "- City:", input$city_filter))
  })
  
  correlation <- reactive({
    
    df_generated_waste <- df2 %>%
      filter(waste == "Waste Generated") %>%
      group_by(city, date) %>%
      summarise(generated_waste = sum(tonnes))
    df_filtered <- df2 %>% 
      filter(waste == input$correlation_waste_filter)
    
    df_merged <- merge(df_filtered, df1)
    df_merged <- left_join(df_merged, df_generated_waste)
    df_merged <- df_merged %>%
      mutate(waste_ratio = tonnes / generated_waste)
    cor(df_merged$rate,df_merged$waste_ratio)
  })
  
  output$scatter_plot_a <- renderPlotly({
    df_generated_waste <- df2 %>%
      filter(waste == "Waste Generated") %>%
      group_by(city, date) %>%
      summarise(generated_waste = sum(tonnes))
    df_filtered <- df2 %>% 
      filter(waste == input$correlation_waste_filter)
    
    df_merged <- merge(df_filtered, df1)
    df_merged <- left_join(df_merged, df_generated_waste)
    df_merged <- df_merged %>%
      mutate(waste_ratio = tonnes / generated_waste)
    plot_ly(df_merged, x = ~waste_ratio, y = ~rate, type = "scatter", mode = "markers") %>%
      layout(xaxis = list(title = "Waste type ratio"), yaxis = list(title = "Infant Mortality Rate"))
  })
  
  output$correlation_output_a <- renderPrint({
    paste("Correlation:", correlation())
  })
  
  filtered_data_bc <- reactive({
    df_filtered <- df2
    
    if (input$city_selector != "All") {
      df_filtered <- df_filtered %>% filter(city == input$city_selector)
    }
    
    if (input$year_filter != "All") {
      df_filtered <- df_filtered %>% filter(date == input$year_filter)
    }
    
    df_grouped <- df_filtered %>%
      group_by(waste) %>% filter(date >= 2015, date <= 2020)
    df_grouped
  })
  
  output$tonnes_bar_chart <- renderPlotly({
    data_bc <- filtered_data_bc()
    plot_ly(data_bc, x = ~waste, y = ~tonnes, type = 'bar') %>%
      layout(xaxis = list(title = "Waste Category"),
             yaxis = list(title = "Tonnes"),
             title = paste("Tonnes Categorized by Waste Type - Year:", input$year_filter, "- City:", input$city_selector),
             height = 600)  # Set the desired height here
  })
  
  output$pie_chart_a <- renderPlotly({
    data_bc <- data_uu() %>%
      filter(waste != "Waste Generated")
    
    plot_ly(data_bc, labels = ~waste, values = ~tonnes, type = "pie") %>%
      layout(title = "Tonnes by Waste Category",
             height = 500,  
             width = 500 )
  })
  
  descriptive_stats <- reactive({
    data <- filtered_data_bc()
    
    stats <- data %>%
      group_by(waste) %>%
      summarise(
        Mean = mean(tonnes, na.rm = TRUE),
        Median = median(tonnes, na.rm = TRUE),
        SD = sd(tonnes, na.rm = TRUE),
        Min = min(tonnes, na.rm = TRUE),
        Q1 = quantile(tonnes, probs = 0.25, na.rm = TRUE),
        Q3 = quantile(tonnes, probs = 0.75, na.rm = TRUE),
        Max = max(tonnes, na.rm = TRUE),
        Count = n()
      )
    
    stats
  })
  
  
  output$stats_table_a <- renderTable({
    descriptive_stats()
  })
  
  
  
  output$data_table_a <- renderDataTable({
    df_filtered <- df2 %>% 
      filter(waste == input$waste_filter_data, city == input$city_filter_data)
    df_merged <- merge(df_filtered, df1)
    df_merged <- arrange(df_merged, date)
    df_merged <- filter(df_merged, date >= input$date_filter_data[1] & date <= input$date_filter_data[2])
    df_merged
  })
  
  
  
  
  filtered_data_map <- reactive({
    df_filtered <- df2
    
    if (input$waste_selector != "All") {
      df_filtered <- df_filtered %>% filter(waste == input$waste_selector)
    }
    
    if (input$year_selector != "All") {
      df_filtered <- df_filtered %>% filter(date == input$year_selector)
    }
    df_filtered
  })
  
  
  output$waste_map <- renderLeaflet({
    datau <- filtered_data_map()
    data <- merge(datau,df_locations)
    color_palette <- colorNumeric(
      palette = "RdYlBu",
      domain = data$tonnes,
      reverse = TRUE
    )
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        color = ~color_palette(tonnes),
        fillColor = ~color_palette(tonnes),
        fillOpacity = 0.9,
        radius = 8,
        stroke = FALSE,
        label = ~paste(waste, tonnes),
        labelOptions = labelOptions(noHide = TRUE),
        options = markerOptions(cursor = "pointer") 
      ) %>%
      addLegend(
        position = "bottomright",
        colors = color_palette(seq(0, 1, length.out = 6)),
        labels = seq(min(data$tonnes), max(data$tonnes), length.out = 6),
        title = "Tonnes"
      )})
  
  # Code Merging Finish -----------------------------------------------------
  
  # output$scotland_map <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%
  #     setView(lng = -4.2026, lat = 56.4907, zoom = 6)
  #   
  
  infant_mortality_percentage <- reactive({
    round(mean_mortality$mean_mortality_rate, 2)
  })
  
  output$gif_image <- renderImage({
    filename <- "a.gif"  
    list(src = filename, alt = "GIF image", style = "max-width: 100%; height: auto;")
  }, deleteFile = FALSE)
  
  output$map <- renderLeaflet({
    pal <- colorNumeric(palette = "Reds", domain = super[[input$input_var]])
    
    leaflet(super) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                       radius = 8, fillOpacity = 1.5,
                       fillColor = ~pal(super[[input$input_var]]),
                       stroke = FALSE,
                       label = ~paste("City:", FeatureName, " ",
                                      "Value:", round(super[[input$input_var]],2), " ", 
                                      sep = ""))
  })
  
  # output$scotland_map <- renderLeaflet({
  #   color_palette <- colorNumeric(palette = "Reds", domain = infant_mortality_percentage())
  #   
  #   leaflet() %>%
  #     addTiles() %>%
  #     setView(lng = -4.2026, lat = 56.4907, zoom = 6) %>%
  #     addCircleMarkers(
  #       data = coordinates,
  #       lng = ~Longitude,
  #       lat = ~Latitude,
  #       radius = 5,
  #       color = ~color_palette(infant_mortality_percentage()),
  #       fillOpacity = 1,
  #       popup = paste(
  #         "Council Area:",
  #         coordinates$council_area,
  #         "<br>",
  #         "Infant Mortality Rate (%):",
  #         infant_mortality_percentage()
  #       )
  #     ) %>%
  #     addLegend(
  #       "bottomright",
  #       title = "Infant Mortality Rate (%)",
  #       pal = colorNumeric(palette = "Reds", domain = infant_mortality_percentage()),
  #       values = infant_mortality_percentage()
  #     )
  # })
}

# Run the application
shinyApp(ui = ui, server = server)


