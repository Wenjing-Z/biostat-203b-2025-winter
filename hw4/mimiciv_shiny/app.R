library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(bigrquery)
library(tidyr)

# Load preprocessed cohort data
cohort_data <- readRDS("mimic_icu_cohort.rds")
# 

# UI: Two-tab structure
ui <- fluidPage(
  titlePanel("ICU Cohort Explorer"),
  tabsetPanel(
    # Tab 1: Cohort Summary
    tabPanel("Cohort Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var", "Select Variable", choices = names(cohort_data), selected = "age"),
                 radioButtons("plot_type", "Plot Type", choices = c("Histogram", "Boxplot"))
               ),
               mainPanel(
                 plotOutput("summaryPlot"),
                 tableOutput("summaryTable")
               )
             )
    ),
    
    # Tab 2: Individual Patient Lookup
    tabPanel("Patient Lookup",
             sidebarLayout(
               sidebarPanel(
                 selectInput("subject_id", "Select Patient", choices = unique(cohort_data$subject_id)),
                 radioButtons("plot_type", "Plot Type", choices = c("ADT", "ICU"))
               ),
               mainPanel(
                 h3("Patient ADT & ICU Stay Information"),
                 tableOutput("adt_table")
               )
             )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Tab 1: Generate summary plot
  output$summaryPlot <- renderPlot({
    req(input$var)
    ggplot(cohort_data, aes_string(x = input$var)) +
      {
        if (input$plot_type == "Histogram") {
          geom_histogram(bins = 30, fill = "steelblue", color = "black")
        } else {
          geom_boxplot(fill = "orange")
        }
      } +
      theme_minimal() +
      labs(title = paste(input$plot_type, "of", input$var))
  })
  
  # Tab 1: Numerical summary table
  output$summaryTable <- renderTable({
    req(input$var)
    cohort_data %>%
      summarise(
        Mean = mean(.data[[input$var]], na.rm = TRUE),
        Median = median(.data[[input$var]], na.rm = TRUE),
        SD = sd(.data[[input$var]], na.rm = TRUE),
        Min = min(.data[[input$var]], na.rm = TRUE),
        Max = max(.data[[input$var]], na.rm = TRUE)
      )
  })
  
  # Tab 2: Query patient ADT & ICU stay from BigQuery
  
  # Tab 2: Generate summary plot
  output$adt_table <- renderPlot({
    req(input$patient_id)
    patient_id <- as.numeric(input$patient_id)
    patient_data <- cohort_data %>%
      filter(subject_id == patient_id) %>%
      select(subject_id,stay_id, charttime, valuenum, 
             hear_rate, no_invasive_blood_pressure_systolic, 
             invasive_blood_pressure_diastolic, respiratory_rate, 
             temperature_fahrenheit)  # 选择相关列))
    ggplot(cohort_data, aes_string(x = input$var)) +
      {
        if (input$plot_type == "ADT") {
          ggplot(data = patient_data, aes(x = charttime, y = valuenum, color = abbreviation))+
            geom_line() +
            geom_point() +
            facet_grid(abbreviation ~ stay_id,scale = "free_x") +
            scale_x_datetime(date_labels = "%b %d %H:%M") +
            labs(
              title = paste("Patient,", input$patient_id, "ICU Stays -
 Vitals"), 
              x = NULL,
              y = NULL, 
              color = "Vital Type"
            ) +
            theme_minimal()
        } 
        else {
          geom_boxplot(fill = "orange")
        }
      } +
      theme_minimal() +
      labs(title = paste(input$plot_type, "of", input$var))
  })
}      
# Run the app
shinyApp(ui, server)
  
  # output$adt_table <- renderTable({
  #   req(input$patient_id)
  #   
  #   patient_id <- as.numeric(input$patient_id)
    
    # patient_data <- cohort_data %>%
    #   filter(subject_id == patient_id) %>%
    #   select(subject_id, hadm_id, stay_id, intime, outtime)  # 选择相关列
    
    
  #   patient_data <- filter(cohort_data, subject_id == patient_id)  # 使用实际数据筛选
  #   
  #   # 例如：绘制患者的血糖水平变化（示例）
  #   plot(patient_data$anchor_age, patient_data$glucose, type = "o", 
  #        xlab = "Time", ylab = "Glucose Level", main = paste("Patient", patient_id, "Glucose Level"))
  # })
  #   
    # observe({
    #   updateSelectizeInput(session, "patient_id",
    #                        choices = unique(cohort_data$subject_id),
    #                        server = TRUE)
    # })
  #   req(nrow(patient_data) > 0)
  #   patient_data
  # })
#}


