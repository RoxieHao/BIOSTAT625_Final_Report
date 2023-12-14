library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Load data
heartdisease <- read.csv("/Users/r.xi/Desktop/BIOS625/Final Project/hd_data_clean.csv", header = T)

con_descriptive_stat <- function(x){
  mean = mean(x)
  median = median(x)
  standard_deviation = sd(x)
  max = max(x)
  min = min(x)
  return(c(mean,median,standard_deviation,max,min))
}


cat_descriptive_stat <- function(x){
  value_counts <- table(x)
  mode <- as.numeric(names(value_counts)[value_counts == max(value_counts)])
  max = max(x)
  min = min(x)
  return(c(mode,max,min))
  
}


continuous_vars <- c("BMI","GeneralHealth", "PhysicalHealthDays", "MentalHealthDays","SleepHours")
categorical_vars <- c("Sex",
                      "LastCheckupTime",
                      "PhysicalActivities",
                      "HadHeartAttack",
                      "HadAngina",
                      "HadStroke",
                      "HadAsthma",
                      "HadDepressiveDisorder",
                      "HadKidneyDisease",
                      "HadDiabetes",
                      "DifficultyWalking",
                      "SmokerStatus",
                      "ChestScan",
                      "RaceEthnicityCategory",
                      "AgeCategory",
                      "AlcoholDrinkers",
                      "HighRiskLastYear",
                      "CovidPos")
## ui
ui <- fluidPage(
  titlePanel("Heart Disease data - Descriptive Statistics"),
  tabsetPanel(
    # Descriptive statistics tables
    tabPanel("Descriptive statistics",
             h2("Continuous variables"), DTOutput("continuous_table"),
             h2("Categorical variables"), DTOutput("categorical_table")),
    # plot
    tabPanel("Plots",
             selectInput("select_con", "Please select a continuous variable", choices = continuous_vars),
             plotOutput("con_plot"), 
             selectInput("select_cat", "Please select a categorical variable", choices = categorical_vars),
             plotOutput("cat_plot"))
  )
)

## server
server <- function(input, output) {
  
  ## discriptive statistics part
  # Descriptive statistics for continuous variables
  output$continuous_table <- renderDT({
    continuous_df <- heartdisease[, continuous_vars]
    descriptives <- data.frame(statistics = c("Mean", "Median", "Standard Deviation", "Max", "Min"),
                               sapply(continuous_df, con_descriptive_stat))
    datatable(descriptives, options = list(searching = FALSE, pageLength = nrow(descriptives)), rownames = FALSE)
  }, server = FALSE)
  
  # Descriptive statistics for categorical variables
  output$categorical_table <- renderDT({
    categorical_df <- heartdisease[, categorical_vars]
    descriptives <- data.frame(statistics = c("Mode", "Max","Min"),
                               sapply(categorical_df, cat_descriptive_stat))
    datatable(descriptives, options = list(searching = FALSE, pageLength = nrow(descriptives)), rownames = FALSE)
  }, server = FALSE)
  
  ## plot part
  output$con_plot <- renderPlot({
    selected_var <- input$select_con
    
    # Histogram
    p1 <- ggplot(heartdisease, aes_string(selected_var)) + geom_histogram() + theme_minimal() + labs(title = "Histogram", x = selected_var)
    
    # Boxplot
    p2 <- ggplot(heartdisease, aes_string(selected_var)) + geom_boxplot() + theme_minimal() + labs(title = "Boxplot", x = selected_var, y = "")
    
    grid.arrange(p1, p2, ncol = 2)
  })
  
  output$cat_plot <- renderPlot({
    selected_var <- input$select_cat
    
    # Barplot
    count_data <- heartdisease %>%
      group_by(.data[[selected_var]]) %>%
      summarise(Count = n())
    
    p1 <- ggplot(count_data, aes_string(selected_var, y = "Count")) + 
      geom_bar(stat="identity", fill="gray") + 
      geom_text(aes(label = Count), vjust = -0.5, size = 4) +
      theme_minimal() + 
      labs(title = "Barplot", x = selected_var, y = "Count")
    
    grid.arrange(p1)
  })
}

## create a shiny app
shinyApp(ui = ui, server = server)
