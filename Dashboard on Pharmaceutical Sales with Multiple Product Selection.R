# Install required packages if not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(plotly)) install.packages("plotly")
if (!require(DT)) install.packages("DT")
if (!require(lubridate)) install.packages("lubridate")
if (!require(scales)) install.packages("scales")

# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)
library(scales)

# Set the date range for the pharmaceutical sales data
start_date_pharma <- as.Date('2022-01-01')
end_date_pharma <- as.Date('2022-12-31')

# Set the random seed for reproducibility
set.seed(456)

# Generate pharmaceutical sales data
num_rows_pharma <- 1000
pharma_customer_ids <- seq(1, num_rows_pharma)
pharma_dates <- sample(seq(start_date_pharma, end_date_pharma, by = "1 day"), num_rows_pharma, replace = TRUE)
pharma_products <- sample(c("Aspirin", "Amoxicillin", "Lipitor", "Vitamin D","Fish Oil Capsules", "Ibuprofen","Ciprofloxacin","Simvastatin"), num_rows_pharma, replace = TRUE)
pharma_categories <- sample(c( "Pain Relief","Cholesterol Management","Vitamins"), num_rows_pharma, replace = TRUE)
pharma_prices <- sample(seq(5, 50, by = 5), num_rows_pharma, replace = TRUE)
pharma_quantities <- sample(1:30, num_rows_pharma, replace = TRUE)

# Add gender, age, and geographic data to the pharmaceutical sales dataset
pharma_gender <- sample(c("Male", "Female"), num_rows_pharma, replace = TRUE)
pharma_age <- sample(18:65, num_rows_pharma, replace = TRUE)
pharma_country <- sample(c("USA", "Canada", "UK"), num_rows_pharma, replace = TRUE)

# Create the pharmaceutical sales data frame
pharma_data <- data.frame(
  CustomerID = pharma_customer_ids,
  Date = pharma_dates,
  Product = pharma_products,
  Category = pharma_categories,
  Price = pharma_prices,
  Quantity = pharma_quantities,
  Gender = pharma_gender,
  Age = pharma_age,
  Country = pharma_country
)

# Define the mapping of products to categories for pharmaceutical sales
pharma_product_category_mapping <- c(
  "Aspirin" = "Pain Relief",
  "Ibuprofen" = "Pain Relief",
  "Amoxicillin" ="Antibiotics" ,
  "Ciprofloxacin" = "Antibiotics",
  "Lipitor" = "Cholesterol Management",
  "Simvastatin"= "Cholesterol Management",
  "Vitamin D" = "Vitamins",
  "Fish Oil Capsules" = "Vitamins"
)


pharma_data <- pharma_data %>%
  mutate(
    Total_Sales = Price * Quantity,
    Month = month(Date),
    Quarter = lubridate::quarter(Date),
    Category = pharma_product_category_mapping[Product]
  )

# If necessary, convert the numeric month values to month abbreviations
if (all(pharma_data$Month %in% 1:12)) {
  pharma_data$Month <- month.abb[pharma_data$Month]
} else {
  print("Invalid month values in the Month column.")
}

# Calculate total pharmaceutical sales statistics
total_sales_pharma <- sum(pharma_data$Price * pharma_data$Quantity)
total_customers_pharma <- n_distinct(pharma_data$CustomerID)
total_products_pharma <- n_distinct(pharma_data$Product)
total_categories_pharma <- n_distinct(pharma_data$Category)

# Define a custom function to update the summary data
updateSummary <- function(data) {
  total_customers <- n_distinct(data$CustomerID)
  total_products <- n_distinct(data$Product)
  total_categories <- n_distinct(data$Category)
  total_sales <- sum(data$Price * data$Quantity)
  list(customers = total_customers, products = total_products, categories = total_categories, revenue = total_sales)
}

# Create a list of quarters
quarters <- 1:4

# Define ordered month levels
ordered_month_levels <- month.abb[1:12]


# Update your UI definition to include the desired layout
ui <- dashboardPage(
  dashboardHeader(title = "Pharmaceutical Sales Dashboard"),
  dashboardSidebar(
    selectInput("filter_month", "Select Month:", choices = c("All", unique(pharma_data$Month))),
    selectizeInput("filter_product", "Select Product:", choices = c("All", unique(pharma_data$Product)), multiple = TRUE),
    # Add the quarters control
    selectInput("filter_quarter", "Select Quarter:", choices = c("All", quarters))
  ),
  dashboardBody(
    tabsetPanel(
      # Dashboard tab
      tabPanel("Dashboard",
               fluidRow(
                 infoBox("Customers", textOutput("customer_summary"), icon = icon("users"),width = 3),
                 infoBox("Products", textOutput("product_summary"), icon = icon("shopping-cart"),width = 3),
                 infoBox("Categories", textOutput("category_summary"), icon = icon("th-large"),width = 3),
                 infoBox("Total Revenue", textOutput("revenue_summary"), icon = icon("usd"),width = 3.5)
               ),
               fluidRow(
                 # First row with 2 plots
                 box(
                   title = "Monthly Sales",
                   status = "primary",
                   solidHeader = TRUE,
                   width = 6,
                   plotlyOutput("monthly_sales_plot")
                 ),
                 box(
                   title = "Product Distribution",
                   status = "warning",
                   solidHeader = TRUE,
                   width = 6,
                   plotlyOutput("product_distribution_plot")
                 )
               ),
               fluidRow(
                 # Second row with 2 plots
                 box(
                   title = "Gender Distribution",
                   status = "info",
                   solidHeader = TRUE,
                   width = 6,
                   plotlyOutput("gender_distribution_plot")
                 ),
                 box(
                   title = "Category Sales by Gender",
                   status = "success",
                   solidHeader = TRUE,
                   width = 6,
                   plotlyOutput("category_sales_gender_plot")
                 )
               ),
               fluidRow(
                 # Third row with 1 plot
                 box(
                   title = "Category Sales by Age",
                   status = "danger",
                   solidHeader = TRUE,
                   width = 12,  # Full width for the single plot
                   plotlyOutput("category_sales_age_plot")
                 )
               ),
               fluidRow(
                 # Fourth row with 1 plot
                 box(
                   title = "Sales Distribution by Country",
                   status = "info",
                   solidHeader = TRUE,
                   width = 6,  
                   plotlyOutput("sales_distribution_pie")
                 )
               )
      ),
      # Tables tab
      tabPanel(" Sample Table",
               box(
                 title = "Pharmaceutical Sales Data",
                 status = "info",
                 solidHeader = TRUE,
                 width = 12,
                 DTOutput("pharma_data_table")
               )
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    filtered <- pharma_data
    if (input$filter_month != "All") {
      filtered <- filtered %>% filter(Month == input$filter_month)
    }
    if (!is.null(input$filter_product) && "All" %in% input$filter_product) {
      # Keep all products
    } else if (!is.null(input$filter_product)) {
      filtered <- filtered %>% filter(Product %in% input$filter_product)
    }
    if (input$filter_quarter != "All") {  # Use input$filter_quarter here
      filtered <- filtered %>% filter(Quarter == as.integer(input$filter_quarter))
    }
    filtered
  })
  
  # Calculate and update summary data
  summary_data <- reactive({
    data <- filtered_data()
    summary_stats <- updateSummary(data)
    summary_stats
  })
  
  # Update text for summary boxes
  output$customer_summary <- renderText({
    paste(summary_data()$customers, "Customers")
  })
  output$product_summary <- renderText({
    paste(summary_data()$products, "Products")
  })
  output$category_summary <- renderText({
    paste(summary_data()$categories, "Categories")
  })
  output$revenue_summary <- renderText({
    paste("$", format(summary_data()$revenue, big.mark = ","), "Total Revenue")
  })
  
  # Render monthly sales plot with Plotly
  output$monthly_sales_plot <- renderPlotly({
    monthly_sales <- filtered_data() %>%
      group_by(Month) %>%
      summarise(TotalSales = sum(Price * Quantity)) %>%
      arrange(factor(Month, levels = ordered_month_levels))  # Reorder factor levels here
    
    # Create a Plotly plot
    plot_ly(data = monthly_sales, x = ~factor(Month, levels = ordered_month_levels), y = ~TotalSales, type = 'scatter', mode = 'lines+markers', name = 'Total Sales') %>%
      layout(
        title = "Monthly Sales Trend",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Total Sales", tickformat = "$,.0f")  # Format ticks as dollars
      )
  })
  
  
  
  # Render product distribution plot with ggplotly
  output$product_distribution_plot <- renderPlotly({
    product_distribution <- pharma_data %>%
      group_by(Category, Product) %>%
      tally() %>%
      mutate(Category = factor(Category, levels = c("Pain Relief", "Antibiotics", "Cholesterol Management", "Vitamins")),
             Product = factor(Product))
    
    p <- ggplot(product_distribution, aes(x = Category, y = n, fill = Product)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.8) +  # Adjust width as needed
      labs(title = "Product Distribution Across Categories",
           x = "Category",
           y = "Number of Products") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  
  # Render gender distribution plot with Plotly
  output$gender_distribution_plot <- renderPlotly({
    gender_distribution <- filtered_data() %>%
      group_by(Gender) %>%
      tally()
    
    plot_ly(data = gender_distribution, labels = ~Gender, values = ~n, type = 'pie') %>%
      layout(title = "Gender Distribution of Customers")
  })
  
  
  # Render category sales by gender plot with Plotly
  output$category_sales_gender_plot <- renderPlotly({
    gender_cat <- filtered_data() %>%
      group_by(Gender, Category) %>%
      summarise(totalsales = sum(Price * Quantity), .groups = "drop")
    
    plot_ly(data = gender_cat, x = ~Gender, y = ~totalsales, type = 'bar', color = ~Category, text = ~Category) %>%
      layout(
        title = "Category Sales by Gender",
        xaxis = list(title = ""),
        yaxis = list(title = "Total Sales", tickformat = "$,.0f"),  # Format ticks as dollars
        barmode = 'group'
      )
  })
  
  
  
# Render category sales by age plot with ggplot2 - Faceted Line Plot
  output$category_sales_age_plot <- renderPlotly({
    age_cat <- filtered_data() %>%
      group_by(Age, Category) %>%
      summarise(totalsales = sum(Price * Quantity), .groups = "drop")
    
    # Create a faceted line plot
    plot <- ggplot(age_cat, aes(x = Age, y = totalsales, color = Category, group = Category)) +
      geom_line() +
      facet_wrap(~Category, scales = "free_y", ncol = 1) +  # Adjusted ncol to 1
      labs(title = "Category Sales by Age", x = "Age", y = "Total Sales") +
      scale_y_continuous(labels = scales::dollar, breaks = scales::pretty_breaks(10)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
    
    # Convert ggplot to Plotly
    ggplotly(plot)
  })
  
  
  
  
  # Render pie chart for sales distribution by country with Plotly
  output$sales_distribution_pie <- renderPlotly({
    country_sales <- filtered_data() %>%
      group_by(Country) %>%
      summarise(TotalSales = sum(Price * Quantity))
    
    plot_ly(data = country_sales, labels = ~Country, values = ~TotalSales, type = 'pie') %>%
      layout(title = "Sales Distribution by Country")
  })
  
  #Render Sales Data Table
  output$pharma_data_table <- renderDT({
    formatted_data <- filtered_data() 
    
    datatable(
      formatted_data,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = list('copy', 'csv', 'excel', 'pdf', 'print', 'pageLength'),
        lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
        scrollX = TRUE
      ),
      class = 'display nowrap compact',
      rownames = FALSE,
      width = 100
    )
  })
  
}

# Run the Shiny app
shinyApp(ui, server)
