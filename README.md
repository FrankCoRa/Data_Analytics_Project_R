# Individual Housing Expenditure Project (R Studio)
Housing Expenditures Insights via R Shiny Dashboards
To see the Project Description and Milestones follow this link
# Data Extraction:
All the data was extracted from Data dictionary for the CE Public Use Microdata (PUMD)
Consumer Expenditure Surveys database: Diary survey 2021 – 2022
https://www.bls.gov/cex/pumd.htm
# Joining Information 
We extracted key housing features from the PUMD dataset, using the accompanying dictionary to interpret variable meanings accurately. After filtering for the years 2021 and 2022, we downloaded crucial datasets such as "NEWID," "PROP_NOG," "QRFINDTG," "FRSTPYMG," "QNEWDATG," and "LOAN_NOG," along with tables like "hel_table.csv," "mor_table.csv," "rnt_table.csv," and "utc_table.csv." These datasets contain valuable information on rent, housing expenses, mortgages, income, age, sex, and other variables critical to our research analysis.
# Gathering Databases
## Joining and Data Cleaning Code
```r
# Install and load required package
install.packages("dplyr")
library(dplyr)

# Import all CSV files and combine them into a single table
file_paths <- list.files("file_path", pattern = "fmli.*.csv", full.names = TRUE)
data_list <- lapply(file_paths, read.csv)
combined_table <- bind_rows(data_list)
write.csv(combined_table, file = "mother_data.csv", row.names = FALSE)

# Convert specific columns to character type
columns_to_convert <- c("NEWID", "PROP_NOG", "QRFINDTG", "FRSTPYMG", "QNEWDATG", "LOAN_NOG")
hel22[columns_to_convert] <- lapply(hel22[columns_to_convert], as.character)

# Function to bind two tables and export as CSV
write_combined_csv <- function(table1, table2, filename) {
  combined_table <- bind_rows(table1, table2)
  write.csv(combined_table, file = filename, row.names = FALSE)
}

# Apply function to multiple table pairs
write_combined_csv(hel21, hel22, "hel_table.csv")
write_combined_csv(mor21, mor22, "mor_table.csv")
write_combined_csv(rnt21, rnt22, "rnt_table.csv")
write_combined_csv(utc21, utc22, "utc_table.csv")

# Select relevant variables from 'hel_table' and export as CSV
hel_data <- hel_table %>% select(QYEAR, NEWID, NEWMRTG, QMRTTRMG, ORGMRTG, 
                                 QESCROWG, QPRINM1G, QPRINM2G, QPRINM3G, 
                                 QADINT1G, QADINT2G, QADINT3G, FRSTPYMG, FRSTPYRG, VARRTEG)
write.csv(hel_data, file = "hel_data.csv", row.names = FALSE)

# Select relevant variables from 'mor_table' and export as CSV
mor_data <- mor_table %>% select(MRTPMTX, NEWID, NEWMRRT, ORGMRTX, QYEAR)
write.csv(mor_data, file = "mor_data.csv", row.names = FALSE)

# Select columns for pilot dataset
pilot_01 <- mother_data %>% select(NEWID, AGE_REF, BATHRMQ, BEDROOMQ, EARNCOMP, POPSIZE, EDUC_REF, HIGH_EDU, 
                                   FAM_SIZE, FINCBTAX, FINDRETX, FSALARYM, FINCBTXM, FINATXEM, BUILT, 
                                   INC_HRS1, QINTRVYR, REF_RACE, RENTEQVX, QINTRVMO, SEX_REF, ROOMSQ, VEHQ, 
                                   STDNTYRX, STATE, LIQUDYRX, LIQUIDX, TOTEXPPQ, HOUSEQPQ, PROPTXPQ, ETOTALP, 
                                   CREDITX, UTILPQ, TRANSPQ, GASMOPQ, EHOUSNGP, FSTAXOWE, FFTAXOWE)

# Merge datasets sequentially
master_data <- pilot_01 %>%
  merge(mor_data, by = "NEWID", all.x = TRUE) %>%
  merge(hel_data, by = "NEWID", all.x = TRUE) %>%
  merge(rnt_data, by = "NEWID", all.x = TRUE)

# Rename columns
names(master_data)[names(master_data) == "Total_Income"] <- "TOTAL_INCOME"
names(master_data)[names(master_data) == "TOTEXPPQ"] <- "TOTAL_EXPENDITURE"
names(master_data)[names(master_data) == "ORGMRTX"] <- "AMOUNT_MORTGAGE"

# Remove rows with missing state values and export final dataset
master_data <- master_data[complete.cases(master_data$STATE), ]
write.csv(master_data, "master_data.csv", row.names = FALSE)

# Select relevant variables from 'rnt_table' and export as CSV
rnt_data <- rnt_table %>% select(QRT3MCMX, QYEAR, NEWID)
write.csv(rnt_data, file = "rnt_data.csv", row.names = FALSE)
```
# Exploratory Analysis
After merging and cleaning all relevant datasets, we are ready to perform exploratory data analysis (EDA) to identify key patterns and trends. Using visualizations and statistical summaries, we will uncover preliminary insights to guide further analysis. Below is the code used for the initial exploration:
```r
# Install & Load Required Packages
install.packages(c("plotrix", "viridis"))
library(dplyr)
library(ggplot2)
library(plotly)
library(plotrix)
library(viridis)

# Income Measures by State
Income_Result <- master_data %>%
  mutate(Individual_Income = TOTAL_INCOME / EARNCOMP) %>%
  group_by(STATE) %>%
  summarize(Avg_Individual_Income = mean(Individual_Income, na.rm = TRUE))

states_data <- data.frame(
  state_code = as.integer(c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "51", "53", "54", "55")),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Virginia", "Washington", "West Virginia", "Wisconsin"))

state_income <- left_join(Income_Result, states_data, by = c("STATE" = "state_code"))

# Plot Average Income by State
ggplot(state_income, aes(x = state_name, y = Avg_Individual_Income, fill = state_name)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("$%.2f", Avg_Individual_Income)), angle = 90, hjust = 1, vjust = 0.5) +
  labs(title = "Annual Average Individual Income by State", x = "State", y = "Avg Income") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Housing Expenses by State
Housing_Expenses <- master_data %>%
  group_by(STATE) %>%
  summarize(Avg_Annual_Housing = mean(EHOUSNGP*4, na.rm = TRUE))

Income_Housing <- left_join(state_income, Housing_Expenses, by = "STATE")
colnames(Income_Housing)[3:4] <- c("Avg_Annual_Income", "Avg_Annual_Housing")

# Plot Income vs Housing Expenses
ggplot(Income_Housing, aes(x = state_name)) +
  geom_bar(aes(y = Avg_Annual_Income, fill = "Income"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Avg_Annual_Housing, fill = "Housing"), stat = "identity", position = "dodge") +
  labs(title = "Annual Average Income & Housing Outlays by State", x = "State", y = "Values") +
  scale_fill_manual(values = c("Income" = "darkblue", "Housing" = "darkgreen")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

# Rent Comparison
Rent_Expenses <- master_data %>%
  filter(POPSIZE %in% c(1, 2)) %>%
  group_by(STATE) %>%
  summarize(Avg_Rent = mean(QRT3MCMX, na.rm = TRUE), Owner_Expected_Rent = mean(RENTEQVX, na.rm = TRUE))

Rent_by_State <- left_join(Rent_Expenses, states_data, by = c("STATE" = "state_code"))

# Plot Rent vs Owner's Expected Rent
ggplot(Rent_by_State, aes(x = state_name)) +
  geom_bar(aes(y = Avg_Rent, fill = "Avg Rent"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Owner_Expected_Rent, fill = "Expected Rent"), stat = "identity", position = "dodge") +
  labs(title = "State-wise Rent Analysis", x = "State", y = "Rent Amount") +
  scale_fill_manual(values = c("Avg Rent" = "blue", "Expected Rent" = "orange")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Loan Analysis
Loan_State <- master_data %>%
  group_by(STATE) %>%
  summarize(mean_MORTGAGE = mean(AMOUNT_MORTGAGE, na.rm = TRUE)) %>%
  left_join(states_data, by = c("STATE" = "state_code"))

# Treemap of Loans
plot_ly(
  type = "treemap",
  labels = Loan_State$state_name,
  values = Loan_State$mean_MORTGAGE,
  textinfo = "label+value",
  hoverinfo = "label+value"
) %>%
  layout(title = "Average Home Equity Loan by State")

# Education & Rent in New York
Education_Result <- master_data %>%
  filter(STATE == 36) %>%
  group_by(EDUC_REF) %>%
  summarize(count = n(), mean_rent = mean(RENTEQVX, na.rm = TRUE)) %>%
  left_join(data.frame(
    EDUC_REF = as.integer(0:19),
    education_name = c("Never attended", "Elementary", "High School", "Less than H.S.", "H.S. graduate", "Some College", "College Grad", "Post-Grad", "Other", rep("Suppressed", 10))
  ), by = "EDUC_REF")

# Pie Chart of Education Levels in NY
pie3D(
  Education_Result$count, 
  labels = sprintf("%s\n%.2f%%", Education_Result$education_name, Education_Result$count / sum(Education_Result$count) * 100), 
  main = "Education Distribution in NY", 
  col = viridis(length(Education_Result$education_name))
)

# Rent vs Education Levels
ggplot(Education_Result, aes(x = education_name, y = mean_rent, fill = as.factor(round(count / sum(count) * 100, 2)))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Mean Rent Across Education Levels in NY", x = "Education Level", y = "Mean Rent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Compute and plot correlation matrix for selected variables, handling missing values
library(corrplot)
master_data %>%
  select(TOTAL_INCOME, EHOUSNGP, EDUC_REF, RENTEQVX, AMOUNT_MORTGAGE, 
         STATE, QRT3MCMX, PROPTXPQ, FAM_SIZE, LIQUIDX, CREDITX, 
         HIGH_EDU, SEX_REF) %>%
  na.omit() %>%
  cor(method = "pearson") %>%
  corrplot(method = "color", type = "upper", order = "hclust",
           tl.col = "black", addCoef.col = "black", addCoef.size = 8)

```
# Data Analysis - Evaluation
With a clearer understanding of the variables needed for modeling individual housing expenditure, we will group the data into clusters and perform PCA analysis. This will help reduce the dataset to the most significant variables. To ensure equitable measurement, it is essential to standardize all variables beforehand.
## Clustering, Standarizing and PCA Analysis
```r
# Load required packages
install.packages("forecast")
library(forecast)
install.packages("cluster")
library(cluster)
install.packages("factoextra")
library(factoextra)

# Filter and preprocess data for New York 2023
data_ny_2023 <- master_data %>%
  filter(STATE == 36, QYEAR.x == 20231) %>%
  select(TOTAL_INCOME, EHOUSNGP, EDUC_REF, RENTEQVX, AMOUNT_MORTGAGE) %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Scale and cluster analysis
set.seed(1)
pam_result <- pam(scale(data_ny_2023), k = 5)
data_ny_2023 <- cbind(data_ny_2023, cluster = pam_result$clustering)

# Summary of clusters
tableny23 <- aggregate(data_ny_2023, by = list(cluster = data_ny_2023$cluster), mean)
print(tableny23)

# Selecting New York data for affordability analysis
afford_ny <- master_data %>%
  filter(STATE == 36, QYEAR.x == 20231) %>%
  select(TOTAL_INCOME, TOTAL_EXPENDITURE, AMOUNT_MORTGAGE)

afford_ny_scaled <- scale(afford_ny)

# Hierarchical clustering
d <- dist(afford_ny_scaled, method = "euclidean")
a_result <- agnes(d, diss = TRUE, method = "ward")
a_clusters <- cutree(a_result, k = 5)
afford_ny <- data.frame(afford_ny, a_clusters)

# Summary table by cluster
summary_afford_ny <- afford_ny %>%
  group_by(a_clusters) %>%
  summarize(across(c(TOTAL_INCOME, TOTAL_EXPENDITURE, AMOUNT_MORTGAGE), mean))

# PCA for state-dependent variable
pilot_data <- master_data %>%
  filter(POPSIZE == 2) %>%
  select(TOTAL_INCOME, EHOUSNGP, EDUC_REF, RENTEQVX, AMOUNT_MORTGAGE, QRT3MCMX, PROPTXPQ, FAM_SIZE, LIQUIDX, CREDITX, HIGH_EDU, SEX_REF) %>%
  mutate(across(everything(), ~replace_na(. + 1e-8, 0)))

standardized_data <- scale(pilot_data)
pca_result <- prcomp(standardized_data)
summary(pca_result)
biplot(pca_result)

```
# House Expenditure Index 
We identified the variables 'STATE, TOTAL_INCOME, EHOUSNGP, EDUC_REF, RENTEQVX, and AMOUNT_MORTGAGE,' which account for 80% of the significance. These variables enable us to construct an accurate multilinear regression equation based on our database, allowing us to estimate the housing expenditure ratio for participating states in the census.
## Key Housing Index Code
```r
# Fill Population Size of 2 (Individuals)
test_data <- master_data %>%
  filter(POPSIZE == 2) %>%
  select(STATE, TOTAL_INCOME, EHOUSNGP, EDUC_REF, RENTEQVX, AMOUNT_MORTGAGE)

# Linear regression analysis
regression_data <- master_data %>%
  filter(POPSIZE == 2) %>%
  mutate(ANNUAL_RENT_EQUIVALENT = RENTEQVX * 12) %>%
  select(QINTRVYR, STATE, TOTAL_INCOME, EHOUSNGP, ANNUAL_RENT_EQUIVALENT, AMOUNT_MORTGAGE, QRT3MCMX) %>%
  mutate(across(c(ANNUAL_RENT_EQUIVALENT, AMOUNT_MORTGAGE, QRT3MCMX), ~replace_na(., 0)))

# Grouping and regression
regression_result <- regression_data %>%
  group_by(QINTRVYR, STATE) %>%
  summarize(across(c(TOTAL_INCOME, EHOUSNGP, ANNUAL_RENT_EQUIVALENT, AMOUNT_MORTGAGE, QRT3MCMX), mean, na.rm = TRUE))

# Perform linear regression
lm_model <- lm(EHOUSNGP ~ TOTAL_INCOME + ANNUAL_RENT_EQUIVALENT + AMOUNT_MORTGAGE + QRT3MCMX, data = regression_result)
summary(lm_model)

# Scatter plot of actual vs predicted values
predicted_values <- predict(lm_model)
plot(regression_result$EHOUSNGP, predicted_values, main = "Actual vs. Estimated Housing Expenses", 
     xlab = "Actual Housing Values", ylab = "Annual Estimated Housing Expenses", pch = 16, col = "blue")
abline(a = 0, b = 1, col = "darkred")
legend("bottomright", legend = c("Actual", "Estimated Coefficients"),
       col = c("blue", "darkred"), pch = c(16, NA), lty = c(NA, 1))

# Fill NA values and calculate index
test_data <- test_data %>%
  mutate(across(c(RENTEQVX, AMOUNT_MORTGAGE), ~replace_na(., 0))) %>%
  mutate(index = rowSums(select(., TOTAL_INCOME, EHOUSNGP, EDUC_REF, RENTEQVX, AMOUNT_MORTGAGE) * 
                          c(0.52, 0.17, 0.13, 0.10, 0.07)))

# Ratio calculations
ratio_data <- test_data %>%
  group_by(STATE) %>%
  summarize(across(c(TOTAL_INCOME, EHOUSNGP, EDUC_REF, RENTEQVX, AMOUNT_MORTGAGE, index), mean, na.rm = TRUE)) %>%
  mutate(ratio_index = (index / sum(index)) * 100)
```
## Final Visualizations and Results: Ratio Map
Once the equation is complete, we proceed to compare the results using a dynamic map chart.
```r
states$StateCode <- as.integer(states$StateCode)
ratio_map <- left_join(ratio_data, states, by = c("STATE" = "StateCode"))

# Create a leaflet map
color_palette <- colorNumeric(palette = "YlOrRd", domain = ratio_map$ratio_index)
leaflet(ratio_map) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 10, fillColor = ~color_palette(ratio_index),
                   color = "black", weight = 1, opacity = 1, fillOpacity = 0.8,
                   popup = ~paste("State: ", StateName, "<br>Ratio Index: ", ratio_index)) %>%
  addLegend(position = "bottomright", pal = color_palette, values = ~ratio_index,
            title = "Ratio Index", opacity = 1)

```
# Shiny Application (R Shiny)
To create dynamic dashboards and storyboards similar to Tableau, we developed a web application using Shiny, an extension of RStudio. This requires three script pages: one for the visualization code and variables, another for the body and structure of the dashboards, and a final page to install the necessary packages for the application.
## Visuals Code (Server)
Here, we consolidate all the plotting code from previous steps, organizing it for easy access in the next script.
```r
function(input, output, session){
  #embebed web
  # Replace "https://example.com" with the URL of the web page you want to embed
  embeddedWebPageURL <- "/PUMD.html  
  output$embeddedWebPage <- renderUI({
    tags$iframe(src = embeddedWebPageURL, width = "100%", height = 600)
  })
  # Data table Output
  # data cleaning or preprocessing steps performed
  output$DataCleaningtext <- renderText({
    # Specify the file path relative to the project directory
    file_path <- "./datacleaning.txt"   
    # Check if the file exists
    if (file.exists(file_path)) {
      # Read the content of the file
      content <- readLines(file_path)
      formatted_content <- paste(content, collapse = "\n")
    } else {
      content <- "Error: File not found."      
    }
    return(formatted_content)
  }) 
  # Data table Output
  output$dataT <- renderDataTable(master_data)    
  # For Structure0 output
  output$structure0 <- renderPrint({
    mother_data %>% 
      str()
  })  
  # For Structure1 output
  output$structure1 <- renderPrint({
    master_data %>% 
      str()
  })  
  # For Summary Output
  output$summary <- renderPrint({
    master_data %>% 
      summary()
  })
  # Income Bar chart
  custom_palette <- scales::seq_gradient_pal(low = "#dad7cd", high = "#344e41")(seq(0, 1, length.out = 50))
  output$incomeplot <- renderPlotly({
    ggplot(state_income, aes(x = state_name, y = Avg_Individual_Income, fill = state_name)) +
      geom_bar(stat = "identity", color = "black") +
      labs(title = "Annual Average Individual Income by State",
           x = "State",
           y = "Average Individual Income") +
      scale_fill_manual(values = custom_palette) +  # Use the custom color palette
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
  })
  ### Housing Bar Chart
  output$housingplot <- renderPlotly({  
    Income_Housing_Plot <- ggplot(Income_Housing, aes(x = state_name)) +
      geom_bar(aes(y = Avg_Anual_Income), stat = "identity", position = "dodge", fill = "#606c38", alpha = 0.7) +
      geom_bar(aes(y = Avg_Anual_HousingExP), stat = "identity", position = "dodge", fill = "#283618", alpha = 0.7) +
      labs(title = "Annual Average Income and Housing Outlays by State",
           x = "State Name",
           y = "Values") +
      scale_fill_manual(values = c("#606c38", "#283618"), name = "Legend", labels = c("Avg_Anual_Income", "Avg_Anual_HousingExP")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_blank(),  # Remove y-axis labels
            legend.position = "top")  # Adjust legend position as needed
    # Convert ggplot to Plotly
    Income_Housing_Plot_Plotly <- ggplotly(Income_Housing_Plot)
    # Print the Plotly plot
    print(Income_Housing_Plot_Plotly)  
  })  
  ### Rent Bar Chart
  output$rentplot <- renderPlotly({   
    Type_Rent_Plot <- ggplot(Expected_Rent_by_State, aes(x = state_name)) +
      geom_bar(aes(y = Avg_Rent, fill = "Avg_Rent"), stat = "identity", position = "dodge", width = 0.7) +
      geom_bar(aes(y = Owner_Expected_Rent, fill = "Owner_Expected_Rent"), stat = "identity", position = "dodge", width = 0.7) +
      labs(title = "State-by-State Analysis of Average Rent for Markets and Landlords",
           y = "Rent Amount",
           x = "State") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c("Avg_Rent" = "#283618", "Owner_Expected_Rent" = "#606c38"),
                        name = "Rent Type",
                        labels = c("Average Rent", "Projected Landlord Rent"))
    # Convert ggplot to Plotly
    Type_Rent_Plotly <- ggplotly(Type_Rent_Plot)
    # Print the Plotly plot
    print(Type_Rent_Plotly)
  }) 
  ### Loan Treemap
  output$loanplot <- renderPlotly({ 
    plot_ly(
      type = "treemap",
      labels = State_Loan$state_name[1:50],
      parents = "",
      values = State_Loan$mean_MORTGAGE[1:50],
      hoverinfo = "label+value+text",
      text = paste("Average: \n", round(State_Loan$mean_MORTGAGE[1:50], 2)),
    ) %>%
      layout(
        title = "Average Home Equity Loan by State",
        showarrow = FALSE,
        x = 0.5,
        y = 0.5
      )
  })  
  ### 3D PIE CHART
  output$educationplot <- renderPlotly({    
    pie_data <- data.frame(
      labels = Education_Result$education_name,
      values = Education_Result$count
    )    
    plot_ly(
      data = pie_data,
      labels = ~labels,
      values = ~values,
      type = 'pie',
      textinfo = 'percent',  # Removed 'label' from textinfo
      marker = list(colors = colors),
      height = 600,
      width = 700
    ) %>%
      layout(
        title = "Distribution of Education during the 2021-2022 Academic Year in New York",
        legend = list(orientation = 'h'),  # Set orientation to 'h' for horizontal
        showlegend = TRUE
      )
  })  
  ### EDUCATION RENT PLOT
  output$education_rent_plot <- renderPlotly({      
      Education_Result %>%
        plot_ly(
          x = ~education_name,
          y = ~mean_rent,
          color = ~as.factor(paste(round(percentages, 2), "%")),
          type = "bar"
        ) %>%
        layout(
          title = "Comparison of Mean Rent Across Education Levels in New York",
          xaxis = list(title = "Education Level", tickangle = 45, tickmode = "array", tickvals = ~education_name),
          yaxis = list(title = "Average Rent"),
          showlegend = FALSE
        )
    })       
  ### CORRELATION PLOT 
  output$correlationplot <- renderPlotly({    
    heatmap_plot <- plot_ly(
      z = cor_matrix,
      x = colnames(cor_matrix),
      y = rownames(cor_matrix),
      type = "heatmap",
      colorscale = "RdBu",   # Adjust colorscale for pastel colors
      reversescale = TRUE,    # Reverses the colorscale for pastel colors
      zmin = -1,              # Set the minimum value for color scale
      zmax = 1,               # Set the maximum value for color scale
      zmid = 0               # Set the midpoint of the colorscale
    )
  })  
  ### PCA PLOT
    output$PCAtext <- renderText({
    # Specify the file path relative to the project directory
    file_path <- "./pca.txt"
    # Check if the file exists
    if (file.exists(file_path)) {
      # Read the content of the file
      content <- readLines(file_path)
      formatted_content <- paste(content, collapse = "\n")
    } else {
      content <- "Error: File not found."      
    }
    return(formatted_content)   
    })     
  ### CLUSTER PLOT    
    output$clusterplot <- renderPlotly({
      fviz_cluster(pam_Kresult, data = DATA_NY2023_SCALED_df)
    })  
    output$ClusterText <- renderText({      
      file_path <- "./clustertext.txt"
      # Check if the file exists
      if (file.exists(file_path)) {
        # Read the content of the file
        content <- readLines(file_path)
        formatted_content <- paste(content, collapse = "\n")
      } else {
        content <- "Error: File not found."        
      }
      return(formatted_content)     
    })  
    # MULTILINEAR REGRESSION
    output$linearplot <- renderPlotly({
      
      plot_ly(x = regression_result$EHOUSNGP, y = predicted_values, type = "scatter", mode = "markers",
              marker = list(color = "#283618", symbol = 16)) %>%
        layout(title = "Actual vs. Estimated Housing Expenses",
               xaxis = list(title = "Actual Housing Values"),
               yaxis = list(title = "Annual Estimated Housing Expenses"),
               showlegend = FALSE) %>%
        add_trace(x = regression_result$EHOUSNGP, y = regression_result$EHOUSNGP,
                  type = "scatter", mode = "lines", line = list(color = "#606c38"),
                  name = "Reference Line") %>%
        add_trace(x = regression_result$EHOUSNGP, y = predicted_values,
                  type = "scatter", mode = "markers", marker = list(color = "#283618", symbol = 16),
                  name = "Actual") %>%
        add_trace(x = regression_result$EHOUSNGP, y = regression_result$EHOUSNGP,
                  type = "scatter", mode = "markers", marker = list(color = "#606c38", symbol = NA),
                  name = "Estimated Coefficients")
    })
    output$RegressionText <- renderText({
      
      file_path <- "./lineartext.txt"
      # Check if the file exists
      if (file.exists(file_path)) {
        # Read the content of the file
        content <- readLines(file_path)
        formatted_content <- paste(content, collapse = "\n")
      } else {
        content <- "Error: File not found."        
      }
      return(formatted_content)   
    })    
    output$map_plot <- renderLeaflet({
      map
    })    
    output$Mapplottext <- renderPrint({
      print(ratio_data)
    })    
    }
```
## Components Script (UI)
We define the application structure, including banners and headers, and assign the desired text, tables, or images.
```r
## Shiny UI component for the Dashboard
dashboardPage( 
  dashboardHeader(title="Comprehensive Analytics on Housing Data in the United States", titleWidth = 720                   
  ),    
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Dataset", tabName = "data", icon = icon("database")),
                menuItem("Visualization", tabName = "viz", icon=icon("chart-line")),
                menuItem("Evaluation", tabName = "map", icon=icon("map"))              
    )
  ),
  dashboardBody(    
    tabItems(
      ## First tab item
      tabItem(tabName = "data", 
              tabBox(id="t1", width = 12, 
                     tabPanel("About", icon=icon("address-card"),
                              fluidRow(
                                column(width = 4, 
                                       tags$br() , 
                                       tags$img(src = "NY.jpg",height = "550px"),
                                       tags$br() , 
                                       # tags$img(src = "datapic.jpg"),
                                       tags$a("")),                        
                                column(width = 5, tags$br() ,
                                       tags$p("Housing Expenditure Interviews and Surveys Database: 2021 – 2023",
                                              style = "font-family: 'Arial'; font-size: 20px; font-weight: bold;"),
                                       tags$p("Utilizing an Analytics Approach on Housing Data in the United States: Leveraging the extensive Consumer Expenditure Survey (CE) data, which delves into expenditures, income, and demographic insights. Our focus centers on the detailed Public Use Microdata (PUMD) files derived from this survey, providing granular individual respondent data. It's crucial to highlight that these files are meticulously crafted to omit any identifying information, prioritizing privacy and confidentiality.",
                                              style = "font-family: 'Arial'; font-size: 16px;"),
                                       tags$p("This analytical endeavor meticulously explores the intricate dynamics within the U.S. housing market, employing a data-centric methodology. By incorporating variables such as income, expenditure, rent, and mortgage loan values, our goal is to craft a comprehensive and robust analysis of housing features. The resulting index serves as a pivotal tool for evaluating the capacity of individuals and households to navigate housing expenses, shedding light on state disparities and overall financial features. Through a combination of statistical analysis and data visualization, our study tackles key inquiries related to the correlation between income levels and housing dynamics. Source Bureau of Labor Statistics ",
                                              style = "font-family: 'Arial'; font-size: 16px ;")
                                       
                                )                              
                              )                                                       
                     ),                   
                     tabPanel("Data Cleaning", verbatimTextOutput("DataCleaningtext"), icon = icon("table")),
                     tabPanel("Raw Structure", verbatimTextOutput("structure0"), icon=icon("uncharted")),
                     tabPanel("Cleaned Structure", verbatimTextOutput("structure1"), icon=icon("uncharted")),
                     tabPanel("Data", dataTableOutput("dataT"), icon = icon("table")), 
                     tabPanel("Summary Stats", verbatimTextOutput("summary"), icon=icon("chart-pie"))
              )              
      ),        
      # Second Tab Item
      tabItem(tabName = "viz", 
              tabBox(id="t2",  width=12, 
                     tabPanel("Income", value="Income1",
                              withSpinner(plotlyOutput("incomeplot", height = "700px"))),
                     tabPanel("Housing Expenses", id="Housing1" , 
                              withSpinner(plotlyOutput("housingplot", height = "700px"))),
                     tabPanel("Average Rent", id="Rent1" , 
                              withSpinner(plotlyOutput("rentplot", height = "700px"))),
                     tabPanel("Average Loan", id="Loan1" , 
                              withSpinner(plotlyOutput("loanplot", height = "600px"))),
                     tabPanel("Education NY", id="Education1" , 
                              withSpinner(plotlyOutput("educationplot", height = "800px"))),
                     tabPanel("Education Rent NY", id="Education2" , 
                              withSpinner(plotlyOutput("education_rent_plot", height = "600px"))),
                     #side = "left"
              ),              
      ),            
      # Third Tab Item
      tabItem(
        tabName = "map",
        tabBox(id="t3",  width=12, 
               tabPanel("Correlation", value="Correlation1",
                        withSpinner(plotlyOutput("correlationplot", height = "700px"))),
               tabPanel("PCA Analysis", verbatimTextOutput("PCAtext"),
                          fluidRow(
                          column(width = 5, 
                                 tags$br() , 
                                 tags$img(src = "PCA2.jpg"),
                                 tags$br() , 
                                 # tags$img(src = "datapic.jpg"),
                                 tags$a("")),                       
                          column(width = 5, 
                                 tags$br() , 
                                 tags$img(src = "PCA1.jpg"),
                                 tags$br() , 
                                 # tags$img(src = "datapic.jpg"),
                                 tags$a(""))                     
                        )),        
               tabPanel("Cluster Analysis", value="Cluster1",
                        withSpinner(plotlyOutput("clusterplot", height = "500px")),
                        verbatimTextOutput("ClusterText")),
               tabPanel("Multilinear Regression", value="Linear1",
                        withSpinner(plotlyOutput("linearplot", height = "500px")),
                        verbatimTextOutput("RegressionText")),
               tabPanel("Ratio Map", withSpinner(leafletOutput("map_plot", height = "500px")),
                                 tags$br(),
                                 verbatimTextOutput("Mapplottext"), icon = icon("map")
                        )
                 )
               )               
               )      
    )
  )
```
## Data Packages and Asigned Variables (Global)
Finally, we compile the necessary packages and variables to run before launching the application; otherwise, it will generate an error.
```r
#### Load the required packages ####
# if packages are not installed already,
# install them using function install.packages(" ")
library(shiny) # shiny features
library(shinydashboard) # shinydashboard functions
library(DT)  # for DT tables
library(dplyr)  # for pipe operator & data manipulations
library(plotly) # for data visualization and plots using plotly 
library(ggplot2) # for data visualization & plots using ggplot2
library(ggtext) # beautifying text on top of ggplot
library(maps) # for USA states map - boundaries used by ggplot for mapping
library(ggcorrplot) # for correlation plot
library(shinycssloaders) # to add a loader while graph is populating
library(stringr)
library(plotrix)
library(viridis)
library(RColorBrewer)
library(factoextra)
library(dplyr)
library(corrplot)
library(cluster)
library(leaflet)
```
You can use these scripts as a guide for adding buttons, banners, or dynamics to your visualizations. When seeking help on ChatGPT, be specific about the variables and expected results. Remember to keep copies of your previous charts, as some packages like ggplot may not work well with Shiny. To avoid issues, keep your R software updated or consider using Plotly instead, as it is more compatible with Shiny. I hope you find these scripts useful, and I appreciate your feedback!



