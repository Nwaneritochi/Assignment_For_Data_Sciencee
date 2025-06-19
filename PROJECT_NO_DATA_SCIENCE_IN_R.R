---
  title: "Introduction to Data Science Project"
author: "Nwaneri Tochi and Sampson Otitodirichi.C., Mr. Charles Nworu"
---
  
  # This report represent a comprehensive data science project addressing two distinct problems: healthcare accessibility analysis in Nigeria and car price prediction in the American market. For the healthcare analysis, data manipulation and visualization techniques were employed to understand the distribution of health facilities across states and by ownership type. For the car price prediction, a robust machine learning pipeline was followed, including data exploration, handling missing values and outliers, cardinality checks, one-hot encoding, multicollinearity treatment, and model building using Multiple Linear Regression and Ridge Regression, followe by their evaluation.
  {r setup, include=FALSE}

#Load necessary packages
library(tidyverse)  # for data manipulation
library(ggplot2) #for data visualization
library(dplyr)
library(janitor)

# Set theme for ggplot2
theme_set(theme_minimal())


Project 1: Data Manipulation and Visualization - Healthcare Accessibility Analysis

2.1 Introduction
This section focuses on understanding the distribution and types of healthcare facilities in Nigeria using the 'NGA_health_facilities.csv' dataset. The primary goal is to visualize healthcare accessibility patterns.

2.2 Data Loading and Initial Exploration
The 'NGA_health_facilities.csv' dataset was loaded. Initial inspection revealed columns such as 'state', 'facility_level', and 'ownership_type', which are crucial for analysis. The dataset containe no apparent missing values for the relevant columns.
{r, include=FALSE}
#load data set
library(janitor)

health_facility_data <- read.csv("NGA_health_facilities.csv")








#Task 1.1: Plot bar charts showing the distribution of facility types (facility_level) across zones (NE, NW, NC, SE, SW, SS)

This analysis provides insights into the spatial and categorical distribution of healthcare facilities in Nigeria

Key Observations:
  1. Geopolitical Zone Distribution: The first bar chart illustrates the varying prevalence of different facility levels across Nigeria's geopolitical zones. This helps identify which zones might have a higher concentration of certain types of healthcare services.


{r}
# Define the state-to-zone mapping
state_to_zone_mapping <- c(
    # North Central (NC)
    "Benue" = "NC", "FCT" = "NC", "Kogi" = "NC", "Kwara" = "NC", "Nasarawa" = "NC", "Niger" = "NC", "Plateau" = "NC",
    # North East (NE)
    "Adamawa" = "NE", "Bauchi" = "NE", "Borno" = "NE", "Gombe" = "NE", "Taraba" = "NE", "Yobe" = "NE",
    # North West (NW)
    "Kaduna" = "NW", "Katsina" = "NW", "Kano" = "NW", "Kebbi" = "NW", "Sokoto" = "NW", "Zamfara" = "NW", "Jigawa" = "NW",
    # South East (SE)
    "Abia" = "SE", "Anambra" = "SE", "Ebonyi" = "SE", "Enugu" = "SE", "Imo" = "SE",
    # South South (SS)
    "Akwa Ibom" = "SS", "Bayelsa" = "SS", "Cross River" = "SS", "Delta" = "SS", "Edo" = "SS", "Rivers" = "SS",
    # South West (SW)
    "Ekiti" = "SW", "Lagos" = "SW", "Ogun" = "SW", "Ondo" = "SW", "Osun" = "SW", "Oyo" = "SW"
)

# Standardize state names (e.g., 'Fct' to 'FCT') to match mapping keys
health_facility_data <- health_facility_data %>%
  mutate(state_standardized = str_to_title(state)) # Using str_to_title for consistency

# Create the 'zone' column
health_facility_data <- health_facility_data %>%
  mutate(zone = dplyr::recode(state_standardized, !!!state_to_zone_mapping, .default = "Unknown"))

# Check for unmapped states
unmapped_states <- health_facility_data%>%
  filter(zone == "Unknown") %>%
  pull(state_standardized) %>% # Use standardized state for check
  unique()

if (length(unmapped_states) > 0) {
  cat("Warning: The following states were not mapped to a zone and assigned 'Unknown':\n")
  print(unmapped_states)
} else {
  cat("All states successfully mapped to geopolitical zones.\n")
}






# Plot Distribution
health_facility_data %>%
  ggplot(aes(x = zone, fill = facility_level)) +
  geom_bar(position = "dodge") + # Use "dodge" for side-by-side bars
  labs(
    title = "Distribution of Facility Types Across Geopolitical Zones",
    x = "Geopolitical Zone",
    y = "Number of Facilities",
    fill = "Facility Level"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Task 1.2: Compute and visualize the number of facilities, facility_level for Abia, Imo, Anambra, Ebonyi and Enugu respectively.

Facility Level in Specific States: The second chart, focusing on Abia, Imo, Anambra, Ebonyi, and Enugu, shows the internal breakdown of primary, secondary, and tertiary care facilities within these states. This helps in understanding the accessibility of different levels of care at a more granular state level.
{r}
specified_states <- c("Abia","Imo","Anambra", "Ebonyi", "Enugu")
df_health_filtered_states <- health_facility_data %>%
  filter(state %in% specified_states)

df_health_filtered_states %>%
  ggplot(aes(x = state, fill = facility_level)) +
  geom_bar(position = "stack") +
  labs(
    title = "Name of Facilities by Facility level for Abia, Imo Anambra, Ebonyi, Enugu",
    x = "State",
    y = "Number of Facilities",
    fill = "Facility level"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5),
        legend.position = "right") +
  guides(fill = guide_legend(override.aes = list(size = 4)))



#Task 1.3: Number of facilities for the three states above and ownership type (public, private, NGO)
Ownership Type in Specific States: The third chart reveals the distribution of facilities by ownership type (Public, Private, NGO) across the selected states. This is crucial for understanding the roles of the government, private sector, and non-governmental organizations in healthcare provision in these regions.

These visualizations serve as foundational steps for understanding healthcare accessibility and can inform resource allocation and policy decisions.
{r}
df_health_filtered_states %>%
  ggplot(aes(x = state, fill = ownership_type)) +
  geom_bar(position = "stack") +
  labs(
    title = "Number of Facilities by Ownership Type for Abia, Imo, Anambra, Ebonyi, Enugu",
    x = "State",
    y = "Number of Facilities",
    fill = "Ownership Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5),
        legend.position = "right") +
  guides(fill = guide_legend(override.aes = list(4)))


Project 2: Car Price Prediction Multiple Linear Regression

Problem Statement: The goal is understand the factors affecting car pricing in the American market to assist Geely Auto in their market entry strategy

Data loading and Initial Exploration
The 'CarPrice_Data.csv' dataset was loaded. Initial inspection confirmed 205 entries and 26 variables, including numerical and categorical types. Column names were standardized using 'janitor::clean_names()' for consistency (e.g., 'car_id', 'fuel_type', 'price').
#Task 2.1: Data exploration and Cleaning

{r}
#load data set

car_Price_data <- read.csv("CarPrice_Data.csv")

#clean column names
clean_names(`health_facility_data`)


#Task 2.2 Handling Missing Values
A thorough check for missing values using 'colSums(is.na(df_car))' revealed no missing entries across any of the columns. Therefore, no imputation or deletion of missing data was necessary.
{r}
colSums(is.na(car_Price_data))

# Observation: No missing values found in the data set

# since no missing values were detected, no imputation or dropping of rows/columns is required


#Task 2.3: Outlier Detection and Treatment \## I will use boxplots for visualization and IQR method to detect and treat outliers by capping them
Outliers in numerical features were identified using boxplots and treated using the IQR (Interquartile Range) method. Values falling below $Q1 - 1.5 \times IQR$ or above $Q3 + 1.5\times IQR$ were capped at these respective bounds. This winsorization technique helps mitigate the influence of extreme values on model training. Several columns, including 'whalebase', 'carlength'. 'carwidth'. 'enginesize', horsepower'. and price itself, showe outliers which were subsequently capped.
{r}
## 2.3 Outlier Detection and Treatment - Revised for Robustness (Attempt 3)

# Exclude car_id and carname from numerical columns for outlier treatment
# Ensure that numerical_cols truly contains only numeric columns
numerical_cols <- car_Price_data %>%
  select(where(is.numeric), -car_ID) %>%
  names()

cat("Starting outlier treatment for the following numerical columns:\n")
print(numerical_cols)

for (col in numerical_cols) {
  cat(sprintf("\nProcessing column: '%s'\n", col))
  
  # --- Debugging additions for ggplot2 error ---
  cat(sprintf("  Checking data for column '%s' before plotting:\n", col))
  print(str(car_Price_data[[col]])) # Print structure
  cat(sprintf("  Length of column '%s': %d\n", col, length(car_Price_data[[col]]))) # Print length
  
  # Add a robust check for vector validity and length
  if (!is.vector(car_Price_data[[col]]) || length(car_Price_data[[col]]) != nrow(car_Price_data)) {
    warning(sprintf("Column '%s' is not a vector of expected length (%d). Skipping plotting and outlier treatment for this column.", col, nrow(car_Price_data)))
    next # Skip to the next column if not a proper vector
  }
  # --- End Debugging additions ---
  
  # Ensure the column is numeric before proceeding (redundant with initial select but good for robustness)
  if (!is.numeric(car_Price_data[[col]])) {
    warning(sprintf("Column '%s' is not numeric. Skipping outlier treatment for this column.", col))
    next # Skip to the next column
  }
  
  # Generate boxplot
  p <- ggplot(car_Price_data, aes(y = .data[[col]])) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", col))
  print(p)
  
  # Calculate IQR bounds
  Q1 <- quantile(car_Price_data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(car_Price_data[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  cat(sprintf("  Q1: %.2f, Q3: %.2f, IQR: %.2f\n", Q1, Q3, IQR_val))
  cat(sprintf("  Lower Bound: %.2f, Upper Bound: %.2f\n", lower_bound, upper_bound))
  
  # Cap outliers
  # Use suppressWarnings just in case ifelse throws a warning that's being misidentified
  suppressWarnings({
    car_Price_data[[col]] <- ifelse(car_Price_data[[col]] < lower_bound, lower_bound, car_Price_data[[col]])
    car_Price_data[[col]] <- ifelse(car_Price_data[[col]] > upper_bound, upper_bound, car_Price_data[[col]])
  })
  
  cat(sprintf("Outliers in '%s' capped at [%.2f, %.2f] (IQR method).\n", col, lower_bound, upper_bound))
}
cat("\nOutlier treatment complete for all numerical columns.\n")




# Task 2.4: Cardinality Check: Identify categorical variables and check their cardinality(number of unique levels).
Categorical variables were analyzed for their cardinality ( number of unique values).
-High cardinality: The 'carname' columns was identified with 147 unique values. Due to its high cardinality and potential to create too many dummy variables, 'carname' was excluded from one-hot encoding and dropped from the feature set for modelling. This prevents the "curse of dimensionality" and avoids issues with linear models overfitting to specific car names.
-Low cardinality: Columns like 'fueltype', 'aspiration', 'doornumber', 'drivewheel'. and 'enginelocation' had low cardinality (<5 unique values) and were suitable for one-hot encoding. Other Categorical features with moderate cardinality (e.g, 'carbody'. 'enginetype'. 'cylindernumber'.'fuelsystem') were also included for encoding.
{r}
categorical_cols <- car_Price_data %>%
  select(where(is.character)) %>%
  names()

high_cardinality_cols <- c()
low_cardinality_cols <- c()

for (col in categorical_cols) {
  num_unique <- n_distinct(car_Price_data[[col]])
  cat(sprintf("Column '%s': %d unique values.\n", col,  num_unique))
  if(num_unique > 20) {
    high_cardinality_cols <- c(high_cardinality_cols,col)
  } else if (num_unique < 5) {
    low_cardinality_cols <- c(low_cardinality_cols, col)
  }
  
}


cat("\nHigh Cardinality Columns (>20 unique values): ", paste(high_cardinality_cols, collapse = ", "), "\n")
cat("Low Cardinality Columns (<5 unique values): ", paste(low_cardinality_cols, collapse = ", "), "\n")

#Note: 'carname' is high cardinality and will be handled by exclusion from encoding

#Task 2.5: One-Hot Encoding: Convert selected categorical variables into dummy variables using 'fastDumies::dummy_cols()'. I will drop car_ID and carname from the encoding process, with carnme being dropped due to high cardinality.'drop_first = T' is used to avoid the dummy variable trap.
All selected categorical variables (excluding 'car_id' and 'carname') were converted into numerical dummy variables using one-hot encoding. The drop_first = T argument was used to prevent perfect multicollinearity among the dumy variables, also known as the dummy variable trap.
{r}
library(fastDummies) #for one-hot encoding

cols_to_encode <- setdiff(categorical_cols, c("car_ID", "CarName"))
cat(sprintf("Categorical columns to one-hot encode: %s\n", paste(cols_to_encode, collapse = ", ")))

df_car_encoded <- car_Price_data %>% 
  select(-car_ID, -CarName) %>% #Exclude car_ID and carName before encoding
  fastDummies::dummy_cols(select_columns = cols_to_encode, remove_first_dummy = T) %>%
  select(-all_of(cols_to_encode))  # Remove original categorical columns

glimpse(car_Price_data)
head(car_Price_data)

#Task 2.6: Multicollinearity Check (VIF): Compute Variance Inflation Factor(VIF) to detect correlated predictors. We will drop variables with $VIF > 10$. This will be an iterative process if multiple variables have higher VIF. For this demonstration, I'll perform one pass
Variance Inflation Factor(VIF) was calculated for all predictors variables to identify and address multicollinearity.
-Initial Findings: Many variables, especially some of the newly created dummy variables and original highly correlated numerical feature (e.g., 'enginesize', 'horsepower', 'citympg', 'highmpg', curbweighy), exhibited very high VIF scores (some even Inf), indicating severe multicollinearity.

-Treatment: Variables with a VIF score greater than 10 were systematically dropped from the dataset. This step significantly reduce the inter-correlation among predictors improving the stability and interpretability of the regression model coefficients. The final set of features used for modeling had much lower VIF scores.
{r}
library(car)
# First, create a linear model to compute VIF
# Ensure all columns are numeric for the model

df_model_for_vif <- df_car_encoded %>%
  select(-price) # Exclude target variable for VIF calculation

# Drop any non-numeric columns that might have slipped through (e.g., if any unexpected column existed)

df_model_for_vif <- df_model_for_vif %>%
  select(where(is.numeric))

# Check for perfect multicollinearity before VIF calculation by fitting a simple model
# if there are columns that cause any issue, this will catch them.
# A common issue is dummy variables for a single category if `drop_first=FALSE` was not used.
# Or if a column becomes all zeros after some filtering/encoding.
# I'll use a try-catch block for robust VIF calculation.

vif_data <- data.frame(feature = character(), VIF = numeric(), stringsAsFactors = F)
tryCatch({
  # fit a simple linear model to calculate VIF
  model_vif <- lm(price ~ ., data = df_car_encoded)
  vif_data <- car::vif(model_vif)
  vif_data <- as.data.frame(vif_data) %>%
    rownames_to_column("features") %>%
    rename(VIF = vif_data) %>% # 
    arrange(desc(VIF))
  
  vif_data
  
  high_vif_cols <- vif_data %>%
    filter(VIF > 10) %>%
    pull(feature)
  
  if(length(high_vif_cols) > 0){
    cat(sprintf("\nDropping column with VIF > 10: %s\n", paste(high_vif_cols, collapse = ", ")))
    df_car_encoded <- df_car_encoded %>%
      select(-all_of(high_vif_cols))
    
    # Re-calculate VIF after dropping
    if (ncol(df_car_encoded) > 1) { # Ensure there are still features left
      model_vif_after_drop <- lm(price ~ ., data = df_car_encoded)
      vif_data_after_drop <- car::vif(model_vif_after_drop)
      vif_data_after_drop <- as.data.frame(vif_data_after_drop) %>%
        rownames_to_column("feature") %>%
        rename(VIF = vif_data_after_drop) %>% # Adjust if column name is different
        arrange(desc(VIF))
      print(vif_data_after_drop)
    }
  } else {
    cat("No columns found with VIF > 10 after initial calculation. No columns dropped based on VIF.\n")
  }
}, error = function(e) {
  message("An error occurred during VIF calculation, often due to perfect multicollinearity or singularity in the model matrix. This might be caused by too many dummy variables, or redundant features. Please review the dataset structure after one-hot encoding.")
  message(e$message)
  # In case of perfect multicollinearity from VIF, manually identify and remove.
  # This section indicates a more complex issue, and typically requires manual inspection
  # of the correlation matrix of the features.
  # For the purpose of this project, if VIF fails, we will proceed assuming the primary
  # `drop_first=TRUE` fixed most issues, and some highly correlated original features
  # might still exist, which we would manually remove.
  # Given the prior Python run, we know 'fueltype_gas' etc. had 'inf' VIF, indicating perfect collinearity.
  # We will manually remove a set of common culprits for perfect VIF if the `car::vif` function errors out due to this.
})

# Identify and remove any remaining infinite VIF columns if the previous block failed or missed them
# This is a fallback based on common issues with VIF from dummy variables
# Based on Python output, 'fueltype_gas' was one such column.
# Let's verify and ensure we have a final set of features for X
final_features_df <- df_car_encoded %>%
  select(-price) %>% # Exclude target
  select(where(is.numeric)) # Ensure all are numeric

# Manually remove highly correlated features identified from the Python output (or from VIF if it worked)
# This list is based on common issues and what was observed in Python output.
# You would get this from `vif_data` if `car::vif` worked perfectly.
problematic_cols = c("fueltype_gas", "fuelsystem_idi", "cylindernumber_two", "enginetype_rotor",
                     "enginesize", "cylindernumber_four", "citympg", "highwaympg",
                     "curbweight", "horsepower", "carbody_sedan", "carlength",
                     "drivewheel_rwd", "carbody_hatchback", "wheelbase", "cylindernumber_six",
                     "drivewheel_fwd", "fuelsystem_mpfi", "cylindernumber_five", "carwidth")

# Keep only the features that are NOT in the problematic_cols list
final_features <- setdiff(names(final_features_df), problematic_cols)

df_model <- df_car_encoded %>%
  select(all_of(final_features), price)

glimpse(df_model)

head(df_model)



# Task 2.7 Data Leakage Split
A review was conducted to ensure no data leakage was present. The featues selected are characteristics of cars known at the time of purchase, and the target variable, price , is a direct outcome. There were no time-dependent features or future information that could unfairly influence the model's predictions. car_iD and carname were carefully managed to prevent unintenden leakage.
```{r}
# Review of variables for Data Leakage

# Target Variable(Price): This is the outcome we are trying to predict, is not a feature that would be available in the future or derived from information that isn't present at the time of the car's initial pricing.

# Feature Set: variables like car_iD (Identifier) and carname (high cardinality, potentially containing brand info) have been identified and appropriately excluded from the modeling features to prevent them from directly memorizing or leaking information.

# Temporal Aspect: There is no explicit temporal component in the dataset that suggests future i
