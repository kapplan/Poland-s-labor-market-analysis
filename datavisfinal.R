# Step 1: Data Importing
# Replace the path with the actual path of your CSV file
data <- read.csv("/Users/apple/Downloads/workinpopbysexaageedu.csv", header = TRUE, sep = ';')
data

# Step 2: Data Cleaning
# Check for missing values and correct data types
library(tidyverse)

data <- data %>%
  dplyr::mutate_at(vars(contains("label")), as.factor) %>% # convert labels to factors
  dplyr::mutate(time = as.integer(time), obs_value = as.numeric(obs_value)) # ensure numeric columns are correct

# Step 3: Exploratory Data Analysis (EDA)
summary(data)
glimpse(data)

# Step 4: Data Transformation

# Step 5: Data Visualization
# Bar plot of working-age population by sex and education level
# Veriyi düzenlemek için dplyr kullanarak özetleyin
colnames(data)

# Create a new variable 'Education_Grouped' to group education levels
data <- data %>%
  mutate(Education_Grouped = case_when(
    grepl("Master's", classif2.label) | grepl("Doctoral", classif2.label) ~ "Master's+",
    grepl("Primary", classif2.label) | grepl("Early childhood", classif2.label) | 
      grepl("Lower secondary", classif2.label) | grepl("Not elsewhere classified", classif2.label) ~ "Primary and below",
    TRUE ~ as.character(classif2.label)
  ))

# Filter the data 
filtered_data <- data %>%
  filter(grepl("Aggregate bands", classif1.label)) %>%  #Only show the aggregate band
  filter(grepl("ISCED", classif2.label) & !grepl("Total", Education_Grouped))  # Exclude "Total" for education

data_summarized <- filtered_data %>%
  group_by(sex.label, classif1.label, Education_Grouped) %>%
  summarize(total.obs.value = sum(obs_value, na.rm = TRUE))

# Stacked bar chart
ggplot(data_summarized, aes(x = classif1.label, y = total.obs.value, fill = Education_Grouped)) +
  geom_bar(stat = "identity") +
  facet_wrap(~sex.label, scales = "free_y") +
  labs(title= "Distribution of Education Levels by Age Bands and Sex", x = "Age Group", y = "Total Population", fill = "Education Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
