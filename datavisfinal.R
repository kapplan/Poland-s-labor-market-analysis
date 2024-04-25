library(ggplot2)
library(tidyverse)
library(viridis)

# Step 1: Data Importing
# Read the data
data_pop <- read.csv('/Users/apple/Downloads/POP_XWAP_SEX_AGE_NB_A-filtered-2024-01-14.csv',  header = TRUE, sep = ',')
data_emp <- read.csv('/Users/apple/Downloads/EMP_DWAP_SEX_AGE_RT_A-filtered-2024-01-14.csv', header = TRUE, sep = ';' )
colnames(data_pop)
colnames(data_emp)

# Standardizing column names if needed

# Merging the datasets
names(data_emp)[names(data_emp) == "time"] <- "Year"
names(data_emp)[names(data_emp) == "sex.label"] <- "Sex"

merged_data <- merge(data_pop, data_emp, by = c("Year", "Sex"))


# Viewing the merged data
head(merged_data)
colnames(merged_data)

data_pop$obs_status.label <- NULL
data_pop$note_classif.label <- NULL
data_pop$note_indicator.label <- NULL
data_pop$note_source.label <- NULL

head(data_pop)

colnames(data_pop) <- c("Country", "Indicator", "Source","Sex", "Age", "Year", "Population")

# Assuming the dataset has columns 'Year', 'Age', 'Sex', and 'Population'

# Prepare data for plotting
plot_data <- merged_data %>%
  group_by(Year, Sex) %>%
  summarise(TotalEmployed = sum(Population, na.rm = TRUE),  # Total employed population
            AvgEmploymentRatio = mean(emp.ratio, na.rm = TRUE)) %>%
  ungroup()

# Splitting the data for line and bar plots
line_data <- plot_data %>%
  group_by(Year) %>%
  summarise(AvgEmploymentRatio = mean(AvgEmploymentRatio, na.rm = TRUE)) %>%
  ungroup()

# Define a factor to scale the employment ratio for the secondary y-axis
ratio_scale_factor <- max(plot_data$TotalEmployed) / max(line_data$AvgEmploymentRatio)

# Create the plot with bars and a line
ggplot() +
  geom_bar(data = plot_data, aes(x = Year, y = TotalEmployed, fill = Sex), 
           stat = "identity", position = position_dodge()) +
  scale_fill_viridis(discrete = TRUE, option = "viridis") +
  geom_line(data = line_data, aes(x = Year, y = AvgEmploymentRatio * ratio_scale_factor),
            group = 1, colour = "blue", size = 1) +
  scale_y_continuous(name = "Total Employed Population (in thousands)",
                     sec.axis = sec_axis(~ ./ratio_scale_factor, name = "Average Employment Ratio (%)")) +
  labs(title = "Total Employed Population by Sex and Employment Ratio Over Time",
       x = "Year") +
  theme_minimal()

#Working Population by Sex
data <- read.csv("/Users/apple/Downloads/workinpopbysexaageedu.csv", header = TRUE, sep = ';')
data

# Step 2: Data Cleaning
# Check for missing values and correct data types

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

# Stacked bar chart: Distribution of Education Levels by Age Bands and Sex
ggplot(data_summarized, aes(x = classif1.label, y = total.obs.value, fill = Education_Grouped)) +
  geom_bar(stat = "identity") +
  facet_wrap(~sex.label, scales = "free_y") +
  labs(title= "Distribution of Education Levels by Age Bands and Sex", x = "Age Group", y = "Total Population", fill = "Education Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#Educational mismatch data 
#Load data
data1 <- read.csv('/Users/apple/Downloads/sexemploymenteducationalmismatch.csv',  header = TRUE, sep = ';')
data1

data1 <- data1 %>%
  dplyr::mutate_at(vars(contains("label")), as.factor) %>% # convert labels to factors
  dplyr::mutate(time = as.integer(time), educational.mismatch.in.thousands = as.numeric(educational.mismatch.in.thousands)) # ensure numeric columns are correct


# Filtering and transforming the data
filtered_data <- data1 %>%
  filter(sex.label %in% c("Sex: Male", "Sex: Female"),
         classif2.label %in% c("Education (Mismatch): Overeducated", 
                               "Education (Mismatch): Undereducated", 
                               "Education (Mismatch): Matched")) %>%
  group_by(time, sex.label, classif2.label) %>%
  summarize(total_mismatch = sum(educational.mismatch.in.thousands, na.rm = TRUE))

# Creating a unique identifier
data1 <- data1 %>% 
  unite("unique_id", c("time", "sex.label", "classif1.label"), remove = FALSE)

# Selecting relevant columns and transforming the data
data_long <- data1 %>%
  select(unique_id, time, sex.label, classif2.label, educational.mismatch.in.thousands) %>%
  spread(key = classif2.label, value = educational.mismatch.in.thousands)

# Calculating the total and percentages for each category
data_long <- data_long %>%
  group_by(time, sex.label) %>%
  summarise(
    Total_Overeducated = sum(`Education (Mismatch): Overeducated`, na.rm = TRUE),
    Total_Matched = sum(`Education (Mismatch): Matched`, na.rm = TRUE),
    Total_Undereducated = sum(`Education (Mismatch): Undereducated`, na.rm = TRUE)
  ) %>%
  mutate(
    Total = Total_Overeducated + Total_Matched + Total_Undereducated,
    Percent_Overeducated = Total_Overeducated / Total * 100,
    Percent_Matched = Total_Matched / Total * 100,
    Percent_Undereducated = Total_Undereducated / Total * 100
  )

# Filtering out the total gender
data_long <- data_long %>%
  filter(sex.label != 'Sex: Total')

# Melting the data for plotting
data_melted <- data_long %>%
  gather(key = 'Category', value = 'Percentage', Percent_Overeducated, Percent_Matched, Percent_Undereducated) 

data_melted$Category <- factor(data_melted$Category, levels = c("Percent_Matched", "Percent_Undereducated", "Percent_Overeducated"))

# Plotting the data
ggplot(data_melted, aes(x = time, y = Percentage, fill = Category)) +
  geom_bar(stat = 'identity', position = 'stack') +
  facet_wrap(~ sex.label) +
  theme_minimal() +
  labs(title = 'Educational Mismatch Over Years by Gender',
       x = 'Year',
       y = 'Percentage (%)')

#What occupation males and females work at
library(lattice)
library(reshape2)  # for melting the data
employment_data <- read.csv('/Users/apple/Downloads/sex.employment.occupation(thousands).csv', header=TRUE, sep=';')
employment_data

str(employment_data)

# You need to adjust this to your actual occupation column names
long_data <- melt(employment_data, id.vars = c("ref_area.label", "sex.label", "classif1.label", "classif2.label", "time"),
                  variable.name = "classif2.label", value.name = "sex.employment.occupation.thousands.")

# Clean and prepare the data
# This might involve renaming columns, converting factors, and filtering out unnecessary rows
# For example, let's rename some columns for clarity
colnames(long_data)[colnames(long_data) == 'ref_area.label'] <- 'Country'
colnames(long_data)[colnames(long_data) == 'sex.label'] <- 'Sex'
colnames(long_data)[colnames(long_data) == 'classif1.label'] <- 'EmploymentStatus'
colnames(long_data)[colnames(long_data) == 'classif2.label'] <- 'SkillLevel'
colnames(long_data)[colnames(long_data) == 'time'] <- 'Year'
colnames(long_data)[colnames(long_data) == 'sex.employment.occupation.thousands.'] <- 'EmploymentCount'


# Filter out unnecessary rows (for example, totals or aggregates that are not needed for the analysis)
# For instance, if we only want to look at detailed occupations, not totals:
long_data <- subset(long_data, SkillLevel != "Total")

# Now, let's create the lattice plot
# We will make a separate barchart for each year and by sex, using different panels for each sex
barchart(EmploymentCount ~ SkillLevel | factor(Year) * Sex, data = long_data,
         layout = c(1, 1),  # Adjust the layout based on how many panels you want per row and column
         auto.key = list(points = FALSE, rectangles = TRUE, space = "right"),
         main = "Employment by Skill Level, Sex, and Year",
         xlab = "Skill Level",
         ylab = "Employment Count",
         scales = list(x = list(rot = 90)),  # Rotate the x-axis labels if they are long
         col = c("blue", "pink"))  # Use colors or any other aesthetic properties as needed
