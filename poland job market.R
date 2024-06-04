library(ggplot2)
library(tidyverse)
library(viridis)
library(plotly)
library(shiny)
library(lattice)
library(reshape2)

# Custom colors
custom_colors <- c("Sex: Female" = "#F08080", "Sex: Male" = "#20B2AA")

# ANLAYSIS 1
# Employment Trends by Year, Sex, Population
data_pop <- read.csv('/Users/apple/Downloads/POP_XWAP_SEX_AGE_NB_A-filtered-2024-01-14.csv', header = TRUE, sep = ',')
data_emp <- read.csv('/Users/apple/Downloads/EMP_DWAP_SEX_AGE_RT_A-filtered-2024-01-14.csv', header = TRUE, sep = ';')

# Clean and rename before merging
# Rename columns in data_pop
names(data_pop)[names(data_pop) == "sex.label"] <- "Sex"
names(data_pop)[names(data_pop) == "time"] <- "Year"

# Remove unwanted columns in data_pop before renaming all to a standardized set
data_pop <- data_pop[, !(names(data_pop) %in% c("obs_status.label", "note_classif.label", "note_indicator.label", "note_source.label"))]
names(data_pop) <- c("Country", "Indicator", "Source", "Sex", "Age", "Year", "Population")  # Assuming all required columns are present and named

# Rename columns in data_emp
names(data_emp)[names(data_emp) == "sex.label"] <- "Sex"
names(data_emp)[names(data_emp) == "time"] <- "Year"

# Verify the changes
print(names(data_pop))
print(names(data_emp))

# Merging the datasets on 'Year' and 'Sex'
merged_data <- merge(data_pop, data_emp, by = c("Year", "Sex"))

# Add a column for employment count if not already calculated
merged_data <- merged_data %>%
  mutate(EmploymentCount = Population * emp.ratio)

# Check the resulting merged data
print(head(merged_data))
colnames(merged_data)

print(unique(merged_data$Sex.x))
print(unique(merged_data$Sex.y))

# Plot data transformation
plot_data <- merged_data %>%
  group_by(Year, Sex) %>%
  summarise(
    TotalEmployment = sum(EmploymentCount, na.rm = TRUE),
    AvgEmploymentRatio = mean(emp.ratio, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Year = as.factor(Year))

# Plot Total Employment and Average Employment Ratio
ggplot(plot_data, aes(x = Year)) +
  geom_col(aes(y = TotalEmployment, fill = Sex), position = position_dodge(width = 0.9)) +
  geom_line(aes(y = AvgEmploymentRatio * 1000, group = Sex, color = Sex), size = 0.5) +
  geom_smooth(aes(y = TotalEmployment, color = Sex, group = Sex), method = "lm", se = FALSE, fullrange = TRUE) +
  geom_smooth(aes(y = AvgEmploymentRatio * 1000, color = Sex, group = Sex), method = "lm", se = FALSE, fullrange = TRUE, linetype = "dashed") +
  scale_y_continuous(
    name = "Total Employment",
    sec.axis = sec_axis(~ ./1000, name = "Employment Ratio (%)")
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Employment Trends by Year and Sex",
    subtitle = "Includes linear trend lines for total employment and employment ratios"
  ) +
  theme_minimal()

# ANALYSIS 2
# Working Population by Sex
data <- read.csv("/Users/apple/Downloads/workinpopbysexaageedu.csv", header = TRUE, sep = ';')
data

# Check for missing values and correct data types
data <- data %>%
  dplyr::mutate_at(vars(contains("label")), as.factor) %>% # convert labels to factors
  dplyr::mutate(time = as.integer(time), obs_value = as.numeric(obs_value)) # ensure numeric columns are correct

# Exploratory Data Analysis (EDA)
summary(data)
glimpse(data)
colnames(data)

# Bar plot of working-age population by sex and education level
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

# ANALYSIS 3 
# Educational mismatch data 
data1 <- read.csv('/Users/apple/Downloads/sexemploymenteducationalmismatch.csv',  header = TRUE, sep = ';')
data1

data1 <- data1 %>%
  dplyr::mutate_at(vars(contains("label")), as.factor) %>% # convert labels to factors
  dplyr::mutate(time = as.integer(time), educational.mismatch.in.thousands = as.numeric(educational.mismatch.in.thousands))

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
  gather(key = 'Category', value = 'Percentage', Percent_Overeducated, Percent_Matched, Percent_Undereducated) %>%
  mutate(Category = fct_recode(Category, 
                               "Overeducated" = "Percent_Overeducated", 
                               "Matched" = "Percent_Matched", 
                               "Undereducated" = "Percent_Undereducated"))

# Plotting
ggplot(data_melted, aes(x = as.factor(time), y = Percentage, fill = Category)) +
  geom_bar(stat = 'identity', position = 'stack') +
  facet_wrap(~ sex.label, scales = 'free_y') +
  scale_fill_viridis_d(begin = 0.3, end = 0.9, direction = 1, option = "D") +  
  theme_minimal() +
  labs(
    title = 'Educational Mismatch by Gender Over Time',
    subtitle = 'Each stack represents the proportion of educational alignment',
    x = 'Year',
    y = 'Percentage (%)',  # Setting the y-axis label here
    fill = 'Mismatch Category'
  ) +
  geom_text(
    aes(label = scales::percent(Percentage/100, accuracy =.05)), 
    position = position_stack(vjust = 0.5), 
    size = 2, 
    color = "white", 
    check_overlap = TRUE
  ) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 10),
    axis.text.y = element_text(size = 12),
    plot.subtitle = element_text(size = 14),
    # Remove or adjust any line that explicitly hides the y-axis title if it was here
  )

# ANALYSIS 4
# What occupation males and females work at
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
# We only want to look at detailed occupations, not totals:
long_data <- subset(long_data, SkillLevel != "Total")

# We only want to look at detailed occupations, not totals:
long_data <- subset(long_data, SkillLevel != "Total")
long_data <- subset(long_data, Sex != "Total")

# We only want to look at detailed occupations, not totals:
long_data <- subset(long_data, SkillLevel != "Total")
long_data <- subset(long_data, Sex != "Sex: Total")
ui <- fluidPage(
  titlePanel("Comparative Employment Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year1", "Select First Year:", choices = unique(long_data$Year)),
      selectInput("year2", "Select Second Year:", choices = unique(long_data$Year))
    ),
    mainPanel(
      plotlyOutput("plot1"),
      plotlyOutput("plot2")
    )
  )
)
server <- function(input, output) {
  # Plot for the first selected year
  output$plot1 <- renderPlotly({
    filtered_data <- long_data[long_data$Year == input$year1,]
    plot_ly(data = filtered_data, x = ~SkillLevel, y = ~EmploymentCount, color = ~Sex, colors = custom_colors, type = 'bar',
            hoverinfo = 'text', text = ~paste('Count:', EmploymentCount)) %>%
      layout(
        title = paste("Employment by Skill Level and Sex for the Year", input$year1),
        xaxis = list(title = "Skill Level"),
        yaxis = list(title = "Employment in thousnds"),
        barmode = 'group',
        colorway = custom_colors
      )
  })
  
  # Plot for the second selected year
  output$plot2 <- renderPlotly({
    filtered_data <- long_data[long_data$Year == input$year2,]
    plot_ly(data = filtered_data, x = ~SkillLevel, y = ~EmploymentCount, color = ~Sex, colors = custom_colors, type = 'bar',
            hoverinfo = 'text', text = ~paste('Count:', EmploymentCount)) %>%
      layout(
        title = paste("Employment by Skill Level and Sex for the Year", input$year2),
        xaxis = list(title = "Skill Level"),
        yaxis = list(title = "Employment i thousands"),
        barmode = 'group',
        colorway = custom_colors
      )
  })
}
shinyApp(ui, server)

# ANALYSIS 5
# Average Monthly Earning by Sex and Education
earnings_data <- read.csv('/Users/apple/Downloads/averagemonthlyearningbysexandoccupation.csv', header = TRUE, sep = ';')

# Preprocess the data, apply filters
earnings_data_filtered <- earnings_data %>%
  # Exclude rows where 'classif2.label' contains 'Occupation (Skill level)'
  filter(!grepl("Occupation \\(Skill level\\):", classif1.label)) %>%
  filter(!grepl("Occupation \\(ISCO-08\\): Total", classif1.label)) %>%
  # Exclude rows where 'classif2.label' is 'Currency: U.S. dollars'
  filter(!grepl("Currency: U.S. dollars", classif2.label)) %>%
  # Exclude rows where 'sex.label' is 'Sex: Total'
  filter(sex.label != "Sex: Total") %>%
  # Exclude rows where 'classif2.label' is 'Occupation (ISCO-08): X. Not elsewhere classified'
  filter(!grepl("Occupation \\(ISCO-08\\): X. Not elsewhere classified", classif1.label))

# Check the filtered data
print(head(earnings_data_filtered))

# Make sure your earnings data is numeric, not character
earnings_data_filtered$average.monthly.earnings <- as.numeric(as.character(earnings_data_filtered$average.monthly.earnings))

# Then we group and summarize if needed
earnings_by_occupation <- earnings_data_filtered %>%
  group_by(classif2.label, time, sex.label) %>%
  summarise(average_earnings = mean(average.monthly.earnings, na.rm = TRUE)) %>%
  ungroup()

# Now, we plot
ggplot(earnings_by_occupation, aes(x = time, y = average_earnings, color = sex.label, group = interaction(classif2.label, sex.label))) +
  geom_line(size = 1.2) +  # Slightly thicker lines for better visibility
  geom_point(size = 3, shape = 21, fill = "white") +  # Add points with white fill
  facet_wrap(~classif2.label, scales = "free_y") + 
  labs(title = "Change in Monthly Earnings by Year and Sex",
       x = "Year",
       y = "Average Monthly Earnings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

# ANALYSIS 6
# Comparison of earnings in female dominant occupations
# Filter data for the year 2022 and the specified occupations
filtered_data <- earnings_data %>%
  filter(time == 2022,
         classif1.label %in% c("Occupation (ISCO-08): 2. Professionals",
                               "Occupation (ISCO-08): 3. Technicians and associate professionals",
                               "Occupation (ISCO-08): 4. Clerical support workers",
                               "Occupation (ISCO-08): 5. Service and sales workers",
                               "Occupation (ISCO-08): 9. Elementary occupations")) %>%
  # Remove the prefix to clean up the occupation names for better display
  mutate(classif1.label = str_remove(classif1.label, "Occupation \\(ISCO-08\\): ")) %>%
  # Exclude rows where 'sex.label' is 'Sex: Total'
  filter(sex.label != "Sex: Total")

# Plotting the data
p <- ggplot(filtered_data, aes(x = classif1.label, y = average.monthly.earnings, fill = sex.label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Comparison of Earnings Between Genders in Female-Dominant ISCO-8 Occupations (2022)",
       x = "Occupation",
       y = "Average Monthly Earnings (in Złoty)",
       fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "top") +
  scale_fill_manual(values = custom_colors)  # Assign colors

# Print the plot
print(p)

