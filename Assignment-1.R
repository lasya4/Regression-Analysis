library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
national_salaries <- read_excel("NationalSalaries.xlsx")
placeholders <- c("***", "**", "#", "*")

national_salaries_clean <- national_salaries %>%
  rowwise() %>%
  filter(!any(c_across(everything()) %in% placeholders))

national_salaries_clean <- national_salaries_clean %>%
  ungroup()

glimpse(national_salaries_clean)
head(national_salaries_clean)

write.xlsx(national_salaries_clean, "C:\\Users\\rajas\\OneDrive\\Desktop\\Masters\\sem-2\\intro to DS\\Assignment-1\\Cleaned_NationalSalaries.xlsx", asTable = FALSE)


#2

columns_to_keep <- c("ST", "STATE", "OCC_CODE", "OCC_TITLE", "GROUP", "TOT_EMP", "H_MEAN", "A_MEAN")

existing_columns <- intersect(columns_to_keep, names(national_salaries_clean))

new_file <- national_salaries_clean %>%
  select(all_of(existing_columns))

glimpse(new_file)
head(new_file)

write.xlsx(new_file, "C:\\Users\\rajas\\OneDrive\\Desktop\\Masters\\sem-2\\intro to DS\\Assignment-1\\new_file.xlsx", asTable = FALSE)


#3

# Assuming 'filtered_national_salaries' is already loaded and dplyr is attached
set.seed(123)
# Randomly select 1500 rows from the dataframe
random_rows <- sample_n(new_file, size = 1500)

# View the structure of the randomly selected rows
glimpse(random_rows)

# Optionally, save the randomly selected rows to a new file
write.xlsx(random_rows, "C:\\Users\\rajas\\OneDrive\\Desktop\\Masters\\sem-2\\intro to DS\\Assignment-1\\RandomRows_NationalSalaries.xlsx", asTable = FALSE)


#4

# Initial filter for individual jobs where GROUP is NA and H_MEAN is less than 15
individual_jobs_under_15 <- new_file %>%
  filter(!grepl("major",GROUP,ignore.case= TRUE),
         !grepl("All Occupations", OCC_TITLE, ignore.case = TRUE),
         H_MEAN < 15)
# Keep only the unique rows

# View the structure and the first few rows of the new dataframe
glimpse(individual_jobs_under_15)
head(individual_jobs_under_15)

# Save the filtered dataset to a new file
write.xlsx(individual_jobs_under_15, "C:\\Users\\rajas\\OneDrive\\Desktop\\Masters\\sem-2\\intro to DS\\Assignment-1\\IndividualJobs_Under15.xlsx", asTable = FALSE)


#5

fil_1<- new_file %>%
  filter(is.na(GROUP)| OCC_TITLE != 'All Occupations')

fil_2<- fil_1 %>% filter(is.na(GROUP))

fil_3 <- fil_2 %>%
  filter(STATE == "Indiana")

# Convert A_MEAN to numeric if it's not already
fil_3 <- fil_3 %>%
  mutate(A_MEAN = as.numeric(as.character(A_MEAN)))

# Create 10 salary bins (breaks = 10) and categorize the data into those bins
fil_3 <- fil_3 %>%
  mutate(SALARY_BIN = cut(A_MEAN, breaks = 10, labels = FALSE))

# Count the number of jobs in each bin
indi_jobs <- fil_3 %>%
  group_by(SALARY_BIN) %>%
  summarise(JOBS_IN_BIN = n())

# Display the binned salary information
head(indi_jobs,n= 10)

#6

national_salaries_clean <- new_file %>%
  mutate(TOT_EMP = as.numeric(as.character(TOT_EMP)))

# Calculate total employment for each state
total_employment_by_state <- national_salaries_clean %>%
  group_by(STATE) %>%
  summarise(Total_Employment = sum(TOT_EMP, na.rm = TRUE))

# View the total employment by state
print(total_employment_by_state)

write.xlsx(total_employment_by_state, "C:\\Users\\rajas\\OneDrive\\Desktop\\Masters\\sem-2\\intro to DS\\Assignment-1\\TotalEmploymentByState.xlsx", asTable = FALSE)


#7
# Assuming 'new_file' is already loaded and contains the relevant columns

# Convert A_MEAN to numeric if it's not already
new_file$A_MEAN <- as.numeric(as.character(new_file$A_MEAN))

# Filter for jobs in Indiana
jobs_in_IN <- new_file %>%
  filter(STATE == "Indiana")

# Calculate the average yearly salary for Indiana
average_salary_IN <- mean(jobs_in_IN$A_MEAN, na.rm = TRUE)

# Print the average yearly salary for Indiana
print(paste("In Indiana, the average annual salary is:", average_salary_IN))

# Calculate the average yearly salary for each state
average_salary_by_state <- new_file %>%
  group_by(STATE) %>%
  summarise(Average_Salary = mean(A_MEAN, na.rm = TRUE)) %>%
  ungroup()

# Rank the states by average salary and find Indiana's rank
average_salary_by_state <- average_salary_by_state %>%
  arrange(desc(Average_Salary)) %>%
  mutate(Rank = row_number())

# Find Indiana's rank
indiana_rank <- average_salary_by_state %>%
  filter(STATE == "Indiana") %>%
  pull(Rank)

# Print Indiana's rank
print(paste("According to average salary, Indiana ranks:", indiana_rank))

# View the entire ranking of states
print(average_salary_by_state)

# Optionally, save the ranking to a new file
write.xlsx(average_salary_by_state, "C:\\Users\\rajas\\OneDrive\\Desktop\\Masters\\sem-2\\intro to DS\\Assignment-1\\AverageSalaryByState.xlsx", asTable = FALSE)


#8

computer_math_jobs_selected_states <- new_file %>%
  filter(grepl("^15-", OCC_CODE), 
         ST %in% c("IN", "CA", "NY")) %>%
  mutate(A_MEAN = as.numeric(as.character(A_MEAN)))

# Calculate and compare the average salary by state for these occupations
average_salary_by_state <- computer_math_jobs_selected_states %>%
  group_by(ST) %>%
  summarise(Average_Salary = mean(A_MEAN, na.rm = TRUE)) %>%
  arrange(desc(Average_Salary))

# Print the average salary by state for Computer and Mathematical Occupations
print(average_salary_by_state)

# Visualize the average salary by state with a bar chart
ggplot(average_salary_by_state, aes(x = ST, y = Average_Salary, fill = ST)) +
  geom_bar(stat = "identity") +  geom_text(aes(label = round(Average_Salary, 2)), vjust = -0.3, size = 3.5) +

  theme_minimal() +
  labs(title = "Average Yearly Salary for Computer and Mathematical Occupations",
       subtitle = "Comparison among Indiana, California, and New York",
       x = "State",
       y = "Average Yearly Salary ($)") +
  scale_fill_manual(values = c("IN" = "blue", "CA" = "red", "NY" = "green")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
