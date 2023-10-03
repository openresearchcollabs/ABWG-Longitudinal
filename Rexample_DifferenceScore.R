# Create a list of required packages
packages_required <- c("tidyverse", "rstatix", "DT")

# Check which packages are not installed and install them
packages_to_install <- setdiff(packages_required, rownames(installed.packages()))
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}

# Load the required packages
lapply(packages_required, library, character.only = TRUE)

library(tidyverse) # Collection of R packages for data science
library(rstatix) # Pipe-friendly framework for basic statistical tests
library(DT) # Rendering interactive data tables

knitr::opts_chunk$set(
  echo = T, message = F, warning = F, error = F,
  comment = NA, cache = T, code_folding = T,
  R.options = list(width = 220, digits = 3),
  fig.align = "center",
  out.width = "75%", fig.asp = .75
)

# Set the data paths
data_path_1 <- "/Users/shawes/ABCD/data/rds/abcd_5.0_rds/demo5.0.rds"
data_path_2 <- "/Users/shawes/ABCD/data/rds/abcd_5.0_rds/core-rds-5.0/non-imaging_excluding_nt_5.0.rds"

# Read the data
data_demographics <- readRDS(data_path_1)
data_nonimaging <- readRDS(data_path_2)

# Subset the nonimaging data to include desired variables
selected_vars <- c("src_subject_id", "eventname", "nihtbx_totalcomp_fc", "anthroweightcalc", "anthroheightcalc")
subset_data <- data_nonimaging[, selected_vars]

library(dplyr)
# # Merge the datasets on 'src_subject_id' and 'eventname'
merged_data <- data_demographics %>%
  full_join(subset_data, by = c("src_subject_id", "eventname"))

# Inspect the merged data structure
str(merged_data)

# Define event names to be retained in the analysis and convert variables to appropriate data types
eventnames_to_include <- c(
  "baseline_year_1_arm_1",
  "1_year_follow_up_y_arm_1",
  "2_year_follow_up_y_arm_1",
  "3_year_follow_up_y_arm_1",
  "4_year_follow_up_y_arm_1"
)

df <- merged_data %>%
  filter(eventname %in% eventnames_to_include) %>%
  mutate(
    src_subject_id = as.factor(src_subject_id),
    eventname = factor(eventname, levels = eventnames_to_include, ordered = TRUE),
    age = as.numeric(age),
    sex = as.factor(sex),
    race.4level = as.factor(race.4level),
    hisp = as.factor(hisp),
    high.educ.bl = as.factor(high.educ.bl),
    household.income.bl = as.factor(household.income.bl),
    acs_raked_propensity_score = as.numeric(acs_raked_propensity_score),
    rel_family_id.bl = as.factor(rel_family_id.bl),
    site_id_l = as.factor(site_id_l),
    nihtbx_totalcomp_fc = as.numeric(nihtbx_totalcomp_fc),
    anthroweightcalc = as.numeric(anthroweightcalc),
    anthroheightcalc = as.numeric(anthroheightcalc)
  ) %>%
  # Exclude cases from unused assessment waves
  filter(!is.na(eventname))

# Define the function to compute difference scores for a given variable and provide a summary
# Function to compute difference scores for a given variable and provide a summary
compute_difference_and_summary <- function(df, variable_name) {
  # Define the event names of interest
  baseline_event <- "baseline_year_1_arm_1"
  followup_event <- "1_year_follow_up_y_arm_1"

  # Compute the difference between Baseline and Year 1 data for the given variable
  diff_data <- df %>%
    filter(eventname %in% c(baseline_event, followup_event)) %>% # Filter for specific event names
    select(src_subject_id, eventname, all_of(variable_name)) %>% # Select required columns
    spread(eventname, variable_name) %>% # Convert data from long to wide format
    mutate(diff = get(followup_event) - get(baseline_event)) %>% # Compute difference between the two time points
    drop_na(diff) # Exclude rows with NA in the computed difference

  # Summarize the computed difference scores
  diff_summary <- summary(diff_data$diff)

  # Return the difference data and its summary
  list(data = diff_data, summary = diff_summary)
}

# List of variables for which difference scores are to be computed
variables_of_interest <- c("anthroheightcalc")

# Compute and store difference scores and summaries for each variable in a list
difference_and_summary_list <- lapply(variables_of_interest, function(var) {
  compute_difference_and_summary(df, var)
})

# Extract the difference data for the 'anthroheightcalc' variable
height_diff_data <- difference_and_summary_list[[1]]$data

# Merge the 'diff' column back to the main df using 'src_subject_id' as the key
df <- left_join(df, height_diff_data %>% select(src_subject_id, diff), by = "src_subject_id")

# Compute statistical summaries for the difference score variable
lapply(difference_and_summary_list, function(item) {
  print(item$summary)
})

## Summary statistics
# Compute summary statistics for Height by eventname
summary <- df %>%
  group_by(eventname) %>%
  get_summary_stats(anthroheightcalc, type = "mean_sd")

data.frame(summary)

# Extract the difference scores for height from the computed list
diff_data <- difference_and_summary_list[[1]]$data$diff

# Perform a one-sample t-test on the difference scores for height
test_result <- t.test(diff_data, mu = 0, na.rm = TRUE)

test_result

# Filter data for Baseline
baseline_data <- df %>% filter(eventname == "Baseline")

# Filter data for Year_1 and join with Baseline data
joined_data <- baseline_data %>%
  left_join(df %>% filter(eventname == "Year_1"), by = "src_subject_id", suffix = c("_baseline", "_year1"))

# Create scatterplot
scatterplot <- ggplot(df, aes(x = anthroheightcalc, y = diff)) +
  geom_point(aes(color = sex), alpha = 0.5) + # Setting alpha to ensure points are visible
  geom_smooth(method = "lm", color = "red") + # Adding linear regression line
  labs(
    title = "Scatterplot of anthroheightcalc vs. Difference Score",
    x = "anthroheightcalc",
    y = "Difference Score"
  ) +
  theme_minimal()

scatterplot
