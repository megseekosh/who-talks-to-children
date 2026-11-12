# This script calculates estimated weekly verbal input hours
# missed by failing to sample children's childcare environments,
# stratified by socioeconomic bracket and age.

library(ggplot2)
library(dplyr)
library(car)          
library(emmeans)     
library(broom)       
library(scales)       

# For combining plots
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  stop("Package gridExtra is required: install.packages('gridExtra')")
}
library(gridExtra)
library(grid)  # For textGrob and gpar

if (!requireNamespace("ARTool", quietly = TRUE)) {
  cat("Note: ARTool package not installed. Non-parametric ANOVA will be skipped if needed.\n")
  cat("      Install with: install.packages('ARTool')\n\n")
}


# Get the directory where this script is located
script_dir <- getwd()  # Default fallback

# Try to get script path from RStudio
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  tryCatch({
    script_path <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(script_path) && nchar(script_path) > 0 && file.exists(script_path)) {
      script_dir <- dirname(script_path)
    }
  }, error = function(e) {
    # If error occurs, script_dir remains as getwd()
  })
}

# If script_dir is empty or doesn't exist, use current working directory
if (is.null(script_dir) || length(script_dir) == 0 || !dir.exists(script_dir)) {
  script_dir <- getwd()
}

cat("Script directory:", script_dir, "\n")
cat("Working directory:", getwd(), "\n\n")

# Create figures directory in the script's directory
figures_dir <- file.path(script_dir, "figures")
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
}
# Ensure directory was created successfully
if (!dir.exists(figures_dir)) {
  stop("Failed to create figures directory: ", figures_dir)
}
cat("Figures directory:", figures_dir, "\n\n")

# Helper function to ensure figures directory exists before saving
ensure_figures_dir <- function() {
  if (!dir.exists(figures_dir)) {
    dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(figures_dir)) {
    stop("Failed to create figures directory: ", figures_dir)
  }
  return(invisible(TRUE))
}


# Use real data values from NCES-based analysis and sleep duration data
# Expected columns: Missed_Observation_Hours, Bracket, Age_Group, Verbal_Input_Ratio

# Define age groups and brackets
age_groups <- c("Infant\n(<12 mos)", "Toddler\n(1-2yrs)", "Preschooler\n(3-5yrs)")
brackets <- c("Lower Income", "Middle Income", "Upper Income")

# 1.1 NAP DURATION DATA (hours per day during childcare)
# Staton et al. (2020). 
# 
# Estimated nap durations (average daytime nap duration for children who nap):
# - Infants (<12 mos): 3.1 hours per day
# - Toddlers (1-2yrs): 2.3 hours per day
# - Preschoolers (3-5yrs): 1.7 hours per day (average of 2-3yr, 3-4yr, and 4-5yr estimates)
# ------------------------------------------------------------------------------
nap_data <- data.frame(
  Age_Group = age_groups,
  Nap_Hours_Per_Day = c(3.1, 2.3, 1.7),  # Nap hours/day from Staton et al. (2020) meta-analysis
  stringsAsFactors = FALSE
)

# Calculate nap hours per week (assuming 5 days/week in childcare)
nap_data$Nap_Hours_Per_Week <- nap_data$Nap_Hours_Per_Day * 5

# Keep sleep data for reference (used in some calculations)
# But primary focus should be on nap durations during childcare hours
# 
# SOURCE: Galland et al. (2012). Normal sleep patterns in infants and children: A systematic 
#         review of observational studies. Sleep Medicine Reviews, 16(3), 213-222.
#         See 1-s2.0-S1087079211000682-main.pdf, Table 2 (approximately page 217)
sleep_data <- data.frame(
  Age_Group = age_groups,
  Sleep_Hours_Per_Day = c(12.7, 12.6, 11.75),  # Mean sleep hours/24h from Galland et al. (2012) meta-analysis (home-based), Table 2
                                                                  # Preschoolers: average of 12.0 and 11.5 = 11.75
  Sleep_Lower = c(8.8, 10.0, 9.1),
  Sleep_Upper = c(20, 15.2, 14.2),
  stringsAsFactors = FALSE
)

# Calculate awake hours per week (for reference, but nap data is more relevant for childcare)
sleep_data$Awake_Hours_Per_Week <- (24 - sleep_data$Sleep_Hours_Per_Day) * 7 # this includes naps inherently, not from Staton et al but because it was part of total sleep


# Percentage of families using center-based care
# SOURCE: NCES Table 202.30 (tabn202.30.xls), "Percent participating in center-based programs" column
#         Row 10: Under 1 year old = 14.132582% (rounded to 14.1%)
#         Row 11: 1 to 2 years old = 26.468433% (rounded to 26.4%)
#         Row 12: 3 to 5 years old = 62.536974% (rounded to 62.5%)
#         NOTE: This data comes from the Excel file (tabn202.30.xls), NOT from the NCES PDF
center_based_pct <- data.frame(
  Age_Group = age_groups,
  Pct_Center_Based = c(14.1, 26.5, 62.5),  # Percentages from NCES Table 202.30 (tabn202.30.xls), for 1, 1-2, and 3-5 year olds
  stringsAsFactors = FALSE
)

# Base weekly hours in nonparental care by child age
# SOURCE: NCES Table 202.30 (tabn202.30.xls), "Average hours per week in nonparental care" column 
#         NOTE: This data comes from the Excel file (tabn202.30.xls), NOT from the NCES PDF
age_base_hours <- data.frame(
  Age_Group = age_groups,
  Base_Weekly_Hours = c(31.8, 32.1, 29.2),  # From NCES Table 202.30 (tabn202.30.xls): Under 1yr, 1-2yrs, 3-5yrs
  stringsAsFactors = FALSE
)


# socioeconomics
# Actual values from NCES Table 202.30 (tabn202.30.xls), column 7: "Average hours per week in nonparental care"
# 
# SOURCE DATA FROM NCES Table 202.30 (tabn202.30.xls), collapsed across race/ethnicity:
# - Row 65, Poor (Lower Income): 30.475993 hrs/week (rounded to 30.48)
# - Row 66, Near-poor (Middle Income): 29.116676 hrs/week (rounded to 29.12)
# - Row 67, Nonpoor (Upper Income): 31.09754 hrs/week (rounded to 31.10)
# NOTE: This data comes from the Excel file (tabn202.30.xls), NOT from the NCES PDF
#       These values are from the "Poverty status of household" section (rows 65-67),
#       which collapses across all race/ethnicity categories.
#
# CALCULATION METHOD:
# These multipliers scale the base weekly hours (by age) to account for 
# socioeconomic differences. Multipliers are calculated relative to Nonpoor 
# (Upper Income) as the baseline (1.000):
#   - Poor multiplier = 30.475993 / 31.09754 = 0.980 (uses childcare 2.0% less than Upper Income)
#   - Near-poor multiplier = 29.116676 / 31.09754 = 0.936 (uses childcare 6.4% less than Upper Income)
#   - Nonpoor multiplier = 31.09754 / 31.09754 = 1.000 (baseline)
#
# These multipliers are applied to age-specific base hours to synthesize
# weekly childcare hours for each age × socioeconomic status combination.
# ------------------------------------------------------------------------------
bracket_multipliers <- data.frame(
  Bracket = brackets,
  SES_Multiplier = c(0.980, 0.936, 1.000),  # Poor, Near-poor, Nonpoor (from NCES Table 202.30, rows 65-67, tabn202.30.xls)
  SE_Hours = c(1.527981, 1.236194, 0.379222),  # Standard errors from NCES Table 202.30, column 10, rows 65-67
  stringsAsFactors = FALSE
)


# 1.5 SYNTHESIZE WEEKLY CHILDCARE HOURS BY AGE × SOCIOECONOMIC STATUS
# Combine age base hours with socioeconomic multipliers
mean_hours_data <- expand.grid(
  Bracket = brackets,
  Age_Group = age_groups,
  stringsAsFactors = FALSE
) %>%
  left_join(age_base_hours, by = "Age_Group") %>%
  left_join(bracket_multipliers, by = "Bracket") %>%
  mutate(
    # Synthesize: base hours from age, adjusted by socioeconomic multiplier
    Mean_Hours = Base_Weekly_Hours * SES_Multiplier,
    # SD based on standard errors from NCES Table 202.30, column 10, rows 65-67
    # Using standard errors as standard deviations for simulation (SE ≈ SD for large samples)
    SD_Hours = SE_Hours
  ) %>%
  select(Bracket, Age_Group, Mean_Hours, SD_Hours)

# Generate individual-level data based on real means
set.seed(123)
n_per_group <- 100  # Sample size per group

# Generate individual records
data_list <- list()
for (i in 1:nrow(mean_hours_data)) {
  bracket <- mean_hours_data$Bracket[i]
  age_group <- mean_hours_data$Age_Group[i]
  mean_hours <- mean_hours_data$Mean_Hours[i]
  sd_hours <- mean_hours_data$SD_Hours[i]
  
  n <- n_per_group
  
  # Generate hours using the real mean and SD values
  # Deterministic: every record in a cell carries the NCES-based mean for that cell.
  # (Earlier versions drew rnorm(n, mean_hours, sd_hours); the survey SEs are now shown as
  # error bars directly rather than through synthetic draws.)
  missed_hours <- rep(mean_hours, n)
  
  data_list[[i]] <- data.frame(
    Missed_Observation_Hours = missed_hours,
    Bracket = bracket,
    Age_Group = age_group,
    stringsAsFactors = FALSE
  )
}

# Combine all data
data <- bind_rows(data_list)

# Ensure factors are properly ordered
data$Bracket <- factor(data$Bracket, levels = c("Lower Income", "Middle Income", "Upper Income"))
data$Age_Group <- factor(data$Age_Group, levels = age_groups)

# Calculate awake childcare hours (accounting for naps)
data <- data %>%
  left_join(nap_data, by = "Age_Group") %>%
  mutate(
    # Awake hours = total childcare hours - nap hours
    Awake_Childcare_Hours = Missed_Observation_Hours - Nap_Hours_Per_Week
  )

# note on verbal input proportion
# We do not have empirical data on what proportion of childcare hours contain
# verbal input. Instead, we use simulation to vary this proportion (see Section 6)
# and calculate missed words as a function of different assumed proportions.
# 
# The main analysis focuses on Missed_Observation_Hours (the actual childcare
# hours that would be missed), while the simulation explores how missed verbal
# input varies across different assumed proportions of childcare hours containing
# verbal input (30-80%).

# descriptive statistics
# Calculate mean, median, SD, IQR, and N for each combination of socioeconomic Bracket and Age_Group

descriptive_stats <- data %>%
  group_by(Bracket, Age_Group) %>%  # Bracket refers to socioeconomic bracket
  summarise(
    N = n(),
    Mean = mean(Missed_Observation_Hours, na.rm = TRUE),
    Median = median(Missed_Observation_Hours, na.rm = TRUE),
    SD = sd(Missed_Observation_Hours, na.rm = TRUE),
    IQR = IQR(Missed_Observation_Hours, na.rm = TRUE),
    Min = min(Missed_Observation_Hours, na.rm = TRUE),
    Max = max(Missed_Observation_Hours, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(Bracket, Age_Group)

# Also create summary by socioeconomic Bracket only
bracket_stats <- data %>%
  group_by(Bracket) %>%
  summarise(
    N = n(),
    Mean = mean(Missed_Observation_Hours, na.rm = TRUE),
    Median = median(Missed_Observation_Hours, na.rm = TRUE),
    SD = sd(Missed_Observation_Hours, na.rm = TRUE),
    IQR = IQR(Missed_Observation_Hours, na.rm = TRUE),
    .groups = 'drop'
  )

# Summary by Age_Group only
age_stats <- data %>%
  group_by(Age_Group) %>%
  summarise(
    N = n(),
    Mean = mean(Missed_Observation_Hours, na.rm = TRUE),
    Median = median(Missed_Observation_Hours, na.rm = TRUE),
    SD = sd(Missed_Observation_Hours, na.rm = TRUE),
    IQR = IQR(Missed_Observation_Hours, na.rm = TRUE),
    .groups = 'drop'
  )

# No inferential test: the per-cell values are deterministic (one NCES-based value per cell,
# repeated n_per_group times), so there is no sampling variance to test. The survey standard
# errors are shown as error bars in the figure instead.
anova_table <- NULL
bracket_pairs_result <- NULL
age_pairs_result <- NULL
interaction_pairs_result <- NULL

# VISUALIZATION
# 5.1 Plot of Raw Missed Observation Hours (before verbal input adjustment)

# Prepare significance data for plotting using ggpubr format
# Extract significant pairwise comparisons within each age group
sig_data <- data.frame()
if (!is.null(interaction_pairs_result)) {
  # Convert emmeans results to data frame
  pairs_df <- as.data.frame(interaction_pairs_result)
  
  # Filter for significant comparisons (p < 0.05)
  sig_comparisons <- pairs_df[pairs_df$p.value < 0.05, ]
  
  if (nrow(sig_comparisons) > 0) {
    # Parse the contrast column to extract groups being compared
    sig_comparisons$contrast <- as.character(sig_comparisons$contrast)
    
    # Check if Age_Group column exists (it should when using by = "Age_Group")
    if (!"Age_Group" %in% names(sig_comparisons)) {
      # If not, try to extract from contrast strings
      # This shouldn't happen with by = "Age_Group", but handle it just in case
      sig_comparisons$Age_Group <- NA
    }
    
    # Calculate y positions for each age group
    y_positions <- data %>%
      group_by(Age_Group) %>%
      summarise(y_max = max(Missed_Observation_Hours, na.rm = TRUE) * 1.15,
                .groups = 'drop')
    
    # Extract bracket names and create annotation data
    for (i in 1:nrow(sig_comparisons)) {
      contrast_str <- sig_comparisons$contrast[i]
      
      # Get age group - should be in Age_Group column when using by = "Age_Group"
      if ("Age_Group" %in% names(sig_comparisons)) {
        age_group <- sig_comparisons$Age_Group[i]
      } else {
        age_group <- NA
      }
      
      # If age_group is NA or missing, try to extract from contrast string
      if (is.na(age_group) || length(age_group) == 0 || nchar(age_group) == 0) {
        # Try to match age group patterns in contrast string
        for (ag in age_groups) {
          # Escape special characters for regex
          ag_escaped <- gsub("\\(", "\\\\(", ag)
          ag_escaped <- gsub("\\)", "\\\\)", ag_escaped)
          ag_escaped <- gsub("\n", "\\\\n", ag_escaped)
          if (grepl(ag_escaped, contrast_str)) {
            age_group <- ag
            break
          }
        }
      }
      
      # Skip if we still don't have an age group
      if (is.na(age_group) || length(age_group) == 0 || nchar(age_group) == 0) {
        next
      }
      
      # Parse contrast to extract bracket names
      # Format might be "Lower Income Infant\n(<12 mos) - Middle Income Infant\n(<12 mos)"
      # or "Lower Income - Middle Income" (when Age_Group is separate column)
      parts <- strsplit(contrast_str, " - ")[[1]]
      if (length(parts) == 2) {
        # Extract bracket name from first part (remove age group info if present)
        group1_str <- trimws(parts[1])
        group2_str <- trimws(parts[2])
        
        # Remove age group patterns to get just bracket name
        group1 <- group1_str
        group2 <- group2_str
        for (ag in age_groups) {
          # Escape for fixed matching
          ag_pattern <- gsub("\\(", "\\\\(", ag)
          ag_pattern <- gsub("\\)", "\\\\)", ag_pattern)
          ag_pattern <- gsub("\n", "\\\\n", ag_pattern)
          group1 <- gsub(ag_pattern, "", group1)
          group2 <- gsub(ag_pattern, "", group2)
        }
        group1 <- trimws(group1)
        group2 <- trimws(group2)
        
        # Verify these are valid bracket names
        if (group1 %in% brackets && group2 %in% brackets && 
            !is.na(age_group) && length(age_group) > 0) {
          # Get y position for this age group
          y_pos <- y_positions$y_max[y_positions$Age_Group == age_group]
          
          if (length(y_pos) == 0 || is.na(y_pos)) {
            next
          }
          
          # Determine significance label
          p_val <- sig_comparisons$p.value[i]
          label <- ifelse(p_val < 0.001, "***",
                         ifelse(p_val < 0.01, "**",
                                ifelse(p_val < 0.05, "*", "")))
          
          sig_data <- rbind(sig_data, data.frame(
            Age_Group = age_group,
            group1 = group1,
            group2 = group2,
            p.value = p_val,
            p.adj = p_val,  # Already adjusted by Tukey
            y.position = y_pos,
            label = label,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
}

# Create base plot
raw_hours_plot <- ggplot(data, 
                         aes(x = Bracket, 
                             y = Missed_Observation_Hours, 
                             fill = Bracket)) +
  
  # Violin plot for distribution shape
  geom_violin(trim = TRUE, 
              alpha = 0.6, 
              scale = "width") +
  
  # Box plot for quartiles and median
  geom_boxplot(width = 0.2, 
               outlier.shape = NA,
               fill = "white", 
               color = "black",
               linewidth = 0.5) +
  
  # Mean points
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 23,
               size = 3, 
               fill = "darkred", 
               color = "black") +
  
  # Facet by Age Group
  facet_wrap(~ Age_Group, 
             scales = "fixed", 
             ncol = length(age_groups)) +
  
  # Labels
  labs(title = "Weekly Missed Observation Hours by Socioeconomic Bracket",
       subtitle = "Raw childcare hours missed (before accounting for verbal input content)",
       x = "Socioeconomic\nBracket",
       y = "Weekly Missed\nObservation Hours",
       caption = "Distribution shown via violin plots. Box plots show median, IQR, and quartiles.\nRed diamonds indicate group means. Significance: * p<0.05, ** p<0.01, *** p<0.001") +
  
  # Theme
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 9, hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 10, angle = 0),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    strip.text = element_text(size = 13, face = "bold")
  ) +
  
  scale_fill_brewer(palette = "Set1")

# Add significance brackets manually (works better with faceted plots)
if (nrow(sig_data) > 0) {
  # Get bracket levels for x-axis positioning
  bracket_levels <- levels(data$Bracket)
  
  # Create annotation data frames for segments and text
  segment_data <- data.frame()
  text_data <- data.frame()
  
  for (i in 1:nrow(sig_data)) {
    age_group <- sig_data$Age_Group[i]
    group1 <- sig_data$group1[i]
    group2 <- sig_data$group2[i]
    y_pos <- sig_data$y.position[i]
    label <- sig_data$label[i]
    
    # Get x positions
    x1 <- which(bracket_levels == group1)
    x2 <- which(bracket_levels == group2)
    
    # Create bracket segments: left vertical, horizontal, right vertical
    segment_data <- rbind(segment_data,
      data.frame(
        Age_Group = age_group,
        x = c(x1, x1, x2),
        xend = c(x1, x2, x2),
        y = c(y_pos, y_pos, y_pos),
        yend = c(y_pos * 0.98, y_pos, y_pos * 0.98),
        stringsAsFactors = FALSE
      )
    )
    
    # Add text label
    text_data <- rbind(text_data,
      data.frame(
        Age_Group = age_group,
        x = mean(c(x1, x2)),
        y = y_pos * 1.02,
        label = label,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Add segments and text to plot
  raw_hours_plot <- raw_hours_plot +
    geom_segment(data = segment_data,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 inherit.aes = FALSE,
                 linewidth = 0.5) +
    geom_text(data = text_data,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE,
              size = 4,
              fontface = "bold")
}



# Save the plot
output_file <- file.path(figures_dir, "missed_observation_hours_plot.png")
ggsave(output_file, 
       plot = raw_hours_plot, 
       width = 12, 
       height = 5, 
       dpi = 300)

# 5.2 Additional Visualization: Interaction Plot (Awake Childcare Hours)
interaction_plot <- ggplot(data, 
                           aes(x = Age_Group, 
                               y = Awake_Childcare_Hours, 
                               color = Bracket,
                               group = Bracket)) +
  stat_summary(fun = mean, 
               geom = "line", 
               linewidth = 2) +
  stat_summary(fun = mean, 
               geom = "point", 
               size = 4.5) +
  # Error bars: the NCES survey standard errors for weekly hours in nonparental
  # care by poverty status (Table 202.30, column 10), not the standard error of
  # the 100 synthetic draws per cell (which would be the survey SE divided by 10)
  geom_errorbar(data = data %>%
                  group_by(Bracket, Age_Group) %>%
                  summarise(Mean_Awake = mean(Awake_Childcare_Hours, na.rm = TRUE), .groups = "drop") %>%
                  left_join(bracket_multipliers %>% select(Bracket, SE_Hours), by = "Bracket"),
                aes(x = Age_Group, ymin = Mean_Awake - SE_Hours, ymax = Mean_Awake + SE_Hours,
                    color = Bracket, group = Bracket),
                inherit.aes = FALSE,
                width = 0.15,
                linewidth = 1) +
  labs(title = NULL,
       subtitle = NULL,
       x = "Age Group",
       y = "Mean Weekly Missed\nAwake Childcare Hours",
       color = "Socioeconomic Bracket") +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(1.5, "cm"),
    legend.margin = margin(b = 15),
    legend.box.margin = margin(b = 10),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.6),
    panel.grid.minor = element_blank()
  ) +
  scale_color_manual(values = c("Lower Income" = "#E41A1C", 
                                 "Middle Income" = "#377EB8", 
                                 "Upper Income" = "#4DAF4A")) +
  scale_x_discrete(limits = age_groups) +
  guides(color = guide_legend(title.position = "top", 
                               title.hjust = 0.5,
                               nrow = 1))

print(interaction_plot)
ggsave(file.path(figures_dir, "interaction_plot.png"), 
       plot = interaction_plot, 
       width = 8, 
       height = 5, 
       dpi = 300)

# simulations: varying verbal input %
# ============================================================================
# Simulate how undersampling varies when verbal input ranges from 30-80%
# of childcare hours

# Define verbal input percentages to test (30% to 80%, continuous)
# Using fine increments for smooth curves
verbal_input_pcts <- seq(0.30, 0.80, by = 0.01)

# Calculate mean missed observation hours by group (from actual data)
group_means <- data %>%
  group_by(Bracket, Age_Group) %>%
  summarise(
    Mean_Missed_Hours = mean(Missed_Observation_Hours, na.rm = TRUE),
    .groups = 'drop'
  )

# Simulate for each verbal input percentage
simulation_results <- expand.grid(
  Bracket = brackets,
  Age_Group = age_groups,
  Verbal_Input_Pct = verbal_input_pcts,
  stringsAsFactors = FALSE
) %>%
  left_join(group_means, by = c("Bracket", "Age_Group")) %>%
  left_join(nap_data, by = "Age_Group") %>%
  mutate(
    # Account for naps: children cannot receive verbal input while napping
    # Awake hours during childcare = Total childcare hours - Nap hours
    Awake_Childcare_Hours = Mean_Missed_Hours - Nap_Hours_Per_Week,
    # Calculate estimated missed verbal input for each percentage
    # Only count awake hours (subtract nap time) - children can't receive verbal input while napping
    Estimated_Missed_Verbal_Input = Awake_Childcare_Hours * Verbal_Input_Pct,
    # Calculate as percentage of total possible verbal input during awake childcare hours
    # Use nap hours per week (already joined from nap_data)
    # Undersampling as percentage of total possible verbal input during awake childcare hours
    Pct_Of_Total_Possible = (Estimated_Missed_Verbal_Input / Awake_Childcare_Hours) * 100
  )

# Note: Each combination of Bracket × Age_Group × Verbal_Input_Pct produces exactly one computed value
# No need to summarize - these are the computed missed verbal input hours from the simulation
simulation_summary <- simulation_results %>%
  select(Bracket, Age_Group, Verbal_Input_Pct, 
         Estimated_Missed_Verbal_Input, Pct_Of_Total_Possible, Awake_Childcare_Hours)

# Convert hours to words using Soderstrom et al. 2018 estimates
# Also calculate total possible words during awake hours (accounting for sleep)
# Based on mean words per 5-minute increment by care setting and number of adults (Soderstrom et al., 2018):
# Home: 1 adult = 104.9 words/5min, 2 adults = 125.6, 3 adults = 164.0, 4 adults = 253.5
# Home Daycare: 1 adult = 73.4, 2 adults = 104.4, 3 adults = 88.6
# Daycare Centre: 1 adult = 119.9, 2 adults = 102.1, 3 adults = 108.6, 4 adults = 87.1, 5+ = 93.4
# 
# Conversion: 1 hour = 12 five-minute increments
# Summary statistics for Daycare Centre values only (since we're analyzing childcare/daycare settings):
# - Mean: 102.22 × 12 = 1,226.6 words/hour
# - Median: 102.1 × 12 = 1,225.2 words/hour  
# - Range: 87.1 × 12 to 119.9 × 12 = 1,045.2 to 1,438.8 words/hour
#
# Using median (102.1 words/5min = 1,225 words/hour) for daycare centres (assuming 2 adults)
# Note: words_per_hour from Soderstrom et al. is words per CLOCK hour (includes silence)
# So we multiply total awake childcare hours by words_per_hour, then by verbal input percentage
words_per_hour <- 102.1 * 12  # Median words per 5-min increment for daycare centres × 12 = 1,225.2 words/hour from Soderstrom et al. 2018

simulation_summary <- simulation_summary %>%
  mutate(
    # Calculate missed words: convert missed verbal input hours to words
    # Estimated_Missed_Verbal_Input is hours of verbal input missed (already accounts for Verbal_Input_Pct)
    # words_per_hour includes silence (averaged across all 5-min increments from Soderstrom et al.)
    # So: missed words = missed verbal input hours × words per hour
    Estimated_Missed_Words = Estimated_Missed_Verbal_Input * words_per_hour,
    # Calculate total possible words during awake childcare hours (accounting for naps)
    # Total possible = all awake childcare hours × words/hour (words_per_hour includes silence)
    # This represents maximum words possible if sampling all awake hours, regardless of verbal input %
    Total_Possible_Words_Awake = Awake_Childcare_Hours * words_per_hour,
    # Missed words as percentage of total possible words during awake hours
    # This shows what % of total possible words are missed, given the Verbal_Input_Pct assumption
    Pct_Of_Total_Possible_Words = (Estimated_Missed_Words / Total_Possible_Words_Awake) * 100
  )

# Plot: Estimated Missed Verbal Input by Verbal Input Percentage
# Y-axis shows computed missed verbal input hours = Missed_Observation_Hours × Verbal_Input_Pct
# This IS accounting for verbal input percentage (multiplied by Verbal_Input_Pct)
# Facet by age group for better readability, ordered: Infant > Toddler > Preschooler

# Ensure age groups are ordered correctly
simulation_summary$Age_Group <- factor(simulation_summary$Age_Group, 
                                       levels = c("Infant\n(<12 mos)", "Toddler\n(1-2yrs)", "Preschooler\n(3-5yrs)"))

# Create plot with words missed on y-axis
verbal_input_simulation_plot <- ggplot(simulation_summary,
                                       aes(x = Verbal_Input_Pct * 100,
                                           y = Estimated_Missed_Words,
                                           color = Bracket,
                                           linetype = Bracket,
                                           group = Bracket)) +
  geom_line(linewidth = 2.5, alpha = 0.6) +
  facet_wrap(~ Age_Group, ncol = 3, scales = "fixed") +
  labs(title = NULL,
       subtitle = NULL,
       x = "Hypothetical Percentage of Childcare Hours\nContaining Verbal Input (%)",
       y = "Estimated Weekly\nMissed Words",
       color = "Socioeconomic Bracket",
       caption = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15), face = "italic"),
    plot.caption = element_text(size = 10, hjust = 0.5, margin = margin(t = 20, b = 5)),
    plot.margin = margin(t = 10, r = 10, b = 100, l = 10),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.position = "none",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(3, "cm"),
    legend.key.height = unit(1, "cm"),
    legend.spacing.x = unit(2, "cm"),
    legend.margin = margin(t = 30, b = 20),
    legend.box.margin = margin(t = 20, b = 20),
    strip.text = element_text(size = 16, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.6)
  ) +
  scale_color_manual(values = c("Lower Income" = "#E41A1C", 
                                 "Middle Income" = "#377EB8", 
                                 "Upper Income" = "#4DAF4A"),
                     labels = c("Lower Income", "Middle Income", "Upper Income")) +
  scale_linetype_manual(values = c("Lower Income" = "solid",
                                    "Middle Income" = "dashed",
                                    "Upper Income" = "dotted"),
                        labels = c("Lower Income", "Middle Income", "Upper Income")) +
  scale_x_continuous(breaks = seq(30, 80, by = 10), limits = c(30, 80)) +
  scale_y_continuous(labels = scales::comma) +
  guides(color = guide_legend(override.aes = list(linewidth = 3.5, linetype = c("solid", "dashed", "dotted"), alpha = 0.8),
                               nrow = 1, byrow = TRUE, title.position = "top", title.hjust = 0.5),
         linetype = "none")

print(verbal_input_simulation_plot)
ggsave(file.path(figures_dir, "verbal_input_simulation_plot.png"),
       plot = verbal_input_simulation_plot,
       width = 16,
       height = 10,
       dpi = 300)

# Combine interaction plot and simulation plot with shared legend
# Extract legend from interaction plot
get_legend <- function(plot) {
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Create versions with larger fonts for combined figure (reduced by 15% from previous)
interaction_plot_combined <- interaction_plot +
  theme(
    plot.subtitle = element_text(size = 23.2, hjust = 0.5),
    axis.title = element_text(size = 29.8, face = "bold"),
    axis.text = element_text(size = 23.2),
    axis.text.x = element_text(size = 23.2, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 23.2),
    legend.title = element_text(size = 26.5, face = "bold"),
    legend.text = element_text(size = 23.2)
  )

verbal_input_simulation_plot_combined <- verbal_input_simulation_plot +
  theme(
    plot.subtitle = element_text(size = 23.2, hjust = 0.5, margin = margin(b = 15), face = "italic"),
    plot.caption = element_text(size = 19.9, hjust = 0.5, margin = margin(t = 20, b = 5)),
    axis.title = element_text(size = 29.8, face = "bold"),
    axis.text = element_text(size = 23.2),
    axis.text.x = element_text(size = 23.2, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 23.2),
    strip.text = element_text(size = 29.8, face = "bold")
  )

# Update interaction plot legend styling for combined figure
interaction_plot_with_legend <- interaction_plot_combined +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 33.2, face = "bold"),
        legend.text = element_text(size = 29.8),
        legend.key.width = unit(2.5, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.spacing.x = unit(2, "cm"),
        legend.margin = margin(t = 20, b = 10))

# Extract legend
shared_legend <- get_legend(interaction_plot_with_legend)

# Remove legends from both plots (use combined versions with larger fonts)
interaction_plot_no_legend <- interaction_plot_combined + theme(legend.position = "none")
verbal_input_simulation_plot_no_legend <- verbal_input_simulation_plot_combined + theme(legend.position = "none")

# Combine plots with shared legend (side by side)
# Create plots with labels (font sizes reduced by 15% from previous)
plot_a <- arrangeGrob(interaction_plot_no_legend, 
                      top = textGrob("A", x = unit(0.02, "npc"), 
                                     gp = gpar(fontsize = 43.1, fontface = "bold")))
plot_b <- arrangeGrob(verbal_input_simulation_plot_no_legend, 
                      top = textGrob("B", x = unit(0.02, "npc"), 
                                     gp = gpar(fontsize = 43.1, fontface = "bold")))

# Combine: plots side by side, legend below spanning both
combined_plots <- grid.arrange(
  plot_a, plot_b,
  shared_legend,
  ncol = 2,
  nrow = 2,
  widths = c(1, 1.5),
  heights = c(1, 0.12),
  layout_matrix = rbind(c(1, 2), c(3, 3))
)

print(combined_plots)
ggsave(file.path(figures_dir, "combined_interaction_simulation_plot.png"),
       plot = combined_plots,
       width = 20,
       height = 8,
       dpi = 300)

# Developmental Review version: add panel C, what a study would find under narrow
# vs permissive verbal input measures (built in study_outcome_simulation.R)
vc_env <- new.env()
vc_env$hist_text_scale <- 1.85
vc_env$script_dir <- script_dir
sys.source(file.path(script_dir, "study_outcome_simulation.R"), envir = vc_env)
vocab_plot_combined <- vc_env$p_hist +
  theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 30))
plot_c <- arrangeGrob(vocab_plot_combined,
                      top = textGrob("C", x = unit(0.02, "npc"),
                                     gp = gpar(fontsize = 43.1, fontface = "bold")))
# Panels A and B on top with their shared legend; panel C below spanning the width
combined_plots_vocab <- grid.arrange(
  plot_a, plot_b,
  shared_legend,
  plot_c,
  ncol = 2,
  nrow = 3,
  widths = c(1, 1.5),
  heights = c(1, 0.12, 1.45),
  layout_matrix = rbind(c(1, 2), c(3, 3), c(4, 4))
)
ggsave(file.path(figures_dir, "combined_interaction_simulation_vocab_plot.png"),
       plot = combined_plots_vocab,
       width = 20,
       height = 20,
       dpi = 300)

# plot: Undersampling as Percentage of Total Possible Verbal Input
# Facet by age group for better readability
undersampling_plot <- ggplot(simulation_summary,
                             aes(x = Verbal_Input_Pct * 100,
                                 y = Pct_Of_Total_Possible,
                                 color = Bracket,
                                 group = Bracket)) +
  geom_line(linewidth = 1.5, alpha = 0.8) +
  facet_wrap(~ Age_Group, ncol = 3, scales = "free_y") +
  labs(title = "Estimated Undersampling: Missed Verbal Input as % of Total Possible",
       subtitle = "Percentage of total possible weekly verbal input that is missed",
       x = "Percentage of Childcare Hours\nContaining Verbal Input (%)",
       y = "Missed Verbal Input as\n% of Total Possible",
       color = "Socioeconomic\nBracket",
       caption = "Total possible = all awake hours per week") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5)
  ) +
  scale_color_brewer(palette = "Set1",
                     labels = c("Lower Income", "Middle Income", "Upper Income")) +
  scale_x_continuous(breaks = seq(30, 80, by = 10), limits = c(30, 80)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))

print(undersampling_plot)
ggsave(file.path(figures_dir, "undersampling_plot.png"),
       plot = undersampling_plot,
       width = 10,
       height = 6,
       dpi = 300)

# Create a table showing key results

# Show results for 40%, 60%, and 80% verbal input
key_pcts <- c(0.40, 0.60, 0.80)
for (pct in key_pcts) {
  cat(sprintf("\n--- At %.0f%% Verbal Input ---\n", pct * 100))
  key_results <- simulation_summary %>%
    filter(Verbal_Input_Pct == pct) %>%
    select(Bracket, Age_Group, Estimated_Missed_Verbal_Input, Pct_Of_Total_Possible) %>%
    arrange(Age_Group, Bracket)
  print(key_results)
}

# Export simulation results
write.csv(simulation_summary,
          file.path(script_dir, "verbal_input_simulation_results.csv"),
          row.names = FALSE)

# Numbers quoted in the manuscript text
awake_by_cell <- simulation_summary %>% filter(Verbal_Input_Pct == min(Verbal_Input_Pct)) %>%
  select(Bracket, Age_Group, Awake_Childcare_Hours) %>%
  left_join(sleep_data %>% select(Age_Group, Awake_Hours_Per_Week), by = "Age_Group") %>%
  mutate(Awake_Per_Day = Awake_Childcare_Hours / 5,
         Pct_Of_Weekly_Waking = 100 * Awake_Childcare_Hours / Awake_Hours_Per_Week)
cat("\nAwake childcare hours per day, mean over the three age groups, by income bracket:\n")
print(awake_by_cell %>% group_by(Bracket) %>% summarise(Awake_Per_Day = round(mean(Awake_Per_Day), 2), .groups = "drop"))
cat(sprintf("Share of weekly waking hours spent awake in childcare, across cells: %.1f%% to %.1f%%\n",
            min(awake_by_cell$Pct_Of_Weekly_Waking), max(awake_by_cell$Pct_Of_Weekly_Waking)))


# Save descriptive statistics to CSV
write.csv(descriptive_stats, 
          file.path(script_dir, "descriptive_statistics_by_group.csv"), 
          row.names = FALSE)

# Save ANOVA results
if (!is.null(anova_table)) write.csv(anova_table, file.path(script_dir, "anova_results.csv"), row.names = FALSE)

