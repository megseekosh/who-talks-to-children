# Script to calculate CDS statistics for all children (infants through 8-year-olds)
# and create plot showing how proportions change with age

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# Set working directory to current script location
# All CSV files should be in the same directory as this script
# Run from this script's directory (the hard-coded setwd() pointed to a path that no longer exists)

# Read CDS annotation data for older children
all_CDS <- read.csv('all_CDS.csv', stringsAsFactors = FALSE)

# Read infant data from biling_context.csv
biling_context <- read.csv('biling_context.csv', stringsAsFactors = FALSE)

# Filter out sleep, researcher present, etc. for older children
# Also filter out rows with missing ID or empty timestamp (annotation instruction rows, not actual data)
all_CDS_filtered <- all_CDS %>%
  filter(sleeping == 0, researcher_present == 0, !is.na(id), timestamp_HHMMSS != "") %>%
  mutate(id = as.character(id)) %>%
  select(id, timestamp_HHMMSS, percents_voc, Otherchild2TargetChild, Adult2TargetChild,
         Otherchild2OtherChild, Adult2OtherChild, Adult2Others, Adult2unsure,
         Otherchild2adults, Otherchild2unsure, Adultfemale, Adultmale)

# Filter out sleep, researcher present, etc. for infants
# Only include Bolivian infants (4-digit IDs: 1000-9999), exclude US infants (3-digit IDs)
biling_context_filtered <- biling_context %>%
  mutate(id = as.character(id)) %>%
  # Filter for 4-digit IDs (Bolivian infants)
  filter(nchar(id) == 4 & !is.na(as.numeric(id)) & as.numeric(id) >= 1000 & as.numeric(id) <= 9999) %>%
  filter(sleeping == 0, researcher_present == 0) %>%
  select(id, timestamp_HHMMSS, percents_voc, Otherchild2TargetChild, Adult2TargetChild,
         Otherchild2OtherChild, Adult2OtherChild, Adult2Others, Adult2unsure,
         Otherchild2adults, Otherchild2unsure, Adultfemale, Adultmale)

# Combine both datasets (selecting only the columns we need)
all_CDS_combined <- bind_rows(
  all_CDS_filtered %>% mutate(data_source = "older_children"),
  biling_context_filtered %>% mutate(data_source = "infants")
)

# Calculate total clips per child (denominator)
cds_data2 <- all_CDS_combined %>%
  group_by(id) %>%
  mutate(total_clips = n()) %>%
  ungroup()

# Calculate clip-based counts: for each clip, check if it contains each speech type
# This allows a clip to count for multiple types (e.g., both TCDS and CDS)

# 1. TARGET CHILD-DIRECTED SPEECH (TCDS) - from one adult caregiver
# Check each clip for Adult2TargetChild
tcds_clips <- cds_data2 %>%
  group_by(id, timestamp_HHMMSS) %>%
  summarize(
    has_TCDS = any(Adult2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua'), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(id) %>%
  summarize(
    num_TCDS_clips = sum(has_TCDS, na.rm = TRUE),
    .groups = 'drop'
  )

# Get total clips per child
total_clips_per_child <- cds_data2 %>%
  group_by(id) %>%
  summarize(total_clips = n_distinct(timestamp_HHMMSS), .groups = 'drop')

# Calculate TCDS proportion
tcds_input <- tcds_clips %>%
  left_join(total_clips_per_child, by = 'id') %>%
  mutate(
    num_TCDS = num_TCDS_clips,
    percen_TCDS = num_TCDS / total_clips
  ) %>%
  select(id, num_TCDS, percen_TCDS, total_clips)

# 1a. TCDS FROM ONE ADULT FEMALE - from one female adult caregiver only
# Check each clip for Adult2TargetChild from female only (Adultfemale == 1, Adultmale == 0)
tcds_female_clips <- cds_data2 %>%
  group_by(id, timestamp_HHMMSS) %>%
  summarize(
    has_TCDS_female = any(Adult2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') & 
                          Adultfemale == 1 & Adultmale == 0, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(id) %>%
  summarize(
    num_TCDS_female_clips = sum(has_TCDS_female, na.rm = TRUE),
    .groups = 'drop'
  )

tcds_female_input <- tcds_female_clips %>%
  left_join(total_clips_per_child, by = 'id') %>%
  mutate(
    num_TCDS_female = num_TCDS_female_clips,
    percen_TCDS_female = num_TCDS_female / total_clips
  ) %>%
  select(id, num_TCDS_female, percen_TCDS_female)

# 2. ANY CDS - from any speaker (adults + children) to any child
# Check each clip for any CDS (includes Adult2TargetChild/TCDS + speech to other children)
# Also calculate clips with CDS that is not TCDS (for the stacked plot increment)
any_cds_clips <- cds_data2 %>%
  group_by(id, timestamp_HHMMSS) %>%
  summarize(
    has_TCDS = any(Adult2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua'), na.rm = TRUE),
    has_any_CDS = any(
      Adult2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
      Adult2OtherChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
      Otherchild2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
      Otherchild2OtherChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua'),
      na.rm = TRUE
    ),
    has_CDS_not_TCDS = has_any_CDS & !has_TCDS,  # Clips with CDS to other children but not TCDS (increment for stacked plot)
    .groups = 'drop'
  ) %>%
  group_by(id) %>%
  summarize(
    num_any_CDS_clips = sum(has_any_CDS, na.rm = TRUE),
    num_CDS_not_TCDS_clips = sum(has_CDS_not_TCDS, na.rm = TRUE),
    .groups = 'drop'
  )

any_cds_input <- any_cds_clips %>%
  left_join(total_clips_per_child, by = 'id') %>%
  mutate(
    num_any_CDS = num_any_CDS_clips,
    percen_any_CDS = num_any_CDS / total_clips,
    percen_CDS_not_TCDS = num_CDS_not_TCDS_clips / total_clips
  ) %>%
  select(id, num_any_CDS, percen_any_CDS, percen_CDS_not_TCDS)

# 2a. SPEECH TO TARGET CHILD FROM ANY SPEAKER (adults OR other children)
# This includes TCDS from any adult AND speech from other children to target child
target_child_speech_clips <- cds_data2 %>%
  group_by(id, timestamp_HHMMSS) %>%
  summarize(
    has_target_child_speech = any(
      Adult2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
      Otherchild2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua'),
      na.rm = TRUE
    ),
    .groups = 'drop'
  ) %>%
  group_by(id) %>%
  summarize(
    num_target_child_speech_clips = sum(has_target_child_speech, na.rm = TRUE),
    .groups = 'drop'
  )

target_child_speech_input <- target_child_speech_clips %>%
  left_join(total_clips_per_child, by = 'id') %>%
  mutate(
    num_target_child_speech = num_target_child_speech_clips,
    percen_target_child_speech = num_target_child_speech / total_clips
  ) %>%
  select(id, num_target_child_speech, percen_target_child_speech)

# 3. ANY SPEECH - including overheard (CDS + ADS)
# Check each clip for any speech (all categories)
any_speech_clips <- cds_data2 %>%
  group_by(id, timestamp_HHMMSS) %>%
  summarize(
    has_any_speech = any(
      Adult2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
      Adult2OtherChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
      Otherchild2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
      Otherchild2OtherChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
      Adult2Others %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
      Adult2unsure %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
      Otherchild2adults %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
      Otherchild2unsure %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua'),
      na.rm = TRUE
    ),
    .groups = 'drop'
  ) %>%
  group_by(id) %>%
  summarize(
    num_any_speech_clips = sum(has_any_speech, na.rm = TRUE),
    .groups = 'drop'
  )

any_speech_input <- any_speech_clips %>%
  left_join(total_clips_per_child, by = 'id') %>%
  mutate(
    num_any_speech = num_any_speech_clips,
    percen_any_speech = num_any_speech / total_clips
  ) %>%
  select(id, num_any_speech, percen_any_speech)

# Calculate annotation counts (separate from clip counts)
# These count each annotation separately, allowing multiple annotations per clip

# 1. TCDS annotations - count each Adult2TargetChild annotation
tcds_annotations <- cds_data2 %>%
  filter(Adult2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua')) %>%
  group_by(id) %>%
  summarize(num_TCDS_annotations = n(), .groups = 'drop')

# 1a. TCDS from female annotations - count each Adult2TargetChild annotation from female only
tcds_female_annotations <- cds_data2 %>%
  filter(Adult2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') &
         Adultfemale == 1 & Adultmale == 0) %>%
  group_by(id) %>%
  summarize(num_TCDS_female_annotations = n(), .groups = 'drop')

# 2. Any CDS annotations - count each CDS annotation (allowing multiple per clip)
# Count rows that have any CDS (TCDS or other CDS)
any_cds_annotations <- cds_data2 %>%
  filter(
    Adult2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
    Adult2OtherChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
    Otherchild2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
    Otherchild2OtherChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua')
  ) %>%
  group_by(id) %>%
  summarize(num_any_CDS_annotations = n(), .groups = 'drop')

# Count annotations for CDS that is not TCDS (for the stacked plot increment)
# This represents the additional CDS beyond TCDS (speech to other children)
# Note: Total CDS = TCDS + CDS_not_TCDS, so CDS includes TCDS
cds_not_tcds_annotations <- cds_data2 %>%
  mutate(
    has_TCDS = Adult2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua'),
    has_other_CDS = Adult2OtherChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
                    Otherchild2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
                    Otherchild2OtherChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua'),
    has_CDS_not_TCDS = has_other_CDS & !has_TCDS  # Annotations with CDS to other children but not TCDS
  ) %>%
  filter(has_CDS_not_TCDS) %>%
  group_by(id) %>%
  summarize(num_CDS_not_TCDS_annotations = n(), .groups = 'drop')

# 3. Any speech annotations - count each speech annotation
any_speech_annotations <- cds_data2 %>%
  filter(
    Adult2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
    Adult2OtherChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
    Otherchild2TargetChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
    Otherchild2OtherChild %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
    Adult2Others %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
    Adult2unsure %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
    Otherchild2adults %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua') |
    Otherchild2unsure %in% c('Quechua', 'Spanish', 'Mixed', 'Unsure', 'English/Quechua')
  ) %>%
  group_by(id) %>%
  summarize(num_any_speech_annotations = n(), .groups = 'drop')

# Merge all proportions and annotation counts
# Start with ALL children who have clips (not just those with TCDS)
# This ensures children with zero TCDS are still included
cds_proportions <- total_clips_per_child %>%
  left_join(tcds_input %>% select(id, num_TCDS_clips = num_TCDS, percen_TCDS_clips = percen_TCDS), by = 'id') %>%
  left_join(tcds_female_input %>% select(id, num_TCDS_female_clips = num_TCDS_female, percen_TCDS_female_clips = percen_TCDS_female), by = 'id') %>%
  left_join(target_child_speech_input %>% select(id, num_target_child_speech_clips = num_target_child_speech, percen_target_child_speech_clips = percen_target_child_speech), by = 'id') %>%
  left_join(any_cds_input %>% select(id, num_any_CDS_clips = num_any_CDS, percen_any_CDS_clips = percen_any_CDS, percen_CDS_not_TCDS_clips = percen_CDS_not_TCDS), by = 'id') %>%
  left_join(any_speech_input %>% select(id, num_any_speech_clips = num_any_speech, percen_any_speech_clips = percen_any_speech), by = 'id') %>%
  left_join(tcds_annotations, by = 'id') %>%
  left_join(tcds_female_annotations, by = 'id') %>%
  left_join(any_cds_annotations, by = 'id') %>%
  left_join(cds_not_tcds_annotations, by = 'id') %>%
  left_join(any_speech_annotations, by = 'id') %>%
  mutate(
    # Calculate proportions - handle missing values by setting to 0
    num_TCDS_clips = ifelse(is.na(num_TCDS_clips), 0, num_TCDS_clips),
    percen_TCDS = ifelse(is.na(percen_TCDS_clips), 0, num_TCDS_clips / total_clips),
    num_TCDS_female_clips = ifelse(is.na(num_TCDS_female_clips), 0, num_TCDS_female_clips),
    percen_TCDS_female = ifelse(is.na(percen_TCDS_female_clips), 0, num_TCDS_female_clips / total_clips),
    num_target_child_speech_clips = ifelse(is.na(num_target_child_speech_clips), 0, num_target_child_speech_clips),
    percen_target_child_speech = ifelse(is.na(percen_target_child_speech_clips), 0, num_target_child_speech_clips / total_clips),
    num_any_CDS_clips = ifelse(is.na(num_any_CDS_clips), 0, num_any_CDS_clips),
    percen_any_CDS = ifelse(is.na(percen_any_CDS_clips), 0, num_any_CDS_clips / total_clips),
    num_any_speech_clips = ifelse(is.na(num_any_speech_clips), 0, num_any_speech_clips),
    percen_any_speech = ifelse(is.na(percen_any_speech_clips), 0, num_any_speech_clips / total_clips),
    # Calculate percen_CDS_not_TCDS as the difference, or use the provided value if available
    percen_CDS_not_TCDS = ifelse(is.na(percen_CDS_not_TCDS_clips), 
                                 pmax(0, percen_any_CDS - percen_TCDS), 
                                 percen_CDS_not_TCDS_clips),
    # Use annotation counts for annotation plots
    num_TCDS = ifelse(is.na(num_TCDS_annotations), 0, num_TCDS_annotations),
    num_TCDS_female = ifelse(is.na(num_TCDS_female_annotations), 0, num_TCDS_female_annotations),
    num_any_CDS = ifelse(is.na(num_any_CDS_annotations), 0, num_any_CDS_annotations),
    num_any_speech = ifelse(is.na(num_any_speech_annotations), 0, num_any_speech_annotations),
    # Use the correct count for CDS_not_TCDS annotations
    num_CDS_not_TCDS = ifelse(is.na(num_CDS_not_TCDS_annotations), 0, num_CDS_not_TCDS_annotations)
  )

# Read age data from multiple sources
# First, try child_age.csv (for infants)
child_age_infants <- read.csv('child_age.csv', stringsAsFactors = FALSE) %>%
  rename(id = child_id, Age_mos = age_mos) %>%
  mutate(id = as.character(id))

# Read age data for the older children (ages only)
nwr_scores <- read.csv('child_ages_older.csv', stringsAsFactors = FALSE)
nwr_ages <- nwr_scores %>%
  select(spkr_ID, Age_mos) %>%
  distinct() %>%
  rename(id = spkr_ID) %>%
  mutate(id = as.character(id))

# Combine age data
all_ages <- bind_rows(
  child_age_infants %>% select(id, Age_mos),
  nwr_ages %>% select(id, Age_mos)
) %>%
  distinct(id, .keep_all = TRUE)

# Set age for child 1005 if missing (calculated age: 46.12 months)
if("1005" %in% all_ages$id && is.na(all_ages$Age_mos[all_ages$id == "1005"])) {
  all_ages$Age_mos[all_ages$id == "1005"] <- 46.12
} else if(!"1005" %in% all_ages$id) {
  # If child 1005 is not in all_ages at all, add them
  all_ages <- bind_rows(all_ages, data.frame(id = "1005", Age_mos = 46.12))
}

# Check what IDs are in the filtered CDS data (after removing missing IDs)
all_cds_ids <- unique(as.character(all_CDS_filtered$id))
cat("\nTotal unique children in all_CDS (after filtering):", length(all_cds_ids), "\n")
cat("Children with age data:", nrow(all_ages), "\n")
cat("Total children with clips:", nrow(total_clips_per_child), "\n")
cat("Children in CDS but missing age:", 
    length(setdiff(all_cds_ids, all_ages$id)), "\n")
if(length(setdiff(all_cds_ids, all_ages$id)) > 0) {
  cat("Missing IDs:", paste(setdiff(all_cds_ids, all_ages$id), collapse = ", "), "\n")
}
cat("Children in cds_proportions (before age filter):", nrow(cds_proportions), "\n")
cat("Children with zero TCDS clips:", sum(cds_proportions$num_TCDS_clips == 0, na.rm = TRUE), "\n")

# Merge CDS proportions with age data
# Ensure all_ages has unique IDs before joining
all_ages_unique <- all_ages %>%
  distinct(id, .keep_all = TRUE)

cds_with_age <- cds_proportions %>%
  mutate(id = as.character(id)) %>%
  left_join(all_ages_unique, by = 'id') %>%
  filter(!is.na(Age_mos)) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(
    Age_years = Age_mos / 12,
    # Calculate differences between definitions
    diff_any_CDS_minus_TCDS = percen_any_CDS - percen_TCDS,
    diff_any_speech_minus_any_CDS = percen_any_speech - percen_any_CDS,
    # Verify data consistency: check if percen_CDS_not_TCDS matches the difference
    CDS_not_TCDS_check = percen_any_CDS - percen_TCDS,
    CDS_consistency = abs(percen_CDS_not_TCDS - CDS_not_TCDS_check) < 0.0001
  )

# Check for data inconsistencies
inconsistent <- cds_with_age %>% 
  filter(!CDS_consistency | percen_TCDS > percen_any_CDS | percen_any_CDS > percen_any_speech)
if(nrow(inconsistent) > 0) {
  cat("\nWARNING: Found", nrow(inconsistent), "children with inconsistent data:\n")
  print(inconsistent %>% select(id, Age_mos, percen_TCDS, percen_any_CDS, percen_any_speech, percen_CDS_not_TCDS, CDS_not_TCDS_check))
}

# Print summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("\nSample size:", nrow(cds_with_age), "children\n")
cat("Age range:", round(min(cds_with_age$Age_mos, na.rm = TRUE), 1), 
    "to", round(max(cds_with_age$Age_mos, na.rm = TRUE), 1), "months\n")
cat("Age range:", round(min(cds_with_age$Age_years, na.rm = TRUE), 1), 
    "to", round(max(cds_with_age$Age_years, na.rm = TRUE), 1), "years\n")

cat("\n=== TCDS FROM ADULT FEMALE - from one adult female caregiver ===\n")
cat("Mean:", round(mean(cds_with_age$percen_TCDS_female, na.rm = TRUE) * 100, 1), "%\n")
cat("SD:", round(sd(cds_with_age$percen_TCDS_female, na.rm = TRUE) * 100, 1), "%\n")
cat("Range:", round(min(cds_with_age$percen_TCDS_female, na.rm = TRUE) * 100, 1), 
    "to", round(max(cds_with_age$percen_TCDS_female, na.rm = TRUE) * 100, 1), "%\n")

cat("\n=== TARGET CHILD-DIRECTED SPEECH (TCDS) - from one adult caregiver ===\n")
cat("Mean:", round(mean(cds_with_age$percen_TCDS, na.rm = TRUE) * 100, 1), "%\n")
cat("SD:", round(sd(cds_with_age$percen_TCDS, na.rm = TRUE) * 100, 1), "%\n")
cat("Range:", round(min(cds_with_age$percen_TCDS, na.rm = TRUE) * 100, 1), 
    "to", round(max(cds_with_age$percen_TCDS, na.rm = TRUE) * 100, 1), "%\n")

cat("\n=== ANY CDS - from any speaker (adults + children) ===\n")
cat("Mean:", round(mean(cds_with_age$percen_any_CDS, na.rm = TRUE) * 100, 1), "%\n")
cat("SD:", round(sd(cds_with_age$percen_any_CDS, na.rm = TRUE) * 100, 1), "%\n")
cat("Range:", round(min(cds_with_age$percen_any_CDS, na.rm = TRUE) * 100, 1), 
    "to", round(max(cds_with_age$percen_any_CDS, na.rm = TRUE) * 100, 1), "%\n")

cat("\n=== ANY SPEECH - including overheard (CDS + ADS) ===\n")
cat("Mean:", round(mean(cds_with_age$percen_any_speech, na.rm = TRUE) * 100, 1), "%\n")
cat("SD:", round(sd(cds_with_age$percen_any_speech, na.rm = TRUE) * 100, 1), "%\n")
cat("Range:", round(min(cds_with_age$percen_any_speech, na.rm = TRUE) * 100, 1), 
    "to", round(max(cds_with_age$percen_any_speech, na.rm = TRUE) * 100, 1), "%\n")

cat("\n=== INCREASE FROM TCDS TO ANY CDS ===\n")
cat("Mean increase:", round(mean(cds_with_age$diff_any_CDS_minus_TCDS, na.rm = TRUE) * 100, 1), "%\n")
cat("SD:", round(sd(cds_with_age$diff_any_CDS_minus_TCDS, na.rm = TRUE) * 100, 1), "%\n")

# Compute statistics for the text (percentage of clips)
cat("\n=== STATISTICS FOR TEXT (PERCENTAGE OF CLIPS) ===\n")
cat("These are clip-based proportions (clips with no speech included in denominator):\n\n")

# 1. TCDS from female adult caregiver
mean_tcds_female_clips <- round(mean(cds_with_age$percen_TCDS_female, na.rm = TRUE) * 100, 1)
sd_tcds_female_clips <- round(sd(cds_with_age$percen_TCDS_female, na.rm = TRUE) * 100, 1)
cat("1. TCDS from one female adult caregiver:\n")
cat("   Mean:", mean_tcds_female_clips, "%\n")
cat("   SD:", sd_tcds_female_clips, "%\n\n")

# 2. Speech to target child from any single adult caregiver (what the plot shows in purple)
mean_tcds_clips <- round(mean(cds_with_age$percen_TCDS, na.rm = TRUE) * 100, 1)
sd_tcds_clips <- round(sd(cds_with_age$percen_TCDS, na.rm = TRUE) * 100, 1)
cat("2. Target child-directed speech from any single adult caregiver:\n")
cat("   Mean:", mean_tcds_clips, "%\n")
cat("   SD:", sd_tcds_clips, "%\n\n")

# 2a. Speech to target child from any speaker (adults including men OR other children)
mean_target_child_speech_clips <- round(mean(cds_with_age$percen_target_child_speech, na.rm = TRUE) * 100, 1)
sd_target_child_speech_clips <- round(sd(cds_with_age$percen_target_child_speech, na.rm = TRUE) * 100, 1)
cat("2a. Speech to target child from any speaker (adults including men OR other children):\n")
cat("   Mean:", mean_target_child_speech_clips, "%\n")
cat("   SD:", sd_target_child_speech_clips, "%\n\n")

# 3. Any CDS (to any child, including speech from other children to target child)
mean_any_cds_clips <- round(mean(cds_with_age$percen_any_CDS, na.rm = TRUE) * 100, 1)
sd_any_cds_clips <- round(sd(cds_with_age$percen_any_CDS, na.rm = TRUE) * 100, 1)
cat("3. Any CDS (to any child):\n")
cat("   Mean:", mean_any_cds_clips, "%\n")
cat("   SD:", sd_any_cds_clips, "%\n\n")

# 4. Any speech (including overheard)
mean_any_speech_clips <- round(mean(cds_with_age$percen_any_speech, na.rm = TRUE) * 100, 1)
sd_any_speech_clips <- round(sd(cds_with_age$percen_any_speech, na.rm = TRUE) * 100, 1)
cat("4. Any speech (including overheard):\n")
cat("   Mean:", mean_any_speech_clips, "%\n")
cat("   SD:", sd_any_speech_clips, "%\n\n")

# Format for LaTeX text
cat("=== FORMATTED TEXT VALUES ===\n")
cat("TCDS from female: ", mean_tcds_female_clips, "\\% (SD=", sd_tcds_female_clips, ")\n", sep = "")
cat("TCDS from any single adult caregiver (plot purple bar): ", mean_tcds_clips, "\\% (SD=", sd_tcds_clips, ")\n", sep = "")
cat("Speech to target child from any speaker: ", mean_target_child_speech_clips, "\\% (SD=", sd_target_child_speech_clips, ")\n", sep = "")
cat("Any CDS: ", mean_any_cds_clips, "\\% (SD=", sd_any_cds_clips, ")\n", sep = "")
cat("Any speech: ", mean_any_speech_clips, "\\% (SD=", sd_any_speech_clips, ")\n", sep = "")
cat("\n")
cat("=== TEXT TO FILL IN (for plot purple bar stat) ===\n")
cat("When this child-directed speech was instead quantified as input directed to the target child from any single adult caregiver, the estimate increased to ", mean_tcds_clips, "\\% (SD=", sd_tcds_clips, ") of the clips from the recordings.\n", sep = "")
cat("\n")
cat("=== TEXT TO FILL IN (for adults + children stat) ===\n")
cat("When this child-directed speech was instead quantified as input directed to the target child from any single speaker, including men (who due to market demands spend limited time close to home during the day) but also speech directed to the target child from another child, the estimate increased to ", mean_target_child_speech_clips, "\\% (SD=", sd_target_child_speech_clips, ") of the clips from the recordings.\n", sep = "")
cat("\n")

cat("\n=== INCREASE FROM ANY CDS TO ANY SPEECH ===\n")
cat("Mean increase:", round(mean(cds_with_age$diff_any_speech_minus_any_CDS, na.rm = TRUE) * 100, 1), "%\n")
cat("SD:", round(sd(cds_with_age$diff_any_speech_minus_any_CDS, na.rm = TRUE) * 100, 1), "%\n")

# Calculate statistics for children aged 6-48 months
cat("\n=== TCDS FROM ADULT FEMALE: CHILDREN AGED 6-48 MONTHS ===\n")
cds_6_48_months <- cds_with_age %>% filter(Age_mos >= 6 & Age_mos <= 48)
cat("Sample size:", nrow(cds_6_48_months), "children\n")
cat("Mean:", round(mean(cds_6_48_months$percen_TCDS_female, na.rm = TRUE) * 100, 1), "%\n")
cat("SD:", round(sd(cds_6_48_months$percen_TCDS_female, na.rm = TRUE) * 100, 1), "%\n")
cat("Range:", round(min(cds_6_48_months$percen_TCDS_female, na.rm = TRUE) * 100, 1), 
    "to", round(max(cds_6_48_months$percen_TCDS_female, na.rm = TRUE) * 100, 1), "%\n")
cat("\nFormatted statistic:\n")
mean_val <- round(mean(cds_6_48_months$percen_TCDS_female, na.rm = TRUE) * 100, 1)
sd_val <- round(sd(cds_6_48_months$percen_TCDS_female, na.rm = TRUE) * 100, 1)
cat("Just ", mean_val, "% of children's daily verbal input between 6 and 48 months took the form of child-directed speech to the target child from one adult female caregiver (SD=", sd_val, "%; Fig X.).\n", sep = "")

# Calculate age-specific statistics
cat("\n=== AGE-SPECIFIC STATISTICS ===\n")
# Find youngest and oldest children
youngest_idx <- which.min(cds_with_age$Age_mos)
oldest_idx <- which.max(cds_with_age$Age_mos)

cat("\nYoungest child (", round(cds_with_age$Age_mos[youngest_idx], 1), " months):\n")
cat("  TCDS from Female:", round(cds_with_age$percen_TCDS_female[youngest_idx] * 100, 1), "%\n")
cat("  TCDS:", round(cds_with_age$percen_TCDS[youngest_idx] * 100, 1), "%\n")
cat("  Any CDS:", round(cds_with_age$percen_any_CDS[youngest_idx] * 100, 1), "%\n")
cat("  Any Speech:", round(cds_with_age$percen_any_speech[youngest_idx] * 100, 1), "%\n")

cat("\nOldest child (", round(cds_with_age$Age_mos[oldest_idx], 1), " months):\n")
cat("  TCDS from Female:", round(cds_with_age$percen_TCDS_female[oldest_idx] * 100, 1), "%\n")
cat("  TCDS:", round(cds_with_age$percen_TCDS[oldest_idx] * 100, 1), "%\n")
cat("  Any CDS:", round(cds_with_age$percen_any_CDS[oldest_idx] * 100, 1), "%\n")
cat("  Any Speech:", round(cds_with_age$percen_any_speech[oldest_idx] * 100, 1), "%\n")

# Model change over age
cat("\n=== CHANGE OVER AGE ===\n")
# Fit linear models
model_tcds <- lm(percen_TCDS ~ Age_mos, data = cds_with_age)
model_any_cds <- lm(percen_any_CDS ~ Age_mos, data = cds_with_age)
model_any_speech <- lm(percen_any_speech ~ Age_mos, data = cds_with_age)

cat("\nTCDS change per month:", round(coef(model_tcds)[2] * 100, 3), "% per month\n")
cat("Any CDS change per month:", round(coef(model_any_cds)[2] * 100, 3), "% per month\n")
cat("Any Speech change per month:", round(coef(model_any_speech)[2] * 100, 3), "% per month\n")

# Calculate at specific ages (youngest and oldest)
age_range <- range(cds_with_age$Age_mos, na.rm = TRUE)
pred_tcds_young <- predict(model_tcds, newdata = data.frame(Age_mos = age_range[1]))
pred_tcds_old <- predict(model_tcds, newdata = data.frame(Age_mos = age_range[2]))

cat("\nPredicted TCDS at", round(age_range[1], 1), "months:", round(pred_tcds_young * 100, 1), "%\n")
cat("Predicted TCDS at", round(age_range[2], 1), "months:", round(pred_tcds_old * 100, 1), "%\n")

# Create barplot data showing number of annotations (not proportions)
# Order children by age for better visualization
cds_with_age_ordered <- cds_with_age %>%
  arrange(Age_mos) %>%
  mutate(
    child_order = row_number(),
    # Create age label in "year;month" format with rounded months (e.g., 5.7 months = "0;6", 12.3 months = "1;0")
    years = floor(Age_mos / 12),
    months = round(Age_mos %% 12),
    age_label = paste0(years, ";", months),
    # Create a unique x-axis position for each child (use child_order) but label with age_label
    x_position = child_order
  )

# Prepare data for barplot - number of annotations
# Grouped plot shows TOTAL values for each definition (cumulative/inclusive)
barplot_data <- cds_with_age_ordered %>%
  select(id, age_label, child_order, Age_mos, num_TCDS_female, num_TCDS, num_any_CDS, num_any_speech) %>%
  pivot_longer(cols = c(num_TCDS_female, num_TCDS, num_any_CDS, num_any_speech),
               names_to = "speech_type",
               values_to = "num_annotations") %>%
  mutate(
    speech_type = factor(speech_type,
                        levels = c("num_TCDS_female", "num_TCDS", "num_any_CDS", "num_any_speech"),
                        labels = c("Target child-directed speech\nfrom a single adult female caregiver",
                                  "Target child-directed speech\nfrom any single adult caregiver",
                                  "Child-directed speech\nto any child",
                                  "Any speech"))
  )

# Prepare data for p1 - percentage of annotations
# Calculate percentage of annotations for each speech type (denominator is total annotations = num_any_speech)
# First ensure we have unique rows per child
cds_with_age_unique <- cds_with_age_ordered %>%
  distinct(id, .keep_all = TRUE)

barplot_data_pct <- cds_with_age_unique %>%
  select(id, age_label, child_order, Age_mos, num_TCDS_female, num_TCDS, num_any_CDS, num_any_speech) %>%
  mutate(
    # Calculate percentages (denominator is num_any_speech = total annotations for each child)
    pct_TCDS_female = ifelse(num_any_speech > 0, num_TCDS_female / num_any_speech, 0),
    pct_TCDS = ifelse(num_any_speech > 0, num_TCDS / num_any_speech, 0),
    pct_any_CDS = ifelse(num_any_speech > 0, num_any_CDS / num_any_speech, 0),
    pct_any_speech = ifelse(num_any_speech > 0, num_any_speech / num_any_speech, 0)  # Always 1.0, but included for consistency
  ) %>%
  pivot_longer(cols = c(pct_TCDS_female, pct_TCDS, pct_any_CDS, pct_any_speech),
               names_to = "speech_type",
               values_to = "pct_annotations") %>%
  distinct(id, speech_type, .keep_all = TRUE) %>%
  mutate(
    speech_type = factor(speech_type,
                        levels = c("pct_TCDS_female", "pct_TCDS", "pct_any_CDS", "pct_any_speech"),
                        labels = c("Target child-directed speech\nfrom a single adult female caregiver",
                                  "Target child-directed speech\nfrom any single adult caregiver",
                                  "Child-directed speech\nto any child",
                                  "Any speech"))
  )

# Create grouped barplot: children on y-axis, percentage of annotations on x-axis
# Create factor for y-axis with proper ordering and labels
# Use the unique child data we already created
# Create unique labels for each child (age + letter to distinguish children with same age)
child_id_labels <- cds_with_age_unique %>%
  select(id, age_label, child_order, Age_mos) %>%
  arrange(child_order) %>%
  # Create unique label: use age_label, but add letter (A, B, C, etc.) for children with same rounded age
  group_by(age_label) %>%
  mutate(
    age_count = n(),
    letter_suffix = ifelse(age_count > 1, LETTERS[row_number()], ""),
    unique_label = ifelse(age_count > 1, paste0(age_label, " (", letter_suffix, ")"), age_label)
  ) %>%
  ungroup()

barplot_data_pct <- barplot_data_pct %>%
  left_join(child_id_labels %>% select(id, unique_label), by = "id") %>%
  mutate(
    id_factor = factor(id, 
                       levels = child_id_labels$id,
                       labels = child_id_labels$unique_label)
  )

p1 <- ggplot(barplot_data_pct, aes(x = pct_annotations, y = id_factor, fill = speech_type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 0.85, preserve = "single")) +
  scale_fill_manual(values = c("Target child-directed speech\nfrom a single adult female caregiver" = "#E91E63",  # Pink/Magenta
                               "Target child-directed speech\nfrom any single adult caregiver" = "#8E44AD",  # Purple
                               "Child-directed speech\nto any child" = "#F39C12",   # Orange/Amber
                               "Any speech" = "#16A085"),  # Teal
                   name = "Speech Register",
                   guide = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0.01, 0.01)) +
  labs(
    x = "Percentage of Annotations that are Child-directed",
    y = "Child (Age: year;month)",
    fill = "Speech Register"
  ) +
  theme_minimal(base_size = 32) +
  theme(
    axis.text.x = element_text(size = 28),
    axis.text.y = element_text(size = 28),
    axis.title = element_text(face = "bold", size = 32),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 32),
    legend.text = element_text(size = 28),
    legend.spacing.x = unit(0.5, "cm"),
    legend.spacing.y = unit(0.3, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 20, 10, 10)
  )

print(p1)

# Create figures directory if it doesn't exist
figures_dir <- file.path(getwd(), "figures")
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
}

# Save plot to figures directory
ggsave(file.path(figures_dir, "CDS_percentage_annotations_grouped_barplot.png"), 
       plot = p1, 
       width = 20, 
       height = 30, 
       dpi = 300)

cat("\n=== Plot saved to figures/ directory ===\n")
cat("  - CDS_percentage_annotations_grouped_barplot.png (grouped bars - percentage of annotations)\n")

# Create split plot version: children on x-axis, split into upper and lower panels
# Split children into two groups
n_children <- length(unique(barplot_data_pct$id))
half_point <- ceiling(n_children / 2)

# Get ordered child IDs
ordered_child_ids <- child_id_labels %>%
  arrange(child_order) %>%
  pull(id)

# Split into two groups
first_half_ids <- ordered_child_ids[1:half_point]
second_half_ids <- ordered_child_ids[(half_point + 1):n_children]

# Create data for each half, recreating factors with correct levels
first_half_labels <- child_id_labels %>%
  filter(id %in% first_half_ids) %>%
  arrange(child_order)

second_half_labels <- child_id_labels %>%
  filter(id %in% second_half_ids) %>%
  arrange(child_order)

barplot_data_pct_first <- barplot_data_pct %>%
  filter(id %in% first_half_ids) %>%
  mutate(
    id_factor = factor(id, 
                       levels = first_half_labels$id,
                       labels = first_half_labels$unique_label)
  )

barplot_data_pct_second <- barplot_data_pct %>%
  filter(id %in% second_half_ids) %>%
  mutate(
    id_factor = factor(id, 
                       levels = second_half_labels$id,
                       labels = second_half_labels$unique_label)
  )

# Create a function to make the plot
create_split_plot <- function(data, title_suffix = "") {
  ggplot(data, aes(x = id_factor, y = pct_annotations, fill = speech_type)) +
    geom_bar(stat = "identity", position = position_dodge2(width = 0.85, preserve = "single")) +
    scale_fill_manual(values = c("Target child-directed speech\nfrom a single adult female caregiver" = "#E91E63",  # Pink/Magenta
                                 "Target child-directed speech\nfrom any single adult caregiver" = "#8E44AD",  # Purple
                                 "Child-directed speech\nto any child" = "#F39C12",   # Orange/Amber
                                 "Any speech" = "#16A085"),  # Teal
                     name = "Speech Register",
                     guide = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1), expand = c(0, 0)) +
    scale_x_discrete(expand = c(0.01, 0.01)) +
    labs(
      x = "Child (Age: year;month)",
      y = "Percentage of Annotations that are Child-directed",
      fill = "Speech Register",
      title = title_suffix
    ) +
    theme_minimal(base_size = 32) +
    theme(
      axis.text.x = element_text(size = 32, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 36),
      axis.title = element_text(face = "bold", size = 32),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 40),
      legend.text = element_text(size = 36),
      legend.spacing.x = unit(0.5, "cm"),
      legend.spacing.y = unit(0.3, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 20, 10, 10),
      plot.title = element_text(size = 28, hjust = 0.5, margin = margin(b = 10))
    )
}

# Create the two plots (remove y-axis labels - will add centered y-axis label later)
p2_upper <- create_split_plot(barplot_data_pct_first) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

p2_lower <- create_split_plot(barplot_data_pct_second) +
  theme(axis.title.y = element_blank()) +
  labs(x = "Child (Age: year;month)")  # Add x-axis label above legend

# Combine plots using gridExtra
if (!require("gridExtra", quietly = TRUE, character.only = TRUE)) {
  stop("Package gridExtra is required: install.packages('gridExtra')")
  library(gridExtra)
}

# Load grid package for text grobs
library(grid)

# Combine plots vertically with centered y-axis label
p2_combined <- arrangeGrob(
  p2_upper,
  p2_lower,
  ncol = 1,
  heights = c(1, 1.2),  # Give more space to lower plot for legend
  left = textGrob("Percentage of Annotations that are Child-directed", rot = 90, gp = gpar(fontsize = 32, fontface = "bold"))
)

print(p2_combined)

# Save the split plot
ggsave(file.path(figures_dir, "CDS_percentage_annotations_grouped_barplot_split.png"), 
       plot = p2_combined, 
       width = 30, 
       height = 20, 
       dpi = 300)

cat("  - CDS_percentage_annotations_grouped_barplot_split.png (grouped bars - split into two panels)\n")

# Create clip-based plot: percentage of clips with each register
# Prepare data for clip proportions plot
cds_with_age_unique_clips <- cds_with_age_ordered %>%
  distinct(id, .keep_all = TRUE)

barplot_data_clips <- cds_with_age_unique_clips %>%
  select(id, age_label, child_order, Age_mos, percen_TCDS_female, percen_TCDS, percen_any_CDS, percen_any_speech) %>%
  pivot_longer(cols = c(percen_TCDS_female, percen_TCDS, percen_any_CDS, percen_any_speech),
               names_to = "speech_type",
               values_to = "pct_clips") %>%
  distinct(id, speech_type, .keep_all = TRUE) %>%
  mutate(
    speech_type = factor(speech_type,
                        levels = c("percen_TCDS_female", "percen_TCDS", "percen_any_CDS", "percen_any_speech"),
                        labels = c("Target child-directed speech\nfrom a single adult female caregiver",
                                  "Target child-directed speech\nfrom any single adult caregiver",
                                  "Child-directed speech\nto any child",
                                  "Any speech"))
  ) %>%
  left_join(child_id_labels %>% select(id, unique_label), by = "id") %>%
  mutate(
    id_factor = factor(id, 
                       levels = child_id_labels$id,
                       labels = child_id_labels$unique_label)
  )

# Split children into two groups (same as annotation plot)
barplot_data_clips_first <- barplot_data_clips %>%
  filter(id %in% first_half_ids) %>%
  mutate(
    id_factor = factor(id, 
                       levels = first_half_labels$id,
                       labels = first_half_labels$unique_label)
  )

barplot_data_clips_second <- barplot_data_clips %>%
  filter(id %in% second_half_ids) %>%
  mutate(
    id_factor = factor(id, 
                       levels = second_half_labels$id,
                       labels = second_half_labels$unique_label)
  )

# Create a function to make the clip plot
create_clip_split_plot <- function(data, title_suffix = "") {
  ggplot(data, aes(x = id_factor, y = pct_clips, fill = speech_type)) +
    geom_bar(stat = "identity", position = position_dodge2(width = 0.85, preserve = "single")) +
    scale_fill_manual(values = c("Target child-directed speech\nfrom a single adult female caregiver" = "#E91E63",  # Pink/Magenta
                                 "Target child-directed speech\nfrom any single adult caregiver" = "#8E44AD",  # Purple
                                 "Child-directed speech\nto any child" = "#F39C12",   # Orange/Amber
                                 "Any speech" = "#16A085"),  # Teal
                     name = "Speech Register",
                     guide = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1), expand = c(0, 0)) +
    scale_x_discrete(expand = c(0.01, 0.01)) +
    labs(
      x = "Child (Age: year;month)",
      y = "Percentage of Clips",
      fill = "Speech Register",
      title = title_suffix
    ) +
    theme_minimal(base_size = 32) +
    theme(
      axis.text.x = element_text(size = 32, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 36),
      axis.title = element_text(face = "bold", size = 45),  # 40% bigger (32 * 1.4 = 44.8, round to 45)
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 48),  # 20% bigger (40 * 1.2 = 48)
      legend.text = element_text(size = 43),  # 20% bigger (36 * 1.2 = 43.2, round to 43)
      legend.spacing.x = unit(0.5, "cm"),
      legend.spacing.y = unit(0.3, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 20, 10, 10),
      plot.title = element_text(size = 28, hjust = 0.5, margin = margin(b = 10))
    )
}

# Create the two clip plots
p3_upper <- create_clip_split_plot(barplot_data_clips_first) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

p3_lower <- create_clip_split_plot(barplot_data_clips_second) +
  theme(axis.title.y = element_blank()) +
  labs(x = "Child (Age: year;month)")  # Add x-axis label above legend

# Combine clip plots vertically with centered y-axis label
# Use height ratio that ensures plot panels have equal height
# The legend in lower plot takes up space, so give it extra height to compensate
# This ensures y-axis tick spacing is identical in both plots
# Need to give lower plot significantly more height so plot panel matches upper plot
height_ratio <- c(1, 1.7)  # Give lower plot more height so plot panel matches upper plot

p3_combined <- arrangeGrob(
  p3_upper,
  p3_lower,
  ncol = 1,
  heights = height_ratio,
  left = textGrob("Percentage of Clips", rot = 90, gp = gpar(fontsize = 45, fontface = "bold"))  # 40% bigger (32 * 1.4 = 44.8, round to 45)
)

print(p3_combined)

# Save the clip-based split plot
ggsave(file.path(figures_dir, "CDS_percentage_clips_grouped_barplot_split.png"), 
       plot = p3_combined, 
       width = 30, 
       height = 20, 
       dpi = 300)

cat("  - CDS_percentage_clips_grouped_barplot_split.png (grouped bars - percentage of clips - split into two panels)\n")

# Save data for reference
write.csv(cds_with_age, "CDS_proportions_with_age_all_children.csv", row.names = FALSE)
cat(sprintf("Sample: N = %d children; age %.1f to %.1f months (mean %.2f months = %d;%d); clips per child M = %.2f, range %d-%d\n",
            nrow(cds_with_age), min(cds_with_age$Age_mos), max(cds_with_age$Age_mos), mean(cds_with_age$Age_mos),
            round(mean(cds_with_age$Age_mos)) %/% 12, round(mean(cds_with_age$Age_mos)) %% 12,
            mean(cds_with_age$total_clips), min(cds_with_age$total_clips), max(cds_with_age$total_clips)))

cat("Data saved as CDS_proportions_with_age_all_children.csv\n")

