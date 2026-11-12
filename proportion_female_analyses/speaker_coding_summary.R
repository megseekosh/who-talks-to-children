# Whose speech was measured in the studies reviewed by Coffey & Snedeker (2025)?
#
# Reproduces the proportions reported in the manuscript: the share of studies
# that measured language input exclusively from the child's mother and the share
# that included mothers plus additional caregivers. (The counts of overheard-speech
# and sibling-input studies in the manuscript are taken from Coffey & Snedeker's
# text, not from this sheet; the sibling count printed below is a check only.)
#
# Input: "IVMA Coding - Data.csv" (Coffey & Snedeker's coding sheet; one row per
# effect size). speakers_incl codes: 1 = mother; 2 = father; 3 = sibling;
# 4 = primary caregiver; 5 = adults; 6 = any; 7 = other.
# Run from this script's directory.

library(dplyr)

coding <- read.csv("IVMA Coding - Data.csv", stringsAsFactors = FALSE, check.names = FALSE,
                   fileEncoding = "UTF-8-BOM")
coding <- coding[, c("study_ID", "speakers_incl")] # the sheet has an unnamed column, so keep only what is needed

# One row per study: the set of speaker codes used across its effect sizes
studies <- coding %>%
  filter(!is.na(study_ID), study_ID != "") %>%
  group_by(study_ID) %>%
  summarise(speaker_codes = paste(sort(unique(trimws(as.character(speakers_incl)))), collapse = "+"),
            .groups = "drop")

n_studies <- nrow(studies)
mother_only <- sum(studies$speaker_codes == "1")
mother_plus <- sum(grepl("(^|\\+)1(\\+|$)", studies$speaker_codes) & studies$speaker_codes != "1")
sibling <- sum(grepl("(^|\\+)3(\\+|$)", studies$speaker_codes))

cat("Studies:", n_studies, "\n")
cat(sprintf("Input exclusively from the mother: %d/%d (%.1f%%)\n", mother_only, n_studies, 100 * mother_only / n_studies))
cat(sprintf("Mother plus additional caregivers: %d/%d (%.1f%%)\n", mother_plus, n_studies, 100 * mother_plus / n_studies))
cat("Studies with a sibling speaker code:", sibling, "\n\n")
print(count(studies, speaker_codes, sort = TRUE))

write.csv(count(studies, speaker_codes, sort = TRUE), "speaker_coding_summary.csv", row.names = FALSE)
