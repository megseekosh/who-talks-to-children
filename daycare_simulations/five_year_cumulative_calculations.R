# Five-Year Cumulative Missed Words Calculations
# 
# This script calculates cumulative missed verbal input over the first 5 years of life,
# broken down by age group and socioeconomic bracket. Calculations account for:
# - Age-specific nap durations (from Staton et al. 2020)
# - Age-specific childcare hours (from NCES Table 202.30; 2019 survey data, published 2021)
# - Socioeconomic differences in childcare usage
# - Verbal input percentage (75% of awake childcare hours)
# - Words per hour conversion (1,225 words/hour from Soderstrom et al. 2018)
#
# Age group breakdown:
# - Year 1: Infants (<12 months)
# - Years 1-2: Toddlers (1-2 years) - spans both years
# - Years 3-5: Preschoolers (3-5 years) - spans 3 years
#
# Assumptions:
# - 46 weeks of childcare per calendar year
# - 5 days per week in childcare
# - 75% of awake childcare hours contain verbal input
# - 1,225 words per clock hour (from Soderstrom et al. 2018)

library(dplyr)

# Source the main analysis script to get data
source('missed_verbal_input_analysis.R', local = TRUE)

# Parameters
words_per_hour <- 1225  # From Soderstrom et al. 2018 (median for daycare centres)
verbal_input_pct <- 0.75  # 75% of awake childcare hours contain verbal input
weeks_per_year <- 46
days_per_week <- 5
days_per_year <- weeks_per_year * days_per_week

cat('=== FIVE-YEAR CUMULATIVE MISSED WORDS CALCULATIONS ===\n\n')
cat('Parameters:\n')
cat('  Words/hour:', words_per_hour, '(Soderstrom et al. 2018)\n')
cat('  Verbal input:', verbal_input_pct * 100, '% of awake hours\n')
cat('  Days per year:', days_per_year, 'days (', weeks_per_year, 'weeks ×', days_per_week, 'days)\n')
cat('  Note: Toddlers span years 1 AND 2 (ages 1-2 years)\n\n')

# Calculate daily awake hours and words for each bracket × age group
daily_calc <- data %>%
  group_by(Bracket, Age_Group) %>%
  summarise(
    Mean_Awake_Hours_Per_Day = mean(Awake_Childcare_Hours, na.rm = TRUE) / 5,
    .groups = 'drop'
  ) %>%
  mutate(
    Daily_Missed_Words = Mean_Awake_Hours_Per_Day * verbal_input_pct * words_per_hour
  )

cat('Daily awake hours and missed words by bracket and age:\n')
print(daily_calc)

# Separate by age groups
infants <- daily_calc %>% filter(Age_Group == 'Infant\n(<12 mos)')
toddlers <- daily_calc %>% filter(Age_Group == 'Toddler\n(1-2yrs)')
preschoolers <- daily_calc %>% filter(Age_Group == 'Preschooler\n(3-5yrs)')

cat('\n=== YEAR 1: INFANTS (<12 months) ===\n')
year1 <- infants %>%
  mutate(
    Year1_Missed_Words = Daily_Missed_Words * days_per_year,
    Year1_Missed_Words_Millions = Year1_Missed_Words / 1000000
  )
print(year1 %>% select(Bracket, Mean_Awake_Hours_Per_Day, Daily_Missed_Words, Year1_Missed_Words_Millions))

cat('\n=== YEARS 1-2: TODDLERS (1-2 years) - SPANS BOTH YEARS ===\n')
toddler_years <- toddlers %>%
  mutate(
    Toddler_Years1_2_Missed_Words = Daily_Missed_Words * days_per_year * 2,  # 2 years
    Toddler_Years1_2_Millions = Toddler_Years1_2_Missed_Words / 1000000
  )
print(toddler_years %>% select(Bracket, Mean_Awake_Hours_Per_Day, Daily_Missed_Words, Toddler_Years1_2_Millions))

cat('\n=== YEARS 3-5: PRESCHOOLERS (3-5 years) ===\n')
preschooler_years <- preschoolers %>%
  mutate(
    Preschooler_Years3_5_Missed_Words = Daily_Missed_Words * days_per_year * 3,  # 3 years
    Preschooler_Years3_5_Millions = Preschooler_Years3_5_Missed_Words / 1000000
  )
print(preschooler_years %>% select(Bracket, Mean_Awake_Hours_Per_Day, Daily_Missed_Words, Preschooler_Years3_5_Millions))

cat('\n=== CUMULATIVE TOTALS OVER 5 YEARS ===\n')
final <- year1 %>%
  left_join(toddler_years %>% select(Bracket, Toddler_Years1_2_Millions), by = 'Bracket') %>%
  left_join(preschooler_years %>% select(Bracket, Preschooler_Years3_5_Millions), by = 'Bracket') %>%
  mutate(
    Total_5yr_Millions = Year1_Missed_Words_Millions + Toddler_Years1_2_Millions + Preschooler_Years3_5_Millions
  )

cat('Final 5-year totals:\n')
print(final %>% select(Bracket, 
                       Year1_Infants = Year1_Missed_Words_Millions,
                       Years1_2_Toddlers = Toddler_Years1_2_Millions,
                       Years3_5_Preschoolers = Preschooler_Years3_5_Millions,
                       Total_5yr = Total_5yr_Millions))

cat('\n=== SUMMARY TABLE ===\n')
summary_table <- final %>%
  mutate(
    Year1 = round(Year1_Missed_Words_Millions, 2),
    Years1_2 = round(Toddler_Years1_2_Millions, 2),
    Years3_5 = round(Preschooler_Years3_5_Millions, 2),
    Total = round(Total_5yr_Millions, 2)
  ) %>%
  select(Bracket, Year1, Years1_2, Years3_5, Total)
print(summary_table)

cat('\n=== DETAILED BREAKDOWN FOR EACH BRACKET ===\n')
for (bracket in c('Lower Income', 'Middle Income', 'Upper Income')) {
  bracket_data <- final %>% filter(Bracket == bracket)
  cat('\n', bracket, ':\n', sep='')
  cat('  Year 1 (Infants):', round(bracket_data$Year1_Missed_Words_Millions, 2), 'million words\n')
  cat('  Years 1-2 (Toddlers):', round(bracket_data$Toddler_Years1_2_Millions, 2), 'million words\n')
  cat('  Years 3-5 (Preschoolers):', round(bracket_data$Preschooler_Years3_5_Millions, 2), 'million words\n')
  cat('  Total 5 years:', round(bracket_data$Total_5yr_Millions, 2), 'million words\n')
}

# Export results to CSV
write.csv(final %>% select(Bracket, 
                           Year1_Infants_Millions = Year1_Missed_Words_Millions,
                           Years1_2_Toddlers_Millions = Toddler_Years1_2_Millions,
                           Years3_5_Preschoolers_Millions = Preschooler_Years3_5_Millions,
                           Total_5yr_Millions = Total_5yr_Millions),
          file = 'five_year_cumulative_missed_words.csv',
          row.names = FALSE)

cat('\n=== Results exported to: five_year_cumulative_missed_words.csv ===\n')
