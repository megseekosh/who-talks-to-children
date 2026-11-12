# Who talks to children? Rethinking what counts as verbal input, and why it matters for language development

Code and data to reproduce the figures and numbers in the manuscript.

## Requirements

R 4.3 or later with the CRAN packages `dplyr`, `tidyr`, `ggplot2`, `gridExtra`, `broom`, `emmeans`, `car`, `scales`, `readr`, and `grid`. (`ARTool` is optional; one exploratory test is skipped without it.) Nothing is installed at run time.

## How to run

Run each script with `Rscript` from its own folder, in this order, because later scripts read files written by earlier ones:

1. `bo_CDS_report/calculate_CDS_statistics.R` writes `CDS_proportions_with_age_all_children.csv` (the per-child proportions) and Figure 2 to `bo_CDS_report/figures/`.
2. `daycare_simulations/study_outcome_simulation.R` reads the per-child file from step 1, writes `study_outcome_tracking_values.csv` and `study_outcome_simulation_summary.csv`, and Figure 3 to `daycare_simulations/figures/`.
3. `daycare_simulations/missed_verbal_input_analysis.R` writes Figure 1 to `daycare_simulations/figures/` plus `verbal_input_simulation_results.csv` and `descriptive_statistics_by_group.csv` (the per-cell values are deterministic, so the SDs in the latter are zero and no ANOVA is run). It also sources step 2 to build a combined three-panel figure that is not in the manuscript.
4. `daycare_simulations/five_year_cumulative_calculations.R` sources step 3 and writes `five_year_cumulative_missed_words.csv`.
5. `proportion_female_analyses/speaker_coding_summary.R` writes `speaker_coding_summary.csv`.

Each script's figures go to a `figures/` folder beside it (steps 1 and 3 also write a few exploratory plots and an `Rplots.pdf` that are not in the manuscript). The copies in the top-level `figures/` folder are the versions in the manuscript, for comparison. All scripts are seeded; the figures regenerate identically.

## What reproduces what

| Result in the manuscript | Script | Where to find it |
|---|---|---|
| 65.3% of reviewed studies (N = 49/75) measured only maternal input; 10.7% (8/75) mother plus others | `speaker_coding_summary.R` | printed; `speaker_coding_summary.csv` |
| Figure 2; Bolivia proportions 7.7%, 11.7%, 29.6%, 44.5%, 64.4% with SDs; N = 50 | `calculate_CDS_statistics.R` | printed; `CDS_proportions_with_age_all_children.csv` |
| Mean age 5;0 (59.7 months), age range; M = 128 clips per child (range 74-271) | `calculate_CDS_statistics.R` | printed; also the `Age_mos` and `total_clips` columns of `CDS_proportions_with_age_all_children.csv` |
| Figure 3; tracking values 1.0/.61/.10 and 1.0/.79/.56; detection 38/16/4% and 39/25/16%; mean r = .14 and near 0; significance threshold r = .28 | `study_outcome_simulation.R` | printed; the two `study_outcome_*.csv` files |
| Figure 1; 3.44, 3.72, 3.84 awake childcare hours per day (mean over the three age groups); 18-26% of weekly waking hours | `missed_verbal_input_analysis.R` | printed at the end of the run; per-cell values in `verbal_input_simulation_results.csv` |
| 2,600-3,800 daily words | `five_year_cumulative_calculations.R` | printed (`Daily_Missed_Words`) |
| 4.90, 4.56, 5.05 million words over five years | `five_year_cumulative_calculations.R` | printed; `five_year_cumulative_missed_words.csv` |

Numbers in the manuscript that come from published sources rather than from this code: the 71 studies with effect sizes, the three overheard-speech and two sibling-input studies, and the null results among them are reported by Coffey and Snedeker (2025), not computed from the coding sheet here; the NCES enrollment percentages (14%, 26%, 63%) are quoted from NCES Table 202.30. `daycare_simulations/tabn202.30.csv` is that table (exported from the NCES spreadsheet), included as the source document; the script carries the values it uses as constants, with the table rows noted in comments.

## Data

- `bo_CDS_report/all_CDS.csv` and `biling_context.csv`: clip-level annotations of 30-second clips from the Bolivian recordings (one row per annotation). Columns: the speech-register and speaker categories, the child ID (`id`; `PID` is an unused placeholder), `percents_voc` (unused), the clip timestamp, and the sleeping and researcher-present flags. File names, recording dates, dates of birth, gender, annotator identities, and local paths have been removed.
- `bo_CDS_report/child_age.csv` and `child_ages_older.csv`: child ID and age in months (`child_age.csv` also lists number of siblings, unused). Clips recorded while the child was asleep or a researcher was present are excluded by the script.
- `bo_CDS_report/CDS_proportions_with_age_all_children.csv`: the per-child proportions the figures use.
- `proportion_female_analyses/IVMA Coding - Data.csv`: the Coffey and Snedeker (2025) study list with the speaker coding added for this paper.
