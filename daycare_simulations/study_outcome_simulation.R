# Figure 3: the correlation between verbal input and vocabulary that a study of 50
# children would find, and how reliably, when verbal input is measured narrowly
# (child-directed speech from the primary caregiver only) or permissively
# (child-directed speech from anyone), in three caregiving environments.
#
# How the simulation works (repeated 5,000 times per facet):
#   1. Make 50 children, each with a true amount of total verbal input (all speech heard).
#   2. Give each child a vocabulary that relates to total input at r = .24 (the pooled
#      estimate across 71 studies in Coffey & Snedeker, 2025). Same in every facet.
#   3. Give each child the study's measurement. Its correlation with the child's true
#      total input is the facet's "tracking" value (below): 1.0 means the measurement is
#      a perfect stand-in for total input, 0 means it tells you nothing about it.
#   4. Correlate the measurement with vocabulary across the 50 children, as a study would.
# The histogram in each facet is the 5,000 correlations from step 4; the percentage is
# the share that reach significance (r >= .28 for 50 children).
#
# Where the tracking values come from (all computed below, not typed in):
#   Primarily dyadic care: Shneidman et al. (2013) single-speaker group. Every utterance
#     the child heard was directed to them by the primary caregiver, so both measures
#     equal total input: 1.0 and 1.0.
#   Multi-speaker households: Shneidman et al. (2013) multi-speaker group (N = 15). The
#     paper reports means and SDs of the shares but not per-child counts, so children
#     are simulated whose shares match the reported values (directed speech = 69%,
#     SD 15, of utterances heard; of directed speech, 76%, SD 22, from the primary
#     caregiver; total tokens M = 6,286, SD 1,837) and the tracking values are the
#     correlations in that simulated sample: about .61 (caregiver only) and .79 (anyone).
#   Distributed/alloparental care: the 50 Bolivian children in Figure 2
#     (bo_CDS_report/CDS_proportions_with_age_all_children.csv). Across children, the
#     proportion of clips with speech to the target child from a single female caregiver
#     correlates with the proportion of clips with any speech at about .10; speech to
#     the target child from any speaker correlates with any speech at about .56.
# The simulation assumes the true input-vocabulary relationship is the same size in
# every environment, so any weakening is due to the measure alone.
library(dplyr); library(ggplot2)
# Text scale: 1 for the standalone figure; set hist_text_scale higher before sourcing for the combined Figure 1
hs <- if (exists("hist_text_scale")) hist_text_scale else 1
here <- if (exists("script_dir")) script_dir else getwd()
r_anchor <- 0.24
n_study  <- 50     # round study size; the mean in Coffey & Snedeker (2025) is 67 (4,760 children / 71 studies)
n_sims   <- 5000

# ---- Tracking values: multi-speaker households, estimated from Shneidman et al. (2013) ----
set.seed(2026)
n_sh <- 20000
beta_draw <- function(m, s, n = n_sh) { k <- m * (1 - m) / s^2 - 1; rbeta(n, m * k, (1 - m) * k) }
total_tokens <- rlnorm(n_sh, meanlog = log(6286) - 0.5 * log(1 + (1837 / 6286)^2),
                       sdlog = sqrt(log(1 + (1837 / 6286)^2)))
directed_tokens <- total_tokens * beta_draw(0.69, 0.15)   # child-directed speech from anyone
pc_tokens <- directed_tokens * beta_draw(0.76, 0.22)       # child-directed speech from the primary caregiver
track_multi_pc  <- cor(pc_tokens, total_tokens)
track_multi_any <- cor(directed_tokens, total_tokens)
cat(sprintf("Check against reported token SDs: all directed %.0f (reported 1465); primary caregiver %.0f (reported 1463)\n",
            sd(directed_tokens), sd(pc_tokens)))

# ---- Tracking values: distributed care, computed from the Bolivian sample (Figure 2) ----
bo <- read.csv(file.path(here, "..", "bo_CDS_report", "CDS_proportions_with_age_all_children.csv"))
track_bo_pc  <- cor(bo$percen_TCDS_female, bo$percen_any_speech)          # single female caregiver vs all speech
track_bo_any <- cor(bo$percen_target_child_speech, bo$percen_any_speech)  # target child from anyone vs all speech

tracking <- tribble(
  ~environment, ~measure, ~track,
  "Primarily dyadic care", "Primary caregiver only", 1.00,
  "Multi-speaker households", "Primary caregiver only", round(track_multi_pc, 2),
  "Distributed/alloparental care", "Primary caregiver only", round(track_bo_pc, 2),
  "Primarily dyadic care", "Child-directed speech from anyone", 1.00,
  "Multi-speaker households", "Child-directed speech from anyone", round(track_multi_any, 2),
  "Distributed/alloparental care", "Child-directed speech from anyone", round(track_bo_any, 2)
) %>% mutate(environment = factor(environment, levels = unique(environment)),
             measure = factor(measure, levels = unique(measure)))
cat("Tracking values used (correlation between the study's measure and total verbal input):\n")
print(as.data.frame(tracking))
write.csv(tracking, file.path(here, "study_outcome_tracking_values.csv"), row.names = FALSE)

set.seed(2026)
one_study <- function(track) {
  z <- rnorm(n_study)                                          # child's total input (standardized)
  vocab <- r_anchor * z + sqrt(1 - r_anchor^2) * rnorm(n_study)
  measured <- track * z + sqrt(1 - track^2) * rnorm(n_study)   # the study's input measure
  cor(vocab, measured)
}
sims <- tracking %>% rowwise() %>%
  mutate(r = list(replicate(n_sims, one_study(track)))) %>% ungroup() %>% tidyr::unnest(r)
# smallest r that reaches p < .05 (two-sided) with n_study children
r_crit <- qt(0.975, n_study - 2) / sqrt(qt(0.975, n_study - 2)^2 + n_study - 2)
cat(sprintf("Smallest significant r for %d children (two-sided p < .05): %.4f\n", n_study, r_crit))
summ <- sims %>% group_by(environment, measure, track) %>%
  summarise(expected = mean(r), lo = quantile(r, .025), hi = quantile(r, .975),
            pct_sig = 100 * mean(r >= r_crit), pct_wrong_sign = 100 * mean(r < 0), .groups = "drop") %>%
  mutate(across(c(expected, lo, hi), ~round(.x, 2)), across(starts_with("pct"), ~round(.x)))
print(as.data.frame(summ))
write.csv(summ, file.path(here, "study_outcome_simulation_summary.csv"), row.names = FALSE)

# ---- Figure: iceberg histograms -------------------------------------------------
# One panel per environment (columns) and measure (rows). Each panel is the histogram
# of correlations the 5,000 simulated studies found; bars at or above r_crit are dark
# (the study would report a significant relationship), the rest light. Black tick on
# the axis: the average result. Dashed line: r = .24.
bins <- seq(-0.45, 0.65, by = 0.02)
hist_d <- sims %>% mutate(bin = cut(r, bins, labels = bins[-length(bins)] + 0.01, include.lowest = TRUE),
                          bin = as.numeric(as.character(bin))) %>%
  count(environment, measure, bin) %>% group_by(environment, measure) %>%
  mutate(pct = 100 * n / sum(n), detected = bin >= r_crit) %>% ungroup()
labs_d <- summ %>% mutate(label = sprintf("%d%%", pct_sig))
tick <- function(env) summ$expected[summ$environment == env & summ$measure == "Primary caregiver only"]
# Bright hues, distinct from Figure 1 (red/blue/green) and Figure 2 (pink/purple/amber/teal)
grid_colors <- c("Primarily dyadic care" = "#F28E2B",        # orange
                 "Multi-speaker households" = "#984EA3",     # purple
                 "Distributed/alloparental care" = "#00838F") # deep teal
notes <- tibble(
  environment = factor(c("Primarily dyadic care", "Distributed/alloparental care"),
                       levels = levels(summ$environment)),
  measure = factor("Primary caregiver only", levels = levels(summ$measure)),
  x_lab = c(1.6, 1.4), y_lab = c(-0.41, -0.27),
  x_pt = c(0.1, 0.1),
  y_pt = c(tick("Primarily dyadic care"), tick("Distributed/alloparental care")),
  label = c("Nearly all verbal input comes\nfrom the primary caregiver; so\nmost input measures give the\nsame r = .24.",
            "Most verbal input comes from\nother people; so caregiver-only\nmeasures of input give r = ~0,\nand are as likely to find a positive\nrelationship between verbal input\nand vocabulary as a negative one."))
p_hist <- ggplot(hist_d, aes(x = pct, y = bin, fill = environment, alpha = detected)) +
  geom_col(orientation = "y", width = 0.02) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4 * hs) +
  geom_hline(yintercept = r_anchor, color = "grey20", linetype = "dashed", linewidth = 0.4 * hs) +
  geom_segment(data = summ, aes(x = -0.4, xend = 0, y = expected, yend = expected), inherit.aes = FALSE,
               color = "black", linewidth = 1.2 * hs) +
  geom_text(data = labs_d, aes(x = 7.8, y = 0.64, label = label), inherit.aes = FALSE,
            hjust = 1, vjust = 1, size = 5 * hs, fontface = "bold") +
  geom_curve(data = notes, aes(x = c(2.6, 2.3), y = c(-0.34, -0.20), xend = x_pt, yend = y_pt), inherit.aes = FALSE,
             color = "grey20", curvature = -0.15, linewidth = 0.8, arrow = arrow(length = unit(0.28, "cm"), type = "closed")) +
  geom_curve(data = tibble(environment = factor("Primarily dyadic care", levels = levels(summ$environment)),
                           measure = factor("Child-directed speech from anyone", levels = levels(summ$measure)),
                           x = 2.4, y = 0.65, xend = 0.1,
                           yend = summ$expected[summ$environment == "Primarily dyadic care" & summ$measure == "Child-directed speech from anyone"]),
             aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE,
             color = "grey20", curvature = 0.15, linewidth = 0.8 * hs, arrow = arrow(length = unit(0.28 * hs, "cm"), type = "closed")) +
  geom_text(data = notes, aes(x = x_lab, y = y_lab, label = label), inherit.aes = FALSE,
            color = "grey15", size = 3.5 * hs, lineheight = 0.9, hjust = 0, vjust = 1) +
  facet_grid(measure ~ environment, labeller = labeller(environment = label_wrap_gen(22),
             measure = as_labeller(c("Primary caregiver only" = "Child-directed speech\nfrom primary caregiver only",
                                     "Child-directed speech from anyone" = "Child-directed\nspeech from anyone")))) +
  scale_fill_manual(values = grid_colors, guide = "none") +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = "none") +
  scale_y_continuous(limits = c(-0.63, 0.66), breaks = seq(-0.4, 0.6, 0.2), labels = function(x) sub("^(-?)0", "\\1", sprintf("%.1f", x))) +
  scale_x_continuous(limits = c(-0.4, 9.0), breaks = seq(0, 6, 2), labels = function(x) format(x * n_sims / 100, big.mark = ","), expand = c(0, 0)) +
  labs(y = sprintf("Verbal input-vocabulary correlation (r)\nthat a %d-child study would find", n_study),
       x = "Number of studies (of 5,000) that found each value of r") +
  theme_minimal(base_size = 12 * hs) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", size = 13 * hs),
        panel.spacing.y = unit(0, "lines"), panel.spacing.x = unit(1.2 * hs, "lines"),
        axis.title = element_text(face = "bold", size = 15 * hs), axis.text = element_text(size = 12 * hs))
if (hs == 1) {
  dir.create(file.path(here, "figures"), showWarnings = FALSE)
  ggsave(file.path(here, "figures", "study_outcome_histograms.png"), p_hist, width = 11, height = 7.2, dpi = 300)
}
