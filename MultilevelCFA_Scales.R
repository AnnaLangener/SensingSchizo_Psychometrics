#####################################################################################################################################################
# Capturing Schizophrenia Symptoms in Daily Life via the Smartphone: Psychometric Evaluation of Newly Proposed Symptom Scales
#####################################################################################################################################################

###############################################
############ Multilevel CFA ###################
###############################################

library(lavaan)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)

####### Load data #######

main_path <- "/Users/anna/Library/CloudStorage/GoogleDrive-langener95@gmail.com/My Drive/Research/9_PsychometricSensing"
combined_df_all <- read.csv(file.path(main_path, "esm_cleaned.csv"),
                            colClasses = c(participant_id = "character"))

# Use the included sample from Data_Cleaning.R.
# Cognitive items are already reversed; do not reverse them again here.

####### Item key #######

item_key <- tibble(
  questionText = c(
    "In the past 4 hours, I would rather have been doing something else.",
    "In the past 4 hours, I have felt unmotivated.",
    "In the past 4 hours, I have felt emotionally flat, numb, or blank.",
    "In the past 4 hours, I have experienced difficulty expressing my emotions or thoughts.",
    "In the past 4 hours, I have not wanted to socialize.",
    "In the past 4 hours, to what degree have you had any unusual experiences?",
    "In the past 4 hours, I have felt detached from reality.",
    "In the past 4 hours, I have felt I possess special powers or abilities.",
    "In the past 4 hours, I have felt I am receiving special messages.",
    "In the past 4 hours, I have felt suspicious of others.",
    "In the past 4 hours, I have found it easy to concentrate.",
    "In the past 4 hours, I have experienced racing thoughts.",
    "In the past 4 hours, I have found it easy to make decisions.",
    "In the past 4 hours, I have experienced difficulty thinking clearly.",
    "In the past 4 hours, I have had trouble remembering things."
  ),
  item = c(paste0("n", 1:5), paste0("p", 1:5), paste0("c", 1:5)),
  domain = rep(c("Negative", "Positive", "Cognitive"), each = 5),
  label = c("Elsewhere", "Unmotivated", "Flat", "Expression", "Social",
            "Unusual experiences", "Detached", "Powers", "Messages", "Suspicious",
            "Concentrate (reversed)", "Racing", "Decisions (reversed)", "Clarity", "Memory")
)

####### Prepare item-level wide data #######

ema_symptoms <- combined_df_all %>%
  filter(ema_category %in% c("Negative symptom", "Positive symptom", "Cognitive symptom")) %>%
  select(participant_id, time_index, ema_category, questionText, response) %>%
  mutate(
    questionText = trimws(questionText),
    response = as.numeric(response)
  ) %>%
  left_join(item_key, by = "questionText")

# Do not silently omit an item with an unexpected wording or average duplicates.
if (anyNA(ema_symptoms$item)) {
  print(unique(ema_symptoms$questionText[is.na(ema_symptoms$item)]))
  stop("Unmatched symptom questions: update item_key before fitting.")
}
stopifnot(!anyNA(ema_symptoms$participant_id),
          all(nzchar(ema_symptoms$participant_id)),
          !anyNA(ema_symptoms$time_index))
if (any(ema_symptoms$response < 0 | ema_symptoms$response > 100, na.rm = TRUE))
  stop("Expected cleaned item responses between 0 and 100.")
if (anyDuplicated(ema_symptoms[c("participant_id", "time_index", "item")]))
  stop("Duplicate responses per participant, occasion, and item; resolve these first.")

ema_wide <- ema_symptoms %>%
  select(participant_id, time_index, item, response) %>%
  pivot_wider(names_from = item, values_from = response) %>%
  arrange(participant_id, time_index)

stopifnot(all(item_key$item %in% names(ema_wide)))

# Retain partial item data for full-information ML. Remove wholly empty occasions.
ema_wide <- ema_wide %>%
  filter(if_any(all_of(item_key$item), ~ !is.na(.))) %>%
  group_by(participant_id) %>%
  filter(n() >= 2) %>%
  ungroup()

# This is an occasion-within-person model, not a model of serial dependence.
# time_index identifies occasions; it is not entered as a predictor.
# Items are treated as continuous 0-100 responses; MLR gives robust inference.
# Within- and between-person loadings are estimated separately.

####### Sample overview #######

occasions_per_person <- ema_wide %>% count(participant_id, name = "occasions")
sample_summary <- data.frame(
  Participants = n_distinct(ema_wide$participant_id),
  Occasions = nrow(ema_wide),
  Min_occasions = min(occasions_per_person$occasions),
  Max_occasions = max(occasions_per_person$occasions),
  Complete_occasions = sum(complete.cases(ema_wide[item_key$item]))
)
sample_summary

item_missingness <- data.frame(
  Item = item_key$item,
  Missing_percent = 100 * colMeans(is.na(ema_wide[item_key$item]))
)
item_missingness

####### Partially saturated CFA models #######

# Ryu & West (2009): evaluate one level while freely estimating all item
# variances/covariances at the other level. Means remain unrestricted.
# Adaptation for our three domains: one factor, three uncorrelated factors,
# and three correlated factors. No experimental-group variable is assumed.
# Method: https://doi.org/10.3389/fpsyg.2014.00081

saturated_block <- function(items) {
  paste(vapply(seq_along(items), function(i)
    paste(items[i], "~~", paste(items[i:length(items)], collapse = " + ")),
    character(1)), collapse = "\n")
}

factor_block <- function(level, type) {
  suffix <- if (level == 1) "w" else "b"
  if (type == "One factor")
    return(paste0("Symptoms_", suffix, " =~ ", paste(item_key$item, collapse = " + ")))
  factors <- paste0(c("Negative", "Positive", "Cognitive"), "_", suffix)
  loadings <- vapply(seq_along(factors), function(i)
    paste(factors[i], "=~", paste(item_key$item[item_key$domain ==
      c("Negative", "Positive", "Cognitive")[i]], collapse = " + ")), character(1))
  pairs <- combn(factors, 2)
  covariances <- apply(pairs, 2, function(x)
    paste(x[1], "~~", if (type == "Three uncorrelated") paste0("0*", x[2]) else x[2]))
  paste(c(loadings, covariances), collapse = "\n")
}

partial_model <- function(level, type) {
  target <- if (type == "Independence")
    paste(item_key$item, "~~", item_key$item, collapse = "\n") else factor_block(level, type)
  saturated <- saturated_block(item_key$item)
  paste("level: 1", if (level == 1) target else saturated,
        "level: 2", if (level == 2) target else saturated, sep = "\n")
}

fit_partial <- function(model) {
  cfa(model, data = ema_wide, cluster = "participant_id",
      estimator = "MLR", missing = "fiml", std.lv = TRUE,
      auto.cov.y = FALSE)
}

model_grid <- expand.grid(Model = c("One factor", "Three uncorrelated", "Three correlated"),
                          Level = c("Within", "Between"), stringsAsFactors = FALSE)
model_syntax <- lapply(seq_len(nrow(model_grid)), function(i)
  partial_model(if (model_grid$Level[i] == "Within") 1 else 2, model_grid$Model[i]))
names(model_syntax) <- paste(model_grid$Level, model_grid$Model, sep = ": ")
fits <- lapply(model_syntax, fit_partial)

# The independence baseline constrains only the level being evaluated.
fit_baseline_within <- fit_partial(partial_model(1, "Independence"))
fit_baseline_between <- fit_partial(partial_model(2, "Independence"))
fit_three_within <- fits[["Within: Three correlated"]]
fit_three_between <- fits[["Between: Three correlated"]]
fit_one_within <- fits[["Within: One factor"]]
fit_one_between <- fits[["Between: One factor"]]

########## Model diagnostics ###########

extract_diagnostics <- function(name, fit) {
  converged <- lavInspect(fit, "converged")
  parameters <- parameterEstimates(fit)
  data.frame(Model = name, Converged = converged,
    Admissible = if (converged) lavInspect(fit, "post.check") else FALSE,
    Negative_variances = sum(parameters$op == "~~" & parameters$lhs == parameters$rhs &
                               parameters$est < 0, na.rm = TRUE))
}
model_diagnostics <- bind_rows(lapply(seq_along(fits), function(i)
  extract_diagnostics(model_grid$Model[i], fits[[i]]) %>% mutate(Level = model_grid$Level[i])))
baseline_diagnostics <- bind_rows(
  extract_diagnostics("Independence", fit_baseline_within) %>% mutate(Level = "Within"),
  extract_diagnostics("Independence", fit_baseline_between) %>% mutate(Level = "Between"))
model_diagnostics
baseline_diagnostics
if (any(!model_diagnostics$Converged | !model_diagnostics$Admissible) ||
    any(!baseline_diagnostics$Converged | !baseline_diagnostics$Admissible))
  warning("Review convergence/admissibility of candidate and baseline models before reporting.")

########## Level-specific model fit ###########

extract_fit <- function(name, level, fit, baseline) {
  measures <- c("chisq", "df", "pvalue", "chisq.scaled", "pvalue.scaled",
                "cfi", "tli", "cfi.robust", "srmr_within", "srmr_between", "aic", "bic")
  values <- setNames(rep(NA_real_, length(measures)), measures)
  if (lavInspect(fit, "converged")) {
    baseline_ok <- lavInspect(baseline, "converged") && lavInspect(baseline, "post.check")
    available <- if (baseline_ok) fitMeasures(fit, baseline.model = baseline) else fitMeasures(fit)
    present <- intersect(measures, names(available))
    values[present] <- available[present]
    if (!baseline_ok) values[c("cfi", "tli", "cfi.robust")] <- NA_real_
  }
  # Original ML level-specific RMSEA: effective N is N-J for WP and J for BP.
  # Deliberately uses unscaled chi-square; do not label this as robust RMSEA.
  effective_n <- if (level == "Within") nrow(ema_wide) - n_distinct(ema_wide$participant_id)
                 else n_distinct(ema_wide$participant_id)
  rmsea <- if (is.finite(values["df"]) && values["df"] > 0 && effective_n > 0)
    sqrt(max(values["chisq"] - values["df"], 0) / (values["df"] * effective_n)) else NA_real_
  data.frame(Level = level, Model = name, Chi_square = values["chisq"], DF = values["df"],
    P = values["pvalue"], Chi_square_scaled = values["chisq.scaled"], P_scaled = values["pvalue.scaled"],
    CFI = values["cfi"], TLI = values["tli"], CFI_robust = values["cfi.robust"],
    RMSEA = rmsea, SRMR = values[if (level == "Within") "srmr_within" else "srmr_between"],
    AIC = values["aic"], BIC = values["bic"], row.names = NULL)
}
fit_summary <- bind_rows(lapply(seq_along(fits), function(i) {
  level <- model_grid$Level[i]
  baseline <- if (level == "Within") fit_baseline_within else fit_baseline_between
  extract_fit(model_grid$Model[i], level, fits[[i]], baseline)
})) %>% left_join(model_diagnostics, by = c("Model", "Level"))

# CFI/TLI/RMSEA are original ML indices; MLR chi-square and robust CFI are
# additional sensitivity results (NA when lavaan does not provide them).
# Compare AIC/BIC only within the same level on this same analysis sample.
# One-factor versus correlated factors involves a boundary: no routine LRT.
fit_table <- fit_summary %>%
  gt(groupname_col = "Level") %>%
  tab_header(title = "Level-specific Confirmatory Factor Analysis",
             subtitle = "Other level saturated: one factor versus three symptom domains") %>%
  fmt_number(columns = c(CFI, TLI, CFI_robust, RMSEA, SRMR), decimals = 3) %>%
  fmt_number(columns = c(Chi_square, Chi_square_scaled, AIC, BIC), decimals = 2) %>%
  fmt_number(columns = c(P, P_scaled), decimals = 3) %>%
  tab_source_note("CFI/TLI use partially saturated baselines. RMSEA is ML with N-J (within) or J (between); it is not robust RMSEA.")
fit_table

########## Standardized loadings and factor correlations ###########

# standardizedSolution() does not retain the level column in some lavaan
# versions. Match repeated parameter names in their within/between order.
standardized_by_level <- function(fit) {
  level_key <- parameterEstimates(fit) %>%
    select(lhs, op, rhs, level) %>%
    group_by(lhs, op, rhs) %>% mutate(Occurrence = row_number()) %>% ungroup()
  result <- standardizedSolution(fit) %>%
    select(-any_of("level")) %>%
    group_by(lhs, op, rhs) %>% mutate(Occurrence = row_number()) %>% ungroup() %>%
    left_join(level_key, by = c("lhs", "op", "rhs", "Occurrence"))
  stopifnot(!anyNA(result$level))
  result
}

standardized_parameters <- bind_rows(
  standardized_by_level(fit_three_within) %>% filter(level == 1),
  standardized_by_level(fit_three_between) %>% filter(level == 2))
loadings <- standardized_parameters %>%
  filter(op == "=~") %>%
  transmute(Level = ifelse(level == 1, "Within", "Between"),
            Factor = lhs, item = rhs, Loading = est.std, SE = se,
            CI_lower = ci.lower, CI_upper = ci.upper) %>%
  left_join(item_key %>% select(item, label, domain), by = "item")

factor_names <- c("Negative_w", "Positive_w", "Cognitive_w",
                  "Negative_b", "Positive_b", "Cognitive_b")
factor_correlations <- standardized_parameters %>%
  filter(op == "~~", lhs != rhs, lhs %in% factor_names, rhs %in% factor_names) %>%
  transmute(Level = ifelse(level == 1, "Within", "Between"),
            Factor_1 = lhs, Factor_2 = rhs, Correlation = est.std,
            SE = se, CI_lower = ci.lower, CI_upper = ci.upper)

loadings
factor_correlations

# Values close to +/-1 suggest weak distinction between factors at that level.
# Do not automatically add cross-loadings or residual correlations to improve fit.
# The between-person model is supported by the number of participants, not by
# the total number of occasions. Report small-sample uncertainty explicitly.

########## Plots of Results ###########

domain_colours <- c("Negative" = "#83AF9B", "Positive" = "#FC9D9A",
                    "Cognitive" = "#C8C8A9")
loadings_plot <- correlations_plot <- fit_plot <- NULL

# Plot substantive estimates only when the three-factor solution is admissible.
three_factor_ok <- with(model_diagnostics,
                       all(Converged[Model == "Three correlated"] & Admissible[Model == "Three correlated"]))

if (isTRUE(three_factor_ok)) {
  #### Standardized item loadings ####
  loadings_plot_data <- loadings %>%
    mutate(Level = factor(Level, levels = c("Within", "Between")),
           domain = factor(domain, levels = c("Negative", "Positive", "Cognitive")),
           label = factor(label, levels = rev(item_key$label)))

  loadings_plot <- ggplot(loadings_plot_data, aes(Loading, label, colour = domain)) +
    geom_vline(xintercept = 0, colour = "grey70", linetype = "dashed") +
    geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper), orientation = "y", width = 0.2) +
    geom_point(size = 2.5) +
    facet_grid(Level ~ domain, scales = "free_y", space = "free_y") +
    scale_colour_manual(values = domain_colours) +
    labs(title = "Standardized Factor Loadings",
         subtitle = "Three-factor multilevel CFA: within- and between-person estimates",
         x = "Standardized loading (95% confidence interval)", y = NULL) +
    theme_bw(base_size = 11) +
    theme(legend.position = "none", panel.grid.minor = element_blank())

  loadings_plot

  #### Correlations between symptom factors ####
  correlation_plot_data <- factor_correlations %>%
    mutate(Level = factor(Level, levels = c("Within", "Between")),
           Pair = paste(sub("_[wb]$", "", Factor_1),
                        sub("_[wb]$", "", Factor_2), sep = " / "))

  correlations_plot <- ggplot(correlation_plot_data, aes(Correlation, Pair)) +
    geom_vline(xintercept = 0, colour = "grey70", linetype = "dashed") +
    geom_errorbar(aes(xmin = CI_lower, xmax = CI_upper), orientation = "y",
                  width = 0.15, colour = "#2A363B") +
    geom_point(size = 3, colour = "#FC9D9A") +
    facet_wrap(~ Level, ncol = 1) +
    labs(title = "Correlations Between Symptom Domains",
         x = "Latent factor correlation (95% confidence interval)", y = NULL) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  correlations_plot
} else {
  message("Loading and correlation plots skipped: review the three-factor model diagnostics.")
}

#### Fit of the three-factor and one-factor models ####
# Different indices have different interpretations; show them in separate panels.
fit_plot_data <- fit_summary %>%
  filter(Converged, Admissible) %>%
  select(Level, Model, CFI, TLI, RMSEA, SRMR) %>%
  pivot_longer(-c(Level, Model), names_to = "Index", values_to = "Value") %>%
  filter(is.finite(Value))

if (nrow(fit_plot_data)) {
  fit_plot <- ggplot(fit_plot_data, aes(Model, Value, colour = Model)) +
    geom_point(size = 3) +
    geom_text(aes(label = sprintf("%.3f", Value)), vjust = -0.8, size = 3) +
    facet_grid(Level ~ Index, scales = "free_y") +
    scale_colour_manual(values = c("Three correlated" = "#83AF9B", "Three uncorrelated" = "#C8C8A9", "One factor" = "#FC9D9A")) +
    scale_y_continuous(expand = expansion(mult = c(0.15, 0.3))) +
    labs(title = "Multilevel CFA Model Fit", x = NULL, y = "Fit index",
         caption = "CFI/TLI: higher is better. RMSEA/SRMR: lower is better. Only admissible, converged models are shown.") +
    theme_bw(base_size = 11) +
    theme(legend.position = "none", panel.grid.minor = element_blank())

  fit_plot
}

####### Save results #######

output_path <- file.path(getwd(), "multilevel_cfa_results")
dir.create(output_path, showWarnings = FALSE)
write.csv(fit_summary, file.path(output_path, "cfa_level_specific_fit.csv"), row.names = FALSE)
write.csv(baseline_diagnostics, file.path(output_path, "cfa_baseline_diagnostics.csv"), row.names = FALSE)
write.csv(loadings, file.path(output_path, "cfa_loadings.csv"), row.names = FALSE)
write.csv(factor_correlations, file.path(output_path, "cfa_factor_correlations.csv"), row.names = FALSE)
saveRDS(list(syntax = model_syntax, fits = fits,
  baselines = list(within = fit_baseline_within, between = fit_baseline_between)),
  file.path(output_path, "cfa_models.rds"))
if (!is.null(fit_plot))
  ggsave(file.path(output_path, "cfa_level_specific_fit.png"), fit_plot, width = 14, height = 7, dpi = 300)
if (!is.null(loadings_plot))
  ggsave(file.path(output_path, "cfa_loadings.png"), loadings_plot, width = 12, height = 9, dpi = 300)
if (!is.null(correlations_plot))
  ggsave(file.path(output_path, "cfa_factor_correlations.png"), correlations_plot, width = 8, height = 7, dpi = 300)


########## Exploratory Factor Analyses ###########

# Explore all 15 items together, separately at the within and between levels.
# This is level-specific EFA of unrestricted multilevel covariance matrices,
# rather than a simultaneous multilevel EFA. lavaan's h1 matrices do not impose
# the CFA loading pattern. The prepared sample and FIML missing-data treatment
# are shared with the CFA. CFA and EFA in this sample are not independent tests.
if (!requireNamespace("psych", quietly = TRUE) ||
    !requireNamespace("GPArotation", quietly = TRUE))
  stop("EFA requires psych and GPArotation.")

unrestricted_covariances <- lavInspect(fit_three_within, "h1")
within_cov <- unrestricted_covariances$within$cov[item_key$item, item_key$item]
between_cov <- unrestricted_covariances$cluster$cov[item_key$item, item_key$item]

prepare_efa_cor <- function(covariance, level) {
  if (any(!is.finite(covariance)) || any(diag(covariance) <= 0)) {
    warning(paste(level, "EFA skipped: invalid or zero item variances."))
    return(NULL)
  }
  correlation <- cov2cor(covariance)
  if (min(eigen(correlation, symmetric = TRUE, only.values = TRUE)$values) <= 1e-8) {
    warning(paste(level, "EFA skipped: correlation matrix is not positive definite."))
    return(NULL)
  }
  correlation
}

# Do not silently smooth an invalid matrix or drop its items.
within_cor <- prepare_efa_cor(within_cov, "Within")
between_cor <- prepare_efa_cor(between_cov, "Between")

#### Three-factor exploratory solutions ####
# Minimum residual extraction and oblimin rotation allow correlated factors.
# All items may load on every factor; no target rotation is used.
# Factor order/signs are arbitrary and need not match between levels.
# n.obs is intentionally omitted: the independent-observation assumptions of
# conventional EFA inference do not apply to these estimated clustered matrices.

efa_within <- efa_between <- NULL
if (!is.null(within_cor)) {
  efa_within <- psych::fa(within_cor, nfactors = 3, fm = "minres",
                          rotate = "oblimin", smooth = FALSE)
  print(efa_within, sort = FALSE, cut = 0.20)
}
if (!is.null(between_cor)) {
  efa_between <- psych::fa(between_cor, nfactors = 3, fm = "minres",
                           rotate = "oblimin", smooth = FALSE)
  print(efa_between, sort = FALSE, cut = 0.20)
}

#### One- to four-factor sensitivity checks ####
efa_within_solutions <- efa_between_solutions <- list()
for (n_factors in 1:4) {
  rotation <- if (n_factors == 1) "none" else "oblimin"
  if (!is.null(within_cor))
    efa_within_solutions[[as.character(n_factors)]] <- if (n_factors == 3) efa_within else
      psych::fa(within_cor, nfactors = n_factors, fm = "minres", rotate = rotation, smooth = FALSE)
  if (!is.null(between_cor))
    efa_between_solutions[[as.character(n_factors)]] <- if (n_factors == 3) efa_between else
      psych::fa(between_cor, nfactors = n_factors, fm = "minres", rotate = rotation, smooth = FALSE)
}

extract_efa_summary <- function(solutions, level) {
  bind_rows(lapply(names(solutions), function(n_factors) {
    result <- solutions[[n_factors]]
    data.frame(Level = level, Factors = as.integer(n_factors), RMSR = result$rms,
               Problem_uniquenesses = sum(result$uniquenesses <= 0 | result$uniquenesses > 1,
                                          na.rm = TRUE))
  }))
}

efa_summary <- bind_rows(extract_efa_summary(efa_within_solutions, "Within"),
                         extract_efa_summary(efa_between_solutions, "Between"))
efa_summary
# RMSR generally decreases as factors are added; it is not a factor-retention
# test. Inspect cross-loadings, interpretability, uniquenesses, and scree plots.

#### Exploratory loading tables ####
extract_efa_loadings <- function(result, level) {
  if (is.null(result)) return(NULL)
  pattern <- as.data.frame(unclass(result$loadings))
  names(pattern) <- paste0("Factor_", seq_len(ncol(pattern)))
  pattern$item <- rownames(pattern)
  pattern$Level <- level
  pattern$Communality <- result$communality
  pattern$Uniqueness <- result$uniquenesses
  left_join(pattern, item_key %>% select(item, label, domain), by = "item")
}

efa_loadings <- bind_rows(extract_efa_loadings(efa_within, "Within"),
                          extract_efa_loadings(efa_between, "Between"))
efa_loadings
if (!is.null(efa_within)) efa_within$Phi
if (!is.null(efa_between)) efa_between$Phi

#### Scree plot ####
extract_eigenvalues <- function(correlation, level) {
  if (is.null(correlation)) return(NULL)
  data.frame(Level = level, Component = seq_len(ncol(correlation)),
             Eigenvalue = eigen(correlation, symmetric = TRUE, only.values = TRUE)$values)
}
scree_data <- bind_rows(extract_eigenvalues(within_cor, "Within"),
                        extract_eigenvalues(between_cor, "Between"))
efa_scree_plot <- efa_loadings_plot <- NULL

if (nrow(scree_data)) {
  efa_scree_plot <- ggplot(scree_data, aes(Component, Eigenvalue, colour = Level)) +
    geom_line(linewidth = 0.7) + geom_point(size = 2) + facet_wrap(~ Level) +
    scale_colour_manual(values = c("Within" = "#83AF9B", "Between" = "#FC9D9A")) +
    scale_x_continuous(breaks = 1:15) +
    labs(title = "Within- and Between-person Scree Plots", x = "Component", y = "Eigenvalue",
         caption = "Eigenvalues of level-specific item correlation matrices; descriptive factor-retention aid.") +
    theme_bw(base_size = 11) + theme(legend.position = "none", panel.grid.minor = element_blank())
  print(efa_scree_plot)
}

#### Three-factor pattern-loading heatmap ####
if (nrow(efa_loadings)) {
  efa_plot_data <- efa_loadings %>%
    mutate(Item_label = factor(paste(domain, label, sep = ": "),
                              levels = rev(paste(item_key$domain, item_key$label, sep = ": "))),
           Level = factor(Level, levels = c("Within", "Between"))) %>%
    pivot_longer(starts_with("Factor_"), names_to = "Factor", values_to = "Loading")

  efa_loadings_plot <- ggplot(efa_plot_data, aes(Factor, Item_label, fill = Loading)) +
    geom_tile(colour = "white") + geom_text(aes(label = sprintf("%.2f", Loading)), size = 3) +
    facet_wrap(~ Level) +
    scale_fill_gradient2(low = "#FE4365", mid = "white", high = "#83AF9B", midpoint = 0) +
    labs(title = "Exploratory Three-factor Pattern Loadings", x = NULL, y = NULL,
         caption = "All items can load on every factor. Factor order and signs are arbitrary across levels.") +
    theme_bw(base_size = 11) + theme(panel.grid = element_blank())
  print(efa_loadings_plot)
}

####### Save exploratory results #######
output_path <- file.path(getwd(), "multilevel_cfa_results")
dir.create(output_path, showWarnings = FALSE)
write.csv(efa_summary, file.path(output_path, "efa_sensitivity_summary.csv"), row.names = FALSE)
write.csv(efa_loadings, file.path(output_path, "efa_three_factor_loadings.csv"), row.names = FALSE)
if (!is.null(within_cor)) write.csv(within_cor, file.path(output_path, "efa_within_correlations.csv"))
if (!is.null(between_cor)) write.csv(between_cor, file.path(output_path, "efa_between_correlations.csv"))
saveRDS(list(Within = efa_within_solutions, Between = efa_between_solutions),
        file.path(output_path, "efa_models.rds"))
if (!is.null(efa_scree_plot))
  ggsave(file.path(output_path, "efa_scree.png"), efa_scree_plot, width = 10, height = 5, dpi = 300)
if (!is.null(efa_loadings_plot))
  ggsave(file.path(output_path, "efa_loadings.png"), efa_loadings_plot, width = 10, height = 8, dpi = 300)

########## Detailed lavaanPlot Diagrams ###########

# Separate diagrams for each level: the same observed item names occur at both
# levels and must not be merged into one node. Use lavaanPlot's exported node,
# edge and Graphviz builders to retain level-specific parameters.
# EFA objects are psych objects, not fitted lavaan models; their results are
# shown in the loading heatmaps above rather than mislabeled as CFA diagrams.

make_lavaan_diagram <- function(fit, level, title) {
  coefficients <- standardized_by_level(fit) %>%
    filter(.data$level == .env$level, op %in% c("=~", "~~"))
  raw_parameters <- parameterEstimates(fit) %>% filter(.data$level == .env$level)
  # create_edges uses an 'est' column, whereas standardizedSolution uses est.std.
  coefficients$est <- coefficients$est.std
  if (!"label" %in% names(coefficients)) coefficients$label <- ""

  labels <- setNames(paste(item_key$item, item_key$label, sep = ": "), item_key$item)
  labels <- c(labels, Negative_w = "Negative (within)", Positive_w = "Positive (within)",
              Cognitive_w = "Cognitive (within)", Negative_b = "Negative (between)",
              Positive_b = "Positive (between)", Cognitive_b = "Cognitive (between)",
              Symptoms_w = "General symptoms (within)", Symptoms_b = "General symptoms (between)")
  nodes <- lavaanPlot::create_nodes(coefficients, labels = labels,
    node_options = list(fontname = "Helvetica", fontsize = 12, style = "filled",
                        fillcolor = "#F5F6F5", color = "#2A363B"))
  edges <- lavaanPlot::create_edges(coefficients, nodes,
    edge_options = list(fontname = "Helvetica", fontsize = 10, color = "#2A363B"),
    coef_labels = TRUE)

  colours <- c(Negative = "#83AF9B", Positive = "#FC9D9A", Cognitive = "#C8C8A9")
  for (i in seq_len(nrow(nodes))) {
    name <- nodes$orig_label[i]
    domain <- item_key$domain[match(name, item_key$item)]
    if (nodes$latent[i]) domain <- sub("_[wb]$", "", name)
    if (!is.na(domain) && domain %in% names(colours)) nodes$fillcolor[i] <- colours[[domain]]
    item <- match(name, item_key$item)
    detail <- if (!is.na(item)) item_key$questionText[item] else name
    # For these CFA measurement models, standardized residual variance = 1 - R-squared.
    residual_std <- coefficients$est.std[coefficients$op == "~~" &
                                          coefficients$lhs == name & coefficients$rhs == name]
    if (!nodes$latent[i] && length(residual_std) == 1)
      nodes$label[i] <- paste0(nodes$label[i], "\nR-squared = ", sprintf("%.2f", 1 - residual_std))
    variance <- raw_parameters$est[raw_parameters$op == "~~" &
                                   raw_parameters$lhs == name & raw_parameters$rhs == name]
    intercept <- raw_parameters$est[raw_parameters$op == "~1" & raw_parameters$lhs == name]
    if (length(variance)) detail <- paste0(detail, " | Raw variance: ", sprintf("%.3f", variance[1]))
    if (length(intercept)) detail <- paste0(detail, " | Intercept: ", sprintf("%.3f", intercept[1]))
    nodes$tooltip[i] <- detail
  }

  for (i in seq_len(nrow(coefficients))) {
    row <- coefficients[i, ]
    variance <- row$op == "~~" && row$lhs == row$rhs
    covariance <- row$op == "~~" && row$lhs != row$rhs
    label <- if (variance) "Variance (std)" else if (covariance) "Correlation" else "Loading (std)"
    edges$label[i] <- paste0(label, " = ", sprintf("%.2f", row$est.std))
    if (!variance && is.finite(row$ci.lower) && is.finite(row$ci.upper))
      edges$label[i] <- paste0(edges$label[i], "\n95% CI [", sprintf("%.2f", row$ci.lower),
                              ", ", sprintf("%.2f", row$ci.upper), "]")
    raw <- raw_parameters %>% filter(lhs == row$lhs, op == row$op, rhs == row$rhs)
    edges$tooltip[i] <- paste0(row$lhs, " ", row$op, " ", row$rhs,
      " | Standardized estimate: ", sprintf("%.3f", row$est.std),
      " | Standardized SE: ", sprintf("%.3f", row$se),
      " | p: ", if (is.finite(row$pvalue)) format.pval(row$pvalue, digits = 3) else "fixed/not available",
      if (nrow(raw)) paste0(" | Raw estimate: ", sprintf("%.3f", raw$est[1])) else "")
    edges$style[i] <- if (covariance) "dashed" else "solid"
  }

  dot <- lavaanPlot::convert_graph(nodes, edges, graph_options = list(
    rankdir = "LR", label = title, labelloc = "t", fontsize = 20,
    fontname = "Helvetica", nodesep = 0.55, ranksep = 2.0,
    splines = "spline", overlap = "false", bgcolor = "white"
  ))
  # Fit is unchanged; the custom DOT preserves the extracted level-specific estimates.
  widget <- lavaanPlot::lavaanPlot2(model = fit, gr_viz = dot)
  attr(widget, "diagram_parameters") <- coefficients %>% select(-any_of("stars"))
  widget
}

diagrams <- list()
if (requireNamespace("lavaanPlot", quietly = TRUE) &&
    requireNamespace("htmlwidgets", quietly = TRUE)) {
  for (i in seq_along(fits)) {
    if (!model_diagnostics$Converged[i] || !model_diagnostics$Admissible[i]) next
    level <- if (model_grid$Level[i] == "Within") 1 else 2
    name <- tolower(gsub("[ :]+", "_", names(fits)[i]))
    diagrams[[name]] <- make_lavaan_diagram(fits[[i]], level,
      paste(model_grid$Model[i], "CFA:", model_grid$Level[i], "(other level saturated)"))
    print(diagrams[[name]])
  }

  #### Save diagrams and their parameter tables ####
  # Without Pandoc keep each _files folder beside its HTML file.
  standalone_html <- requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()
  for (name in names(diagrams)) {
    diagram <- diagrams[[name]]
    if (is.null(diagram)) next
    htmlwidgets::saveWidget(diagram, file.path(output_path, paste0(name, ".html")),
                            selfcontained = standalone_html)
    write.csv(attr(diagram, "diagram_parameters"),
              file.path(output_path, paste0(name, "_parameters.csv")), row.names = FALSE)
    if (requireNamespace("DiagrammeRsvg", quietly = TRUE))
      writeLines(DiagrammeRsvg::export_svg(diagram), file.path(output_path, paste0(name, ".svg")))
  }
  if (!standalone_html) message("Diagram HTML files require the accompanying _files folders (Pandoc unavailable).")
} else {
  message("To add path diagrams, install.packages(c('lavaanPlot', 'htmlwidgets')) and rerun this section.")
}
