#####################################################################################################################################################
# Capturing Schizophrenia Symptoms in Daily Life via the Smartphone: Psychometric Evaluation of Newly Proposed Symptom Scales
#####################################################################################################################################################

#####################################################################
# Multilevel CFA:
# Negative, positive, and cognitive symptom structure
#####################################################################

library(lavaan)
library(dplyr)
library(tidyr)

#####################################################################
# 1. Load data
#####################################################################

main_path <- paste0(
  "/Users/anna/Library/CloudStorage/",
  "GoogleDrive-langener95@gmail.com/My Drive/",
  "Research/9_PsychometricSensing"
)

combined_df_all <- read.csv(
  file.path(main_path, "esm_cleaned.csv"),
  colClasses = c(participant_id = "character")
)

required_columns <- c(
  "participant_id",
  "time_index",
  "ema_category",
  "questionText",
  "response"
)


#####################################################################
# 2. Define the 15 items
#####################################################################

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
  item = c(
    paste0("n", 1:5),
    paste0("p", 1:5),
    paste0("c", 1:5)
  ),
  domain = rep(
    c("Negative", "Positive", "Cognitive"),
    each = 5
  ),
  label = c(
    "Elsewhere",
    "Unmotivated",
    "Flat",
    "Expression",
    "Social",
    "Unusual experiences",
    "Detached",
    "Powers",
    "Messages",
    "Suspicious",
    "Concentrate (reversed)",
    "Racing",
    "Decisions (reversed)",
    "Clarity",
    "Memory"
  )
)

#####################################################################
# 3. Prepare item-level data
#####################################################################

# Cognitive items are assumed to have already been reverse-coded in
# esm_cleaned.csv. They are therefore not reverse-coded again here.

ema_symptoms <- combined_df_all %>%
  filter(
    ema_category %in% c(
      "Negative symptom",
      "Positive symptom",
      "Cognitive symptom"
    )
  ) %>%
  select(
    participant_id,
    time_index,
    ema_category,
    questionText,
    response
  ) %>%
  mutate(
    participant_id = as.character(participant_id),
    questionText = trimws(questionText),
    response = as.numeric(response)
  ) %>%
  left_join(
    item_key,
    by = "questionText"
  )

#####################################################################
# 4. Check the item matching and data structure
#####################################################################

if (anyNA(ema_symptoms$item)) {
  
  unmatched_questions <- unique(
    ema_symptoms$questionText[is.na(ema_symptoms$item)]
  )
  
  print(unmatched_questions)
  
  stop(
    "Some symptom questions did not match item_key. ",
    "Review the printed questions and update item_key."
  )
}

if (anyNA(ema_symptoms$participant_id) ||
    any(ema_symptoms$participant_id == "")) {
  stop("Missing or empty participant IDs were detected.")
}

if (anyNA(ema_symptoms$time_index)) {
  stop("Missing time_index values were detected.")
}

if (any(
  ema_symptoms$response < 0 |
  ema_symptoms$response > 100,
  na.rm = TRUE
)) {
  stop("Responses outside the expected range of 0 to 100 were detected.")
}

duplicate_rows <- ema_symptoms %>%
  count(
    participant_id,
    time_index,
    item,
    name = "n"
  ) %>%
  filter(n > 1)

if (nrow(duplicate_rows) > 0) {
  print(duplicate_rows)
  
  stop(
    "Duplicate responses were found for the same participant, ",
    "occasion, and item. Resolve these duplicates before fitting ",
    "the CFA models."
  )
}

#####################################################################
# 5. Convert to wide format
#####################################################################

ema_wide <- ema_symptoms %>%
  select(
    participant_id,
    time_index,
    item,
    response
  ) %>%
  pivot_wider(
    names_from = item,
    values_from = response
  ) %>%
  arrange(
    participant_id,
    time_index
  )

missing_items <- setdiff(item_key$item, names(ema_wide))

if (length(missing_items) > 0) {
  stop(
    "The following items are missing from ema_wide: ",
    paste(missing_items, collapse = ", ")
  )
}

# Retain occasions with at least one completed symptom item.

ema_wide <- ema_wide %>%
  filter(
    if_any(
      all_of(item_key$item),
      ~ !is.na(.x)
    )
  )

# Multilevel CFA requires repeated observations per participant.

ema_wide <- ema_wide %>%
  group_by(participant_id) %>%
  filter(n() >= 2) %>%
  ungroup()

#####################################################################
# 6. Describe the analysis sample
#####################################################################

occasions_per_person <- ema_wide %>%
  count(
    participant_id,
    name = "occasions"
  )

sample_summary <- data.frame(
  Participants = n_distinct(ema_wide$participant_id),
  Occasions = nrow(ema_wide),
  Minimum_occasions = min(occasions_per_person$occasions),
  Maximum_occasions = max(occasions_per_person$occasions),
  Median_occasions = median(occasions_per_person$occasions),
  Complete_occasions = sum(
    complete.cases(ema_wide[item_key$item])
  )
)

print(sample_summary)

item_missingness <- data.frame(
  Item = item_key$item,
  Missing_percent = 100 * colMeans(
    is.na(ema_wide[item_key$item])
  )
) %>%
  left_join(
    item_key %>% select(item, domain, label),
    by = c("Item" = "item")
  )

print(item_missingness)

#####################################################################
# 7. Three-factor multilevel CFA
#####################################################################

# The same three-factor structure is specified at:
# Level 1: occasions within participants
# Level 2: differences between participants

model_three_factor <- '
  level: 1

    Negative_w =~ n1 + n2 + n3 + n4 + n5
    Positive_w =~ p1 + p2 + p3 + p4 + p5
    Cognitive_w =~ c1 + c2 + c3 + c4 + c5

    Negative_w ~~ Positive_w
    Negative_w ~~ Cognitive_w
    Positive_w ~~ Cognitive_w

  level: 2

    Negative_b =~ n1 + n2 + n3 + n4 + n5
    Positive_b =~ p1 + p2 + p3 + p4 + p5
    Cognitive_b =~ c1 + c2 + c3 + c4 + c5

    Negative_b ~~ Positive_b
    Negative_b ~~ Cognitive_b
    Positive_b ~~ Cognitive_b
'

fit_three_factor <- cfa(
  model = model_three_factor,
  data = ema_wide,
  cluster = "participant_id",
  estimator = "MLR",
  missing = "fiml",
  std.lv = TRUE
)

library(semPlot)

draw_cfa_path_diagram <- function() {
  
  semPaths(
    object = fit_three_factor,
    
    # Display standardized estimates
    what = "std",
    whatLabels = "std",
    
    # Diagram appearance
    style = "lisrel",
    layout = "tree2",
    rotation = 2,
    
    # Separate the within- and between-person parts
    panelGroups = TRUE,
    combineGroups = FALSE,
    
    # Remove less important elements to reduce visual complexity
    residuals = FALSE,
    intercepts = FALSE,
    thresholds = FALSE,
    exoVar = FALSE,
    
    # Retain correlations between symptom factors
    exoCov = TRUE,
    curveAdjacent = TRUE,
    
    # Labels and sizes
    nCharNodes = 0,
    nDigits = 2,
    edge.label.cex = 0.65,
    sizeMan = 5,
    sizeLat = 8,
    asize = 2,
    
    # General appearance
    edge.color = "#2A363B",
    pastel = TRUE,
    fade = FALSE,
    mar = c(6, 6, 6, 6),
    
    # Do not open interactive menus
    ask = FALSE
  )
}

# 
# draw_cfa_path_diagram()
# 
# png(
#   filename = file.path(
#     "three_factor_multilevel_cfa_path_diagram.png"
#   ),
#   width = 4800,
#   height = 3000,
#   res = 300
# )
# 
# draw_cfa_path_diagram()
# 
# dev.off()

#####################################################################
# 8. One-factor comparison model
#####################################################################

model_one_factor <- '
  level: 1

    Symptoms_w =~
      n1 + n2 + n3 + n4 + n5 +
      p1 + p2 + p3 + p4 + p5 +
      c1 + c2 + c3 + c4 + c5

  level: 2

    Symptoms_b =~
      n1 + n2 + n3 + n4 + n5 +
      p1 + p2 + p3 + p4 + p5 +
      c1 + c2 + c3 + c4 + c5
'

fit_one_factor <- cfa(
  model = model_one_factor,
  data = ema_wide,
  cluster = "participant_id",
  estimator = "MLR",
  missing = "fiml",
  std.lv = TRUE
)

#####################################################################
# 9. Check convergence and admissibility
#####################################################################

extract_diagnostics <- function(fit, model_name) {
  
  parameter_table <- parameterEstimates(fit)
  
  negative_observed_variances <- parameter_table %>%
    filter(
      op == "~~",
      lhs == rhs,
      lhs %in% item_key$item,
      est < 0
    ) %>%
    nrow()
  
  data.frame(
    Model = model_name,
    Converged = lavInspect(fit, "converged"),
    Admissible = lavInspect(fit, "post.check"),
    Negative_observed_variances = negative_observed_variances
  )
}

model_diagnostics <- bind_rows(
  extract_diagnostics(
    fit_three_factor,
    "Three-factor model"
  ),
  extract_diagnostics(
    fit_one_factor,
    "One-factor model"
  )
)

print(model_diagnostics)

#####################################################################
# 10. Extract model-fit indices
#####################################################################

get_fit_value <- function(fit_values, name) {
  
  if (name %in% names(fit_values)) {
    return(unname(fit_values[name]))
  }
  
  NA_real_
}

extract_model_fit <- function(fit, model_name) {
  
  fit_values <- fitMeasures(fit)
  
  data.frame(
    Model = model_name,
    
    Chi_square_scaled = get_fit_value(
      fit_values,
      "chisq.scaled"
    ),
    
    df_scaled = get_fit_value(
      fit_values,
      "df.scaled"
    ),
    
    p_scaled = get_fit_value(
      fit_values,
      "pvalue.scaled"
    ),
    
    CFI = get_fit_value(
      fit_values,
      "cfi"
    ),
    
    CFI_robust = get_fit_value(
      fit_values,
      "cfi.robust"
    ),
    
    TLI = get_fit_value(
      fit_values,
      "tli"
    ),
    
    TLI_robust = get_fit_value(
      fit_values,
      "tli.robust"
    ),
    
    RMSEA = get_fit_value(
      fit_values,
      "rmsea"
    ),
    
    RMSEA_robust = get_fit_value(
      fit_values,
      "rmsea.robust"
    ),
    
    SRMR_within = get_fit_value(
      fit_values,
      "srmr_within"
    ),
    
    SRMR_between = get_fit_value(
      fit_values,
      "srmr_between"
    ),
    
    AIC = get_fit_value(
      fit_values,
      "aic"
    ),
    
    BIC = get_fit_value(
      fit_values,
      "bic"
    )
  )
}

fit_comparison <- bind_rows(
  extract_model_fit(
    fit_three_factor,
    "Three-factor model"
  ),
  extract_model_fit(
    fit_one_factor,
    "One-factor model"
  )
)

print(fit_comparison)

# The models are compared descriptively using their fit indices and
# information criteria. A routine likelihood-ratio test is not used because
# representing the one-factor model as a three-factor model would require
# fixing latent correlations to the boundary value of 1.

#####################################################################
# 11. Extract standardized factor loadings
#####################################################################

three_factor_parameters <- parameterEstimates(
  fit_three_factor,
  standardized = TRUE,
  ci = TRUE
)

factor_loadings <- three_factor_parameters %>%
  filter(op == "=~") %>%
  transmute(
    Level = ifelse(
      level == 1,
      "Within-person",
      "Between-person"
    ),
    Factor = lhs,
    Item = rhs,
    Loading_unstandardized = est,
    SE = se,
    p = pvalue,
    Loading_standardized = std.all
  ) %>%
  left_join(
    item_key %>%
      select(
        item,
        domain,
        label,
        questionText
      ),
    by = c("Item" = "item")
  ) %>%
  arrange(
    Level,
    domain,
    Item
  )

print(factor_loadings)

#####################################################################
# 12. Extract correlations among the three factors
#####################################################################

factor_names <- c(
  "Negative_w",
  "Positive_w",
  "Cognitive_w",
  "Negative_b",
  "Positive_b",
  "Cognitive_b"
)

factor_correlations <- three_factor_parameters %>%
  filter(
    op == "~~",
    lhs != rhs,
    lhs %in% factor_names,
    rhs %in% factor_names
  ) %>%
  transmute(
    Level = ifelse(
      level == 1,
      "Within-person",
      "Between-person"
    ),
    Factor_1 = lhs,
    Factor_2 = rhs,
    Correlation = std.all,
    SE = se,
    CI_lower = ci.lower,
    CI_upper = ci.upper,
    p = pvalue
  )

print(factor_correlations)

if (
  nrow(factor_correlations) > 0 &&
  any(abs(factor_correlations$Correlation) >= .95, na.rm = TRUE)
) {
  warning(
    "At least one latent factor correlation is >= |.95|. ",
    "This may indicate that two symptom factors are not clearly ",
    "distinguishable at that level."
  )
}

#####################################################################
# 13. Extract observed-item residual variances
#####################################################################

residual_variances <- three_factor_parameters %>%
  filter(
    op == "~~",
    lhs == rhs,
    lhs %in% item_key$item
  ) %>%
  transmute(
    Level = ifelse(
      level == 1,
      "Within-person",
      "Between-person"
    ),
    Item = lhs,
    Residual_variance = est,
    Residual_variance_standardized = std.all,
    SE = se,
    p = pvalue
  ) %>%
  left_join(
    item_key %>%
      select(
        item,
        domain,
        label
      ),
    by = c("Item" = "item")
  ) %>%
  arrange(
    Level,
    domain,
    Item
  )

print(residual_variances)

#####################################################################
# 14. Print complete model summaries
#####################################################################

summary(
  fit_three_factor,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)

summary(
  fit_one_factor,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)


######### Report Results ######
model_diagnostics # converged
fit_comparison
factor_loadings
factor_correlations

# ============================================================
# Save multilevel CFA factor loadings as a supplementary table
# ============================================================

# Load packages
library(dplyr)
library(flextable)
library(officer)

# ============================================================
# Prepare table
# ============================================================

factor_loadings_supplement <- factor_loadings %>%
  mutate(
    Level = factor(
      Level,
      levels = c("Within-person", "Between-person")
    ),
    Domain = factor(
      domain,
      levels = c("Negative", "Positive", "Cognitive")
    ),
    Item = toupper(Item),
    `Unstandardized loading` = sprintf("%.3f", Loading_unstandardized),
    SE = sprintf("%.3f", SE),
    `p value` = case_when(
      p < .001 ~ "< .001",
      TRUE ~ sub("^0", "", sprintf("%.3f", p))
    ),
    `Standardized loading` = sprintf("%.3f", Loading_standardized)
  ) %>%
  arrange(Level, Domain, Item) %>%
  select(
    Level,
    Domain,
    Item,
    `Unstandardized loading`,
    SE,
    `p value`,
    `Standardized loading`
  )

# View the final table
print(factor_loadings_supplement)

# ============================================================
# Save as CSV
# ============================================================

write.csv(
  factor_loadings_supplement,
  file = "Supplementary_Table_MCFA_Factor_Loadings.csv",
  row.names = FALSE,
  na = ""
)

# ============================================================
# Create formatted Word table
# ============================================================

factor_loadings_ft <- flextable(factor_loadings_supplement) %>%
  set_header_labels(
    Level = "Level",
    Domain = "Symptom domain",
    Item = "Item",
    `Unstandardized loading` = "B",
    SE = "SE",
    `p value` = "p",
    `Standardized loading` = "Standardized loading"
  ) %>%
  merge_v(j = c("Level", "Domain")) %>%
  valign(j = c("Level", "Domain"), valign = "top") %>%
  align(
    j = c(
      "Unstandardized loading",
      "SE",
      "p value",
      "Standardized loading"
    ),
    align = "center",
    part = "all"
  ) %>%
  bold(part = "header") %>%
  autofit() %>%
  theme_booktabs() %>%
  add_footer_lines(
    values = paste0(
      "Note. B = unstandardized factor loading; SE = standard error. ",
      "SEs and p values refer to the unstandardized loadings. ",
    )
  )

# ============================================================
# Save as Word document
# ============================================================

supplement_doc <- read_docx() %>%
  body_add_par(
    "Supplementary Table S1",
    style = "heading 2"
  ) %>%
  body_add_par(
    paste0(
      "Factor Loadings From the Three-Factor Multilevel ",
      "Confirmatory Factor Analysis"
    ),
    style = "Normal"
  ) %>%
  body_add_flextable(factor_loadings_ft)

print(
  supplement_doc,
  target = "Supplementary_Table_MCFA_Factor_Loadings.docx"
)

