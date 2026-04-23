

#####################################################################################################################################################
# Capturing Schizophrenia Symptoms in Daily Life via the Smartphone: Psychometric Evaluation of Newly Proposed Positive and Negative Symptom Scales #
#####################################################################################################################################################



library(dplyr)
library(readr)
library(purrr)
library(ggplot2)
library(DBI)
library(RPostgres)
library(tidyr)
library(reshape2)
library(patchwork)
library(hrbrthemes)
library(lubridate)
library(misty)



###############################################
################ Descriptive #################
###############################################

combined_df_all = read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/esm_cleaned.csv")
combined_df_all$response <- as.numeric(combined_df_all$response)

source("HelperFunctions.R")

######## Correlation ##########
melt_corr <- function(corr_matrix) melt(corr_matrix, na.rm = TRUE)

plot_heatmap <- function(df, title) {
  ggplot(df, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "#d95f02",
      mid = "white",
      high = "#1b9e77",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    geom_text(aes(label = round(value, 2)), size = 3) +
    theme_ipsum() +
    labs(title = title, x = "", y = "") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(face = "bold", size = 12),
      legend.position = "none"
    )
}

plots_by_category <- list()

categories <- unique(combined_df_all$ema_category)

for (cat in categories) {
  
  df_cat <- combined_df_all %>%
    filter(ema_category == cat)
  
  # Pivot to wide: items become columns
  wide_df <- df_cat %>%
    select(participant_id, questionText, response) %>%
    group_by(participant_id, questionText) %>%
    summarise(response = mean(response, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = questionText, values_from = response)
  
  # Remove ID column
  mat <- wide_df %>% select(-participant_id)
  
  # Skip if too few items
  if (ncol(mat) < 2) next
  
  # Correlation
  corr_matrix <- cor(mat, use = "pairwise.complete.obs")
  
  melt_df <- melt(corr_matrix)
  
  plots_by_category[[cat]] <- plot_heatmap(melt_df, cat)
}

wrap_plots(plots_by_category, ncol = 4)

plots_by_category$`Negative symptom`
plots_by_category$`Cognitive symptom`
plots_by_category$`Positive symptom`




##### Distribution
plot_distribution <- function(df, title) {
  
  ggplot(df, aes(x = response)) +
    geom_histogram(bins = 30, fill = "#1b9e77", alpha = 0.8) +
    theme_ipsum() +
    labs(title = title, x = "Response", y = "Count") +
    theme(
      plot.title = element_text(face = "bold", size = 12)
    )
}

dist_plots <- list()

for (cat in unique(combined_df_all$ema_category)) {
  
  df_cat <- combined_df_all %>%
    filter(ema_category == cat)
  
  dist_plots[[cat]] <- plot_distribution(df_cat, cat)
}

dist_plots$`Negative symptom`
dist_plots$`Positive symptom`
dist_plots$`Cognitive symptom`


get_summary_table <- function(df) {
  
  df %>%
    group_by(ema_category) %>%
    summarise(
      n = n(),
      n_participants = n_distinct(participant_id),
      mean = mean(response, na.rm = TRUE),
      sd = sd(response, na.rm = TRUE),
      min = min(response, na.rm = TRUE),
      max = max(response, na.rm = TRUE),
      missing_pct = mean(is.na(response)) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(n))
}


combined_df_all$response <- as.numeric(combined_df_all$response)

get_summary_table(combined_df_all)

###############################################
################ Reliability ##################
###############################################

library(lme4)
library(psych)
library(misty)
library(lavaan)
library(tidyr)
library(gt)
library(dplyr)

source("HelperFunctions.R")


combined_df_all = read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/esm_cleaned.csv")
combined_df_all$response <- as.numeric(combined_df_all$response)


# ############# Alpha ################
# # Filter negative items
# neg_df <- combined_df_all %>%
#   filter(ema_category == "Negative symptom") %>%
#   mutate(response = as.numeric(response))
# 
# # Create item-level wide data (items = columns)
# neg_wide <- neg_df %>%
#   group_by(participant_id, surveyId, questionText) %>%
#   summarise(response = mean(response, na.rm = TRUE), .groups = "drop") %>%
#   pivot_wider(
#     names_from = questionText,
#     values_from = response
#   )
# 
# # Compute alpha (drop IDs)
# neg_alpha <- psych::alpha(
#   neg_wide %>% select(-participant_id, -surveyId),
#   na.rm = TRUE
# )
# 
# neg_alpha
# 



###### Negative Symptoms ######
#mlr_negative <- run_mlr_reliability(combined_df_all, "Negative symptom", "time_index", "participant_id") # (just to double check)

mlr_neg <- GeneralizabilityTheory(
  combined_df_all, "Negative symptom",
  time_var = "time_index",
  uid_var = "participant_id",
  optimizer = "Nelder_Mead"
)


nez_neg <- GeneralizabilityTheory_Nezlek(
  combined_df_all, "Negative symptom",
  time_var = "time_index",
  uid_var = "participant_id",
  optimizer = "nloptwrap"
)

omega_negative <- run_multilevel_omega(combined_df_all, "Negative symptom", "time_index", "participant_id")


###### Positive Symptoms ######

#mlr_positive <- run_mlr_reliability(combined_df_all, "Positive symptom", "time_index", "participant_id")

mlr_pos <- GeneralizabilityTheory(
  combined_df_all, "Positive symptom",
  time_var = "time_index",
  uid_var = "participant_id",
  optimizer = "Nelder_Mead"
)


nez_pos <- GeneralizabilityTheory_Nezlek(
  combined_df_all, "Positive symptom",
  time_var = "time_index",
  uid_var = "participant_id",
  optimizer = "nloptwrap"
)

omega_positive <- run_multilevel_omega(combined_df_all, "Positive symptom", "time_index", "participant_id")


####### Cognitive Symptoms  ######
#mlr_cognitive <- run_mlr_reliability(combined_df_all, "Cognitive symptom", "time_index", "participant_id")

mlr_cogn <- GeneralizabilityTheory(
  combined_df_all, "Cognitive symptom",
  time_var = "time_index",
  uid_var = "participant_id",
  optimizer = "Nelder_Mead"
)


nez_cogn <- GeneralizabilityTheory_Nezlek(
  combined_df_all, "Cognitive symptom",
  time_var = "time_index",
  uid_var = "participant_id",
  optimizer = "nloptwrap"
)


omega_cognitive <- run_multilevel_omega(combined_df_all, "Cognitive symptom", "time_index", "participant_id")


########## Summary of Results ###########
extract_construct <- function(name, mlr, gt, nez, omega_data) {
  
  data.frame(
    Construct = name,
    
    # --- Generalizability Theory (choose main coefficient: Rkr = most general)
    GT_Between = gt$Rkr,
    GT_Within = gt$Rc,
    
    # --- Nezlek (Between / Within style split)
    Nez_Between = nez$rel[1],
    Nez_Within  = nez$rel[2],
    
    # --- Omega (between / within proxies if available)
    Omega_Between = omega_data$result$omega$omega[omega_data$result$omega$type == "omega.b"],
    Omega_Within  = omega_data$result$omega$omega[omega_data$result$omega$type == "omega.w"]
  )
}

reliability_summary <- rbind(
  
  extract_construct("Negative",
                    mlr_negative, mlr_neg, nez_neg, omega_negative),
  
  extract_construct("Positive",
                    mlr_positive, mlr_pos, nez_pos, omega_positive),
  
  extract_construct("Cognitive",
                    mlr_cognitive, mlr_cogn, nez_cogn, omega_cognitive)
)

reliability_summary_long <- data.frame(
  
  Construct = rep(reliability_summary$Construct, each = 2),
  Level = rep(c("Between", "Within"), times = nrow(reliability_summary)),
  
  GT = c(
    as.vector(t(cbind(
      reliability_summary$GT_Between,
      reliability_summary$GT_Within
    )))
  ),
  
  Nezlek = c(
    as.vector(t(cbind(
      reliability_summary$Nez_Between,
      reliability_summary$Nez_Within
    )))
  ),
  
  Omega = c(
    as.vector(t(cbind(
      reliability_summary$Omega_Between,
      reliability_summary$Omega_Within
    )))
  )
)

reliability_summary_long[ , 3:5] <-
  round(reliability_summary_long[ , 3:5], 2)

rel_cols <- scales::col_numeric(
  palette = c("#FE4365", "#FC9D9A", "#F9CDAD",
              "#C8C8A9", "#83AF9B", "#2A363B"),
  domain = c(0, 1)
)
### Viz of Results ##
reliability_summary_long %>%
  gt(rowname_col = NULL) %>%
  tab_options(
    table.width = pct(80),   # full width
    table.font.size = px(20)
  ) %>%
  
  # Title
  tab_header(
    title = "Internal Consistency",
    subtitle = "Between- and within-person reliability estimates"
  ) %>%
  
  # Format numbers
  fmt_number(
    columns = c(GT, Nezlek, Omega),
    decimals = 2
  ) %>%
  
  # Color scale for GT
  data_color(
    columns = GT,
    colors = scales::col_numeric(
      palette = rel_cols,
      domain = c(0, 1)
    )
  ) %>%
  
  # Color scale for Nezlek
  data_color(
    columns = Nezlek,
    colors = scales::col_numeric(
      palette =rel_cols,
      domain = c(0, 1)
    )
  ) %>%
  
  # Color scale for Omega
  data_color(
    columns = Omega,
    colors = scales::col_numeric(
      palette = rel_cols,
      domain = c(0, 1)
    )
  ) %>%
  
  # Style tweaks
  tab_options(
    table.font.size = "big",
    data_row.padding = px(4)
  )
