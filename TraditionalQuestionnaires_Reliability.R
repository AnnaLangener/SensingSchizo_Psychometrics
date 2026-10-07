###############################################
###### Traditional Questionnaire Reliability ##
###############################################

library(lme4)
library(psych)
library(misty)
library(tidyr)
library(dplyr)
library(gt)

source("HelperFunctions.R")

####### Load data #######

main_path <- "/Users/anna/Library/CloudStorage/GoogleDrive-langener95@gmail.com/My Drive/Research/9_PsychometricSensing"

ema <- read.csv(file.path(main_path, "esm_cleaned.csv"),
                colClasses = c(participant_id = "character"))
weekly <- read.csv(file.path(main_path, "weekly_redcap.csv"))
post <- read.csv(file.path(main_path, "post_assessment.csv"))

weekly$participant_id <- sprintf("%05d", weekly$record_id)

combined_df_all <- read.csv(file.path(main_path, "esm_cleaned.csv"), colClasses = c(participant_id = "character"))
weekly <- weekly[weekly$participant_id %in% combined_df_all$participant_id,]

weekly <- weekly[weekly$w_timestamp != "[not completed]",]
post$participant_id <- sprintf("%05d", post$record_id)

weekly <- weekly[weekly$participant_id %in% ema$participant_id, ]
post <- post[post$participant_id %in% ema$participant_id, ]

weekly <- weekly %>%
  distinct(participant_id, w_timestamp, .keep_all = TRUE)

####### Weekly time index #######

weekly <- weekly %>%
  mutate(
    w_timestamp = as.Date(w_timestamp)
  ) %>%
  group_by(participant_id) %>%
  mutate(
    week = floor(as.numeric(w_timestamp - min(w_timestamp, na.rm = TRUE)) / 7) + 1L
  ) %>%
  ungroup()

weekly <- weekly %>%
  filter(week <= 12)

####### Weekly questionnaire scoring #######
# Weekly
# PS-R positive symptom 
# NSI-PR negative symptom 

ps_items <- paste0("ps_r_", 1:12, "_w") # PS-R positive symptom 
nsi_weekly_items <- paste0("nsi_w_", 1:11) # NSI-PR negative symptom 

# ####### Prepare weekly data for the existing helpers #######

# Both GT and omega use the same complete assessments per scale, retaining
# participants with at least two assessments. Missing items are not set to zero.
prepare_weekly <- function(data, items, construct) {
  stopifnot(is.data.frame(data))
  data %>%
    select(participant_id, week, all_of(items)) %>%
    drop_na() %>%
    group_by(participant_id) %>%
    filter(n() >= 2) %>%
    ungroup() %>%
    pivot_longer(all_of(items), names_to = "questionText", values_to = "response") %>%
    mutate(ema_category = construct)
}

weekly_ps <- prepare_weekly(weekly, ps_items, "PS-R positive")
weekly_nsi_nodomains <- prepare_weekly(weekly, nsi_weekly_items, "NSI-PR no domains")

###### Weekly Positive Symptoms (PS-R) ######

gt_ps <- GeneralizabilityTheory(weekly_ps, "PS-R positive", "week", "participant_id", optimizer = "Nelder_Mead")
nezlek_ps <- GeneralizabilityTheory_Nezlek(weekly_ps, "PS-R positive", "week", "participant_id", optimizer = "Nelder_Mead")
omega_ps <- run_multilevel_omega(weekly_ps, "PS-R positive", "week", "participant_id")

# ###### Weekly Negative Symptoms (NSI-PR total) ######

gt_nsi_nodomains <- GeneralizabilityTheory(weekly_nsi_nodomains, "NSI-PR no domains", "week", "participant_id")
nezlek_nsi_nodomains <- GeneralizabilityTheory_Nezlek(weekly_nsi_nodomains, "NSI-PR no domains", "week", "participant_id", optimizer = "Nelder_Mead")
omega_nsi_nodomains <- run_multilevel_omega(weekly_nsi_nodomains, "NSI-PR no domains", "week", "participant_id")

########## Summary of Weekly Results ###########

# GT Rkr: reliability of a participant's average across the observed weeks.
# GT Rc: reliability of within-person change. Rkr uses the number of distinct
# study weeks; review this D-study assumption if participants have unequal coverage.
# Inspect GT warnings/conv/singular and omega model warnings before reporting.
extract_weekly <- function(name, data, gt, nezlek, omega_data) {
  assessments <- distinct(data, participant_id, week)
  weeks_per_person <- table(assessments$participant_id)
  data.frame(
    Scale = name,
    N = n_distinct(data$participant_id),
    Assessments = nrow(assessments),
    Min_weeks = min(weeks_per_person),
    Max_weeks = max(weeks_per_person),
    GT_Dstudy_weeks = n_distinct(data$week),
    GT_Between = gt$Rkr,
    GT_Within = gt$Rc,
    Nez_Between = nezlek$rel[1],
    Nez_Within = nezlek$rel[2],
    Omega_Between = omega_data$result$omega$omega[omega_data$result$omega$type == "omega.b"],
    Omega_Within = omega_data$result$omega$omega[omega_data$result$omega$type == "omega.w"]
  )
}

weekly_summary <- bind_rows(
  extract_weekly("NSI-PR negative (no domains)", weekly_nsi_nodomains,
                 gt_nsi_nodomains, nezlek_nsi_nodomains, omega_nsi_nodomains),
  extract_weekly("PS-R positive", weekly_ps, gt_ps, nezlek_ps, omega_ps)
)

weekly_summary_long <- data.frame(
  Construct = rep(weekly_summary$Scale, each = 2),
  Level = rep(c("Between", "Within"), times = nrow(weekly_summary)),
  GT = as.vector(t(weekly_summary[c("GT_Between", "GT_Within")])),
  Nezlek = as.vector(t(weekly_summary[c("Nez_Between", "Nez_Within")])),
  Omega = as.vector(t(weekly_summary[c("Omega_Between", "Omega_Within")]))
)

rel_cols <- scales::col_numeric(
  palette = c("#FE4365", "#FC9D9A", "#F9CDAD", "#C8C8A9", "#83AF9B", "#2A363B"),
  domain = c(0, 1)
)

weekly_table <- weekly_summary_long %>%
  gt(rowname_col = NULL) %>%
  tab_options(table.width = pct(80), table.font.size = "big",
              data_row.padding = px(4)) %>%
  tab_header(
    title = "Internal Consistency",
    subtitle = "Between- and within-person reliability estimates"
  ) %>%
  fmt_number(columns = c(GT, Nezlek, Omega), decimals = 2) %>%
  data_color(columns = c(GT, Nezlek, Omega), fn = rel_cols)

weekly_table

####### Post-assessment questionnaire scoring #######


# Post
# PNS-Q positive symptom 
# PNS-Q negative symptoms 
# NSI-PR negative 

nsi_post_items <- paste0("nsi_", 1:11, "_p") # NSI-PR negative symptom 


post <- post %>%
  mutate(
    across(nsi_1_p:nsi_6_p, ~ 7 - .),
    across(nsi_7_p:nsi_11_p, ~ 10 - .)
  )

pns_key_68 <- c(
  "P","N","N","P", "N","N","P","N", "P","P","P","P",
  "N","P","N","P", "N","P","N","P", "N","N","P","N",
  "P","P","P","P", "P","P","N","P", "N","N","N","N",
  "N","N","P","N", "P","P","N","P", "P","N","P","N",
  "P","N","N","N", "N","N","N","P", "N","N","N","N",
  "P","N","P","P", "P","N","P","P"
)
pns_items <- paste0("pns_q_", 1:68, "_p")
P_items <- pns_items[pns_key_68 == "P"]
N_items <- pns_items[pns_key_68 == "N"]

# 1 = yes, 2 = no; preserve missing values.
stopifnot(all(unlist(post[pns_items]) %in% c(1, 2, NA)))
post[pns_items] <- lapply(post[pns_items], function(x) ifelse(x == 1, 1, 0))

###### Post NSI-PR ######

# No domains: alpha across the 11 individual items.
post_nsi <- post %>% select(all_of(nsi_post_items)) %>% drop_na()
alpha_nsi <- psych::alpha(post_nsi, check.keys = FALSE, delete = FALSE)


###### Post PNS Positive and Negative Symptoms ######

# Raw alpha for binary items is equivalent to KR-20.
post_pns_positive <- post %>% select(all_of(P_items)) %>% drop_na()
post_pns_negative <- post %>% select(all_of(N_items)) %>% drop_na()

alpha_pns_positive <- psych::alpha(post_pns_positive, check.keys = FALSE, delete = FALSE)
alpha_pns_negative <- psych::alpha(post_pns_negative, check.keys = FALSE, delete = FALSE)

########## Summary of Post Results ###########

extract_post <- function(name, data, alpha_data) {
  data.frame(Scale = name, N = nrow(data), Items = ncol(data),
             Alpha = alpha_data$total$raw_alpha,
             Raw_alpha = alpha_data$total$raw_alpha)
}

post_summary <- bind_rows(
  extract_post("NSI-PR negative (no domains)", post_nsi, alpha_nsi),
  extract_post("PNS positive", post_pns_positive, alpha_pns_positive),
  extract_post("PNS negative", post_pns_negative, alpha_pns_negative)
)

post_table <- post_summary %>%
  select(Construct = Scale, N, Items, Alpha) %>%
  gt(rowname_col = NULL) %>%
  tab_options(table.width = pct(80), table.font.size = "big",
              data_row.padding = px(4)) %>%
  tab_header(
    title = "Internal Consistency",
    subtitle = "Post-assessment questionnaire reliability estimates"
  ) %>%
  cols_label(Alpha = "Cronbach's alpha", Items = "Items / domains") %>%
  fmt_number(columns = Alpha, decimals = 2) %>%
  data_color(columns = Alpha, fn = rel_cols) %>%
  tab_source_note(source_note = paste(
    "Raw Cronbach's alpha is based on indicator variances and covariances.",
    "NSI-PR domains uses three domain means; no domains uses all 11 items.",
    "Both NSI-PR total versions use the same complete participants."
  ))

post_table

