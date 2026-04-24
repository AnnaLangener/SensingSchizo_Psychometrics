
#####################################################################################################################################################
# Capturing Schizophrenia Symptoms in Daily Life via the Smartphone: Psychometric Evaluation of Newly Proposed Positive and Negative Symptom Scales #
#####################################################################################################################################################


## Old way, to do it from VM
# psql -U hannah -h localhost -d moodtriggers -c "\COPY ema_responses_final3 TO 'table_new.csv' CSV HEADER"
# psql -U hannah -h localhost -d moodtriggers -c "\COPY overall_status_cache TO 'table_overall_new.csv' CSV HEADER"

# combined_df <- read.csv("/Users/f007qrc/Downloads/table.csv") # downloaded from VM
# cache <- read.csv("/Users/f007qrc/Downloads/table_overall.csv") # downloaded from VM

# NEW WAY

# 1) ssh-keygen -t ed25519 -f ~/.ssh/id_vm_access (create key locally)
# 2) cat ~/.ssh/id_vm_access.pub (Copy key)
# 3) echo ["INSERT KEY"] >> ~/.ssh/authorized_keys

# ssh -i ~/.ssh/id_vm_access -L 5433:localhost:5432 anna_m_langener@34.44.141.225


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


# 
# ## Data Cleaning ##
# con <- dbConnect(
#   Postgres(),
#   dbname = "moodtriggers",
#   host = "localhost",
#   port = 5433,
#   user = "hannah",
#   password = "moodtriggers2025"
# )
# 
# 
# combined_df <- dbReadTable(con, "ema_responses_final3")
# cache <- dbReadTable(con, "overall_status_cache")

combined_df <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/table_new.csv") # downloaded from VM
cache <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/table_overall_new.csv") # downloaded from VM



############# Code Questions ############

combined_df_all <- combined_df %>%
  mutate(ema_category = case_when(
    # Negative symptoms
    grepl("I would rather have been doing something else\\.", questionText) ~ "Negative symptom",
    grepl("I have felt unmotivated\\.", questionText) ~ "Negative symptom",
    grepl("I have felt emotionally flat, numb, or blank\\.", questionText) ~ "Negative symptom",
    grepl("I have experienced difficulty expressing my emotions or thoughts\\.", questionText) ~ "Negative symptom",
    grepl("I have not wanted to socialize\\.", questionText) ~ "Negative symptom",
    
    # Positive symptoms
    grepl("to what degree have you had any unusual experiences\\?", questionText) ~ "Positive symptom",
    grepl("I have felt detached from reality\\.", questionText) ~ "Positive symptom",
    grepl("I have felt I possess special powers or abilities\\.", questionText) ~ "Positive symptom",
    grepl("I have felt I am receiving special messages\\.", questionText) ~ "Positive symptom",
    grepl("I have felt suspicious of others\\.", questionText) ~ "Positive symptom",
    
    # Cognitive symptoms
    grepl("I have found it easy to concentrate\\.", questionText) ~ "Cognitive symptom",
    grepl("I have experienced racing thoughts\\.", questionText) ~ "Cognitive symptom",
    grepl("I have found it easy to make decisions\\.", questionText) ~ "Cognitive symptom",
    grepl("I have experienced difficulty thinking clearly\\.", questionText) ~ "Cognitive symptom",
    grepl("I have had trouble remembering things\\.", questionText) ~ "Cognitive symptom",
    
    # Functional outcomes
    grepl("I have been able to complete my daily activities\\.", questionText) ~ "Functional outcome",
    grepl("I have been able to interact with others\\.", questionText) ~ "Functional outcome",
    grepl("I have been able to manage my physical and mental health\\.", questionText) ~ "Functional outcome",
    
    # Suicide ideation and follow-up
    grepl("I had had thoughts of hurting myself or that I would be better off dead\\.", questionText) ~ "Suicide ideation/self-harm",
    grepl("Do you intend to end your life now or in the near future\\?", questionText) ~ "Suicide risk follow-up",
    
    # Sleep quality
    grepl("Last night I had trouble with sleep\\.", questionText) ~ "Sleep quality",
    
    # Medication adherence
    grepl("I took my medications for psychotic symptoms today\\.", questionText) ~ "Medication adherence",
    
    # Default case
    TRUE ~ NA_character_
  ))


# Define reverse-coded items
reverse_items <- c(
  "In the past 4 hours, I have found it easy to concentrate.",
  "In the past 4 hours, I have found it easy to make decisions."
)

sum(combined_df_all$questionText %in% reverse_items)

combined_df_all$response <- as.numeric(as.character(combined_df_all$response))

# Apply reverse coding 
combined_df_all <- combined_df_all %>%
  mutate(
    reverse_flag = questionText %in% reverse_items,
    response = ifelse(
      reverse_flag,
      (0 + 100) - response,
      response
    )
  )

# Filter out unwanted categories
combined_df <- combined_df_all %>%
  filter(
    !ema_category %in% c("Medication adherence", "Suicide ideation/self-harm"),
    !is.na(ema_category)
  ) %>%
  mutate(response = as.numeric(response))


################# Exclude participants ##################

# Ensure end_date is Date type
cache$study_start_date <- as.Date(cache$study_start_date)

# Define today's date
today <- Sys.Date()

exclude <- cache$participant_id[
  cache$excluded == "t" | cache$study_start_date + days(90) > today
]

# Apply exclusions
combined_df_all <- combined_df_all[
  !combined_df_all$participant_id %in% exclude, 
]

# Remove test / empty IDs
combined_df_all <- combined_df_all[
  !combined_df_all$participant_id %in% c("Test", "",  "00006",  "00015"), #00006 has almost no data belongs to 00008 with also no data , 15 dropout
] 

# Join study_start_date into main dataframe
combined_df_all <- combined_df_all %>%
  left_join(cache[, c("participant_id", "study_start_date")], by = "participant_id")

# Keep only rows within 90 days of study start
combined_df_all <- combined_df_all %>%
  filter(date <= study_start_date + days(90))


# Count remaining participants
length(unique(combined_df_all$participant_id)) # PARTICIPANTS THAT COMPLETED STUDY


# Remove participants with less than 50 compliance
exclude <- cache$participant_id[cache$overall_compliance < 50]
# Apply exclusions
combined_df_all <- combined_df_all[
  !combined_df_all$participant_id %in% exclude, 
]

# Apply exclusions
combined_df_all <- combined_df_all[
  !combined_df_all$participant_id %in% exclude, 
]

length(unique(combined_df_all$participant_id)) # INCLUDED PARTICIPANTS

###### Compliance #######

mean(cache$overall_compliance[cache$participant_id %in% combined_df_all$participant_id])
min(cache$overall_compliance[cache$participant_id %in% combined_df_all$participant_id])
max(cache$overall_compliance[cache$participant_id %in% combined_df_all$participant_id])


####### Remove duplicates ####

combined_df_all <- combined_df_all[!duplicated(combined_df_all), ]

dup_rows <- duplicated(combined_df_all[, c("timestamp", "participant_id", "response", "questionText")])
sum(dup_rows)

combined_df_all = combined_df_all[!dup_rows,]


######### Add time index (because some participants restarted we can't use survey ID) #######
test1 = combined_df_all %>%
  group_by(participant_id,date) %>%
  summarise(n = n())

# If survey is filled out in the same 10 minutes they belong to one survey
combined_df_all <- combined_df_all %>%
  mutate(
    ts = ymd_hms(date),
    day = as.Date(ts),
    ts_round = round_date(ts, unit = "10 minutes")
  ) %>%
  group_by(participant_id, day) %>%
  arrange(ts_round, .by_group = TRUE) %>%
  mutate(survey_in_day = dense_rank(ts_round)) %>%  # 1–3 per day
  ungroup() %>%
  group_by(participant_id) %>%
  arrange(day, survey_in_day, .by_group = TRUE) %>%
  mutate(time_index = dense_rank(paste(day, survey_in_day))) %>%
  ungroup()

unique(combined_df_all$time_index)


# We still have duplicates (e.g., if date was stored differently)
combined_df_all <- combined_df_all[!duplicated(combined_df_all), ]
dup_rows <- duplicated(combined_df_all[, c("time_index", "participant_id", "response", "questionText")])
sum(dup_rows)
combined_df_all = combined_df_all[!dup_rows,]


# Now we still have some participants that have multiple surveys (it should be ~20 as we have 20-22 questions per index)
test2 = combined_df_all %>%
  group_by(participant_id,time_index) %>%
  summarise(n = n())

bad_groups <- combined_df_all %>%
  group_by(participant_id, time_index) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 30)

combined_df_all <- combined_df_all %>%
  anti_join(bad_groups, by = c("participant_id", "time_index"))


combined_df_all %>%
  group_by(participant_id, time_index) %>%
  summarise(n = n()) %>%
  summary()

test2 = combined_df_all %>%
  group_by(participant_id,time_index) %>%
  summarise(n = n())

######### Some Basic Checks #######

compliance <- combined_df_all %>%
  group_by(participant_id)  %>%
  summarise(n = max(time_index),
            com = max(time_index/270)) # For compliance use cache, as this accounts for multiple questions


unique(combined_df_all$participant_id[combined_df_all$time_index > 273])

combined_df_all = combined_df_all[combined_df_all$time_index < 271,]

colnames(combined_df_all)



write.csv(combined_df_all,"/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/esm_cleaned.csv" )


####### Descriptives ######

read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/esm_cleaned.csv" )


# ============================================================
# Publication-ready descriptives for EMA symptom domains
# ============================================================

library(dplyr)
library(tidyr)
library(psych)
library(lme4)
library(performance)
library(Hmisc)
library(dplyr)
library(gt)
library(gtExtras)
library(lme4)
library(purrr)
library(tidyr)

# -----------------------------
# 1. Keep only target domains
# -----------------------------
target_domains <- c("Positive symptom", "Negative symptom", "Cognitive symptom")

# ============================================================
# 2. ITEM-LEVEL DESCRIPTIVES
# Mean, SD, N, min, max per item
# ============================================================


get_icc <- function(df, outcome) {
  df <- df %>%
    filter(!is.na(.data[[outcome]])) %>%
    filter(!is.na(participant_id))
  
  if (nrow(df) == 0 || dplyr::n_distinct(df$participant_id) < 2) {
    return(NA_real_)
  }
  
  fit <- tryCatch(
    lmer(
      stats::as.formula(paste0(outcome, " ~ 1 + (1 | participant_id)")),
      data = df,
      REML = TRUE
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(NA_real_)
  }
  
  vc <- as.data.frame(VarCorr(fit))
  var_between <- vc$vcov[vc$grp == "participant_id"]
  var_within <- vc$vcov[vc$grp == "Residual"]
  
  if (length(var_between) == 0 || length(var_within) == 0) {
    return(NA_real_)
  }
  
  var_between / (var_between + var_within)
}

#---------------------------
# Item ICCs from raw repeated item responses
#---------------------------
item_icc <- ema_symptoms %>%
  filter(!is.na(response)) %>%
  group_by(ema_category, item_label) %>%
  nest() %>%
  mutate(
    icc = map_dbl(data, ~ get_icc(.x, "response"))
  ) %>%
  select(-data)

#---------------------------
# Item-level rows
#---------------------------
item_rows <- ema_symptoms %>%
  filter(!is.na(response)) %>%
  group_by(ema_category, item_label, participant_id) %>%
  summarise(
    person_value = mean(response, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ema_category, item_label) %>%
  summarise(
    mean = mean(person_value, na.rm = TRUE),
    sd = sd(person_value, na.rm = TRUE),
    # median = median(person_value, na.rm = TRUE),
    # min = min(person_value, na.rm = TRUE),
    # max = max(person_value, na.rm = TRUE),
    values = list(person_value),
    row_type = "Item",
    .groups = "drop"
  ) %>%
  left_join(item_icc, by = c("ema_category", "item_label"))

#---------------------------
# Occasion-level scale scores
# First average items within EMA occasion
#---------------------------
scale_occasions <- ema_symptoms %>%
  filter(!is.na(response)) %>%
  group_by(ema_category, participant_id, time_index) %>%
  summarise(
    scale_at_assessment = mean(response, na.rm = TRUE),
    .groups = "drop"
  )

#---------------------------
# Scale ICCs from raw repeated occasion-level scale scores
#---------------------------
scale_icc <- scale_occasions %>%
  group_by(ema_category) %>%
  nest() %>%
  mutate(
    icc = map_dbl(data, ~ get_icc(.x, "scale_at_assessment"))
  ) %>%
  select(-data)

#---------------------------
# Scale-level rows
# Then average occasion-level scale scores across time per participant
#---------------------------
scale_rows <- scale_occasions %>%
  group_by(ema_category, participant_id) %>%
  summarise(
    person_value = mean(scale_at_assessment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ema_category) %>%
  summarise(
    mean = mean(person_value, na.rm = TRUE),
    sd = sd(person_value, na.rm = TRUE),
    # median = median(person_value, na.rm = TRUE),
    # min = min(person_value, na.rm = TRUE),
    # max = max(person_value, na.rm = TRUE),
    values = list(person_value),
    row_type = "Scale",
    .groups = "drop"
  ) %>%
  left_join(scale_icc, by = "ema_category") %>%
  mutate(item_label = "Scale mean") %>%
  select(ema_category, item_label, mean, sd, icc, values, row_type)

#---------------------------
# Combine and format
#---------------------------
descriptives_tbl <- bind_rows(item_rows, scale_rows) %>%
  mutate(
    row_type = factor(row_type, levels = c("Item", "Scale"))
  ) %>%
  arrange(ema_category, row_type, item_label) %>%
  gt(groupname_col = "ema_category") %>%
  fmt_number(
    columns = c(mean, sd, icc),
    decimals = 2
  ) %>%
  gt_plt_dist(column = values) %>%
  cols_label(
    item_label = "Item / Scale",
    mean = "Mean",
    sd = "SD",
    # median = "Median",
    # min = "Min",
    # max = "Max",
    icc = "ICC",
    values = "Distribution",
    row_type = "Type"
  ) %>%
  cols_move_to_start(columns = c(row_type, item_label)) %>%
  tab_footnote(
    footnote = paste(
      "Item rows summarize participant-level mean item scores.",
      "For each participant, responses were first averaged across all available observations for a given item within an EMA category.",
      "The table then reports the mean, SD, median, minimum, and maximum of those participant-level item means across participants.",
      "The distribution graphic for item rows displays the distribution of these participant-level item means.",
      "ICC for item rows was estimated from raw repeated item responses using a random-intercept model with observations nested within participants."
    ),
    locations = cells_column_labels(columns = c(item_label, mean, sd, icc, values))
  ) %>%
  tab_footnote(
    footnote = paste(
      "Scale rows summarize participant-level mean scale scores within each EMA category.",
      "For each participant and EMA occasion, item responses belonging to that category were first averaged to create an occasion-level scale score.",
      "These occasion-level scale scores were then averaged across time within participant.",
      "The table then reports the mean, SD, median, minimum, and maximum of those participant-level scale means across participants.",
      "The distribution graphic for scale rows displays the distribution of these participant-level scale means.",
      "ICC for scale rows was estimated from raw repeated occasion-level scale scores using a random-intercept model with observations nested within participants."
    ),
    locations = cells_body(
      columns = item_label,
      rows = row_type == "Scale"
    )
  ) %>%
  tab_source_note(
    source_note = paste(
      "Note. Descriptive statistics are based on participant-level averages.",
      "Item rows use each participant's mean score for a given item.",
      "Scale rows use each participant's mean of occasion-level category scores, where category scores are computed by averaging all available items within category at each EMA occasion and then averaging those scores across occasions.",
      "ICC is the proportion of total variance attributable to between-person differences.",
      "Higher values indicate higher endorsement of the underlying item or scale content."
    )
  )

descriptives_tbl



#### Interitem Correlation


library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

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
  item_short = c(
    "Elsewhere",
    "Unmotivated",
    "Flat",
    "Expression",
    "Social",
    "Unusual exp.",
    "Detached",
    "Powers",
    "Messages",
    "Suspicious",
    "Concentrate",
    "Racing",
    "Decisions",
    "Clarity",
    "Memory"
  )
)

library(hrbrthemes)

item_means <- ema_symptoms %>%
  filter(!is.na(response)) %>%
  group_by(ema_category, participant_id, questionText) %>%
  summarise(
    person_mean = mean(response, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(item_key, by = "questionText")

make_corr_plot <- function(cat_name, df) {
  
  wide_df <- df %>%
    filter(ema_category == cat_name) %>%
    select(participant_id, item_short, person_mean) %>%
    tidyr::pivot_wider(names_from = item_short, values_from = person_mean)
  
  corr_mat <- wide_df %>%
    select(-participant_id) %>%
    cor(use = "pairwise.complete.obs")
  
  corr_df <- as.data.frame(as.table(corr_mat)) %>%
    rename(item_x = Var1, item_y = Var2, r = Freq)
  
  ggplot(corr_df, aes(item_x, item_y, fill = r)) +
    geom_tile(color = "white", linewidth = 0.3) +
    
    # ✅ add correlation values
    geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
    
    coord_equal() +
    scale_fill_gradientn(
      colours = c("#FE4365", "#FC9D9A", "#F9CDAD",
                  "#C8C8A9", "#83AF9B"),
      limits = c(-1, 1),
      name = "r"
    ) +
    labs(title = cat_name, x = NULL, y = NULL) +
    theme_ipsum(axis_title_size = 14)+
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

library(purrr)
library(patchwork)

plots <- item_means %>%
  distinct(ema_category) %>%
  pull(ema_category) %>%
  map(~ make_corr_plot(.x, item_means))

corr_plot = wrap_plots(plots, ncol = 3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") &
  plot_annotation(
    title = "Item Correlation by EMA Category",
    subtitle = "Correlations computed from participant-level mean item scores",
  )

ggsave(
  "inter_item_heatmaps.png",
  corr_plot,
  width = 10,
  height = 7,
  dpi = 300
)



########## MINI ########
combined_df_all <- read.csv(
  "/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/esm_cleaned.csv",
  colClasses = c(participant_id = "character")
)

MINI <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/MINI.csv")
MINI$record_id <- sprintf("%05d", MINI$record_id)

MINI <- MINI[MINI$record_id %in% combined_df_all$participant_id,]

sum(MINI$k_schizophrenia_current) ## CURRENT PSYCHOTIC SYMPTOMS
sum(MINI$k_r1_schizophrenia_current) ## CURRENT PSYCHOTIC SYMPTOMS without clinicians rating


current_id = MINI$k_schizophrenia_current == 1




########## Baseline ########
combined_df_all <- read.csv(
  "/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/esm_cleaned.csv",
  colClasses = c(participant_id = "character")
)

baseline <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/Baseline.csv")
baseline$record_id <- sprintf("%05d", baseline$record_id)

unique(combined_df_all$participant_id[!combined_df_all$participant_id %in% baseline$record_id])
baseline = baseline[baseline$b_timestamp != "[not completed]",]
baseline = baseline[baseline$b_timestamp != "",]

baseline <- baseline[baseline$record_id %in% combined_df_all$participant_id,]

#dem 4: race
sum(baseline$dem_4___1) # White
sum(baseline$dem_4___2)# Black or African American
sum(baseline$dem_4___3) # American Indian or Alaska Native
sum(baseline$dem_4___4) # Asian
sum(baseline$dem_4___5) # Native Hawaiian or Pacific Islander
sum(baseline$dem_4___6) #Other



#dem_5: gdem_4___1#dem_5: gender (1: male, 2: female, 3: non binary, 4: other)
table(baseline$dem_5)

# dem_26: age

describe(baseline$dem_26)

