
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
