##############################################
############ Convergent Validity #############
##############################################

library(dplyr)
library(tidyr)
library(readxl)

####### Load data #######
#!/usr/bin/env Rscript
combined_df_all <- read.csv(
  "/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/esm_cleaned.csv",
  colClasses = c(participant_id = "character")
)

weekly <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/weekly_redcap.csv")
weekly$record_id <- sprintf("%05d", weekly$record_id)

weekly <- weekly[weekly$record_id %in% combined_df_all$participant_id,]
#weekly <- read_xlsx("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/weekly_clean_data.xlsx")

####### Weekly questionnaire scoring (reversed items) #######
weekly <- weekly %>%
  mutate(
    # Reverse 0–7 items
    across(
      nsi_w_1:nsi_w_6,
      ~ 7 - .
    ),
    
    # Reverse 0–10 items
    across(
      nsi_w_7:nsi_w_11,
      ~ 10 - .
    )
  ) %>%
  mutate(
    # Asociality
    asociality = rowMeans(
      select(., nsi_w_1, nsi_w_7, nsi_w_8),
      na.rm = TRUE
    ),
    
    # Avolition
    avolition = rowMeans(
      select(., nsi_w_2, nsi_w_3, nsi_w_9, nsi_w_10),
      na.rm = TRUE
    ),
    
    # Anhedonia
    anhedonia = rowMeans(
      select(., nsi_w_4, nsi_w_5, nsi_w_6, nsi_w_11),
      na.rm = TRUE
    )
  )

weekly <- weekly %>%
  mutate(
    negative_weekly_mean = rowMeans(
      select(., asociality, avolition, anhedonia),
      na.rm = TRUE
    )
  )

weekly <- weekly %>%
  mutate(
    positive_weekly_mean = rowMeans(
      select(.,ps_r_1_w, ps_r_2_w, ps_r_3_w, ps_r_4_w, ps_r_5_w, ps_r_6_w,
             ps_r_7_w, ps_r_8_w, ps_r_9_w, ps_r_10_w,
             ps_r_11_w, ps_r_12_w),
      na.rm = TRUE
    )
  )

########
library(data.table)


ema <- as.data.table(combined_df_all)
wk  <- as.data.table(weekly)

# Dates
ema[, day := as.IDate(day)]
wk[, w_timestamp := as.IDate(w_timestamp)]

# Match ID column names
wk[, participant_id := record_id]

# Keep needed EMA columns
ema <- ema[, .(
  participant_id,
  ema_day = day,
  ema_category,
  response
)]


wk[, weekly_row_id := .I]
wk[, window_start := w_timestamp - 7]
wk[, window_end := w_timestamp]

joined <- wk[ema,
             on = .(
               participant_id,
               window_start <= ema_day,
               window_end > ema_day
             ),
             allow.cartesian = TRUE,
             nomatch = NA
]

# Mean by weekly row and EMA category
prior_means <- joined[
  ,
  .(ema_prior_week_mean = mean(response, na.rm = TRUE)),
  by = .(weekly_row_id, ema_category)
]

# Wide format
prior_means_wide <- dcast(
  prior_means,
  weekly_row_id ~ ema_category,
  value.var = "ema_prior_week_mean"
)

# Optional renaming
old_names <- setdiff(names(prior_means_wide), "weekly_row_id")
setnames(prior_means_wide, old_names, paste0("ema_prior_week_mean_", old_names))

# Merge back
final_df <- merge(wk, prior_means_wide, by = "weekly_row_id", all.x = TRUE)

# Cleanup
final_df[, c("weekly_row_id", "window_start", "window_end") := NULL]




####### Convergent validity #######


library(ggExtra)



# Spearman correlation
pos_cor <- cor.test(
  final_df$positive_weekly_mean,
  final_df$`ema_prior_week_mean_Positive symptom`,
  method = "spearman",
  use = "complete.obs",
  exact = FALSE
)

# Extract rho
rho_pos <- round(pos_cor$estimate, 2)

# Plot with correlation in title
p_pos <- ggplot(
  final_df,
  aes(
    x = `ema_prior_week_mean_Positive symptom`,
    y = positive_weekly_mean
  )
) +
  geom_point(alpha = 0.7,  size = 0.6) +
  geom_smooth(method = "lm", se = TRUE,  color = "#2A363B") +
  theme_ipsum(axis_title_size = 14) +
  labs(
    x = "EMA Mean",
    y = "Weekly Mean",
    title = paste0("Positive Symptoms (Spearman ρ = ", rho_pos, ")")
  )

a = ggMarginal(p_pos, type = "histogram",  fill = "grey70",
               color = "white")





# Spearman correlation
rho_neg <- cor.test(
  final_df$negative_weekly_mean,
  final_df$`ema_prior_week_mean_Negative symptom`,
  method = "spearman",
  use = "complete.obs",
  exact = FALSE
)

# Extract rho
rho_neg <- round(rho_neg$estimate, 2)

# Plot with correlation in title
p_pos <- ggplot(
  final_df,
  aes(
    x = `ema_prior_week_mean_Negative symptom`,
    y = negative_weekly_mean
  )
) +
  geom_point(alpha = 0.7, size = 0.6) +
  geom_smooth(method = "lm", se = TRUE,  color = "#2A363B") +
  theme_ipsum(axis_title_size = 14) +
  labs(
    x = "EMA Mean",
    y = "Weekly Mean",
    title = paste0("Negative Symptoms (Spearman ρ = ", rho_neg, ")")
  )

b = ggMarginal(p_pos, type = "histogram",   fill = "grey70",
               color = "white")


library(cowplot)
p = plot_grid(a, b, ncol = 2)


ggsave(
  filename = "convergent_validity_plot.png",
  plot = p,
  width = 11,
  height = 5,
  dpi = 300
)



######

length(unique(final_df$record_id))

test = final_df %>% group_by(participant_id)  %>% summarize(n = n())

min(test$n)
max(test$n)
mean(test$n)


