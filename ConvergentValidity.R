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
  .(
    ema_prior_week_mean = mean(response, na.rm = TRUE),
    ema_prior_week_n    = sum(!is.na(response))
  ),
  by = .(weekly_row_id, ema_category)
]
# Wide format
prior_means <- joined[
  ,
  .(
    ema_prior_week_mean = mean(response, na.rm = TRUE),
    ema_prior_week_n    = sum(!is.na(response))
  ),
  by = .(weekly_row_id, ema_category)
]

means_wide <- dcast(
  prior_means,
  weekly_row_id ~ ema_category,
  value.var = "ema_prior_week_mean"
)

setnames(
  means_wide,
  old = names(means_wide)[-1],
  new = paste0("ema_7d_", names(means_wide)[-1], "_mean")
)
n_wide <- dcast(
  prior_means,
  weekly_row_id ~ ema_category,
  value.var = "ema_prior_week_n"
)

setnames(
  n_wide,
  old = names(n_wide)[-1],
  new = paste0("ema_7d_", names(n_wide)[-1], "_n")
)
final_df <- Reduce(function(x, y) merge(x, y, by = "weekly_row_id", all.x = TRUE),
                   list(wk, means_wide, n_wide))

# Cleanup
final_df[, c("weekly_row_id", "window_start", "window_end") := NULL]

final_df$`ema_7d_Cognitive symptom_n`/5 # (It should be all the same)

mean(final_df$`ema_7d_Cognitive symptom_n`/5, na.rm = T)
max(final_df$`ema_7d_Cognitive symptom_n`/5, na.rm = T)
min(final_df$`ema_7d_Cognitive symptom_n`/5, na.rm = T)


####### Convergent validity #######


library(ggExtra)



# Spearman correlation
pos_cor <- cor.test(
  final_df$positive_weekly_mean,
  final_df$`ema_7d_Positive symptom_mean`,
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
    x = `ema_7d_Positive symptom_mean`,
    y = positive_weekly_mean
  )
) +
  geom_point(alpha = 0.7,  size = 0.6) +
  geom_smooth(method = "lm", se = TRUE,  color = "#FC9D9A") +
  theme_ipsum(axis_title_size = 14) +
  labs(
    x = "EMA Mean",
    y = "Weekly Mean (PS-R)",
    title = paste0("Positive Symptoms (ρ = ", rho_pos, ")")
  )

a = ggMarginal(p_pos, type = "histogram",  fill = "grey70",
               color = "white")





# Spearman correlation
rho_neg <- cor.test(
  final_df$negative_weekly_mean,
  final_df$`ema_7d_Negative symptom_mean`,
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
    x = `ema_7d_Negative symptom_mean`,
    y = negative_weekly_mean
  )
) +
  geom_point(alpha = 0.7, size = 0.6) +
  geom_smooth(method = "lm", se = TRUE,  color = "#2A363B") +
  theme_ipsum(axis_title_size = 14) +
  labs(
    x = "EMA Mean",
    y = "Weekly Mean (NSI-PR)",
    title = paste0("Negative Symptoms (ρ = ", rho_neg, ")")
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


######### Post Assessment #######
post <- read.csv("/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/post_assessment.csv")
post$record_id <- sprintf("%05d", post$record_id)

#!/usr/bin/env Rscript
ema <- read.csv(
  "/Users/f007qrc/Library/CloudStorage/GoogleDrive-anna.m.langener@dartmouth.edu/My Drive/Darmouth Drive/9_PsychometricSensing/esm_cleaned.csv",
  colClasses = c(participant_id = "character")
)

post <- post[post$record_id %in% ema$participant_id,]


pns_key_68 <- c(
  "P","N","N","P",
  "N","N","P","N",
  "P","P","P","P",
  "N","P","N","P",
  "N","P","N","P", #25
  "N","N","P","N",
  "P","P","P","P",
  "P","P","N","P", #38
  "N","N","N","N",
  "N","N","P","N", #48
  "P","P","N","P",
  "P","N","P","N", #56
  "P","N","N","N", #60
  "N","N", "N","P",
  "N","N","N","N",
  "P","N","P","P",
  "P","N","P","P"
)

# 1 = yes
# 2 = no

post_items <- paste0("pns_q_", 1:68, "_p")

post[post_items] <- lapply(post[post_items], function(x) {
  ifelse(x == 1, 1,
         ifelse(x == 2, 0, NA_real_))
})

items <- paste0("pns_q_", 1:68, "_p")

key_df <- data.frame(
  item = items,
  key = pns_key_68
)

P_items <- key_df$item[key_df$key == "P"]
N_items <- key_df$item[key_df$key == "N"]

post$pns_sum_P <- rowSums(post[P_items], na.rm = TRUE)
post$pns_sum_N <- rowSums(post[N_items], na.rm = TRUE)



####### Weekly questionnaire scoring (reversed items) #######
post <- post %>%
  mutate(
    # Reverse 0–7 items
    across(
      nsi_1_p:nsi_6_p,
      ~ 7 - .
    ),
    
    # Reverse 0–10 items
    across(
      nsi_7_p:nsi_11_p,
      ~ 10 - .
    )
  ) %>%
  mutate(
    # Asociality
    asociality = rowMeans(
      select(., nsi_1_p, nsi_7_p, nsi_8_p),
      na.rm = TRUE
    ),
    
    # Avolition
    avolition = rowMeans(
      select(., nsi_2_p, nsi_3_p, nsi_9_p, nsi_10_p),
      na.rm = TRUE
    ),
    
    # Anhedonia
    anhedonia = rowMeans(
      select(., nsi_4_p, nsi_5_p, nsi_6_p, nsi_11_p),
      na.rm = TRUE
    )
  )

post <- post %>%
  mutate(
    negative_post_nsi = rowMeans(
      select(., asociality, avolition, anhedonia),
      na.rm = TRUE
    )
  )


###### Merge ####

# ----------------------------
# 1. PREP DATA
# ----------------------------
post <- as.data.table(post)
post[, participant_id := record_id]
post[, post_day := as.IDate(p_timestamp)]

ema <- as.data.table(ema)
ema[, ema_day := as.IDate(day)]

# ----------------------------
# 2. DEFINE WINDOWS (clean, no duplication yet)
# ----------------------------
windows <- c(1, 7, 14, 21, 28, 35,49,63,90)

win_dt <- rbindlist(lapply(windows, function(w) {
  data.table(
    participant_id = post$participant_id,
    post_day       = post$post_day,
    window_start   = post$post_day - w,
    window_end     = post$post_day,
    window         = paste0(w, "d")
  )
}))

# ----------------------------
# 3. JOIN EMA TO WINDOWS (correct constraint)
# ----------------------------
joined <- win_dt[ema,
                 on = .(
                   participant_id,
                   window_start <= ema_day,
                   window_end > ema_day
                 ),
                 allow.cartesian = TRUE,
                 nomatch = 0
]

# ----------------------------
# 4. AGGREGATE (correct grain)
# ----------------------------
all_ema <- joined[
  ,
  .(
    ema_mean = mean(response, na.rm = TRUE),
    ema_n    = .N
  ),
  by = .(participant_id, window, ema_category)
]

# ----------------------------
# 5. WIDE FORMAT (SAFE: separate mean + n)
# ----------------------------
mean_wide <- dcast(
  all_ema,
  participant_id ~ window + ema_category,
  value.var = "ema_mean"
)

n_wide <- dcast(
  all_ema,
  participant_id ~ window + ema_category,
  value.var = "ema_n"
)

# ----------------------------
# 6. CLEAN NAMES
# ----------------------------
setnames(mean_wide,
         old = names(mean_wide)[-1],
         new = gsub(" ", "_", names(mean_wide)[-1]))

setnames(n_wide,
         old = names(n_wide)[-1],
         new = gsub(" ", "_", names(n_wide)[-1]))

# ----------------------------
# 7. MERGE EMA FEATURES
# ----------------------------
ema_wide <- ema_wide <- dcast(
  all_ema,
  participant_id ~ window + ema_category,
  value.var = c("ema_mean", "ema_n")
)

# ----------------------------
# 8. REMOVE OVERLAP WITH POST (SAFE GUARD)
# ----------------------------
ema_cols <- setdiff(names(ema_wide), "participant_id")
overlap  <- intersect(names(post), ema_cols)

if (length(overlap) > 0) {
  ema_wide[, (overlap) := NULL]
}

# ----------------------------
# 9. FINAL MERGE (NO .x/.y EVER)
# ----------------------------
final_post <- ema_wide[post, on = "participant_id"]


length(unique(final_post$participant_id)) # 80
##########

# Spearman correlation
rho_neg <- cor.test(
  final_post$pns_sum_N,
  final_post$`ema_mean_7d_Negative symptom`,
  method = "spearman",
  use = "complete.obs",
  exact = FALSE
)

# Extract rho
rho_neg <- round(rho_neg$estimate, 2)

# Plot with correlation in title
p_pos <- ggplot(
  final_post,
  aes(
    x = `ema_mean_7d_Negative symptom`,
    y = pns_sum_N
  )
) +
  geom_point(alpha = 0.7, size = 0.6) +
  geom_smooth(method = "lm", se = TRUE,  color = "#83AF9B") +
  theme_ipsum(axis_title_size = 14) +
  labs(
    x = "EMA Mean (7 days prior)",
    y = "Post Assessment (PNS-Q)",
    title = paste0("Negative Symptoms (ρ = ", rho_neg, ")")
  )

a = ggMarginal(p_pos, type = "histogram",   fill = "grey70",
               color = "white")


# Spearman correlation
rho_neg <- cor.test(
  final_post$negative_post_nsi,
  final_post$`ema_mean_7d_Negative symptom`,
  method = "spearman",
  use = "complete.obs",
  exact = FALSE
)

# Extract rho
rho_neg <- round(rho_neg$estimate, 2)

# Plot with correlation in title
p_pos <- ggplot(
  final_post,
  aes(
    x = `ema_mean_7d_Negative symptom`,
    y = negative_post_nsi
  )
) +
  geom_point(alpha = 0.7, size = 0.6) +
  geom_smooth(method = "lm", se = TRUE,  color = "#2A363B") +
  theme_ipsum(axis_title_size = 14) +
  labs(
    x = "EMA Mean (7 days prior)",
    y = "Post Assessment (NSI-PR)",
    title = paste0("Negative Symptoms (ρ = ", rho_neg, ")")
  )

b = ggMarginal(p_pos, type = "histogram",   fill = "grey70",
               color = "white")




######### Positive

# Spearman correlation
rho_neg <- cor.test(
  final_post$pns_sum_P,
  final_post$`ema_mean_7d_Positive symptom`,
  method = "spearman",
  use = "complete.obs",
  exact = FALSE
)

# Extract rho
rho_neg <- round(rho_neg$estimate, 2)

# Plot with correlation in title
p_pos <- ggplot(
  final_post,
  aes(
    x = `ema_mean_7d_Positive symptom`,
    y = pns_sum_P
  )
) +
  geom_point(alpha = 0.7, size = 0.6) +
  geom_smooth(method = "lm", se = TRUE,  color = "#FC9D9A") +
  theme_ipsum(axis_title_size = 14) +
  labs(
    x = "EMA Mean (7 days prior)",
    y = "Post Assessment (PNS-Q)",
    title = paste0("Positive Symptoms (ρ = ", rho_neg, ")")
  )

c = ggMarginal(p_pos, type = "histogram",   fill = "grey70",
               color = "white")

p1 = plot_grid(c, a, b, ncol = 3)


ggsave(
  filename = "convergent_validity_plot_post_neg.png",
  plot = p,
  width = 12,
  height = 5,
  dpi = 300
)


#######
library(data.table)
library(ggplot2)

dt <- as.data.table(final_post)
dt[, post_day := as.IDate(post_day)]

# ----------------------------
# Time bins (weekly)
# ----------------------------
dt[, time_bin := as.IDate(cut(post_day, "7 days"))]

# ----------------------------
# EMA windows
# ----------------------------
windows <- c("1d", "7d", "14d", "21d","28d","35d","49d","63d","90d")

# ----------------------------
# CORRECT column-safe loop
# ----------------------------
corr_pos <- rbindlist(lapply(windows, function(w) {
  
  ema_col <- paste0("ema_mean_", w, "_Positive symptom")
  
  if (!ema_col %in% names(dt)) return(NULL)
  
  dt[
    !is.na(pns_sum_P) & !is.na(get(ema_col)),
    .(
      window = w,
      domain = "Positive EMA vs PNS-P",
      rho = cor(pns_sum_P,
                get(ema_col),
                method = "spearman",
                use = "complete.obs"),
      n = .N
    )
  ]
}))


corr_neg_pnsN <- rbindlist(lapply(windows, function(w) {
  
  ema_col <- paste0("ema_mean_", w, "_Negative symptom")
  
  if (!ema_col %in% names(dt)) return(NULL)
  
  dt[
    !is.na(pns_sum_N) & !is.na(get(ema_col)),
    .(
      window = w,
      domain = "Negative EMA vs PNS-N",
      rho = cor(pns_sum_N,
                get(ema_col),
                method = "spearman",
                use = "complete.obs"),
      n = .N
    )
  ]
}))

corr_neg_nsi <- rbindlist(lapply(windows, function(w) {
  
  ema_col <- paste0("ema_mean_", w, "_Negative symptom")
  
  if (!ema_col %in% names(dt)) return(NULL)
  
  dt[
    !is.na(negative_post_nsi) & !is.na(get(ema_col)),
    .(
      window = w,
      domain = "Negative EMA vs NSI",
      rho = cor(negative_post_nsi,
                get(ema_col),
                method = "spearman",
                use = "complete.obs"),
      n = .N
    )
  ]
}))

colors <- c("#2A363B","#83AF9B","#C8C8A9",
            "#F9CDAD","#FC9D9A","#FE4365","#FFD700")


corr_all <- rbindlist(list(
  corr_pos,
  corr_neg_pnsN,
  corr_neg_nsi
), fill = TRUE)

corr_all[, window := factor(window, levels = windows)]

library(ggplot2)
library(data.table)

domain_colors <- c(
  "Positive EMA vs PNS-P"   = "#FC9D9A", 
  "Negative EMA vs PNS-N"   = "#83AF9B",  
  "Negative EMA vs NSI"= "#2A363B"   
)
p = ggplot(corr_all,
       aes(x = window,
           y = rho,
           color = domain,
           size = n,
           group = domain)) +
  
  geom_line(linewidth = 0.9, alpha = 0.8) +
  geom_point(alpha = 0.95) +
  
  scale_color_manual(values = domain_colors) +
  
  scale_size_continuous(
    range = c(2.5, 9),
    name = "N"
  ) +
  
  ylim(0,1) +
  
  theme_ipsum(axis_title_size = 14) +
  
  labs(
    x = "EMA window (mean aggregated over the preceding X days)",
    y = "Spearman correlation (ρ)",
    color = "Association",
    title = "Convergent validity across Time",
    subtitle = "Point size reflects number of observations"
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


p2 = plot_grid(p1,p, ncol = 1)


ggsave(
  filename = "convergent_validity_overtime.png",
  plot = p2,
  width = 11.8,
  height = 8,
  dpi = 300
)


  