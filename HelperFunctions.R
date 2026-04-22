

############### Helper Functions ###############

############# MLR (psych::mlr) ################
run_mlr_reliability <- function(data, construct, time_var, uid_var,
                                item_var = "questionText", value_var = "response") {
  data_long <- data %>%
    filter(ema_category == construct) %>%
    mutate(
      !!value_var := as.numeric(.data[[value_var]]),
      !!item_var := as.character(.data[[item_var]])
    )
  
  warn_msgs <- character(0)
  out <- withCallingHandlers(
    psych::mlr(
      data_long,
      grp = uid_var,
      Time = time_var,
      items = item_var,
      values = value_var,
      long = TRUE,
      lmer = TRUE,
      lme = FALSE,
      alpha = FALSE,
      aov = FALSE
    ),
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  if (length(warn_msgs) > 0) {
    message(paste0("mlr warnings (", construct, "):"))
    message(paste0(" - ", unique(warn_msgs), collapse = "\n"))
  }
  
  attr(out, "mlr_warnings") <- unique(warn_msgs)
  out
}



run_multilevel_omega <- function(data,
                                 construct,
                                 time_var = "time_index",
                                 uid_var = "participant_id",
                                 item_var = "questionText",
                                 value_var = "response") {
  
  # Ensure correct subset first
  data <- data %>% select(all_of(c(uid_var, time_var, item_var, value_var, "ema_category")))
  
  df_sub <- data %>%
    filter(ema_category == construct)
  
  # Long → wide
  wide <- df_sub %>%
    group_by(.data[[uid_var]], .data[[time_var]], .data[[item_var]]) %>%
    summarise(
      response = mean(.data[[value_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = all_of(item_var),
      values_from = response
    )
  
  # Rename item columns to x1:x4
  item_cols <- setdiff(colnames(wide), c(uid_var, time_var))
  colnames(wide)[colnames(wide) %in% item_cols] <- paste0("x", seq_along(item_cols))
  
  # Convert to data.frame
  wide <- as.data.frame(wide)
  
  # Remove time column (keep clustering at participant level)
  wide_items <- wide %>%
    select(-all_of(c(uid_var, time_var)))
  
  wide <- wide %>%
    select(-all_of(c(time_var)))
  
  # Run multilevel omega
  multilevel.omega(
    wide[,colnames(wide_items)],
    cluster = wide$participant_id,
    missing = "listwise"
  )
}



GeneralizabilityTheory_Nezlek <-  function(data, construct, time_var, uid_var,
                                           item_var = "questionText", value_var = "response",
                                           optimizer = "bobyqa", maxfun = 1e5) {
  long <- data %>%
    filter(ema_category == construct) %>%
    mutate(
      !!value_var := as.numeric(.data[[value_var]]),
      !!item_var := as.character(.data[[item_var]])
    )
  
  formula <- as.formula(
    paste0("response ~  (1 | ", uid_var, "/", time_var, ")")
  )
  
  warn_msgs <- character(0)
  mod <- withCallingHandlers(
    lmer(
      formula,
      long,
      control = lmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
    ),
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  var_ml3_pa <- VarCorr(mod)
  varP   <- var_ml3_pa[[uid_var]][[1]]
  varOxP <- var_ml3_pa[[paste0(time_var, ":", uid_var)]][[1]]
  varE   <- (attributes(var_ml3_pa)$sc)^2
  
  k <- length(unique(long[[time_var]]))
  m <- length(unique(long[[item_var]]))
  
  rkrn <- varP / (varP + varOxP / k + varE / (m * k))
  rcn  <- varOxP / (varOxP + varE / m)
  
  list(
    rel = c(rkrn, rcn),
    warnings = unique(warn_msgs),
    conv = unlist(mod@optinfo$conv$lme4),
    singular = isSingular(mod, tol = 1e-4),
    model = mod
  )
}


GeneralizabilityTheory <- function(data, construct, time_var, uid_var,
                                   item_var = "questionText", value_var = "response",
                                   optimizer = "bobyqa", maxfun = 1e5) {
  data_long <- data %>%
    filter(ema_category == construct) %>%
    transmute(
      id = .data[[uid_var]],
      time = .data[[time_var]],
      items = as.character(.data[[item_var]]),
      values = as.numeric(.data[[value_var]])
    )
  
  warn_msgs <- character(0)
  mod <- withCallingHandlers(
    lme4::lmer(
      values ~ 1 +
        (1 | id) +
        (1 | time) +
        (1 | items) +
        (1 | id:time) +
        (1 | id:items) +
        (1 | items:time),
      data = data_long,
      control = lmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
    ),
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  vc <- lme4::VarCorr(mod)
  MS_id     <- vc$id[1, 1]
  MS_time   <- vc$time[1, 1]
  MS_items  <- vc$items[1, 1]
  MS_pxt    <- vc[["id:time"]][[1]]
  MS_pxitem <- vc[["id:items"]][[1]]
  MS_txitem <- vc[["items:time"]][[1]]
  error     <- (attributes(vc)$sc)^2
  
  n_time  <- length(table(data_long$time))
  n_items <- length(table(data_long$items))
  
  Rkf <- (MS_id + MS_pxitem / n_items) /
    (MS_id + MS_pxitem / n_items + error / (n_time * n_items))
  R1r <- (MS_id + MS_pxitem / n_items) /
    (MS_id + MS_pxitem / n_items + MS_time + MS_pxt + error / n_items)
  Rkr <- (MS_id + MS_pxitem / n_items) /
    (MS_id + MS_pxitem / n_items + MS_time / n_time + MS_pxt / n_time + error / (n_time * n_items))
  Rc  <- (MS_pxt) / (MS_pxt + error / n_items)
  
  list(
    Rkf = Rkf,
    R1r = R1r,
    Rkr = Rkr,
    Rc  = Rc,
    var_components = data.frame(
      id = MS_id,
      time = MS_time,
      items = MS_items,
      id_time = MS_pxt,
      id_items = MS_pxitem,
      time_items = MS_txitem,
      residual = error
    ),
    warnings = unique(warn_msgs),
    conv = unlist(mod@optinfo$conv$lme4),
    singular = lme4::isSingular(mod, tol = 1e-4),
    model = mod
  )
}
