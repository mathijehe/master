# === Lag datasett med sjokkene ===
shock_list <- c("oil_shock", "intl_bnp_growth_shock", "finans_shock",
                "monetary_shock","trend_shock", "prod_shock")


# === Standardiser alle sjokk før analyse ===
data8 <- data8 %>%
  mutate(across(
    all_of(shock_list),
    ~ as.numeric(scale(.)),  # z-score
    .names = "{.col}_std"
  ))

# === Oppdater shock_list med de standardiserte navnene ===
shock_list <- paste0(shock_list, "_std")


# === Tom resultat-tabell ===
resultater <- data.frame(
  Shock = character(0),
  Total_D_R_gap = numeric(0),
  Explained_D_R_gap = numeric(0),
  SE_Explained = numeric(0),
  Sample_Start = as.Date(character(0)),
  Sample_End = as.Date(character(0)),
  N_obs = integer(0),
  stringsAsFactors = FALSE
)


# === LOOP ===
for (shock in shock_list) {
  
  # --- Bygg datasett for dette sjokket ---
  data_temp <- data8 %>%
    dplyr::select(year, bnp, Regjering, all_of(shock)) %>%
    drop_na()
  
  # --- Lag zoo ---
  data_zoo <- zoo(data_temp[, c("bnp", shock)], order.by = data_temp$year)
  
  # --- Estimer distributed lag ---
  model <- dynlm(bnp ~ L(get(shock), 0:2), data = data_zoo)
  
  # --- Dataframe ---
  data_zoo_df <- as.data.frame(data_zoo)
  data_zoo_df$Regjering <- data_temp$Regjering
  
  # --- Koef ---
  b <- coef(model)
  b_shock0 <- b[2]
  b_shock1 <- b[3]
  b_shock2 <- b[4]
  
  # --- Shock effect ---
  data_zoo_df[[paste0(shock, "_effect")]] <- NA
  
  for (i in 3:nrow(data_zoo_df)) {
    data_zoo_df[i, paste0(shock, "_effect")] <- b_shock0 * data_zoo_df[i, shock] +
      b_shock1 * data_zoo_df[i-1, shock] +
      b_shock2 * data_zoo_df[i-2, shock]
  }
  
  # --- Gap ---
  DRTotal <- mean(data_zoo_df$bnp[data_zoo_df$Regjering == "Venstre"], na.rm=TRUE) -
    mean(data_zoo_df$bnp[data_zoo_df$Regjering == "Høyre"], na.rm=TRUE)
  
  DRExplained <- mean(data_zoo_df[[paste0(shock, "_effect")]][data_zoo_df$Regjering == "Venstre"], na.rm=TRUE) -
    mean(data_zoo_df[[paste0(shock, "_effect")]][data_zoo_df$Regjering == "Høyre"], na.rm=TRUE)
  
  vcov_mat <- vcov(model)
  shock_diff <- mean(data_zoo_df[[shock]][data_zoo_df$Regjering=="Venstre"], na.rm=TRUE) -
    mean(data_zoo_df[[shock]][data_zoo_df$Regjering=="Høyre"], na.rm=TRUE)
  gradient <- c(shock_diff, shock_diff, shock_diff)
  var_DR <- t(gradient) %*% vcov_mat[2:4, 2:4] %*% gradient
  se_DR <- sqrt(var_DR)
  
  # === Automatisk sample info ===
  sample_start <- min(data_temp$year)
  sample_end <- max(data_temp$year)
  n_obs <- nrow(data_temp)
  
  # --- Lagre ---
  resultater <- rbind(resultater, data.frame(
    Shock = shock,
    Total_D_R_gap = DRTotal,
    Explained_D_R_gap = DRExplained,
    SE_Explained = se_DR,
    Sample_Start = sample_start,
    Sample_End = sample_end,
    N_obs = n_obs
  ))
  
  print(paste("Ferdig med sjokk:", shock))
}


# === Tom resultat-tabell ===
resultater <- data.frame(
  Shock = character(0),
  Total_D_R_gap = numeric(0),
  Explained_D_R_gap_Common = numeric(0),
  SE_Explained_Common = numeric(0),
  Explained_D_R_gap_PartySpec = numeric(0),
  SE_Explained_Party = numeric(0),
  Sample_Start = as.Date(character(0)),
  Sample_End = as.Date(character(0)),
  N_obs = integer(0),
  stringsAsFactors = FALSE
)

# === LOOP ===
for (shock in shock_list) {
  
  # --- Data ---
  data_temp <- data8 %>%
    dplyr::select(year, bnp, Regjering, all_of(shock)) %>%
    drop_na() %>%
    mutate(
      D = ifelse(Regjering == "Venstre", 1, 0),
      R = ifelse(Regjering == "Høyre", 1, 0),
      lag0 = dplyr::lag(!!sym(shock), 0),
      lag1 = dplyr::lag(!!sym(shock), 1),
      lag2 = dplyr::lag(!!sym(shock), 2),
      D_lag0 = D * lag0,
      D_lag1 = D * lag1,
      D_lag2 = D * lag2
    ) %>%
    slice(3:n())  # Dropper de første pga lag
  
  # --- Sample info ---
  sample_start <- min(data_temp$year)
  sample_end   <- max(data_temp$year)
  n_obs        <- nrow(data_temp)
  
  # --- Zoo ---
  data_zoo <- zoo(data_temp[, c("bnp", "D", "R", "lag0", "lag1", "lag2", "D_lag0", "D_lag1", "D_lag2")],
                  order.by = data_temp$year)
  
  # === Modell 1: Common ===
  model_common <- dynlm(bnp ~ lag0 + lag1 + lag2, data = data_zoo)
  
  # === Modell 2: Party-specific ===
  model_party <- dynlm(bnp ~ D + R + lag0 + lag1 + lag2 + D_lag0 + D_lag1 + D_lag2, data = data_zoo)
  
  # === Total D-R gap ===
  DRTotal <- mean(data_temp$bnp[data_temp$D==1]) - mean(data_temp$bnp[data_temp$R==1])
  
  # === Explained gap (Common) ===
  b_common <- coef(model_common)
  shock_effect_common <- b_common["lag0"] * data_temp$lag0 +
    b_common["lag1"] * data_temp$lag1 +
    b_common["lag2"] * data_temp$lag2
  DRExplained_common <- mean(shock_effect_common[data_temp$D==1], na.rm=TRUE) -
    mean(shock_effect_common[data_temp$R==1], na.rm=TRUE)
  
  # === Explained gap (Party-Specific) ===
  b_party <- coef(model_party)
  shock_effect_party <- b_party["lag0"] * data_temp$lag0 +
    b_party["lag1"] * data_temp$lag1 +
    b_party["lag2"] * data_temp$lag2 +
    b_party["D_lag0"] * data_temp$D_lag0 +
    b_party["D_lag1"] * data_temp$D_lag1 +
    b_party["D_lag2"] * data_temp$D_lag2
  
  DRExplained_party <- mean(shock_effect_party[data_temp$D==1], na.rm=TRUE) -
    mean(shock_effect_party[data_temp$R==1], na.rm=TRUE)
  
  # === Robust SE (Common) ===
  nw_common <- NeweyWest(model_common, lag = 2)
  shock_diff <- mean(data_temp[[shock]][data_temp$D==1]) - mean(data_temp[[shock]][data_temp$R==1])
  gradient_common <- rep(shock_diff, 3)
  var_common <- t(gradient_common) %*% nw_common[2:4, 2:4] %*% gradient_common
  se_common <- sqrt(var_common)
  
  # === Robust SE (Party-Specific) ===
  nw_party <- NeweyWest(model_party, lag = 2)
  vcov_names <- rownames(nw_party)
  idx_party <- grep("D_lag", vcov_names)  # vi bruker bare D_lag interaksjoner
  
  if (length(idx_party) > 0) {
    gradient_party <- rep(shock_diff, length(idx_party))
    var_party <- t(gradient_party) %*% nw_party[vcov_names[idx_party], vcov_names[idx_party]] %*% gradient_party
    se_party <- sqrt(var_party)
  } else {
    se_party <- NA
  }
  
  # === Lagre ===
  resultater <- rbind(resultater, data.frame(
    Shock = shock,
    Total_D_R_gap = DRTotal,
    Explained_D_R_gap_Common = DRExplained_common,
    SE_Explained_Common = se_common,
    Explained_D_R_gap_PartySpec = DRExplained_party,
    SE_Explained_Party = se_party,
    Sample_Start = sample_start,
    Sample_End = sample_end,
    N_obs = n_obs
  ))
  
  cat("✅ Ferdig med sjokk:", shock, "\n")
}
