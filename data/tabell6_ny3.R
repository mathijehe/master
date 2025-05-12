# === Pakker ===
library(tidyverse)
library(vars)
library(svars)
library(zoo)
library(dynlm)
library(broom)
library(gt)
library(purrr)
library(lubridate)


# === Data ===
# data8 må inneholde alle serier du vil bruke + kontrollvariabler (z)

data2 <- list(bnp, bnp_int,finans,quarterly_prod,kpi,ravarepriser,data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na() 


# === Definer sjokk du vil generere ===
shock_vars <- c("oil", "intl_bnp_growth", "finans","prod")   # legg inn alle x-variabler du vil lage sjokk for

# === Definer kontrollvariabler (z) ===
control_vars <- c("bnp", "index", "kpi")   # velg dine kontrollvariable

# === Optimal laglengde (du kan evt. hardkode p=6)




# === Lager sjokkene en etter en ===
for (x in shock_vars) {
  
  # Velg relevante variabler
  var_data <- data2 %>%
    dplyr::select(all_of(control_vars), all_of(x))
  
  # Identifiser radene med komplette data
  complete_rows <- complete.cases(var_data)
  var_data_clean <- var_data[complete_rows, ]
  
  # Velg optimal laglengde
  lag_selection <- VARselect(var_data_clean, lag.max = 8, type = "const")
  optimal_p <- lag_selection$selection["AIC(n)"]
  
  # Estimer VAR-modellen
  var_model <- VAR(var_data_clean, p = optimal_p, type = "const")
  
  # Stabilitetssjekk
  røtter <- roots(var_model)
  if (any(Mod(røtter) >= 1)) {
    warning(paste("⚠️ VAR-modellen for", x, "er IKKE stabil!"))
  } else {
    cat(paste("✅ VAR-modellen for", x, "er stabil.\n"))
  }
  
  # Hent residualene
  shock_series <- residuals(var_model)[, x]
  
  # === Riktig plassering i data2:
  # Vi bruker bare radene der var_data var komplett, OG fjerner de første `p` observasjonene som går tapt i VAR
  valid_rows_in_data2 <- which(complete_rows)[(optimal_p + 1):length(complete_rows)]
  
  # Lag en vektor med NA og legg inn sjokkene på rett sted
  shock_vector <- rep(NA, nrow(data2))
  shock_vector[valid_rows_in_data2] <- shock_series
  
  # Legg til sjokk i data_shocks
  data2[[paste0(x, "_shock")]] <- shock_vector
  
  cat("✅ Ferdig med sjokk:", x, "\n")
}





data6 <- list(forvent, bnp, kpi, ravarepriser)
data6 <-  reduce(data6, full_join, by="year") 

data6 <- data6 %>% 
  arrange(year) %>% 
  drop_na() 
  

# === Sjokkvariabler i data6 ===
shock_vars <- c("trend")  

# === Kontrollvariabler i VAR-systemet ===
control_vars <- c("bnp", "oil", "index", "kpi")  

# === Loop over sjokkvariabler ===
for (x in shock_vars) {
  
  # --- Sett opp VAR-data med x sist
  var_data <- data6 %>%
    dplyr::select(all_of(control_vars), all_of(x)) %>%
    drop_na()
  
  # --- Velg optimal laglengde
  lag_selection <- VARselect(var_data, lag.max = 8, type = "const")
  optimal_p <- lag_selection$selection["AIC(n)"]
  
  # --- Estimer VAR-modellen
  var_model <- VAR(var_data, p = optimal_p, type = "const")
  
  # --- Sjekk stabilitet
  røtter <- roots(var_model)
  if (any(Mod(røtter) >= 1)) {
    warning(paste("⚠️ VAR-modellen for", x, "er IKKE stabil!"))
  } else {
    cat(paste("✅ VAR-modellen for", x, "er stabil.\n"))
  }
  
  # --- Hent residualer (sjokk)
  shock_series <- residuals(var_model)[, x]
  
  # --- Legg sjokket inn i data6 (etter trimming)
  data6 <- data6 %>%
    slice((optimal_p + 1):n()) %>%
    mutate(!!paste0(x, "_shock") := shock_series)
  
  cat(paste("✅ Ferdig med sjokk:", x, "\n"))
}





data4 <- list(styringsrente3,bnp, kpi,ravarepriser)
data4 <-  reduce(data4, full_join, by="year") 

data4 <- data4 %>% 
  arrange(year) %>% 
  drop_na()

# === Liste over sjokkvariabler for SVAR ===
svar_list <- list(
  list(name = "monetary", var = c("bnp", "kpi", "index", "rente"), shock_var = "rente")
)

# === Initialiser liste for sjokkserier og databehandling ===
svar_shocks <- list()

for (svar in svar_list) {
  
  # === Datasett ===
  svar_data <- data4 %>%
    dplyr::select(all_of(svar$var)) %>%
    drop_na()
  
  # === Velg optimal laglengde med AIC ===
  lag_selection <- VARselect(svar_data, lag.max = 8, type = "const")
  optimal_p <- lag_selection$selection["AIC(n)"]
  
  # === Estimer SVAR ===
  svar_model <- VAR(svar_data, p = optimal_p, type = "const")
  
  # === Sjekk stabilitet ===
  stability <- roots(svar_model, modulus = TRUE)
  if (any(stability >= 1)) {
    warning(paste("⚠️ VAR-modellen for", svar$name, "er ikke stabil!"))
  } else {
    cat(paste("✅ SVAR-modellen for", svar$name, "er stabil.\n"))
  }
  
  # === Hent residualer ===
  svar_residuals <- residuals(svar_model)
  
  # === Cholesky-dekomponering ===
  B_chol <- t(chol(cov(svar_residuals)))
  
  # === Strukturelle sjokk ===
  u_t <- t(solve(B_chol) %*% t(svar_residuals))
  
  # === Hent riktig sjokk (siste variabel i rekkefølgen) ===
  shock_series <- u_t[, ncol(u_t)]
  
  # === Legg til i datasettet ===
  data4 <- data4 %>%
    slice((optimal_p + 1):n()) %>%
    mutate(!!paste0(svar$name, "_shock") := shock_series)
  
  cat(paste("🎯 Ferdig med sjokk:", svar$name, "\n"))
}




data8 <- list(data2,data4,data6) %>%
  reduce(full_join, by = c("year","kpi","index","bnp","oil")) %>% 
  filter(year >= as.Date("1978-04-01") & year <= as.Date("2021-07-01"))





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


# Start på nytt: tom tabell
resultater <- data.frame(
  Shock = character(0),
  Total_D_R_gap = numeric(0),
  Explained_D_R_gap_Common = numeric(0),
  SE_Explained_Common = numeric(0),
  Explained_D_R_gap_PartySpec = numeric(0),
  SE_Explained_Party = numeric(0),
  P_Value_FTest = numeric(0),
  Sample_Start = as.Date(character(0)),
  Sample_End = as.Date(character(0)),
  N_obs = integer(0),
  stringsAsFactors = FALSE
)

# Loop over sjokk
for (shock in shock_list) {
  
  # Dataoppsett
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
    slice(3:n())
  
  # Sample info
  sample_start <- min(data_temp$year)
  sample_end   <- max(data_temp$year)
  n_obs        <- nrow(data_temp)
  
  # Zoo
  data_zoo <- zoo(data_temp[, c("bnp", "D", "R", "lag0", "lag1", "lag2", "D_lag0", "D_lag1", "D_lag2")],
                  order.by = data_temp$year)
  
  # Common modell
  model_common <- dynlm(bnp ~ lag0 + lag1 + lag2, data = data_zoo)
  
  # Party-specific modell (uten intercept)
  model_party <- dynlm(bnp ~ 0 + D + R + lag0 + lag1 + lag2 + D_lag0 + D_lag1 + D_lag2, data = data_zoo)
  
  # Gap totalt
  DRTotal <- mean(data_temp$bnp[data_temp$D == 1]) - mean(data_temp$bnp[data_temp$R == 1])
  
  # Explained common
  b_common <- coef(model_common)
  shock_effect_common <- b_common["lag0"] * data_temp$lag0 +
    b_common["lag1"] * data_temp$lag1 +
    b_common["lag2"] * data_temp$lag2
  DRExplained_common <- mean(shock_effect_common[data_temp$D == 1]) -
    mean(shock_effect_common[data_temp$R == 1])
  
  # Explained party-specific
  b_party <- coef(model_party)
  shock_effect_party <- b_party["lag0"] * data_temp$lag0 +
    b_party["lag1"] * data_temp$lag1 +
    b_party["lag2"] * data_temp$lag2 +
    b_party["D_lag0"] * data_temp$D_lag0 +
    b_party["D_lag1"] * data_temp$D_lag1 +
    b_party["D_lag2"] * data_temp$D_lag2
  DRExplained_party <- mean(shock_effect_party[data_temp$D == 1]) -
    mean(shock_effect_party[data_temp$R == 1])
  
  # SE: common
  nw_common <- NeweyWest(model_common, lag = 2)
  shock_diff <- mean(data_temp[[shock]][data_temp$D == 1]) - mean(data_temp[[shock]][data_temp$R == 1])
  gradient_common <- rep(shock_diff, 3)
  var_common <- t(gradient_common) %*% nw_common[2:4, 2:4] %*% gradient_common
  se_common <- sqrt(var_common)
  
  # SE: party-specific
  nw_party <- NeweyWest(model_party, lag = 2)
  vcov_names <- rownames(nw_party)
  idx_party <- grep("D_lag", vcov_names)
  if (length(idx_party) > 0) {
    gradient_party <- rep(shock_diff, length(idx_party))
    var_party <- t(gradient_party) %*% nw_party[vcov_names[idx_party], vcov_names[idx_party]] %*% gradient_party
    se_party <- sqrt(var_party)
  } else {
    se_party <- NA
  }
  
  # F-test
  f_test <- linearHypothesis(
    model_party,
    c("D_lag0 = 0", "D_lag1 = 0", "D_lag2 = 0"),
    vcov. = nw_party
  )
  p_value <- as.numeric(f_test$`Pr(>F)`[2])
  
  # Lagre i tabell
  resultater <- rbind(resultater, data.frame(
    Shock = shock,
    Total_D_R_gap = DRTotal,
    Explained_D_R_gap_Common = DRExplained_common,
    SE_Explained_Common = se_common,
    Explained_D_R_gap_PartySpec = DRExplained_party,
    SE_Explained_Party = se_party,
    P_Value_FTest = p_value,
    Sample_Start = sample_start,
    Sample_End = sample_end,
    N_obs = n_obs
  ))
  
  cat("✅ Ferdig med sjokk:", shock, " (p-verdi =", round(p_value, 3), ")\n")
}


# === Funksjon for kvartal ===
get_quarter <- function(date) {
  paste0(year(date), ":Q", quarter(date))
}

# === Panel informasjon ===
shock_info <- data.frame(
  Shock = c("oil_shock", "intl_bnp_growth_shock", "finans_shock", "monetary_shock", "trend_shock", "prod_shock"),
  Shock_Type = c("Oil", "International", "Fiscal", "Monetary", "Trend", "Productivity"),
  Panel = c("Panel A: Oil",
            "Panel B: International",
            "Panel C: Fiscal",
            "Panel D: Monetary",
            "Panel E: Trend",
            "Panel F: Productivity"),
  stringsAsFactors = FALSE
)

# === Kombiner resultater ===
tabell <- resultater %>%
  mutate(Shock = gsub("_std", "", Shock)) %>% 
  left_join(shock_info, by = "Shock") %>%
  mutate(
    Sample_Period = paste0(get_quarter(Sample_Start), " – ", get_quarter(Sample_End)),
    Total_D_R_gap = round(Total_D_R_gap, 3),
    Explained_D_R_gap_Common = round(Explained_D_R_gap_Common, 3),
    SE_Explained_Common = round(SE_Explained_Common, 3),
    Explained_D_R_gap_PartySpec = round(Explained_D_R_gap_PartySpec, 3),
    SE_Explained_Party = round(SE_Explained_Party, 3),
    
    `% Explained (Common)` = paste0(round(100 * Explained_D_R_gap_Common / abs(Total_D_R_gap),1), "%"),
    `% Explained (Party-specific)`  = paste0(round(100 * Explained_D_R_gap_PartySpec / abs(Total_D_R_gap),1), "%"),
    
    Explained_Common = paste0(Explained_D_R_gap_Common, " (", SE_Explained_Common, ")"),
    Explained_Party  = paste0(
      Explained_D_R_gap_PartySpec, 
      " (", SE_Explained_Party, ")",
      " [", format.pval(P_Value_FTest, digits = 2, eps = 0.01), "]"
    )
  )



# === Norsk oversettelse ===
tabell <- tabell %>%
  mutate(
    Panel = dplyr::recode(Panel,
                   "Panel A: Oil" = "Panel A: Olje",
                   "Panel B: International" = "Panel B: Internasjonal økonomi",
                   "Panel C: Fiscal" = "Panel C: Finanspolitikk",
                   "Panel D: Monetary" = "Panel D: Pengepolitikk",
                   "Panel E: Trend" = "Panel E: Innbyggerforventninger",
                   "Panel F: Productivity" = "Panel F: Produktivitet"),
    Shock = dplyr::recode(Shock,
                   "oil_shock" = "Prissjokk",
                   "intl_bnp_growth_shock" = "BNP-vekst handelspartnere",
                   "finans_shock" = "Offentlige utgiftsendringer",
                   "monetary_shock" = "Renteendringer",
                   "trend_shock" = "Forventningsendringer",
                   "prod_shock" = "Produktivitetsendringer")
  )

# === Panel-rekkefølge ===
panel_order <- c(
  "Panel A: Olje",
  "Panel B: Internasjonal økonomi",
  "Panel C: Finanspolitikk",
  "Panel D: Pengepolitikk",
  "Panel E: Innbyggerforventninger",
  "Panel F: Produktivitet"
)


tabell_clean <- tabell %>%
  dplyr::select(
    Panel,
    Shock,
    Sample_Period,
    Total_D_R_gap,
    Explained_Common,
    Explained_Party
  )


# === Endelig GT-tabell med forbedringer ===

gt_tab <- tabell_clean %>%
  dplyr::select(-Panel) %>%
  gt() %>%
  
  # === Header ===
  #tab_header(
  #  title = md("**Table 6 — Explaining the S-K Growth Gap**"),
  #  subtitle = md("Shock-specific regressions with common and party-specific effects")
  #) %>%
  
  # === Kolonnenavn ===
  cols_label(
    Shock = "Sjokk",
    Sample_Period = "Tidsperiode",
    Total_D_R_gap = "Total S-K gap",
    Explained_Common = "Felles",
    Explained_Party = "Parti-spesifikk"
  ) %>%
  
  # === SPANNER ===
  tab_spanner(
    label = md("**Forklart S-K gap**"),
    columns = c(Explained_Common, Explained_Party)
  ) %>%
  
  # === Panelet ===
  purrr::reduce(
    rev(panel_order),
    function(tbl, p){
      tbl %>% tab_row_group(label = p, rows = tabell$Panel == p, id = p)
    },
    .init = .
  ) %>%
  
  # === Layout ===
  tab_options(
    table.font.size = 11,
    row_group.as_column = FALSE,
    row_group.font.weight = "bold",
    row_group.border.top.width = px(0.5),
    row_group.border.bottom.width = px(0),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    row_group.padding = px(15),
    column_labels.font.weight = "bold",
    column_labels.border.top.style = "solid",
    column_labels.border.bottom.style = "solid",
    table_body.hlines.style = "none",
    table_body.hlines.width = px(3),
    table_body.hlines.color = "gray",
    data_row.padding = px(2),
    heading.align = "left"
  ) #%>%
  
  # === Fotnote ===
  #tab_source_note(
  #  source_note = md("**Notes:** The table reports decompositions of the S-K growth gap using distributed lag models with Newey-West standard errors. Explained values are given in growth points, with standard errors in parentheses.")
  #)



# === Viser tabellen ===
gt_tab







# === Automatisk tolkning ===
tolkning <- tabell %>%
  mutate(
    Forklart_share_common = round((Explained_D_R_gap_Common / Total_D_R_gap) * 100, 1),
    Forklart_share_party  = round((Explained_D_R_gap_PartySpec / Total_D_R_gap) * 100, 1),
    
    # Tolkning av retning
    Retning_common = ifelse(sign(Explained_D_R_gap_Common) == sign(Total_D_R_gap), "Samme retning", "Motsatt retning"),
    Retning_party  = ifelse(sign(Explained_D_R_gap_PartySpec) == sign(Total_D_R_gap), "Samme retning", "Motsatt retning"),
    
    # Størrelse på forskjellen
    Forskjell_common_vs_party = round(Explained_D_R_gap_Common - Explained_D_R_gap_PartySpec, 3)
  ) %>%
  dplyr::select(
    Shock,
    Total_D_R_gap,
    Explained_D_R_gap_Common,
    Explained_D_R_gap_PartySpec,
    Forklart_share_common,
    Forklart_share_party,
    Retning_common,
    Retning_party,
    Forskjell_common_vs_party
  )

# === Vis resultat ===
tolkning %>%
  gt() %>%
  tab_header(title = md("**Automatisk Tolkning av S-K Gap**")) %>%
  tab_options(table.font.size = 11)


