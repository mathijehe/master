data_regjering <- read_excel("~/Desktop/data-regjering.xlsx")
data_regjering <- data_regjering %>%
  mutate(year = ymd(year))


data_combined <- list(data5, data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# Lag dummy for Høyre (1 hvis høyre, 0 hvis venstre)
data_combined <- data_combined %>%
  mutate(Venstre = ifelse(Regjering == "Venstre", 1, 0))

# Beregn gjennomsnittlig BNP-vekst under hver regjeringstype
bnp_summary <- data_combined %>%
  group_by(Venstre) %>%
  summarise(
    Mean_BNP_Growth = mean(bnp, na.rm = TRUE),
    SD_BNP_Growth = sd(bnp, na.rm = TRUE),
    Count = n()
  )

# Beregn BNP-vekstforskjellen mellom høyre og venstre
bnp_gap_oil <- diff(bnp_summary$Mean_BNP_Growth)


# Velg variabler som inngår i (z, x)-VAR
var_data <- data_combined %>%
  dplyr::select(oil, bnp, index, kpi) %>%
  drop_na()

# Estimer optimal laglengde (som artikkelen bruker 6 kvartaler)
lag_selection <- VARselect(var_data, lag.max = 8, type = "const")
optimal_p <- lag_selection$selection["AIC(n)"]

# Estimer VAR-modell med optimal laglengde
var_model <- VAR(var_data, p = optimal_p, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_oil <- residuals(var_model)[, "oil"]


# Legg sjokkene til datasettet
data_combined <- data_combined %>%
  slice((optimal_p+1):n()) %>%  # Dropper første observasjoner pga lags
  mutate(
    oil_shock = shock_oil
  )

# Modell uten olje-sjokk
model1 <- lm(bnp ~ Venstre, data = data_combined)
bnp_gap_uten_shock_oil <- coef(model1)["Venstre"]

# Modell med olje-sjokk
model2 <- lm(bnp ~ Venstre + oil_shock, data = data_combined)
bnp_gap_med_shock_oil <- coef(model2)["Venstre"]

# Forklart BNP-gap
explained_gap_oil <- bnp_gap_uten_shock_oil - bnp_gap_med_shock_oil
explained_gap_se_oil <- coeftest(model2, vcov. = vcovHAC(model2))["oil_shock", 2]

#  Generer tabellen
results_table <- data.frame(
  Shock = c("Oil Shock"),
  Sample_Period = "XXXX-YYYY",  # Sett riktig tidsperiode
  Total_Gap = sprintf("%.3f (%.3f)", bnp_gap_uten_shock, sd(data_combined$bnp)), 
  Explained_Gap = sprintf("%.3f (%.3f)", explained_gap, explained_gap_se)
)

#  Formater tabellen med GT
results_table %>%
  gt() %>%
  tab_header(
    title = "Explaining the Partisan BNP Growth Gap"
  ) %>%
  cols_label(
    Shock = "Shock",
    Sample_Period = "Sample Period",
    Total_Gap = "Total H-V gap",
    Explained_Gap = "Explained BNP-gap (Newey-West SE)"
  )
















data_regjering <- read_excel("~/Desktop/data-regjering.xlsx")
data_regjering <- data_regjering %>%
  mutate(year = ymd(year))


data_combined <- list(bnp_int, data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# Lag dummy for Høyre (1 hvis høyre, 0 hvis venstre)
data_combined <- data_combined %>%
  mutate(Venstre = ifelse(Regjering == "Venstre", 1, 0))

# Beregn gjennomsnittlig BNP-vekst under hver regjeringstype
bnp_summary <- data_combined %>%
  group_by(Venstre) %>%
  summarise(
    Mean_BNP_Growth = mean(bnp, na.rm = TRUE),
    SD_BNP_Growth = sd(bnp, na.rm = TRUE),
    Count = n()
  )

# Beregn BNP-vekstforskjellen mellom høyre og venstre
bnp_gap_int <- diff(bnp_summary$Mean_BNP_Growth)


# Velg variabler som inngår i (z, x)-VAR
var_data <- bnp_int %>%
  dplyr::select(Belgium, Denmark, France, Germany, 
                Italy, Netherlands, Sweden, 
                `United Kingdom`, `United States`) %>%
  drop_na()

weights <- c(Belgium = 0.07, Denmark = 0.04, France = 0.10, 
             Germany = 0.30, Italy = 0.02, Netherlands = 0.09, `United Kingdom`=0.25,
             `United States`=0.07, Sweden=0.10)

bnp_int$intl_bnp_growth <- rowSums(var_data[, names(weights)] * weights[colnames(var_data)])

var_data <- bnp_int %>%
  dplyr::select(intl_bnp_growth, bnp, index, oil, kpi)

# Estimer optimal laglengde (som artikkelen bruker 6 kvartaler)
lag_selection <- VARselect(var_data, lag.max = 8, type = "const")
optimal_p <- lag_selection$selection["AIC(n)"]

# Estimer VAR-modell med optimal laglengde
var_model <- VAR(var_data, p = optimal_p, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_int <- residuals(var_model)[, "intl_bnp_growth"]


# Legg sjokkene til datasettet
data_combined <- data_combined %>%
  slice((optimal_p+1):n()) %>%  # Dropper første observasjoner pga lags
  mutate(
    int_shock = shock_int
  )

# Modell uten olje-sjokk
model1 <- lm(bnp ~ Venstre, data = data_combined)
bnp_gap_uten_shock_int <- coef(model1)["Venstre"]

# Modell med olje-sjokk
model2 <- lm(bnp ~ Venstre + int_shock, data = data_combined)
bnp_gap_med_shock_int <- coef(model2)["Venstre"]

# Forklart BNP-gap
explained_gap_int <- bnp_gap_uten_shock_int - bnp_gap_med_shock_int
explained_gap_se_int <- coeftest(model2, vcov. = vcovHAC(model2))["int_shock", 2]

# 📌 Generer tabellen
results_table <- data.frame(
  Shock = c("int Shock"),
  Sample_Period = "XXXX-YYYY",  # Sett riktig tidsperiode
  Total_Gap = sprintf("%.3f (%.3f)", bnp_gap_uten_shock, sd(data_combined$bnp)), 
  Explained_Gap = sprintf("%.3f (%.3f)", explained_gap, explained_gap_se)
)

# 📌 Formater tabellen med GT
results_table %>%
  gt() %>%
  tab_header(
    title = "Explaining the Partisan BNP Growth Gap"
  ) %>%
  cols_label(
    Shock = "Shock",
    Sample_Period = "Sample Period",
    Total_Gap = "Total H-V gap",
    Explained_Gap = "Explained BNP-gap (Newey-West SE)"
  )






data_regjering <- read_excel("~/Desktop/data-regjering.xlsx")
data_regjering <- data_regjering %>%
  mutate(year = ymd(year))


data_combined <- list(data9, data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# Lag dummy for Høyre (1 hvis høyre, 0 hvis venstre)
data_combined <- data_combined %>%
  mutate(Venstre = ifelse(Regjering == "Venstre", 1, 0))

# Beregn gjennomsnittlig BNP-vekst under hver regjeringstype
bnp_summary <- data_combined %>%
  group_by(Venstre) %>%
  summarise(
    Mean_BNP_Growth = mean(bnp, na.rm = TRUE),
    SD_BNP_Growth = sd(bnp, na.rm = TRUE),
    Count = n()
  )

# Beregn BNP-vekstforskjellen mellom høyre og venstre
bnp_gap_finans <- diff(bnp_summary$Mean_BNP_Growth)


# Velg variabler som inngår i (z, x)-VAR
var_data <- data_combined %>%
  dplyr::select(finans, bnp, index, kpi,oil) %>%
  drop_na()

# Estimer optimal laglengde (som artikkelen bruker 6 kvartaler)
lag_selection <- VARselect(var_data, lag.max = 8, type = "const")
optimal_p <- lag_selection$selection["AIC(n)"]

# Estimer VAR-modell med optimal laglengde
var_model <- VAR(var_data, p = optimal_p, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_finans <- residuals(var_model)[, "finans"]


# Legg sjokkene til datasettet
data_combined <- data_combined %>%
  slice((optimal_p+1):n()) %>%  # Dropper første observasjoner pga lags
  mutate(
    finans_shock = shock_finans
  )

# Modell uten olje-sjokk
model1 <- lm(bnp ~ Venstre, data = data_combined)
bnp_gap_uten_shock_finans <- coef(model1)["Venstre"]

# Modell med olje-sjokk
model2 <- lm(bnp ~ Venstre + finans_shock, data = data_combined)
bnp_gap_med_shock_finans <- coef(model2)["Venstre"]

# Forklart BNP-gap
explained_gap_finans <- bnp_gap_uten_shock_finans - bnp_gap_med_shock_finans
explained_gap_se_finans <- coeftest(model2, vcov. = vcovHAC(model2))["finans_shock", 2]

# 📌 Generer tabellen
results_table <- data.frame(
  Shock = c("Oil Shock"),
  Sample_Period = "XXXX-YYYY",  # Sett riktig tidsperiode
  Total_Gap = sprintf("%.3f (%.3f)", bnp_gap_uten_shock, sd(data_combined$bnp)), 
  Explained_Gap = sprintf("%.3f (%.3f)", explained_gap, explained_gap_se)
)

# 📌 Formater tabellen med GT
results_table %>%
  gt() %>%
  tab_header(
    title = "Explaining the Partisan BNP Growth Gap"
  ) %>%
  cols_label(
    Shock = "Shock",
    Sample_Period = "Sample Period",
    Total_Gap = "Total H-V gap",
    Explained_Gap = "Explained BNP-gap (Newey-West SE)"
  )









data_regjering <- read_excel("~/Desktop/data-regjering.xlsx")
data_regjering <- data_regjering %>%
  mutate(year = ymd(year))


data_combined <- list(data8, data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# Lag dummy for Høyre (1 hvis høyre, 0 hvis venstre)
data_combined <- data_combined %>%
  mutate(Venstre = ifelse(Regjering == "Venstre", 1, 0))

# Beregn gjennomsnittlig BNP-vekst under hver regjeringstype
bnp_summary <- data_combined %>%
  group_by(Venstre) %>%
  summarise(
    Mean_BNP_Growth = mean(bnp, na.rm = TRUE),
    SD_BNP_Growth = sd(bnp, na.rm = TRUE),
    Count = n()
  )

# Beregn BNP-vekstforskjellen mellom høyre og venstre
bnp_gap_trend <- diff(bnp_summary$Mean_BNP_Growth)


# Velg variabler som inngår i (z, x)-VAR
var_data <- data_combined %>%
  dplyr::select(trend, bnp, index, kpi, oil) %>%
  drop_na()

# Estimer optimal laglengde (som artikkelen bruker 6 kvartaler)
lag_selection <- VARselect(var_data, lag.max = 8, type = "const")
optimal_p <- lag_selection$selection["AIC(n)"]

# Estimer VAR-modell med optimal laglengde
var_model <- VAR(var_data, p = optimal_p, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_forv <- residuals(var_model)[, "trend"]


# Legg sjokkene til datasettet
data_combined <- data_combined %>%
  slice((optimal_p+1):n()) %>%  # Dropper første observasjoner pga lags
  mutate(
    forv_shock = shock_forv
  )

# Modell uten olje-sjokk
model1 <- lm(bnp ~ Venstre, data = data_combined)
bnp_gap_uten_shock_trend <- coef(model1)["Venstre"]

# Modell med olje-sjokk
model2 <- lm(bnp ~ Venstre + forv_shock, data = data_combined)
bnp_gap_med_shock_trend <- coef(model2)["Venstre"]

# Forklart BNP-gap
explained_gap_trend <- bnp_gap_uten_shock_trend - bnp_gap_med_shock_trend
explained_gap_se_trend <- coeftest(model2, vcov. = vcovHAC(model2))["forv_shock", 2]

# 📌 Generer tabellen
results_table <- data.frame(
  Shock = c("forve Shock"),
  Sample_Period = "XXXX-YYYY",  # Sett riktig tidsperiode
  Total_Gap = sprintf("%.3f (%.3f)", bnp_gap_uten_shock, sd(data_combined$bnp)), 
  Explained_Gap = sprintf("%.3f (%.3f)", explained_gap, explained_gap_se)
)

# 📌 Formater tabellen med GT
results_table %>%
  gt() %>%
  tab_header(
    title = "Explaining the Partisan BNP Growth Gap"
  ) %>%
  cols_label(
    Shock = "Shock",
    Sample_Period = "Sample Period",
    Total_Gap = "Total H-V gap",
    Explained_Gap = "Explained BNP-gap (Newey-West SE)"
  )










data_regjering <- read_excel("~/Desktop/data-regjering.xlsx")
data_regjering <- data_regjering %>%
  mutate(year = ymd(year))


data_combined <- list(data4, data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# Lag dummy for Høyre (1 hvis høyre, 0 hvis venstre)
data_combined <- data_combined %>%
  mutate(Venstre = ifelse(Regjering == "Venstre", 1, 0))

# Beregn gjennomsnittlig BNP-vekst under hver regjeringstype
bnp_summary <- data_combined %>%
  group_by(Venstre) %>%
  summarise(
    Mean_BNP_Growth = mean(bnp, na.rm = TRUE),
    SD_BNP_Growth = sd(bnp, na.rm = TRUE),
    Count = n()
  )

# Beregn BNP-vekstforskjellen mellom høyre og venstre
bnp_gap_rente <- diff(bnp_summary$Mean_BNP_Growth)


# Velg variabler som inngår i (z, x)-VAR
var_data <- data_combined %>%
  dplyr::select(rente, bnp, index, kpi, oil) %>%
  drop_na()

# Estimer optimal laglengde (som artikkelen bruker 6 kvartaler)
lag_selection <- VARselect(var_data, lag.max = 8, type = "const")
optimal_p <- lag_selection$selection["AIC(n)"]

# Estimer VAR-modell med optimal laglengde
var_model <- VAR(var_data, p = optimal_p, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_rente <- residuals(var_model)[, "rente"]


# Legg sjokkene til datasettet
data_combined <- data_combined %>%
  slice((optimal_p+1):n()) %>%  # Dropper første observasjoner pga lags
  mutate(
    rente_shock = shock_rente
  )

# Modell uten olje-sjokk
model1 <- lm(bnp ~ Venstre, data = data_combined)
bnp_gap_uten_shock_rente <- coef(model1)["Venstre"]

# Modell med olje-sjokk
model2 <- lm(bnp ~ Venstre + rente_shock, data = data_combined)
bnp_gap_med_shock_rente <- coef(model2)["Venstre"]

# Forklart BNP-gap
explained_gap_rente <- bnp_gap_uten_shock_rente - bnp_gap_med_shock_rente
explained_gap_se_rente <- coeftest(model2, vcov. = vcovHAC(model2))["rente_shock", 2]

# 📌 Generer tabellen
results_table <- data.frame(
  Shock = c("rente Shock"),
  Sample_Period = "XXXX-YYYY",  # Sett riktig tidsperiode
  Total_Gap = sprintf("%.3f (%.3f)", bnp_gap_uten_shock, sd(data_combined$bnp)), 
  Explained_Gap = sprintf("%.3f (%.3f)", explained_gap, explained_gap_se)
)

# 📌 Formater tabellen med GT
results_table %>%
  gt() %>%
  tab_header(
    title = "Explaining the Partisan BNP Growth Gap"
  ) %>%
  cols_label(
    Shock = "Shock",
    Sample_Period = "Sample Period",
    Total_Gap = "Total H-V gap",
    Explained_Gap = "Explained BNP-gap (Newey-West SE)"
  )









data_regjering <- read_excel("~/Desktop/data-regjering.xlsx", sheet = 2)



data_combined <- list(data6, data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# Lag dummy for Høyre (1 hvis høyre, 0 hvis venstre)
data_combined <- data_combined %>%
  mutate(Venstre = ifelse(Regjering == "Venstre", 1, 0))

# Beregn gjennomsnittlig BNP-vekst under hver regjeringstype
bnp_summary <- data_combined %>%
  group_by(Venstre) %>%
  summarise(
    Mean_BNP_Growth = mean(BNP, na.rm = TRUE),
    SD_BNP_Growth = sd(BNP, na.rm = TRUE),
    Count = n()
  )

# Beregn BNP-vekstforskjellen mellom høyre og venstre
bnp_gap_prod <- diff(bnp_summary$Mean_BNP_Growth)


# Velg variabler som inngår i (z, x)-VAR
var_data <- data_combined %>%
  dplyr::select(prod, BNP) %>%
  drop_na()

# Estimer optimal laglengde (som artikkelen bruker 6 kvartaler)
lag_selection <- VARselect(var_data, lag.max = 8, type = "const")
optimal_p <- lag_selection$selection["AIC(n)"]

# Estimer VAR-modell med optimal laglengde
var_model <- VAR(var_data, p = optimal_p, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_prod <- residuals(var_model)[, "prod"]


# Legg sjokkene til datasettet
data_combined <- data_combined %>%
  slice((optimal_p+1):n()) %>%  # Dropper første observasjoner pga lags
  mutate(
    prod_shock = shock_prod
  )

# Modell uten olje-sjokk
model1 <- lm(BNP ~ Venstre, data = data_combined)
bnp_gap_uten_shock_prod <- coef(model1)["Venstre"]

# Modell med olje-sjokk
model2 <- lm(BNP ~ Venstre + prod_shock, data = data_combined)
bnp_gap_med_shock_prod <- coef(model2)["Venstre"]

# Forklart BNP-gap
explained_gap_prod <- bnp_gap_uten_shock_prod - bnp_gap_med_shock_prod
explained_gap_se_prod <- coeftest(model2, vcov. = vcovHAC(model2))["prod_shock", 2]

# 📌 Generer tabellen
results_table <- data.frame(
  Shock = c("forve Shock"),
  Sample_Period = "XXXX-YYYY",  # Sett riktig tidsperiode
  Total_Gap = sprintf("%.3f (%.3f)", bnp_gap_uten_shock, sd(data_combined$BNP)), 
  Explained_Gap = sprintf("%.3f (%.3f)", explained_gap, explained_gap_se)
)

# 📌 Formater tabellen med GT
results_table %>%
  gt() %>%
  tab_header(
    title = "Explaining the Partisan BNP Growth Gap"
  ) %>%
  cols_label(
    Shock = "Shock",
    Sample_Period = "Sample Period",
    Total_Gap = "Total H-V gap",
    Explained_Gap = "Explained BNP-gap (Newey-West SE)"
  )









all_results <- list(
  results_oil = data.frame(
    Shock = "Oil Shock",
    Sample_Period = "XXXX-YYYY",
    Total_Gap = sprintf("%.3f (%.3f)", bnp_gap_uten_shock_oil, sd(data_combined$bnp)),
    Explained_Gap = sprintf("%.3f (%.3f)", explained_gap_oil, explained_gap_se_oil)
  ),
  
  results_int = data.frame(
    Shock = "International Shock",
    Sample_Period = "XXXX-YYYY",
    Total_Gap = sprintf("%.3f (%.3f)", bnp_gap_uten_shock_int, sd(data_combined$bnp)),
    Explained_Gap = sprintf("%.3f (%.3f)", explained_gap_int, explained_gap_se_int)
  ),
  
  results_finans = data.frame(
    Shock = "Financial Shock",
    Sample_Period = "XXXX-YYYY",
    Total_Gap = sprintf("%.3f (%.3f)", bnp_gap_uten_shock_finans, sd(data_combined$bnp)),
    Explained_Gap = sprintf("%.3f (%.3f)", explained_gap_finans, explained_gap_se_finans)
  ),
  
  results_forv = data.frame(
    Shock = "Expectation Shock",
    Sample_Period = "XXXX-YYYY",
    Total_Gap = sprintf("%.3f (%.3f)", bnp_gap_uten_shock_trend, sd(data_combined$bnp)),
    Explained_Gap = sprintf("%.3f (%.3f)", explained_gap_trend, explained_gap_se_trend)
  ),
  
  results_rente = data.frame(
    Shock = "Interest Rate Shock",
    Sample_Period = "XXXX-YYYY",
    Total_Gap = sprintf("%.3f (%.3f)", bnp_gap_uten_shock_rente, sd(data_combined$bnp)),
    Explained_Gap = sprintf("%.3f (%.3f)", explained_gap_rente, explained_gap_se_rente)
  ),
  
  results_prod = data.frame(
    Shock = "Productivity Shock",
    Sample_Period = "XXXX-YYYY",
    Total_Gap = sprintf("%.3f (%.3f)", bnp_gap_uten_shock_prod, sd(data_combined$bnp)),
    Explained_Gap = sprintf("%.3f (%.3f)", explained_gap_prod, explained_gap_se_prod)
  )
)

# 📌 Kombiner alle tabellene til én stor tabell
final_results_table <- bind_rows(all_results)

final_results_table %>%
  gt() %>%
  tab_header(
    title = "Explaining the Partisan BNP Growth Gap"
  ) %>%
  cols_label(
    Shock = "Shock",
    Sample_Period = "Sample Period",
    Total_Gap = "Total H-V gap",
    Explained_Gap = "Explained BNP-gap"
  )
