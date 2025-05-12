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

data2 <- list(bnp, bnp_int,finans,kpi,ravarepriser,data_regjering,quarterly_prod) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()


# === Definer sjokk du vil generere ===
shock_vars <- c("oil", "intl_bnp_growth", "finans","prod")   # legg inn alle x-variabler du vil lage sjokk for

# === Definer kontrollvariabler (z) ===
control_vars <- c("bnp", "index", "kpi")   # velg dine kontrollvariable

# === Optimal laglengde (du kan evt. hardkode p=6)

optimal_p <- 3   # Artikkelen bruker 6



# === Lager sjokkene en etter en ===
for (x in shock_vars) {
  
  # Sett opp (z, x)-VAR med x sist
  var_data <- data2 %>%
    dplyr::select(all_of(control_vars), all_of(x)) %>%
    drop_na()
  
  # Estimer VAR
  var_model <- VAR(var_data, p = optimal_p, type = "const")
  
  # Hent residualen som sjokk
  shock_series <- residuals(var_model)[, x]
  
  # Legg sjokk inn i data8 (trimmer de første p observasjoner)
  data2 <- data2 %>%
    slice((optimal_p+1):n()) %>%    # matcher var_data som mister p observasjoner
    mutate(!!paste0(x, "_shock") := shock_series)
  
  print(paste("Laget sjokk for", x))
}





data6 <- list(forvent, bnp, kpi, ravarepriser)
data6 <-  reduce(data6, full_join, by="year") 

data6 <- data6 %>% 
  arrange(year) %>% 
  drop_na()

# Velg variabler som inngår i (z, x)-VAR
var_data <- data6 %>%
  dplyr::select(bnp, oil,index, kpi, trend) %>%
  drop_na()

# Estimer optimal laglengde (som artikkelen bruker 6 kvartaler)
lag_selection <- VARselect(var_data, lag.max = 8, type = "const")
optimal_p <- lag_selection$selection["AIC(n)"]

# Estimer VAR-modell med optimal laglengde
var_model <- VAR(var_data, p = optimal_p, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_trend <- residuals(var_model)[, "trend"]


# Legg sjokkene til datasettet
data6 <- data6 %>%
  slice((optimal_p+1):n()) %>%  # Dropper første observasjoner pga lags
  mutate(
    trend_shock = shock_trend
  )





data4 <- list(styringsrente3,bnp, kpi,ravarepriser)
data4 <-  reduce(data4, full_join, by="year") 

data4 <- data4 %>% 
  arrange(year) %>% 
  drop_na()

svar_data <- data4 %>%
  dplyr::select(bnp, kpi, index, rente) %>%
  drop_na()

VARselect(svar_data, lag.max = 6, type = "const")

# === Estimer SVAR ===
svar_model <- VAR(svar_data, p = 3, type = "const")

svar_residuals <- residuals(svar_model)

# === Riktig Cholesky ===
B_chol <- t(chol(cov(svar_residuals)))  # Bruk NEDRE trekantmatrise

# === Strukturelle sjokk ===
u_t <- t(solve(B_chol) %*% t(svar_residuals))  # legg merke til transposisjon før og etter


# === Monetary policy shock ===
monetary_shock <- u_t[, ncol(u_t)]


# 6. Legg til i data8
data4 <- data4 %>%
  slice((3+1):n()) %>%
  mutate(monetary_shock = monetary_shock) 





data8 <- list(data2,data4,data6) %>%
  reduce(full_join, by = c("year","kpi","index","bnp","oil"))





# === Lag datasett med sjokkene ===
shock_list <- c("oil_shock", "intl_bnp_growth_shock", "finans_shock",
                "monetary_shock","trend_shock", "prod_shock")


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


# Funksjon for å lage kvartal
get_quarter <- function(date) {
  paste0(year(date), ":Q", quarter(date))
}

# === Definer panel/grupper for shocks ===
shock_info <- data.frame(
  Shock = c("oil_shock", "intl_bnp_growth_shock", "finans_shock", "monetary_shock", "trend_shock","prod_shock"),
  Shock_Type = c("Oil", "International", "Fiscal", "Monetary", "Trend","prod"),
  stringsAsFactors = FALSE
)



tabell <- tabell %>%
  mutate(
    Panel = factor(Panel, levels = c(
      "Panel A: Oil",
      "Panel B: International",
      "Panel C: Fiscal",
      "Panel D: Monetary",
      "Panel E: Trend",
      "Panel F: Prod"
    ))
  ) %>%
  arrange(Panel)

tabell <- resultater %>%
  left_join(shock_info, by = "Shock") %>%
  mutate(
    Sample_Period = paste0(get_quarter(Sample_Start), "–", get_quarter(Sample_End)),
    Total_D_R_gap = round(Total_D_R_gap, 3),
    Explained_D_R_gap = round(Explained_D_R_gap, 3),
    SE_Explained = round(SE_Explained, 3),
    Explained = paste0(Explained_D_R_gap, " (", SE_Explained, ")")
  ) %>%
  dplyr::select(
    Panel,
    Shock,
    Sample_Period,
    N_obs,
    Total_D_R_gap,
    Explained
  )

# === Sett riktig rekkefølge og sorter ===
panel_order <- c(
  "Panel A: Oil",
  "Panel B: International",
  "Panel C: Fiscal",
  "Panel D: Monetary",
  "Panel E: Trend",
  "Panel F: Prod"
)

tabell <- tabell %>%
  mutate(Panel = factor(Panel, levels = panel_order)) %>%
  arrange(Panel)

# === Lag gt-tabellen ===
gt_tab <- tabell %>%
  dplyr::select(-Panel) %>%
  gt() %>%
  tab_header(
    title = md("**Table 6: Explaining the S-K Growth Gap**"),
    subtitle = "Shock-specific regressions"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  purrr::reduce(
    rev(panel_order),   # ← SNU rekkefølgen her!!
    function(tbl, p){
      tbl %>% tab_row_group(group = p, rows = tabell$Panel == p, id = p)
    },
    .init = .
  ) %>%
  tab_options(table.font.size = 12, table.width = pct(100), heading.align = "left")




gt_tab


























--------------------------------------
  
  tabell <- resultater %>%
  left_join(shock_info, by = "Shock") %>%
  mutate(
    Sample_Period = paste0(get_quarter(Sample_Start), "–", get_quarter(Sample_End)),
    Total_D_R_gap = round(Total_D_R_gap, 3),
    Explained_D_R_gap = round(Explained_D_R_gap, 3),
    SE_Explained = round(SE_Explained, 3),
    Explained = paste0(Explained_D_R_gap, " (", SE_Explained, ")")
  ) %>%
  dplyr::select(
    Panel,
    Shock,
    Sample_Period,
    N_obs,
    Total_D_R_gap,
    Explained
  )
  
  # === Lag % Explained og fargekode ===
  tabell <- tabell %>%
  mutate(
    Explained_Percent = 100 * Explained_D_R_gap / abs(Total_D_R_gap),
    Explained_Percent_Label = sprintf("%.1f%%", Explained_Percent)
  )

# === Lag gt-tabell ===
gt_tab <- tabell %>%
  select(-Panel) %>%
  gt() %>%
  tab_header(
    title = md("**Table 6: Explaining the S-K Growth Gap**"),
    subtitle = "Shock-specific regressions"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  
  # === Row groups i korrekt rekkefølge ===
  purrr::reduce(
    rev(panel_order),   # ← OBS! reversert
    function(tbl, p){
      tbl %>% tab_row_group(group = p, rows = tabell$Panel == p, id = p)
    },
    .init = .
  ) %>%
  
  # === Fargekode Explained_Percent ===
  data_color(
    columns = vars(Explained_Percent_Label),
    colors = scales::col_bin(
      palette = c("firebrick", "forestgreen"),
      bins = c(-Inf, 0, Inf)
    )
  ) %>%
  
  # === Fancy options ===
  tab_options(
    table.font.size = 11,
    heading.align = "left",
    data_row.padding = px(4),
    row_group.font.weight = "bold",
    table.border.top.color = "black",
    table.border.bottom.color = "black"
  )





