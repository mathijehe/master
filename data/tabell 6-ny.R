
#oljesjokk
adf.test(var_data$intl_bnp_growth)

# Klargjør data (dobbel differensiering av oljeprisen)
var_data <- data5 %>%
  dplyr::select(pris, bnp, exr) %>%
  mutate(pris = c(NA,NA, NA, diff(diff(diff(pris))))) %>%  # Dobbel differensiering
  drop_na()

#  Velg optimalt antall lag
p_opt <- max(VARselect(var_data, lag.max = 12, type = "const")$selection)
p_opt <- max(p_opt, 8)  # Sett minst 8 lag

# Estimer VAR-modellen
var_model <- VAR(var_data, p = p_opt, type = "const")

# Hent residualene
var_residuals <- residuals(var_model)
cor(var_residuals[, "pris"], var_data$pris[(p_opt + 1):nrow(var_data)])
# Definer B-matrisen for SVAR-modellen
bmat <- matrix(c(
  NA,  0,  0,  # Oljepris (påvirkes ikke av BNP eller valutakurs på kort sikt)
  NA, NA,  0,  # BNP (påvirkes av olje, men ikke av valutakurs på kort sikt)
  NA, NA, NA   # Valutakurs (kan påvirke både oljepris og BNP)
), 3, 3, byrow = TRUE)

# Estimer SVAR-modellen med langsiktige restriksjoner
svar_model <- SVAR(var_model, estmethod = "scoring", Bmat = bmat)

# Hent estimerte B-matrise og strukturelle sjokk
B_est <- svar_model$B  
structural_shocks <- t(solve(B_est) %*% t(var_residuals))

# Legg til sjokkene i datasettet
oil_sjokk<- data5 %>% 
  slice(12:n()) %>%   # Bruk riktig p_opt-verdi
  mutate(oil_shock = as.numeric(structural_shocks[, 1]))  # Konverter til numerisk vektor
data6$oil_shock <- data6$oil_shock * sd(bvar_residuals[, "pris"]) + mean(bvar_residuals[, "pris"])

sd(data6$oil_shock)

summary(data6$oil_shock)
cor(data6$oil_shock, data6$pris, use = "complete.obs")

saveRDS(oil_sjokk, "oil_sjokk.rds")

#Rentesjokk

adf.test(data4$rente)

# Klargjør data (dobbel differensiering av oljeprisen)
var_data <- data4 %>%
  dplyr::select(rente, bnp, kpi) %>%
  mutate(rente = c(NA, NA, diff(diff(rente)))) %>%  # Dobbel differensiering
  drop_na()

#  Velg optimalt antall lag
p_opt <- max(VARselect(var_data, lag.max = 12, type = "const")$selection)
p_opt <- max(p_opt, 8)  # Sett minst 8 lag

# Estimer VAR-modellen
var_model <- VAR(var_data, p = p_opt, type = "const")

# Hent residualene
var_residuals <- residuals(var_model)
cor(var_residuals[, "rente"], var_data$rente[(p_opt + 1):nrow(var_data)])
# Definer B-matrisen for SVAR-modellen
bmat <- matrix(c(
  NA,  0,  0,  # Rente påvirkes ikke av BNP eller KPI langsiktig
  NA, NA,  0,  # BNP påvirkes av rente, men ikke av KPI langsiktig
  NA, NA, NA   # KPI påvirkes av både rente og BNP
), 3, 3, byrow = TRUE)

# Estimer SVAR-modellen med langsiktige restriksjoner
svar_model <- SVAR(var_model, estmethod = "scoring", Bmat = bmat)

# Hent estimerte B-matrise og strukturelle sjokk
B_est <- svar_model$B  
structural_shocks <- t(solve(B_est) %*% t(var_residuals))

# Legg til sjokkene i datasettet
rente_sjokk <- data4 %>% 
  slice(11:n()) %>%   # Bruk riktig p_opt-verdi
  mutate(rente_shock = as.numeric(structural_shocks[, 1]))  # Konverter til numerisk vektor

sd(data7$rente_shock)

summary(data6$oil_shock)
cor(data7$rente_shock, data7$rente, use = "complete.obs")

saveRDS(rente_sjokk, "rente_sjokk.rds")



#produktivitet

# Velg variabler for SVAR
var_data <- data6 %>%
  dplyr::select(prod, BNP) %>%
  na.omit()

# velg lags
lag_selection <- VARselect(var_data, lag.max = 8, type = "const")
p_opt <- lag_selection$selection["AIC(n)"]

# Estimer VAR-modellen
var_model <- VAR(var_data, p = p_opt, type = "const")

# Definer langsiktige restriksjoner: kun TFP-sjokk kan ha permanent effekt

bmat <- matrix(c(NA, 0, NA, NA), 2, 2) 

svar_model <- SVAR(var_model, estmethod = "scoring", Bmat = bmat)

var_residuals <- residuals(var_model)

# Multipliser med den estimerte B-matrisen for å få strukturelle sjokk
B_est <- svar_model$B  # Henter estimerte B-matrisen fra SVAR-modellen
structural_shocks <- t(solve(B_est) %*% t(var_residuals))  # Transformerer residualene

prod_sjokk <- data6 %>% 
  slice(4:n()) %>% 
  mutate(prod_shock = as.numeric(structural_shocks[, 1])) 

cor(data6$prod_shock, data6$prod, use = "complete.obs")
sd(data6$prod_shock)

saveRDS(prod_sjokk, "prod_sjokk.rds")



#Forbrukerforventniger
# Klargjør data (dobbel differensiering av oljeprisen)
var_data <- data8 %>%
  dplyr::select(trend, bnp) %>%
  mutate(trend = c(NA,NA ,NA,diff(diff(diff(trend))))) %>%  # Dobbel differensiering
  drop_na()

#  Velg optimalt antall lag
p_opt <- max(VARselect(var_data, lag.max = 12, type = "const")$selection)
p_opt <- max(p_opt,8)  # Sett minst 8 lag

# Estimer VAR-modellen
var_model <- VAR(var_data, p = p_opt, type = "const")

# Hent residualene
var_residuals <- residuals(var_model)
cor(var_residuals[, "trend"], var_data$trend[(p_opt + 1):nrow(var_data)])
# Definer B-matrisen for SVAR-modellen
bmat <- matrix(c(NA, 0, NA, NA), 2, 2)


# Estimer SVAR-modellen med langsiktige restriksjoner
svar_model <- SVAR(var_model, estmethod = "scoring", Bmat = bmat)

# Hent estimerte B-matrise og strukturelle sjokk
B_est <- svar_model$B  
structural_shocks <- t(solve(B_est) %*% t(var_residuals))

# Legg til sjokkene i datasettet
forvet_sjokk <- data8 %>% 
  slice(14:n()) %>%   # Bruk riktig p_opt-verdi
  mutate(rente_shock = as.numeric(structural_shocks[, 1]))  # Konverter til numerisk vektor
cor(data9$rente_shock, data9$trend, use = "complete.obs")
##litt rar


saveRDS(forvet_sjokk, "forvet_sjokk.rds")

#finanspolitikk 
# Klargjør data (dobbel differensiering av oljeprisen)
var_data <- data9 %>%
  dplyr::select(finans, bnp) %>%
  mutate(finans = c(NA, NA, NA, diff(diff(diff(finans))))) %>%  # Dobbel differensiering
  drop_na()

# velg lags
lag_selection <- VARselect(var_data, lag.max = 8, type = "const")
p_opt <- lag_selection$selection["AIC(n)"]

# Estimer VAR-modellen
var_model <- VAR(var_data, p = p_opt, type = "const")

# Definer langsiktige restriksjoner: kun TFP-sjokk kan ha permanent effekt

bmat <- matrix(c(NA, 0, NA, NA), 2, 2) 

svar_model <- SVAR(var_model, estmethod = "scoring", Bmat = bmat)

var_residuals <- residuals(var_model)

# Multipliser med den estimerte B-matrisen for å få strukturelle sjokk
B_est <- svar_model$B  # Henter estimerte B-matrisen fra SVAR-modellen
structural_shocks <- t(solve(B_est) %*% t(var_residuals))  # Transformerer residualene

finans_sjokk <- data9 %>% 
  slice(13:n()) %>% 
  mutate(finans_shock = as.numeric(structural_shocks[, 1])) 

cor(data10$finans_shock, data10$finans, use = "complete.obs")
sd(data10$finans_shock)


saveRDS(finans_sjokk, "finans_sjokk.rds")

#internasjonale sjokk

var_data <- bnp_int %>%
  dplyr::select(Belgium, Canada, Denmark, France, Germany, 
                Italy, Japan, Netherlands, Sweden, 
                `United Kingdom`, `United States`, bnp) %>% 
  mutate(across(everything(), as.numeric))



VARselect(var_data, lag.max = 10, type = "const")

# 2. Estimer BVAR-modellen
bvar_model <- bvar(var_data, lags = 1, priors = bv_priors())

# 3. Hent residualene
bvar_residuals <- residuals(bvar_model)


# 4. Bruk riktig Cholesky dekomponering
B_chol <- t(chol(cov(bvar_residuals)))  # Bruk NEDRE trekantmatrise

# 5. Transformer residualene for å få strukturelle sjokk
bvar_shocks <- solve(B_chol) %*% t(bvar_residuals)  

# 6. Legg til sjokkene i datasettet (sjekk dimensjoner)
int_sjokk <- bnp_int %>% 
  slice(2:n()) %>%
  mutate(int_shock = rowMeans(t(bvar_shocks[1:(nrow(bvar_shocks)-1), ])))

saveRDS(int_sjokk, "int_sjokk.rds")


#settesammen

finans_sjokk <- readRDS("finans_sjokk.rds")
forvet_sjokk <- readRDS("forvet_sjokk.rds")
prod_sjokk <- readRDS("prod_sjokk.rds")
rente_sjokk <- readRDS("rente_sjokk.rds")
oil_sjokk <- readRDS("oil_sjokk.rds")
int_sjokk <- readRDS("int_sjokk.rds")



prod_sjokk <- prod_sjokk %>%
  mutate(year = as.integer(year)) %>%  # Sørg for at 'year' er numerisk
  expand(year, quarter = c(1, 2, 3, 4)) %>%  # Lag kvartalsvise rader
  left_join(prod_sjokk, by = "year") %>%  # Legg til originale data
  mutate(date = ymd(paste0(year, "-", (quarter - 1) * 3 + 1, "-01"))) %>%  # Lag datoer
  dplyr::select(-quarter, -year, -BNP) %>%  # Fjern 'quarter' hvis ikke nødvendig
  fill(everything(), .direction = "down") %>% 
  rename(year=date) # Fyll inn verdier for kvartaler


finans_shock <- finans_sjokk %>% 
  dplyr::select(finans_shock, year)

forven_scock <- forvet_sjokk %>% 
  dplyr::select(forven_scock=rente_shock, year)

prod_shock <- prod_sjokk %>% 
  dplyr::select(prod_shock, year)

rente_shock <- rente_sjokk %>% 
  dplyr::select(rente_shock,year)

oil_shock <- oil_sjokk %>% 
  dplyr::select(oil_shock,year)

int_shock <- int_sjokk %>% 
  dplyr::select(int_shock,year, bnp)


  

data <- list(oil_shock, int_shock,rente_shock,prod_shock, finans_shock, forven_scock)
data <- reduce(data, full_join, by = c("year"))
data <- data %>% arrange(year)
saveRDS(data, "data.rds")
data <- readRDS("data.rds")

library(fixest)


data <- data %>% mutate(panel_id = 1)  # Kunstig gruppe-ID

results <- list()  # Opprett en tom liste for resultatene

for (sjokk in sjokk_var) {
  # Fjern kun NA i den spesifikke sjokkvariabelen, ikke hele datasettet
  data_subset <- data %>% drop_na(bnp, all_of(sjokk))  
  
  # Kjør regresjonen med den filtrerte dataen
  model <- feols(as.formula(paste0("bnp ~ l(", sjokk, ", 0:4)")), 
                 data = data_subset, 
                 panel.id = ~panel_id + year)
  
  results[[sjokk]] <- model  # Lagre modellen
}


library(broom)

# Lag en tom dataframe for tabellen
table_data <- data.frame(
  Shock = character(),
  Total_D_R_Gap = numeric(),
  Explained_D_R_Gap = numeric(),
  SE = numeric(),
  Party_Specific = numeric(),
  SE_Party = numeric()
)

# Hent resultater fra hver regresjon
for (sjokk in names(results)) {
  model <- results[[sjokk]]
  
  # Hent koeffisientene
  coef_est <- coef(model)  
  
  # Hent varians-kovariansmatrisen, men sjekk om den inneholder NA
  vcov_mat <- vcov(model)
  if (any(is.na(vcov_mat))) {
    warning(paste("Advarsel: vcov inneholder NA for sjokk", sjokk))
    next  # Hopp over denne iterasjonen
  }
  
  se_est <- sqrt(diag(vcov_mat))  # Standardfeil
  
  # Summen av koeffisientene for alle lag
  explained_gap <- sum(coef_est[-1], na.rm = TRUE)  # Unngå NA-problemer
  
  # Standardfeil for summen (unngå NA-feil)
  se_gap <- sqrt(sum(se_est[-1]^2, na.rm = TRUE))
  
  # Hvis partispecifikke effekter er med i modellen:
  party_effect <- if ("X:party_dummy" %in% names(coef_est)) coef_est["X:party_dummy"] else NA
  se_party <- if ("X:party_dummy" %in% names(se_est)) se_est["X:party_dummy"] else NA
  
  # Legg til i tabellen
  table_data <- rbind(table_data, data.frame(
    Shock = sjokk,
    Total_D_R_Gap = coef_est[1],  
    Explained_D_R_Gap = explained_gap,
    SE = se_gap,
    Party_Specific = party_effect,
    SE_Party = se_party
  ))
}


# Se tabellen
print(table_data)


library(gt)

table_data %>%
  gt() %>%
  tab_header(title = "Forklaring av vekstgapet mellom regjeringer i Norge") %>%
  fmt_number(columns = 2:6, decimals = 2) %>%
  cols_label(
    Shock = "Sjokk",
    Total_D_R_Gap = "Total Vekstforskjell",
    Explained_D_R_Gap = "Forklart Gap",
    SE = "SE",
    Party_Specific = "Partispesifikk Effekt",
    SE_Party = "SE (Partispesifikk)"
  )



for (sjokk in names(results)) {
  model <- results[[sjokk]]
  
  # Sjekk om vcov() inneholder NA
  vcov_mat <- vcov(model)
  if (any(is.na(vcov_mat))) {
    warning(paste("Varning: vcov() har NA for sjokk", sjokk, "- Hopper over."))
    next  # Hopp over denne iterasjonen
  }
  
  # Hent koeffisienter og standardfeil
  coef_est <- coef(model)
  se_est <- sqrt(diag(vcov_mat))  # Standardfeil
  
  # Beregn summen av koeffisientene
  explained_gap <- sum(coef_est[-1], na.rm = TRUE)
  se_gap <- sqrt(sum(se_est[-1]^2, na.rm = TRUE))
  
  # Håndter partispecifikke effekter
  party_effect <- if ("X:party_dummy" %in% names(coef_est)) coef_est["X:party_dummy"] else NA
  se_party <- if ("X:party_dummy" %in% names(se_est)) se_est["X:party_dummy"] else NA
  
  # Legg til i tabellen
  table_data <- rbind(table_data, data.frame(
    Shock = sjokk,
    Total_D_R_Gap = coef_est[1],  
    Explained_D_R_Gap = explained_gap,
    SE = se_gap,
    Party_Specific = party_effect,
    SE_Party = se_party
  ))
}


