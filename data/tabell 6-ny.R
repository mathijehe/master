
#oljesjokk
adf.test(data5$pris)

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



data <- list(finans_sjokk, forvet_sjokk,rente_sjokk,oil_sjokk,int_sjokk)
data <- reduce(data, full_join, by = c("year", "bnp"))
