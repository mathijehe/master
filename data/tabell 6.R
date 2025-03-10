library(BVAR)
library(car)
library(svars)

#tester
adf.test(var_data$pris)
adf.test(data4$bnp)
var_data$kpi <- c(NA, diff(var_data$kpi))
var_data <- na.omit(var_data)

cor(var_residuals[, "pris"], var_data$pris[(p_opt + 1):n()])
vif_values <- vif(lm(bnp ~ rente + kpi, data = var_data))  
print(vif_values)


var_data <- var_data %>% 
  slice(3:n())
cor(data6$oil_shock, var_data$pris, use = "complete.obs")
cor(data8$rente_shock, var_data$bnp, use = "complete.obs")
cor(data8$rente_shock, var_data$kpi, use = "complete.obs")


pr(mfrow = c(2, 2))  
hist(bvar_shocks[2, ], main = "Histogram of Rente Shock", breaks = 30)
plot(var_data$rente, data8$rente_shock, main = "Rente vs. Rente Shock")
plot(var_data$bnp, data8$rente_shock, main = "BNP vs. Rente Shock")
plot(var_data$kpi, data8$rente_shock, main = "KPI vs. Rente Shock")




#oljesjokk
var_data <- data5 %>%
  dplyr::select(pris,exr, bnp)  # Sørg for at 'bnp' kommer først



VARselect(var_data, lag.max = 10, type = "const")
# 2. Estimer BVAR-modellen
bvar_model <- bvar(var_data, lags = 2, priors = bv_priors())

# 3. Hent residualene
bvar_residuals <- residuals(bvar_model)

# 4. Bruk riktig Cholesky dekomponering
B_chol <- t(chol(cov(bvar_residuals)))  # Bruk NEDRE trekantmatrise

# 5. Transformer residualene for å få strukturelle sjokk
bvar_shocks <- solve(B_chol) %*% t(bvar_residuals)  

# 6. Legg til sjokkene i datasettet (sjekk dimensjoner)
data6 <- data5 %>% 
  slice(3:n()) %>% 
  mutate(oil_shock = as.numeric(bvar_shocks[1, ]))  # Konverter til numerisk vektor
data6$oil_shock <- data6$oil_shock * sd(bvar_residuals[, "pris"]) + mean(bvar_residuals[, "pris"])

sd(data6$oil_shock)

summary(data6$oil_shock)
cor(data6$oil_shock, data6$pris, use = "complete.obs")




#rentesjokk

var_data <- data4 %>%
  dplyr::select(rente, kpi,bnp)

VARselect(var_data, lag.max = 10, type = "const")
# 2. Estimer BVAR-modellen
bvar_model <- bvar(var_data, lags = 5, priors = bv_priors())

# 3. Hent residualene
bvar_residuals <- residuals(bvar_model)


# 4. Bruk riktig Cholesky dekomponering
B_chol <- t(chol(cov(bvar_residuals)))  # Bruk NEDRE trekantmatrise

# 5. Transformer residualene for å få strukturelle sjokk
bvar_shocks <- solve(B_chol) %*% t(bvar_residuals)  

# 6. Legg til sjokkene i datasettet (sjekk dimensjoner)
data8 <- data4 %>% 
  slice(6:n()) %>% 
  mutate(rente_shock = as.numeric(bvar_shocks[1, ]))  # Konverter til numerisk vektor
data8$rente_shock <- data8$rente_shock * sd(bvar_residuals[, "rente"]) + mean(bvar_residuals[, "rente"])

sd(data8$rente_shock)

cor(var_data, use = "complete.obs")
cor(data8$rente_shock, data8$rente, use = "complete.obs")



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
data9 <- bnp_int %>% 
  slice(2:n()) %>%
  mutate(int_shock = rowMeans(t(bvar_shocks[1:(nrow(bvar_shocks)-1), ])))
    # Konverter til numerisk vektor

sd(data9$int_shock)


#produktivitet

# Velg variabler for SVAR
var_data <- data6 %>%
  dplyr::select(prod, BNP) %>%
  na.omit()


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

data6 <- data6 %>% 
  slice(4:n()) %>% 
  mutate(prod_shock = as.numeric(structural_shocks[, 1])) 

cor(data6$prod_shock, data6$prod, use = "complete.obs")
sd(data6$prod_shock)




#konsumentforventninger
var_data <- data8 %>%
  dplyr::select(trend, bnp)  # Sørg for at 'bnp' kommer først
#Finner antall lags
VARselect(var_data, lag.max = 10, type = "const")
# 2. Estimer BVAR-modellen
bvar_model <- bvar(var_data, lags = 2, priors = bv_priors())

# 3. Hent residualene
bvar_residuals <- residuals(bvar_model)

# 4. Bruk riktig Cholesky dekomponering
B_chol <- t(chol(cov(bvar_residuals)))  # Bruk NEDRE trekantmatrise

# 5. Transformer residualene for å få strukturelle sjokk
bvar_shocks <- solve(B_chol) %*% t(bvar_residuals)  

# 6. Legg til sjokkene i datasettet (sjekk dimensjoner)
data8 <- data8 %>% 
  slice(3:n()) %>% 
  mutate(trend_shock = as.numeric(bvar_shocks[1, ]))  # Konverter til numerisk vektor
data8$trend_shock <- data8$trend_shock * sd(bvar_residuals[, "trend"]) + mean(bvar_residuals[, "trend"])

sd(data8$trend_shock)

summary(data6$oil_shock)
cor(data8$trend_shock, data8$trend, use = "complete.obs")


#finanspolitikk 
var_data <- data9 %>%
  dplyr::select(finans, bnp) %>%   # Sørg for at 'finans' kommer først
  drop_na()

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
data9 <- data9 %>% 
  slice(3:n()) %>% 
  mutate(finans_shock = as.numeric(bvar_shocks[1, ]))  # Konverter til numerisk vektor
data9$finans_shock <- data9$finans_shock * sd(bvar_residuals[, "finans"]) + mean(bvar_residuals[, "finans"])

mean(data8$trend_shock)

summary(data6$oil_shock)
cor(data9$finans_shock, data9$finans, use = "complete.obs")



