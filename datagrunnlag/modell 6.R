library(BVAR)

var_data <- data5 %>%
  select(pris, bnp)  # Sørg for at 'bnp' kommer først

# 2. Estimer BVAR-modellen
bvar_model <- bvar(var_data, lags = 4, priors = bv_priors())

# 3. Hent residualene
bvar_residuals <- residuals(bvar_model)

# 4. Bruk riktig Cholesky dekomponering
B_chol <- t(chol(cov(bvar_residuals)))  # Bruk NEDRE trekantmatrise

# 5. Transformer residualene for å få strukturelle sjokk
bvar_shocks <- solve(B_chol) %*% t(bvar_residuals)  

# 6. Legg til sjokkene i datasettet (sjekk dimensjoner)
data6 <- data5 %>% 
  slice(5:n()) %>% 
  mutate(oil_shock = as.numeric(bvar_shocks[2, ]))  # Konverter til numerisk vektor

mean(data6$oil_shock)

summary(data6$oil_shock)
cor(data6$oil_shock, data6$pris, use = "complete.obs")

data6$oil_shock <- data6$oil_shock * sd(bvar_residuals[, "pris"]) + mean(bvar_residuals[, "pris"])
