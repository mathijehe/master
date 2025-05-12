# Installer og last inn nødvendige pakker
install.packages("vars")
install.packages("svars")
install.packages("dplyr")
install.packages("readxl")
install.packages("lubridate")
install.packages("purrr")

library(vars)
library(svars)
library(dplyr)
library(readxl)
library(lubridate)
library(purrr)

# 1️⃣ Hent og klargjør data
var_data <- data5 %>% 
  dplyr::select(oil, bnp, index, kpi)

# 2️⃣ Test for stasjonaritet
adf.test(var_data$oil)
adf.test(var_data$bnp)
adf.test(var_data$kpi)

# 3️⃣ Velg optimal laglengde (eller bruk p = 3 som i artikkelen)
VARselect(var_data, lag.max = 8, type = "const")

# 4️⃣ Estimer en VAR-modell med 3 lags
var_model <- VAR(var_data, p = 3, type = "const")

# 5️⃣ Estimer en SVAR-modell med Cholesky-dekomponering
svar_model <- id.chol(var_model)

# 6️⃣ Hent de strukturelle sjokkene fra SVAR-modellen
cov_matrix <- vcov(var_model) 
B_matrix <- svar_model$B  
var_residuals <- residuals(var_model)  
shock_oil_svar <- t(solve(B_matrix) %*% t(var_residuals))

# 7️⃣ Konverter sjokkene til dataframe
shock_oil_svar <- as.data.frame(shock_oil_svar)
colnames(shock_oil_svar) <- colnames(var_residuals)

# 8️⃣ Legg sjokkene til datasettet
data1 <- data5 %>%
  slice(4:n()) %>%
  mutate(oil_shock = shock_oil_svar$oil)

# 9️⃣ Winsorisering av sjokkene for å håndtere ekstreme verdier
data1 <- data1 %>%
  mutate(oil_shock = ifelse(oil_shock > quantile(oil_shock, 0.99), quantile(oil_shock, 0.99),
                            ifelse(oil_shock < quantile(oil_shock, 0.01), quantile(oil_shock, 0.01), oil_shock)))

# 🔟 Lag lags av oljeprissjokkene
data9 <- data1 %>%
  mutate(oil_shock_lag1 = lag(oil_shock, 1),
         oil_shock_lag2 = lag(oil_shock, 2),
         oil_shock_lag3 = lag(oil_shock, 3))

# 1️⃣1️⃣ Kjør regresjonsmodellen
model_lagged <- lm(bnp ~ oil_shock + oil_shock_lag1 + oil_shock_lag2 + oil_shock_lag3, data = data9)
summary(model_lagged)

# 1️⃣2️⃣ Hent regjeringens data og slå sammen med økonomiske data
data_regjering <- read_excel("~/Desktop/data-regjering.xlsx") %>%
  mutate(year = ymd(year))

data6 <- list(data1, data_regjering) %>%
  reduce(full_join, by = "year") %>%
  drop_na()

# 1️⃣3️⃣ Beregn BNP-gapet mellom regjeringene
total_gap_oil <- mean(data6$bnp[data6$Regjering == "Venstre"]) - 
  mean(data6$bnp[data6$Regjering == "Høyre"])

# 1️⃣4️⃣ Hent koeffisientene fra modellen
coef_shock_oil <- sum(coef(model_lagged)[grep("oil_shock", names(coef(model_lagged)))])

# 1️⃣5️⃣ Beregn bidragene til BNP-gapet
explained_oil <- coef_shock_oil * mean(data6$oil_shock, na.rm = TRUE)


# 1️⃣6️⃣ Lag en tabell med resultatene
results_table <- data.frame(
  Shock = c("Oil Shock"),
  Explained_Gap_Mean = c(explained_oil),
  Explained_Gap_IQR = c(explained_oil_iqr)
)

# 1️⃣7️⃣ Print resultatene
print(results_table)




###################
# 1️⃣ Hent og klargjør data
var_data <- data4 %>% 
  dplyr::select(rente, bnp, index, kpi, oil)

# 2️⃣ Test for stasjonaritet
adf.test(var_data$rente)
adf.test(var_data$bnp)
adf.test(var_data$kpi)

# 3️⃣ Velg optimal laglengde (eller bruk p = 3 som i artikkelen)
VARselect(var_data, lag.max = 8, type = "const")

# 4️⃣ Estimer en VAR-modell med 3 lags
var_model <- VAR(var_data, p = 3, type = "const")

# 5️⃣ Estimer en SVAR-modell med Cholesky-dekomponering
svar_model <- id.chol(var_model)

# 6️⃣ Hent de strukturelle sjokkene fra SVAR-modellen
cov_matrix <- vcov(var_model) 
B_matrix <- svar_model$B  
var_residuals <- residuals(var_model)  
shock_rente_svar <- t(solve(B_matrix) %*% t(var_residuals))

# 7️⃣ Konverter sjokkene til dataframe
shock_rente_svar <- as.data.frame(shock_rente_svar)
colnames(shock_rente_svar) <- colnames(var_residuals)

# 8️⃣ Legg sjokkene til datasettet
data4 <- data4 %>%
  slice(4:n()) %>%
  mutate(rente_shock = shock_rente_svar$rente)

# 9️⃣ Winsorisering av sjokkene for å håndtere ekstreme verdier
data4 <- data4 %>%
  mutate(rente_shock = ifelse(rente_shock > quantile(rente_shock, 0.99), quantile(rente_shock, 0.99),
                              ifelse(rente_shock < quantile(rente_shock, 0.01), quantile(rente_shock, 0.01), rente_shock)))

# 🔟 Lag lags av rente-sjokkene
data9 <- data4 %>%
  mutate(rente_shock_lag1 = lag(rente_shock, 1),
         rente_shock_lag2 = lag(rente_shock, 2),
         rente_shock_lag3 = lag(rente_shock, 3))

# 1️⃣1️⃣ Kjør regresjonsmodellen
model_lagged <- lm(bnp ~ rente_shock + rente_shock_lag1 + rente_shock_lag2 + rente_shock_lag3, data = data9)
summary(model_lagged)

# 1️⃣2️⃣ Hent regjeringens data og slå sammen med økonomiske data
data_regjering <- read_excel("~/Desktop/data-regjering.xlsx") %>%
  mutate(year = ymd(year))

data6 <- list(data4, data_regjering) %>%
  reduce(full_join, by = "year") %>%
  drop_na()

# 1️⃣3️⃣ Beregn BNP-gapet mellom regjeringene
total_gap_rente <- mean(data6$bnp[data6$Regjering == "Venstre"]) - 
  mean(data6$bnp[data6$Regjering == "Høyre"])

# 1️⃣4️⃣ Hent koeffisientene fra modellen
coef_shock_rente <- sum(coef(model_lagged)[grep("rente_shock", names(coef(model_lagged)))])

# 1️⃣5️⃣ Beregn bidragene til BNP-gapet
explained_rente <- coef_shock_rente * mean(data6$rente_shock, na.rm = TRUE)

