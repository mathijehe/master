var_data <- data5 %>% 
  dplyr::select(oil, bnp, index, kpi)

adf.test(var_data$oil)
adf.test(var_data$bnp)
adf.test(var_data$kpi)



VARselect(var_data, lag.max = 8, type = "const")

# Estimer VAR-modellen med 6 lags (som i artikkelen)
var_model <- VAR(var_data, p = 3, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_oil <- residuals(var_model)[, "oil"]


# Legg sjokkene til datasettet
data1 <- data5 %>% 
  slice(4:n()) %>% 
  mutate(oil_shock = shock_oil)


data9 <- data1 %>%
  mutate(oil_shock_lag1 = lag(oil_shock, 1),
         oil_shock_lag2 = lag(oil_shock, 2),
         oil_shock_lag3 = lag(oil_shock, 3))

# Kjør modellen med eksplisitte lag
model_lagged <- lm(bnp ~ oil_shock + oil_shock_lag1 + oil_shock_lag2 + oil_shock_lag3, data = data9)
summary(model_lagged)




data_regjering <- read_excel("~/Desktop/data-regjering.xlsx")
data_regjering <- data_regjering %>%
  mutate(year = ymd(year))

data6 <- list(data1, data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# Beregn BNP-gapet mellom regjeringene
total_gap_oil <- mean(data6$bnp[data6$Regjering == "Venstre"]) - 
  mean(data6$bnp[data6$Regjering == "Høyre"])

# Hent koeffisientene fra den laggede modellen
coef_shock_oil <- sum(coef(model_lagged)[grep("oil_shock", names(coef(model_lagged)))])

# Beregn bidragene til BNP-gapet
explained_oil <- coef_shock_oil * mean(data6$oil_shock, na.rm = TRUE)


summary(model_lagged)# Lag en tabell
results_table <- data.frame(
  Shock = c("Oil Shock"),
  Explained_Gap = c(explained_oil)
)

print(results_table)





#####
var_data <- bnp_int %>%
  dplyr::select(Belgium, Denmark, France, Germany, 
                Italy, Netherlands, Sweden, 
                `United Kingdom`, `United States`) %>% 
  mutate(across(everything(), as.numeric))



weights <- c(Belgium = 0.07, Denmark = 0.04, France = 0.10, 
             Germany = 0.30, Italy = 0.02, Netherlands = 0.09, `United Kingdom`=0.25,
             `United States`=0.07, Sweden=0.10)

bnp_int$intl_bnp_growth <- rowSums(var_data[, names(weights)] * weights[colnames(var_data)])

var_data <- bnp_int %>%
  dplyr::select(intl_bnp_growth, bnp, index, oil, kpi)

adf.test(var_data$intl_bnp_growth)



VARselect(var_data, lag.max = 8, type = "const")

# Estimer VAR-modellen med 6 lags (som i artikkelen)
var_model <- VAR(var_data, p = 4, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_int <- residuals(var_model)[, "intl_bnp_growth"]


# Legg sjokkene til datasettet
bnp_int2 <- bnp_int %>% 
  slice(5:n()) %>% 
  mutate(shock_int = shock_int)

bnp_int2 <- bnp_int2 %>%
  mutate(shock_int = ifelse(shock_int > quantile(shock_int, 0.99), quantile(shock_int, 0.99),
                            ifelse(shock_int < quantile(shock_int, 0.01), quantile(shock_int, 0.01), shock_int)))

data9 <- bnp_int2 %>%
  mutate(shock_int_lag1 = lag(shock_int, 1),
         shock_int_lag2 = lag(shock_int, 2),
         shock_int_lag3 = lag(shock_int, 3))




# Kjør modellen med eksplisitte lag
model_lagged <- lm(bnp ~ shock_int + shock_int_lag1 + shock_int_lag2 + shock_int_lag3, data = data9)
summary(model_lagged)

data_regjering <- read_excel("~/Desktop/data-regjering.xlsx")
data_regjering <- data_regjering %>%
  mutate(year = ymd(year))

data6 <- list(bnp_int2, data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# Beregn BNP-gapet mellom regjeringene
total_gap_int <- (mean(data6$bnp[data6$Regjering == "Venstre"]) - 
                    mean(data6$bnp[data6$Regjering == "Høyre"]))



# Hent koeffisientene fra den laggede modellen
coef_shock_int <- sum(coef(model_lagged)[grep("shock_int", names(coef(model_lagged)))])


# Beregn bidragene til BNP-gapet
explained_int <- coef_shock_int * mean(data6$shock_int, na.rm = TRUE)


# Lag en tabell
results_table <- data.frame(
  Shock = c("int Shock"),
  Explained_Gap = c(explained_int)
)

print(results_table)


#####
var_data <- data4 %>% 
  dplyr::select(rente, bnp, index, kpi,oil)

adf.test(var_data$rente)

VARselect(var_data, lag.max = 8, type = "const")


# Estimer VAR-modellen med 6 lags (som i artikkelen)
var_model <- VAR(var_data, p = 3, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_rente <- residuals(var_model)[, "rente"]


# Legg sjokkene til datasettet
data4 <- data4 %>% 
  slice(4:n()) %>% 
  mutate(rente_shock = shock_rente)

data1 <- data4 %>%
  mutate(rente_shock = ifelse(rente_shock > quantile(rente_shock, 0.99), quantile(rente_shock, 0.99),
                            ifelse(rente_shock < quantile(rente_shock, 0.01), quantile(rente_shock, 0.01), rente_shock)))


data9 <- data1 %>%
  mutate(shock_rente_lag1 = lag(rente_shock, 1),
         shock_rente_lag2 = lag(rente_shock, 2),
         shock_rente_lag3 = lag(rente_shock, 3))

# Kjør modellen med eksplisitte lag
model_lagged <- lm(bnp ~ rente_shock + shock_rente_lag1 + shock_rente_lag2 + shock_rente_lag3, data = data9)
summary(model_lagged)


cor(data4$shock_rente, data4$rente, use = "complete.obs")

data_regjering <- read_excel("~/Desktop/data-regjering.xlsx")
data_regjering <- data_regjering %>%
  mutate(year = ymd(year))

data6 <- list(data4, data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# Beregn BNP-gapet mellom regjeringene
total_gap_rente <- mean(data6$bnp[data6$Regjering == "Venstre"]) - 
  mean(data6$bnp[data6$Regjering == "Høyre"])

# Hent koeffisientene fra den laggede modellen
coef_shock_rente <- sum(coef(model_lagged)[grep("rente_shock", names(coef(model_lagged)))])

# Beregn bidragene til BNP-gapet
explained_rente <- coef_shock_rente * mean(data6$rente_shock, na.rm = TRUE)

# Lag en tabell
results_table <- data.frame(
  Shock = c("rente Shock"),
  Explained_Gap = c(explained_rente)
)

print(results_table)


######
var_data <- data9 %>% 
  dplyr::select(finans, bnp, index, kpi,oil) %>% 
  drop_na()

adf.test(var_data$finans)

VARselect(var_data, lag.max = 8, type = "const")


# Estimer VAR-modellen med 6 lags (som i artikkelen)
var_model <- VAR(var_data, p = 3, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_finans <- residuals(var_model)[, "finans"]


# Legg sjokkene til datasettet
data4 <- data9 %>% 
  slice(5:n()) %>% 
  mutate(finans_shock = shock_finans)


data1 <- data4 %>%
  mutate(finans_shock = ifelse(finans_shock > quantile(finans_shock, 0.99), quantile(finans_shock, 0.99),
                            ifelse(finans_shock < quantile(finans_shock, 0.01), quantile(finans_shock, 0.01), finans_shock)))


data9 <- data1 %>%
  mutate(shock_finans_lag1 = lag(shock_finans, 1),
         shock_finans_lag2 = lag(shock_finans, 2),
         shock_finans_lag3 = lag(shock_finans, 3))

# Kjør modellen med eksplisitte lag
model_lagged <- lm(bnp ~ shock_finans + shock_finans_lag1 + shock_finans_lag2 + shock_finans_lag3, data = data9)
summary(model_lagged)


cor(data4$finans_shock, data4$finans, use = "complete.obs")

data_regjering <- read_excel("~/Desktop/data-regjering.xlsx")
data_regjering <- data_regjering %>%
  mutate(year = ymd(year))

data6 <- list(data4, data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# Beregn BNP-gapet mellom regjeringene
total_gap_rente <- mean(data6$bnp[data6$Regjering == "Venstre"]) - 
  mean(data6$bnp[data6$Regjering == "Høyre"])

# Hent koeffisientene fra den laggede modellen
coef_shock_finans <- sum(coef(model_lagged)[grep("shock_finans", names(coef(model_lagged)))])

# Beregn bidragene til BNP-gapet
explained_finans <- coef_shock_finans * mean(data6$finans_shock, na.rm = TRUE)

# Lag en tabell
results_table <- data.frame(
  Shock = c("rente Shock"),
  Explained_Gap = c(explained_finans)
)

print(results_table)



######
var_data <- data8 %>% 
  dplyr::select(trend, bnp, index, kpi,oil)

adf.test(var_data$trend)

VARselect(var_data, lag.max = 8, type = "const")


# Estimer VAR-modellen med 6 lags (som i artikkelen)
var_model <- VAR(var_data, p = 2, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_trend <- residuals(var_model)[, "trend"]


# Legg sjokkene til datasettet
data4 <- data8 %>% 
  slice(3:n()) %>% 
  mutate(trend_shock= trend)
data4$trend <- shock_trend


data9 <- data4 %>%
  mutate(shock_trend_lag1 = lag(shock_trend, 1),
         shock_trend_lag2 = lag(shock_trend, 2),
         shock_trend_lag3 = lag(shock_trend, 3))

# Kjør modellen med eksplisitte lag
model_lagged <- lm(bnp ~ shock_trend + shock_trend_lag1 + shock_trend_lag2 + shock_trend_lag3, data = data9)
summary(model_lagged)


cor(data4$trend_shock, data4$trend, use = "complete.obs")

data_regjering <- read_excel("~/Desktop/data-regjering.xlsx")
data_regjering <- data_regjering %>%
  mutate(year = ymd(year))

data6 <- list(data4, data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# Beregn BNP-gapet mellom regjeringene
total_gap_trend <- mean(data6$bnp[data6$Regjering == "Venstre"]) - 
  mean(data6$bnp[data6$Regjering == "Høyre"])

# Hent koeffisientene fra den laggede modellen
coef_shock_trend <- sum(coef(model_lagged)[grep("shock_trend", names(coef(model_lagged)))])

# Beregn bidragene til BNP-gapet
explained_trend <- coef_shock_trend * total_gap_trend

# Lag en tabell
results_table <- data.frame(
  Shock = c("trend Shock"),
  Explained_Gap = c(explained_trend)
)

print(results_table)


####
var_data <- data6 %>% 
  dplyr::select(prod, BNP) %>% 
  drop_na()

adf.test(var_data$prod)

var_data <- var_data %>%
  mutate(prod = c(NA, diff(prod))) %>% 
  drop_na()

VARselect(var_data, lag.max = 8, type = "const")

# Estimer VAR-modellen med 6 lags (som i artikkelen)
var_model <- VAR(var_data, p = 1, type = "const")

# Hent residualene (definert som sjokkene e_t)
shock_prod <- residuals(var_model)[, "prod"]


# Legg sjokkene til datasettet
data4 <- data6 %>% 
  slice(5:n()) %>% 
  mutate(prod_shock= prod)
data4$prod <- shock_prod


data9 <- data4 %>%
  mutate(shock_prod_lag1 = lag(shock_prod, 1),
         shock_prod_lag2 = lag(shock_prod, 2),
         shock_prod_lag3 = lag(shock_prod, 3))

# Kjør modellen med eksplisitte lag
model_lagged <- lm(BNP ~ shock_prod + shock_prod_lag1 + shock_prod_lag2 + shock_prod_lag3, data = data9)
summary(model_lagged)


cor(data4$prod_shock, data4$prod, use = "complete.obs")

data_regjering <- read_excel("~/Desktop/data-regjering.xlsx", sheet = 2)


data6 <- list(data4, data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# Beregn BNP-gapet mellom regjeringene
total_gap_prod <- mean(data6$BNP[data6$Regjering == "Venstre"]) - 
  mean(data6$BNP[data6$Regjering == "Høyre"])

# Hent koeffisientene fra den laggede modellen
coef_shock_trend <- sum(coef(model_lagged)[grep("shock_prod", names(coef(model_lagged)))])

# Beregn bidragene til BNP-gapet
explained_prod <- coef_shock_trend * total_gap_prod

# Lag en tabell
results_table <- data.frame(
  Shock = c("prod Shock"),
  Explained_Gap = c(explained_prod)
)

print(results_table)
