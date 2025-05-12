# === Velg variabler du vil inkludere ===
deskriptiv_data <- data8 %>%
  dplyr::select(bnp,
         oil_shock_std,
         intl_bnp_growth_shock_std,
         finans_shock_std,
         monetary_shock_std,
         trend_shock_std,
         prod_shock_std)



# === Velg variabler du vil inkludere ===
deskriptiv_data <- data8 %>%
  dplyr::select(bnp,
                oil_shock,
                intl_bnp_growth_shock,
                finans_shock,
                monetary_shock,
                trend_shock,
                prod_shock)
# === Lag deskriptiv statistikk-tabell ===
deskriptiv_tabell <- deskriptiv_data %>%
  summarise(across(
    .cols = everything(),
    .fns = list(
      N = ~sum(!is.na(.)),
      Gjennomsnitt = ~mean(., na.rm = TRUE),
      Median = ~median(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Maks = ~max(., na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(cols = everything(),
               names_to = c("Variabel", "Statistikk"),
               names_sep = "_(?=[^_]+$)",  # ✅ viktig endring
               names_transform = list(Statistikk = as.character),
               values_to = "Verdi") %>%
  pivot_wider(names_from = Statistikk, values_from = Verdi)

deskriptiv_tabell <- deskriptiv_tabell %>%
  mutate(
    Variabel = recode(Variabel,
                      "bnp" = "BNP",
                      "oil_shock_std" = "Oljesjokk",
                      "intl_bnp_growth_shock_std" = "Internasonalesjokk",
                      "finans_shock_std" = "Finanspolitiskesjokk ",
                      "monetary_shock_std" = "Pengepolitiskesjokk",
                      "trend_shock_std" = "Forventningssjokk",
                      "prod_shock_std" = "Produktivitetssjokk"
    )
  )

# === Vis tabellen pent med gt() ===
gt(deskriptiv_tabell) %>%
  tab_header(title = md("**Deskriptiv statistikk**")) %>%
  fmt_number(columns = where(is.numeric), decimals = 3)






data5 <- list(bnp, bnp_int,finans,kpi,ravarepriser,data_regjering,
              quarterly_prod,styringsrente3, forvent) %>%
  reduce(full_join, by = "year")


data5 <- data5 %>% 
  arrange(year) %>% 
  filter(year >= as.Date("1978-04-01") & year <= as.Date("2021-07-01"))

# === Velg variabler du vil inkludere ===
deskriptiv_data <- data5 %>%
  dplyr::select(bnp,
                oil,
                intl_bnp_growth,
                finans,
                trend,
                prod,
                rente,
                kpi,
                index
                )


  
# === Lag deskriptiv statistikk-tabell ===
deskriptiv_tabell <- deskriptiv_data %>%
  summarise(across(
    .cols = everything(),
    .fns = list(
      N = ~sum(!is.na(.)),
      Gjennomsnitt = ~mean(., na.rm = TRUE),
      Median = ~median(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Maks = ~max(., na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(cols = everything(),
               names_to = c("Variabel", "Statistikk"),
               names_sep = "_(?=[^_]+$)",  # ✅ viktig endring
               names_transform = list(Statistikk = as.character),
               values_to = "Verdi") %>%
  pivot_wider(names_from = Statistikk, values_from = Verdi)


enhet_df <- tibble(
  Variabel = c("bnp", "oil", "intl_bnp_growth", "finans", "trend",
               "prod", "rente", "kpi", "index"),
  Enhet = c("Prosent", "Prosent", "Prosent", "Prosent", "Indeks-poeng",
            "Prosent", "Prosentpoeng", "Prosent", "Prosent")
)

deskriptiv_tabell <- deskriptiv_tabell %>%
  left_join(enhet_df, by = "Variabel")

recode_navn <- c(
  "bnp" = "BNP-vekst",
  "oil" = "Oljepris",
  "intl_bnp_growth" = "Internasjonal vekst",
  "finans" = "Offentlig konsum",
  "rente" = "Renteendring",
  "prod" = "Produktivitets vekst",
  "trend" = "Forventningsendring",
  "kpi" = "KPI-vekst",
  "index" = "Råvarepris-vekst"
)

ønsket_rekkefølge <- recode_navn  # samme rekkefølge

# === 2. Gjør rekkefølge og oversett samtidig ===
deskriptiv_tabell <- deskriptiv_tabell %>%
  mutate(Variabel = recode(Variabel, !!!recode_navn)) %>%
  mutate(Variabel = factor(Variabel, levels = ønsket_rekkefølge)) %>%
  arrange(Variabel)
  



# === Vis tabellen pent med gt() ===
gt(deskriptiv_tabell) %>%
  #tab_header(title = md("**Deskriptiv statistikk**")) %>%
  cols_label(
    Variabel = "Variabel",
    N = "N",
    Gjennomsnitt = "Gjennomsnitt",
    Median = "Median",
    SD = "SD",
    Min = "Min",
    Maks = "Maks",
    Enhet = "Enhet"
  ) %>%
  fmt_number(
    columns = N,
    decimals = 0
  ) %>%
  fmt_number(
    columns = c(Gjennomsnitt, Median, SD, Min, Maks),
    decimals = 2
  ) %>% 
  tab_options(
    data_row.padding = px(10),                # Mellomrom mellom rader
    table.font.size = 15,                    # Valgfritt: gjør teksten leselig
    column_labels.padding = px(10),           # Mer luft i kolonneoverskrifter
    table_body.hlines.width = px(1)        # Kontroll på linjene (valgfritt)
  )




#vekt tabell
vekter %>%
  mutate(
    Land = recode(Land,
                  "United Kingdom" = "Storbritannia",
                  "Germany" = "Tyskland",
                  "Netherlands" = "Nederland",
                  "Sweden" = "Sverige",
                  "France" = "Frankrike",
                  "United States" = "USA",
                  "Belgium" = "Belgia",
                  "Denmark" = "Danmark")
  ) %>% 
  arrange(desc(Vekt)) %>%
  #mutate(Vekt_prosent = round(Vekt * 100, 1)) %>%
  gt() %>%
  #tab_header(title = "Vekter basert på gjennomsnittlig eksport") %>%
  fmt_number(columns = c(Gjennomsnitt), decimals = 0) %>% 
  fmt_number(columns = c(Vekt), decimals = 2)
  




# Eksempel: BNP-vekst som ts-serie
bnp_ts <- ts(bnp$bnp, start = c(1978, 1), frequency = 4)

# Glidende gjennomsnitt
trend_inf <- rep(mean(bnp_ts, na.rm = TRUE), length(bnp_ts))
trend_100 <- zoo::rollmean(bnp_ts, k = 100, fill = NA, align = "center")
trend_67  <- zoo::rollmean(bnp_ts, k = 67, fill = NA, align = "center")
trend_33  <- zoo::rollmean(bnp_ts, k = 33, fill = NA, align = "center")

# Plot
plot(bnp_ts, type = "l", col = "black", ylab = "BNP-vekst (%)", xlab = "Tid", main = "BNP-vekst og ulike trender")
lines(trend_inf, col = "orange", lty = 2)
lines(trend_100, col = "brown", lty = 3)
lines(trend_67,  col = "purple", lty = 4)
lines(trend_33,  col = "deeppink", lty = 1)
legend("topright", legend = c("BNP-vekst", "κ = ∞", "κ = 100", "κ = 67", "κ = 33"),
       col = c("black", "orange", "brown", "purple", "deeppink"), lty = c(1,2,3,4,1))







#####


data1 <- list(bnp,data_regjering %>%
  reduce(full_join, by = "year") %>% 
  drop_na()

# === Lag glidende trender ===
bnp_ts <- zoo(data1$bnp, order.by = data1$year)

data1 <- data1 %>%
  mutate(
    trend_inf = mean(bnp, na.rm = TRUE),
    trend_100 = rollmean(bnp_ts, k = 100, fill = NA, align = "center"),
    trend_67  = rollmean(bnp_ts, k = 67,  fill = NA, align = "center"),
    trend_33  = rollmean(bnp_ts, k = 33,  fill = NA, align = "center")
  )



# === Funksjon for å beregne trendjusterte S–K-gap ===
beregn_gap <- function(trendnavn) {
  df <- data1 %>%
    mutate(detrended = bnp - .data[[trendnavn]]) %>%
    filter(!is.na(detrended))
  
  d_avg <- mean(df$detrended[df$Regjering == "Venstre"], na.rm = TRUE)
  r_avg <- mean(df$detrended[df$Regjering == "Høyre"], na.rm = TRUE)
  gap <- d_avg - r_avg
  
  return(data.frame(
    Trend = trendnavn,
    `Gj.sn. Venstre` = round(d_avg, 3),
    `Gj.sn. Høyre` = round(r_avg, 3),
    `S–K gap (trendjustert)` = round(gap, 3)
  ))
}

# === Beregn for alle trendvalg ===
trendvalg <- c("trend_inf", "trend_100", "trend_67", "trend_33")
resultater <- lapply(trendvalg, beregn_gap) %>% bind_rows()

# === Gi penere navn ===
resultater$Trend <- recode(resultater$Trend,
                           "trend_inf" = "κ = ∞",
                           "trend_100" = "κ = 100",
                           "trend_67"  = "κ = 67",
                           "trend_33"  = "κ = 33")

# === Lag tabell ===
resultater %>%
  gt() %>%
  tab_header(
    title = md("**Tabell: Trendjustert S–K BNP-gap**"),
    subtitle = "Dekomponering med ulike trender (κ)"
  ) %>%
  cols_label(
    Trend = "Trendvalg (κ)",
    `Gj.sn. Venstre` = "Venstre (D)",
    `Gj.sn. Høyre` = "Høyre (R)",
    `S–K gap (trendjustert)` = "S–K gap"
  ) %>%
  tab_options(table.font.size = 12)