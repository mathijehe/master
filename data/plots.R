library(ggthemes)
data_regjering <- read_excel("~/Desktop/data-regjering.xlsx")
data_regjering <- data_regjering %>%
  mutate(year = ymd(year))

bnp <- read_excel("~/Downloads/BNP-vekst (kvartal) SSB rett!.xlsx", 
                  col_names = FALSE)
bnp <- bnp %>% 
  dplyr::select("year"="...1", "bnp"="...2") %>% 
  arrange(year) %>% 
  mutate(
    year = str_replace(year, "K1", "-01-01"),
    year = str_replace(year, "K2", "-04-01"),
    year = str_replace(year, "K3", "-07-01"),
    year = str_replace(year, "K4", "-10-01"),
    year = ymd(year)  # Konverterer til datoformat
  )



data <- list(bnp,data_regjering) %>%
  reduce(full_join, by = "year") %>% 
  filter(year <= as.Date("2021-07-01")) %>% 
  drop_na() 

# Anta data har: year, bnp, Regjering, ar
data <- data |>
  arrange(year) |>
  mutate(
    ar_justert = ifelse(ar > 4, ar - 4, ar),
    regjering_skifte = Regjering != lag(Regjering)
  )

# Lag datasett for arvet BNP (year 0 = bnp året før skifte)
arvet_data <- data |>
  filter(regjering_skifte) |>
  transmute(
    Regjering = lead(Regjering),  # Ny regjering
    ar_justert = 0,
    bnp = bnp
  )

arvet_data <- arvet_data |>
  filter(!is.na(Regjering))
# Kombiner med originaldata
data_justert <- data |>
  dplyr::select(Regjering, ar_justert, bnp) |>
  bind_rows(arvet_data)

# Oppsummer til gjennomsnitt per år og regjering
data_summary <- data_justert |>
  group_by(Regjering, ar_justert) |>
  summarise(mean_bnp = mean(bnp, na.rm = TRUE), .groups = "drop")

# Konverter ar_justert til faktor med riktig rekkefølge og etiketter
data_summary$ar_justert <- factor(
  data_summary$ar_justert,
  levels = 0:4,
  labels = c("Arvet", "1", "2", "3", "4")
)

# Definer posisjon for søylene manuelt
data_summary <- data_summary |>
  mutate(
    base_x = as.numeric(ar_justert),
    x_pos = ifelse(Regjering == "Høyre", base_x - 0.15, base_x + 0.15)
  )

# Plot
ggplot(data_summary, aes(x = x_pos, y = mean_bnp, fill = Regjering)) +
  geom_col(width = 0.3, color = "black") +
  scale_x_continuous(
    breaks = 1:5,
    labels = c("Arvet", "År 1", "År 2", "År 3", "År 4")
  ) +
  scale_fill_manual(
    values = c("Høyre" = "blue2", "Venstre" = "red3"),
    labels = c("Høyre" = "Konservativ", "Venstre" = "Sosialistisk")
  ) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "azure4", linewidth = 1) +
  labs(x = NULL, y = NULL) +
  theme_few() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed")
  )







########################

library(slider)
library(stats)
library(janitor)


# Biweight kernel-funksjon
biweight_kernel <- function(n) {
  x <- seq(-1, 1, length.out = n)
  weights <- (1 - x^2)^2
  weights / sum(weights)
}

# Analysefunksjon
analyser_detrend_gap <- function(data, kappa_values = c(Inf, 100, 67, 33)) {
  
  resultater <- lapply(kappa_values, function(kappa) {
    
    message("Analyserer kappa = ", kappa)
    
    if (is.infinite(kappa)) {
      trend_vals <- rep(mean(data$bnp, na.rm = TRUE), nrow(data))
    } else {
      kernel <- biweight_kernel(kappa)
      half_window <- (kappa - 1) %/% 2
      
      trend_vals <- slide_dbl(
        .x = data$bnp,
        .f = function(x) {
          if (length(x) == kappa) sum(x * kernel) else NA_real_
        },
        .before = half_window,
        .after = half_window,
        .complete = TRUE
      )
    }
    
    # Lag datasett med detrendede verdier
    data_mod <- data |>
      mutate(
        trend_bnp = trend_vals,
        detrended_bnp = bnp - trend_bnp
      ) |>
      filter(!is.na(detrended_bnp), Regjering %in% c("Høyre", "Venstre"))
    
    # Telle observasjoner
    gruppe_telling <- table(data_mod$Regjering)
    
    # Hvis én gruppe har færre enn 10 observasjoner, hopp over
    if (any(gruppe_telling < 10)) {
      message("Hopper over kappa = ", kappa, " pga. for få observasjoner i én gruppe:")
      print(gruppe_telling)
      return(tibble(
        kappa = kappa,
        mean_venstre = NA_real_,
        mean_hoyre   = NA_real_,
        diff         = NA_real_,
        se_diff      = NA_real_,
        p_value      = NA_real_
      ))
    }
    
    # Beregn stats
    stats <- data_mod |>
      group_by(Regjering) |>
      summarise(
        mean = mean(detrended_bnp, na.rm = TRUE),
        sd   = sd(detrended_bnp, na.rm = TRUE),
        n    = n(),
        se   = sd / sqrt(n),
        .groups = "drop"
      ) |>
      mutate(kappa = kappa)  # Legg til kappa som id
    
    # Spre til bredt format – én rad per kappa
    stats_spread <- stats |>
      pivot_wider(
        id_cols = kappa,
        names_from = Regjering,
        values_from = c(mean, se),
        values_fill = NA
      ) |>
      janitor::clean_names()
    
    # Valider antall rader (feilsikring)
    if (nrow(stats_spread) != 1) {
      warning("Flere rader for samme kappa – dette skal ikke skje.")
      return(NULL)
    }
    
    # Bruk OLS med dummy for Regjering (Konservativ som referanse)
    mod <- lm(detrended_bnp ~ Regjering, data = data_mod)
    
    # Newey-West robust standardfeil og t-statistikk
    nw_se <- sqrt(diag(sandwich::vcovHAC(mod)))
    coef_est <- coef(mod)["RegjeringVenstre"]
    
    # Beregn t-verdi og p-verdi
    t_stat <- coef_est / nw_se["RegjeringVenstre"]
    p_val <- 2 * pt(-abs(t_stat), df = mod$df.residual)
    
    # Returner resultat
    tibble(
      kappa = kappa,
      mean_venstre = stats_spread$mean_venstre,
      mean_hoyre   = stats_spread$mean_hoyre,
      diff         = stats_spread$mean_venstre - stats_spread$mean_hoyre,
      se_diff      = nw_se["RegjeringVenstre"],
      p_value      = p_val
    )
    
  })
  
  bind_rows(resultater)
}


resultater <- analyser_detrend_gap(data, kappa_values = c(Inf, 100, 67, 33))


# Lag en pen tabell
resultater |> 
  mutate(
    diff = round(diff, 3),
    se_diff = round(se_diff, 3),
    mean_venstre = round(mean_venstre, 3),
    mean_hoyre = round(mean_hoyre, 3),
    p_value = round(p_value, 3)
  ) |>
  gt() |>
  #tab_header(
   # title = "Avvik fra trend i BNP-vekst etter regjeringsblokk",
  #  subtitle = "Beregnet med ulike trendspesifikasjoner (κ)"
 # ) |>
  cols_label(
    kappa = "κ",
    mean_venstre = "Sosialistisk",
    mean_hoyre = "Konservativ",
    diff = "Forskjell",
    se_diff = "Standardfeil",
    p_value = "p-verdi"
  ) |>
  sub_missing(
    columns = everything(),
    missing_text = "–"
  ) |>
  tab_options(
    table.font.names = "Arial",
    column_labels.font.weight = "bold"
  )





##################

beregn_trend <- function(bnp_vec, kappa) {
  if (is.infinite(kappa)) {
    return(rep(mean(bnp_vec, na.rm = TRUE), length(bnp_vec)))
  }
  kernel <- biweight_kernel(kappa)
  half_window <- (kappa - 1) %/% 2
  
  slide_dbl(
    .x = bnp_vec,
    .f = function(x) if (length(x) == kappa) sum(x * kernel) else NA_real_,
    .before = half_window,
    .after = half_window,
    .complete = TRUE
  )
}

# Lag trendkolonner
data_trend <- data |>
  arrange(year) |>
  mutate(
    trend_inf = beregn_trend(bnp, Inf),
    #trend_100 = beregn_trend(bnp, 100),
    trend_67  = beregn_trend(bnp, 67),
    trend_33  = beregn_trend(bnp, 33)
  )


data_long <- data_trend |>
  pivot_longer(
    cols = starts_with("trend_"),
    names_to = "trend",
    values_to = "value"
  ) |>
  mutate(trend = recode(trend,
                        trend_inf = "κ = ∞",
                        #trend_100 = "κ = 100",
                        trend_67  = "κ = 67",
                        trend_33  = "κ = 33"))



ggplot() +
  geom_line(data = data_trend, aes(x = year, y = bnp), color = "black", linewidth = 0.5, alpha = 0.6) +
  geom_line(data = data_long, aes(x = year, y = value, color = trend, linetype = trend), linewidth = 1) +
  scale_color_manual(values = c("κ = ∞" = "black", "κ = 100" = "green", "κ = 67" = "blue", "κ = 33" = "red")) +
  scale_linetype_manual(values = c("κ = ∞" = "solid", "κ = 100" = "dashed", "κ = 67" = "dotted", "κ = 33" = "dotdash")) +
  labs(x = NULL, y = NULL,
       color="Trend", linetype="Trend") +
  theme_few() +
  theme(
    #panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    #panel.grid.major.y = element_line(color = "grey80", linetype = "dashed")
  ) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.4),
    legend.position = "right"  # evt. "top", "bottom", "left", "none"
  )









