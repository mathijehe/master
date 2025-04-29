library(tidyverse)
library(gt)
library(glue)
library(sandwich)
library(lmtest)

data3 <- readRDS("data3.rds")

data3 <- data3 %>%
  rename(
    total = "Totalt for næringer",
    fiske = starts_with("Fiske"),
    sysend = "sysselsettning_endring",
    arbend = "arbeidsledighet_endring"
  ) %>%
  mutate(
    handelbalanse = (lag(handelbalanse) - handelbalanse) / handelbalanse
  ) %>%
  slice(4:n()) %>%
  slice(1:(n() - 3)) %>%
  filter(year >= 1978 & year <= 2021)   # <-- ✨ NEW LINE: only keep 1978-2021


# Variables
vars <- c("norway", "total", "fiske", "Bergverksdrift", "Industri",
          "sysend", "arbend", "arbeidsledighet", "handelbalanse",
          "inflasjonsvekst", "produktivitet", "STYRINGSRENTE", "OSEBX", "SPXTR")

# Final Table
summary_table <- map_dfr(vars, function(var) {
  df <- data3 %>% select(Regjering, all_of(var)) %>% drop_na()
  
  # Classical SEs
  means <- df %>% group_by(Regjering) %>%
    summarise(
      mean = mean(.data[[var]]),
      se = sd(.data[[var]]) / sqrt(n()),
      .groups = "drop"
    )
  
  # Difference
  diff_value <- means$mean[means$Regjering == "Venstre"] - means$mean[means$Regjering == "Høyre"]
  se_diff_classical <- sqrt(
    means$se[means$Regjering == "Venstre"]^2 +
      means$se[means$Regjering == "Høyre"]^2
  )
  
  # Regression for NW robust SE
  df <- df %>%
    mutate(Regjering_binary = ifelse(Regjering == "Venstre", 1, 0))
  
  model <- lm(df[[var]] ~ Regjering_binary, data = df)
  nw_se <- sqrt(diag(vcovHAC(model)))[2]
  
  # t-stat and p-value
  t_stat <- diff_value / nw_se
  p_val <- 2 * pt(-abs(t_stat), df = Inf)
  
  tibble(
    variabel = var,
    
    mean_venstre = means$mean[means$Regjering == "Venstre"],
    se_venstre = means$se[means$Regjering == "Venstre"],
    
    mean_hoyre = means$mean[means$Regjering == "Høyre"],
    se_hoyre = means$se[means$Regjering == "Høyre"],
    
    diff = diff_value,
    se_diff = se_diff_classical,
    nw_se_diff = nw_se,
    p_value = p_val
  )
})

# Format the final display
final_table <- summary_table %>%
  mutate(
    Sosialistisk = glue("{round(mean_venstre, 2)} ({round(se_venstre, 2)}) [{round(se_venstre, 2)}]"),
    Konservativ = glue("{round(mean_hoyre, 2)} ({round(se_hoyre, 2)}) [{round(se_hoyre, 2)}]"),
    Forskjell = glue("{round(diff, 2)} ({round(se_diff, 2)}) [{round(nw_se_diff, 2)}]"),
    p_value = ifelse(p_value < 0.001, "<0.001", round(p_value, 3))
  ) %>%
  select(variabel, Sosialistisk, Konservativ, Forskjell, p_value)

# Rename nicely
final_table <- final_table %>%
  mutate(
    variabel = case_when(
      variabel == "norway" ~ "BNP per innbygger",
      variabel == "total" ~ "Total for alle næringer (VR)",
      variabel == "fiske" ~ "Fiskerinæringen (VR)",
      variabel == "Bergverksdrift" ~ "Olje og gass (VR)",
      variabel == "Industri" ~ "Industri (VR)",
      variabel == "sysend" ~ "Sysselsetting: utvikling (VR)",
      variabel == "arbend" ~ "Arbeidsledighet: utvikling (VR)",
      variabel == "arbeidsledighet" ~ "Arbeidsledighet: nivå (%)",
      variabel == "handelbalanse" ~ "Handelbalanse (VR)",
      variabel == "inflasjonsvekst" ~ "Inflasjonsvekst (%)",
      variabel == "produktivitet" ~ "Produktivitetsvekst (VR)",
      variabel == "STYRINGSRENTE" ~ "Styringsrente (%)",
      variabel == "OSEBX" ~ "OSEBX (%)",
      variabel == "SPXTR" ~ "S&P500 Index (%)",
      TRUE ~ variabel
    )
  )

# Assign panels
final_table <- final_table %>%
  mutate(
    panel = case_when(
      variabel %in% c("BNP per innbygger", "Total for alle næringer (VR)", "Fiskerinæringen (VR)",
                      "Olje og gass (VR)", "Industri (VR)") ~ "Panel A: Økonomiske indikatorer",
      variabel %in% c("Sysselsetting: utvikling (VR)", "Arbeidsledighet: nivå (%)", "Arbeidsledighet: utvikling (VR)") ~ "Panel B: Arbeidsmarked",
      variabel %in% c("OSEBX (%)", "S&P500 Index (%)") ~ "Panel C: Stock returns",
      variabel %in% c("Produktivitetsvekst (VR)") ~ "Panel D: Real lønn og produktivitet",
      variabel %in% c("Handelbalanse (VR)") ~ "Panel E: Government tall",
      variabel %in% c("Inflasjonsvekst (%)") ~ "Panel F: Inflasjon",
      variabel %in% c("Styringsrente (%)") ~ "Panel G: Renter",
      TRUE ~ "Andre variabler"
    )
  ) %>%
  arrange(panel, variabel)

final_table <- final_table %>%
  filter(variabel != "BNP per innbygger")

# Create final gt() Table
final_table %>%
  select(-panel) %>%
  gt() %>%
  tab_header(
    title = "",
    subtitle = ""
  ) %>%
  tab_row_group(
    label = "Panel G: Renter",
    rows = variabel == "Styringsrente (%)"
  ) %>%
  tab_row_group(
    label = "Panel F: Inflasjon",
    rows = variabel == "Inflasjonsvekst (%)"
  ) %>%
  tab_row_group(
    label = "Panel E: Internasjonal handel",
    rows = variabel == "Handelbalanse (VR)"
  ) %>%
  tab_row_group(
    label = "Panel D: Real lønn og produktivitet",
    rows = variabel == "Produktivitetsvekst (VR)"
  ) %>%
  tab_row_group(
    label = "Panel C: Avkastning aksjemarkeder",
    rows = variabel %in% c("OSEBX (%)", "S&P500 Index (%)")
  ) %>%
  tab_row_group(
    label = "Panel B: Arbeidsmarked",
    rows = variabel %in% c("Sysselsetting: utvikling (VR)", "Arbeidsledighet: nivå (%)", "Arbeidsledighet: utvikling (VR)")
  ) %>%
  tab_row_group(
    label = "Panel A: Økonomiske indikatorer",
    rows = variabel %in% c("BNP per innbygger", "Fiskerinæringen (VR)", "Industri (VR)", "Olje og gass (VR)", "Total for alle næringer (VR)")
  ) %>%
  cols_label(
    variabel = "Variabel",
    Sosialistisk = "Sosialistisk",
    Konservativ = "Konservativ",
    Forskjell = "Forskjell",
    p_value = "p-verdi"
  ) %>%
  tab_options(
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12,
    table.border.top.style = "solid",
    table.border.bottom.style = "none",
    table_body.border.bottom.style = "none",
    row_group.border.top.style = "solid",
    row_group.border.bottom.style = "none",
    row_group.font.weight = "bold"
  )










library(lmtest)
library(dplyr)
library(broom)

# List of variables to test (these match your final_table)
variables_to_test <- c("norway", "total", "fiske", "Bergverksdrift", "Industri",
                       "sysend", "arbend", "arbeidsledighet", "handelbalanse", 
                       "inflasjonsvekst", "produktivitet", "STYRINGSRENTE", 
                       "OSEBX", "SPXTR")

# Initialize an empty data frame to store test results
diagnostic_results <- data.frame(
  Variable = character(),
  BP_p_value = numeric(),
  DW_p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each variable and run BP and DW tests
for (var in variables_to_test) {
  formula <- as.formula(glue::glue("{var} ~ Regjering"))
  model <- lm(formula, data = data3)
  
  # Breusch-Pagan test
  bp_test <- bptest(model)
  
  # Durbin-Watson test
  dw_test <- dwtest(model)
  
  # Store results
  diagnostic_results <- rbind(
    diagnostic_results,
    data.frame(
      Variable = var,
      BP_p_value = round(bp_test$p.value, 3),
      DW_p_value = round(dw_test$p.value, 3)
    )
  )
}

# Clean variable names for readability
diagnostic_results$Variable <- recode(diagnostic_results$Variable,
                                      norway = "BNP per innbygger",
                                      total = "Total for alle næringer",
                                      fiske = "Fiskeri næringen",
                                      Bergverksdrift = "Olje og gass",
                                      Industri = "Industri",
                                      sysend = "Sysselsetting utvikling",
                                      arbend = "Arbeidsledighet utvikling",
                                      arbeidsledighet = "Arbeidsledighet nivå",
                                      handelbalanse = "Handelbalanse",
                                      inflasjonsvekst = "Inflasjonsvekst",
                                      produktivitet = "Produktivitetsvekst",
                                      STYRINGSRENTE = "Styringsrente",
                                      OSEBX = "OSEBX",
                                      SPXTR = "S&P500 Index"
)

# Print results
print(diagnostic_results)




# Load necessary libraries
library(lmtest)
library(sandwich)
library(dplyr)
library(tidyr)

# Function to compute HAC SEs
compute_HAC_SE <- function(var) {
  model <- lm(as.formula(paste(var, "~ Regjering")), data = data3)
  sqrt(diag(vcovHAC(model)))[1]  # Extract Newey-West SE
}

# Compute means and ordinary standard errors
summary_table <- data3 %>%
  group_by(Regjering) %>%
  summarise(across(c(norway, total, fiske, Bergverksdrift, Industri,
                     sysend, arbend, arbeidsledighet, handelbalanse, inflasjonsvekst,
                     produktivitet, STYRINGSRENTE, OSEBX, SPXTR), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))), 
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = -Regjering, names_to = c("variabel", ".value"), names_sep = "_") %>%
  pivot_wider(names_from = Regjering, values_from = c(mean, se))

# Compute Newey-West SEs for each variable
nw_se_values <- tibble(
  variabel = unique(summary_table$variabel),
  NW_SE_Høyre = sapply(unique(summary_table$variabel), compute_HAC_SE),
  NW_SE_Venstre = sapply(unique(summary_table$variabel), compute_HAC_SE)
)

# Merge Newey-West SEs into the table
summary_table <- summary_table %>%
  left_join(nw_se_values, by = "variabel") %>%
  mutate(
    diff = mean_Venstre - mean_Høyre,
    diff_se = sqrt(NW_SE_Høyre^2 + NW_SE_Venstre^2)  # Newey-West SE for difference
  )

# Format for final output
final_table <- summary_table %>%
  mutate(
    Sosialistisk = glue("{round(mean_Venstre, 2)} ({round(se_Venstre, 2)}) [{round(NW_SE_Venstre, 2)}]"),
    Konservativ = glue("{round(mean_Høyre, 2)} ({round(se_Høyre, 2)}) [{round(NW_SE_Høyre, 2)}]"),
    Forskjell = glue("{round(diff, 2)} ({round(diff_se, 2)})")
  ) %>%
  select(variabel, Sosialistisk, Konservativ, Forskjell)

# Print the updated table
print(final_table)


# Load necessary libraries
library(lmtest)
library(sandwich)
library(dplyr)
library(tidyr)

# Select relevant numeric variables
numeric_vars <- c("norway", "total", "fiske", "Bergverksdrift", "Industri",
                  "sysend", "arbend", "arbeidsledighet", "handelbalanse", 
                  "inflasjonsvekst", "produktivitet", "STYRINGSRENTE", 
                  "OSEBX", "SPXTR")

# Initialize dataframe to store test results
test_results <- data.frame(Variable = numeric_vars,
                           Breusch_Pagan_p = NA,
                           Durbin_Watson_p = NA,
                           HC_Required = NA,
                           HAC_Required = NA)

# Loop through each variable to run tests
for (var in numeric_vars) {
  
  # Fit a simple linear model: Variable ~ Ideologi
  formula <- as.formula(paste(var, "~ Regjering"))
  model <- lm(formula, data = data3)
  
  # Perform Breusch-Pagan test for heteroskedasticity
  bp_test <- bptest(model)
  bp_p_value <- bp_test$p.value
  
  # Perform Durbin-Watson test for autocorrelation
  dw_test <- dwtest(model)
  dw_p_value <- dw_test$p.value
  
  # Determine if HC or HAC standard errors are needed
  hc_needed <- ifelse(bp_p_value < 0.05, "Yes", "No")
  hac_needed <- ifelse(dw_p_value < 0.05, "Yes", "No")
  
  # Store results
  test_results[test_results$Variable == var, ] <- c(var, bp_p_value, dw_p_value, hc_needed, hac_needed)
}

# Convert numeric values to numeric type
test_results <- test_results %>%
  mutate(across(Breusch_Pagan_p:Durbin_Watson_p, as.numeric))

# Print results
print(test_results)





data4 <- readRDS("data4.rds")
#tabell predeksjon, fra 1991

stat <- data4 %>% 
  slice(1:(n() - 24)) %>% 
  select(Regjering,year, BNP_pred, `BNP fastlands Norge`,ar)

stat <- stat[stat$ar == "1", ]
stat <- stat %>% 
  group_by(Regjering) %>%
  summarise(
    Predikert = mean(BNP_pred, na.rm = TRUE),
    Faktisk = mean(`BNP fastlands Norge`, na.rm = TRUE),
    Forskjell = Faktisk - Predikert  # Avvik mellom faktisk og predikert BNP
  ) %>%
  pivot_longer(cols = c(Predikert, Faktisk, Forskjell),
               names_to = "Type",
               values_to = "Value")

stat %>%
  pivot_wider(names_from = Regjering, values_from = Value) %>%
  gt() %>%
  tab_header(
    title = "BNP Vekstprognoser",
    subtitle = "Gjennomsnittlig BNP-vekst i første år av regjeringens periode"
  ) %>%
  cols_label(
    Type = " ",
    Venstre = "Venstre",
    Høyre = "Høyre",
  ) %>%
  fmt_number(
    columns = c(Venstre, Høyre),
    decimals = 1
  ) %>%
  tab_style(
    style = cell_borders(sides = "top", weight = px(1)),  # Tykk linje over Difference
    locations = cells_body(
      columns = everything(),
      rows = Type == "Forskjell"
    )
  ) %>%
  tab_options(
    table.font.size = "medium"
  )


