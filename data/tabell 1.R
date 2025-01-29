
library(tidyverse)
library(stargazer)
library(glue)
library(gt)


  
data3 <- readRDS("data3.rds")
data3 <- data3 %>% 
  rename(total="Totalt for næringer",
         fiske=starts_with("Fiske"), 
         sysend="sysselsettning_endring",
         arbend="arbeidsledighet_endring") %>%
  mutate(handelbalanse=(lag(handelbalanse)-handelbalanse)/handelbalanse) %>% 
  slice(4:n()) %>% 
  slice(1:(n() - 3)) 
  


summary_table <- data3 %>%
  group_by(Regjering) %>%
  summarise(across(c(norway, total, fiske, Bergverksdrift, Industri,
                     sysend, arbend, arbeidsledighet, handelbalanse, inflasjonsvekst,
                     produktivitet, STYRINGSRENTE), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),  # Teller kun tilgjengelige verdier
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = -Regjering, names_to = c("variabel", ".value"), names_sep = "_") %>%
  pivot_wider(names_from = Regjering, values_from = c(mean, se)) %>%
  mutate(
    diff = mean_Venstre - mean_Høyre,
    diff_se = sqrt(se_Høyre^2 + se_Venstre^2)  # SE for differansen
  )

p_values <- data3 %>%
  summarise(across(c(norway, total,fiske,Bergverksdrift, Industri,
                     sysend, arbend, arbeidsledighet,
                     handelbalanse,inflasjonsvekst,
                     produktivitet,STYRINGSRENTE), 
                   ~t.test(.x ~ Regjering)$p.value, 
                   .names = "{.col}")) %>%
  pivot_longer(cols = everything(), names_to = "variabel", values_to = "p_value")

final_table <- left_join(summary_table, p_values, by = "variabel")

final_table <- final_table %>%
  mutate(
    Høyre = glue("{round(mean_Høyre, 2)} ({round(se_Høyre, 2)})"),
    Venstre = glue("{round(mean_Venstre, 2)} ({round(se_Venstre, 2)})"),
    Forskjell = glue("{round(diff, 2)} ({round(diff_se, 2)})"),
    p_value = round(p_value, 3)
  ) %>%
  select(variabel,Venstre , Høyre, Forskjell, p_value)

final_table <- final_table %>%
  mutate(
    variabel = case_when(
      variabel == "norway" ~ "BNP per innbygger",
      variabel == "total" ~ "Total for alle næringer",
      variabel == "fiske" ~ "Fiskeri næringen",
      variabel == "Bergverksdrift" ~ "Olje og gass",
      variabel == "Industri" ~ "Industri",
      variabel == "sysend" ~ "Sysselsetting utvikling",
      variabel == "arbend" ~ "Arbeidsledighet utvikling",
      variabel == "arbeidsledighet" ~ "Arbeidsledighet nivå",
      variabel == "handelbalanse" ~ "Handelbalanse",
      variabel == "inflasjonsvekst" ~ "Inflasjonsvekst",
      variabel == "produktivitet" ~ "Produktivitetsvekst",
      variabel == "STYRINGSRENTE" ~ "Styringsrente",
      TRUE ~ variabel  # Beholder andre variabler uendret
    )
  )



final_table <- final_table %>%
  mutate(
    panel = case_when(
      variabel %in% c("BNP per innbygger", "Total for alle næringer", "Fiskeri næringen",
                      "Olje og gass", "Industri" ) ~ "Panel A: Økonomiske indikatorer",
      variabel %in% c("Sysselsetting utvikling", "Arbeidsledighet nivå", 
                      "Arbeidsledighet utvikling") ~ "Panel B: Arbeidsmarked",
      variabel %in% c("Produktivitetsvekst") ~ "Panel C: Real lønn og prduktivitet",
      variabel %in% c("Handelbalanse") ~ "Panel D: Goverment tall",
      variabel %in% c("Inflasjonsvekst") ~ "Panel E: Inflasjon",
      variabel %in% c("Styringsrente") ~ "Panel F: Renter",
      TRUE ~ "Andre variabler"
    )
  ) %>%
  arrange(panel, variabel)



final_table <- final_table %>%
  mutate(panel = factor(panel, levels = c(
    "Panel A: Økonomiske indikatorer",
    "Panel B: Arbeidsmarked",
    "Panel C: Real lønn og prduktivitet",
    "Panel D: Goverment tall",
    "Panel E: Inflasjon",
    "Panel F: Renter"
  ))) %>%
  arrange(panel)  # Sorterer tabellen slik at Panel A kommer først


final_table %>%
  select(-panel) %>%  # Fjerner panel-kolonnen etter at vi har brukt den til sortering
  gt() %>%
  tab_header(
    title = "Summary Statistics",
    subtitle = "Gjennomsnittlig verdi basert på regjering"
  ) %>%
  tab_row_group(
    group = "Panel F: Renter",
    rows = variabel %in% c("Styringsrente")
  ) %>%
  tab_row_group(
    group = "Panel E: Inflasjon",
    rows = variabel %in% c("Inflasjonsvekst")
  ) %>%
  tab_row_group(
    group = "Panel D: Goverment tall",
    rows = variabel %in% c("Handelbalanse")
  ) %>%
  tab_row_group(
    group = "Panel C: Real lønn og prduktivitet",
    rows = variabel %in% c("Produktivitetsvekst")
  ) %>%
  tab_row_group(
    group = "Panel B: Arbeidsmarked",
    rows = variabel %in% c("Sysselsetting utvikling","Arbeidsledighet nivå", "Arbeidsledighet utvikling")
  ) %>%
  tab_row_group(
    group = "Panel A: Økonomiske indikatorer",
    rows = variabel %in% c("BNP per innbygger", "Fiskeri næringen", "Industri", "Olje og gass", "Total for alle næringer")
  ) %>% 
  cols_label(
    variabel = "Variabel",
    Venstre = "Venstre",
    Høyre = "Høyre",
    Forskjell = "Forskjell",
    p_value = "p-verdi"
  ) %>%
  tab_options(
    row_group.as_column = FALSE,  # Holder panelene som overskrifter
    row_group.font.weight = "bold",
    table.font.size = "medium"
  )

