data3 <- readRDS("data3.rds")
library(tidyverse)
library(stargazer)
library(glue)
library(gt)


data3 <- data3 %>% 
  rename(total="Totalt for næringer") %>% 
  select(year, Regjering, norway, total) %>% 
  slice(4:n()) %>% 
  slice(1:(n() - 3))

summary_table <- data3 %>%
  group_by(Regjering) %>%
  summarise(across(c(norway, total), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        se = ~sd(.x, na.rm = TRUE) / sqrt(n())),  # Bruker SE i stedet for SD
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = -Regjering, names_to = c("variabel", ".value"), names_sep = "_") %>%
  pivot_wider(names_from = Regjering, values_from = c(mean, se)) %>%
  mutate(
    diff = mean_Venstre - mean_Høyre,
    diff_se = sqrt(se_Høyre^2 + se_Venstre^2)  # SE for differansen
  )

p_values <- data3 %>%
  summarise(across(c(norway, total), 
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
      variabel == "total" ~ "Total for alle Næringer",
      TRUE ~ variabel  # Beholder andre variabler uendret
    )
  )

final_table %>%
  gt() %>%
  cols_label(
    variabel = "Variabel",
    Venstre = "Venstre",
    Høyre = "Høyre ",
    Forskjell = "Forskjell",
    p_value = "p-verdi"
  )
