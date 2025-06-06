data <- readRDS("data.rds")
data2 <- readRDS("data2.rds")
data3 <- readRDS("data3.rds")
library(tidyverse)
library(ggthemes)
library(broom)
library(moments)
library(knitr)
library(gt)
library(DT)


##FIGUR 1 ÅRLIG
data2 %>% 
  slice(1:(n() - 3)) %>% 
  slice(4:n()) %>% 
  group_by(...2) %>% 
  mutate(mean = mean(BNP)) %>% 
  arrange(year) %>% 
  ggplot(aes(x = reorder(...2, year), y = mean, fill = Regjering)) +
  geom_col(position = "dodge") +
  labs(
       x = NULL,  # Fjerner tekst på x-aksen
       y = NULL) + # Fjerner tekst på y-aksen
  theme_few() +
  scale_fill_manual(values = c("Høyre" = "blue2", "Venstre" = "red3"),
                    labels = c("Høyre" = "Konservativ", "Venstre" = "Sosialistisk")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)) +  # Roterer teksten 45 grader
  scale_y_continuous(breaks = c(1,2,3,4,5))



library(ggplot2)
library(dplyr)
library(ggthemes)

# Beregn gjennomsnitt for hver statsminister og for hele partiet
data2_processed <- data2 %>%
  filter(year >= 1978 & year <= 2021) %>%
  group_by(Statsminister, Regjering) %>%
  mutate(mean = mean(`BNP fastlands Norge`)) %>%
  ungroup() %>%
  group_by(Regjering) %>%
  mutate(overall_mean = mean(`BNP fastlands Norge`)) %>%
  ungroup() %>%
  rename(navn = ...2)


# Lag en egen dataframe for partigjennomsnittene (for å plassere dem separat)
avg_data <- data2_processed %>%
  distinct(Regjering, overall_mean) %>%
#<<<<<<< HEAD
  mutate(navn = paste(Regjering, "Gj.snitt")) %>%  # Setter etikett "Høyre Avg" / "Venstre Avg"
#saveRDS(avg_data, "avg_data.rds")
#=======
  mutate(navn = paste(Regjering, "Gj.snitt"))
#>>>>>>> bfeb5c2a530e701221458ceea9a5ee7390f8d44b

# Kombiner statsministre og gjennomsnitt i én dataframe
plot_data <- bind_rows(data2_processed, avg_data)

# Lag plottet
ggplot(plot_data, aes(x = reorder(navn, year), y = mean, fill = Regjering)) +
  
  geom_col(data = filter(plot_data, !grepl("Gj.snitt", navn)),
           position = position_dodge(), width = 0.6) +
  
  geom_col(data = filter(plot_data, grepl("Gj.snitt", navn)),
           aes(y = overall_mean, fill = Regjering), width = 0.6) +
  
  labs(title = "",
       x = NULL,  
       y = "") + 
  theme_few() +
  scale_fill_manual(values = c("Konservativ" = "blue2", "Sosialistisk" = "red3")) +
  
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed")
  ) +
  
  scale_y_continuous(breaks = seq(0, max(data2$`BNP fastlands Norge`, na.rm = TRUE), by = 1))   # <--- Also updated here

mean(data2_processed$`BNP fastlands Norge`, na.rm = TRUE)

data2_processed %>%
  group_by(Regjering) %>%
  summarise(mean_bnp_fastlands = mean(`BNP fastlands Norge`, na.rm = TRUE))


#FIGUR 1 KVARTALSVIS
library(ggplot2)
library(dplyr)
library(ggthemes)

# Step 1: Prepare data using BNP vekstrate from data_clean
data_clean_plot <- data_clean %>%
  mutate(
    `BNP vekstrate` = as.numeric(`BNP vekstrate`),
    year = as.numeric(substr(Tidsperiode, 1, 4))
  ) %>%
  filter(year >= 1978 & year <= 2021) %>%
  group_by(Statsminister, Ideologi) %>%
  mutate(mean = mean(`BNP vekstrate`, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Ideologi) %>%
  mutate(overall_mean = mean(`BNP vekstrate`, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(navn = Statsminister)


# Step 2: Create party-wide means for overlay bars
avg_data <- data_clean_plot %>%
  distinct(Ideologi, overall_mean) %>%
  mutate(navn = paste(Ideologi, "Gj.snitt"))

# Step 3: Combine all data
plot_data <- bind_rows(data_clean_plot, avg_data)

# Step 4: Plot
ggplot(plot_data, aes(x = reorder(navn, year), y = mean, fill = Ideologi)) +
  
  geom_col(data = filter(plot_data, !grepl("Gj.snitt", navn)),
           position = position_dodge(), width = 0.6) +
  
  geom_col(data = filter(plot_data, grepl("Gj.snitt", navn)),
           aes(y = overall_mean, fill = Ideologi), width = 0.6) +
  
  labs(title = "",
       x = NULL,
       y = NULL) +
  theme_few() +
  scale_fill_manual(values = c("Konservativ" = "blue2", "Sosialistisk" = "red3")) +
  
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed")
  ) +
  
  scale_y_continuous(breaks = seq(0, max(data_clean$`BNP vekstrate`, na.rm = TRUE), by = 1))

# Coerce to numeric first, then compute the overall mean
mean(as.numeric(data_clean$`BNP vekstrate`), na.rm = TRUE)

# GDP growth by ideology
data_clean %>%
  mutate(`BNP vekstrate` = as.numeric(`BNP vekstrate`)) %>%
  group_by(Ideologi) %>%
  summarise(mean_bnp_growth = mean(`BNP vekstrate`, na.rm = TRUE))




data1 <- data %>% 
  filter(forrige == 1) %>% 
  group_by(Regjering) %>% 
  mutate(arvet = mean(BNP))

l1 <- data %>% 
  filter(ar < 5) %>% 
  slice(1:(n() - 3)) %>% 
  group_by(ar,Regjering) %>% 
  mutate(mean_ar = mean(BNP)) %>% 
  select(ar,mean_ar, Regjering,forrige)

l2 <- data %>% 
  filter(forrige == 1) %>% 
  group_by(Regjering) %>% 
  mutate(arvet = mean(BNP)) %>%
  ungroup() %>% 
  select(ar,arvet, Regjering,forrige) %>% 
  mutate(ar = 5) %>% 
  mutate(Regjering = ifelse(Regjering == "Venstre", "Høyre1", Regjering)) %>% 
  mutate(Regjering = ifelse(Regjering == "Høyre", "Venstre", Regjering)) %>% 
  mutate(Regjering = ifelse(Regjering == "Høyre1", "Høyre", Regjering))
 
l3 <- data.frame(
  mean = c(l1$mean_ar, l2$arvet),
  ar = c(l1$ar,l2$ar),
  Regjering = c(l1$Regjering, l2$Regjering)
)



l3 %>%
ggplot(aes(x = ar, y = mean, fill = Regjering)) +
  geom_col(position = "dodge", width = 0.7) + # Separate søyler for hver gruppe
  scale_fill_manual(values = c("Høyre" = "blue2", "Venstre" = "red3")) + # Tilpassede farger
  geom_vline(xintercept = 4.5, linetype = "solid", color = "Black", size = 0.8) + # Vertikal linje
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Roter x-akse etiketter
    legend.position = "right", # Flytt legenden
    panel.grid.major.x = element_blank() # Fjern vertikale grid-linjer
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5),  # Hvor tekstene skal vises
    labels = c("År 1", "År 2", "År 3", "År 4", "Arvet")  # Tekstene du vil vise
  ) +
  labs(x = "Custom X Labels", y = "Value") +
  labs(
    title = "BNP vekst per år, for en regjeringsperiode",
    x = NULL,  # Fjern x-akse-tekst
    y = NULL   # Fjern y-akse-tekst
  ) +
  theme_classic()


 data2 %>% 
  select(year, kvartal,"BNP"="Bruttonasjonalprodukt, markedsverdi",
         "BNP_fast" = "Bruttonasjonalprodukt Fastlands-Norge, markedsverdi", Regjering) %>% 
  ggplot(aes(BNP, fill = Regjering)) +
   geom_histogram(position = "dodge") +
   scale_x_continuous(breaks = c(-1,-2,-3,0, -4,-5,1,2,3,4,5)) +
   scale_fill_manual(values = c("Høyre" = "blue2", "Venstre" = "red3")) +
   theme_classic()


 summary_stat <- data2 %>% 
   select(year, kvartal,"BNP"="Bruttonasjonalprodukt, markedsverdi",
          "BNP_fast" = "Bruttonasjonalprodukt Fastlands-Norge, markedsverdi", Regjering) %>%
   group_by(Regjering) %>% 
   summarise(
     N = n(),
     Mean = mean(BNP, na.rm = TRUE),
     Median = median(BNP, na.rm = TRUE),
     Std_Dev = sd(BNP, na.rm = TRUE),
     Skewness = skewness(BNP, na.rm = TRUE),
     Kurtosis = kurtosis(BNP, na.rm = TRUE) - 3, # Excess kurtosis
     Min = min(BNP, na.rm = TRUE),
     Max = max(BNP, na.rm = TRUE)
   ) 
 summary_stat <- summary_stat %>%
   pivot_longer(
     cols = c(N, Mean, Median, Std_Dev, Skewness, Kurtosis, Min, Max), 
     names_to = "Statistic", 
     values_to = "Value"
   ) %>%
   pivot_wider(
     names_from = Regjering,  # Lager egne kolonner for Høyre og Venstre
     values_from = Value      # Fyller verdier inn i de nye kolonnene
   ) 
 
 summary_stat %>%
   gt() %>%
   tab_header(
     title = "Summary Statistics",
     subtitle = "Descriptive statistics for the dataset"
   ) %>%
   fmt_number(
     columns = where(is.numeric),
     decimals = 2
   ) %>%
   cols_label(
     Statistic = "Statistic",
     Høyre = "Høyre",
     Venstre = "Venstre"
   ) %>%
   fmt_number(
     rows = Statistic == "N",    # Formater kun radene for "N"
     decimals = 0                # Ingen desimaler for "N"
   )

 

data3 %>% 
  ggplot(aes(year, norway)) +
  geom_line() +
  labs(title = "Vekst i BNP per innbygger",
       x = NULL,  # Fjerner tekst på x-aksen
       y = NULL) + # Fjerner tekst på y-aksen
  theme_classic()



##TABELL 1
saveRDS(BNP_Fastlands_kvartal, "BNP_Fastlands_kvartal.rds")


# ===========================================
# STEP 1: Load Libraries
# ===========================================

library(dplyr)
library(readr)
library(lmtest)
library(sandwich)
library(broom)
library(gt)

# ===========================================
# STEP 2: Load & Clean Dataset
# ===========================================

data_raw <- read_rds("BNP_Fastlands_kvartal.rds")

data_clean <- data_raw %>%
  rename(
    Tidsperiode = `09190: Makroøkonomiske hovedstørrelser, etter kvartal, statistikkvariabel og makrostørrelse`,
    BNP = `...2`,
    BNP_Fastlands_Norge = `...3`,
    Regjeringsparti = `...4`,
    Statsminister = `...5`,
    Ideologi = `...6`
  ) %>%
  filter(
    !is.na(Tidsperiode),
    grepl("^[0-9]{4}K[1-4]$", Tidsperiode),
    Tidsperiode >= "1978K2" & Tidsperiode <= "2021K3"
  ) %>%
  rename(`BNP vekstrate` = BNP_Fastlands_Norge)

data_clean <- data_clean %>%
  mutate(
    Regjeringsparti = if_else(Tidsperiode == "1997K4", "KrF", Regjeringsparti),
    Statsminister   = if_else(Tidsperiode == "1997K4", "Bondevik 1", Statsminister),
    Ideologi        = if_else(Tidsperiode == "1997K4", "Konservativ", Ideologi)
  )

# ===========================================
# STEP 3: Identify Recession Quarters
# ===========================================

data_clean <- data_clean %>%
  arrange(Tidsperiode) %>%
  mutate(Recession = ifelse(lag(`BNP vekstrate`, 1) < 0 & `BNP vekstrate` < 0, 1, 0))

# ===========================================
# STEP 4: Cap Terms at Max 16 Quarters
# ===========================================

data_terms_split <- data_clean %>%
  arrange(Statsminister, Tidsperiode) %>%
  group_by(Statsminister) %>%
  mutate(
    quarter_index = row_number(),
    pseudo_term = (quarter_index - 1) %/% 16 + 1
  ) %>%
  ungroup() %>%
  mutate(
    Term_ID = paste(Statsminister, pseudo_term, sep = "_")
  )

n_terms_by_ideology <- data_terms_split %>%
  group_by(Ideologi, Term_ID) %>%
  summarise(
    Term_Length = n(),
    .groups = "drop"
  ) %>%
  group_by(Ideologi) %>%
  summarise(
    Number_of_Terms = n(),
    Average_Term_Length = mean(Term_Length) / 4  # In years
  )

# ===========================================
# STEP 5: Compute GDP Statistics
# ===========================================

data_gdp <- data_terms_split %>%
  dplyr::select(Ideologi, `BNP vekstrate`) %>%
  mutate(`BNP vekstrate` = as.numeric(`BNP vekstrate`))

gdp_stats <- data_gdp %>%
  group_by(Ideologi) %>%
  summarise(
    Avg_GDP_Growth = mean(`BNP vekstrate`, na.rm = TRUE),
    Standard_Error = sd(`BNP vekstrate`, na.rm = TRUE) / sqrt(n())
  )

gdp_nw_se <- data_gdp %>%
  group_by(Ideologi) %>%
  summarise(
    NW_SE = sqrt(diag(vcovHAC(lm(`BNP vekstrate` ~ 1, data = cur_data()))))[1]
  )

gdp_stats <- gdp_stats %>%
  left_join(gdp_nw_se, by = "Ideologi")

# ===========================================
# STEP 6.1: Compute Recession Share
# ===========================================

recession_stats <- data_terms_split %>%
  group_by(Ideologi) %>%
  summarise(
    Total_Recession_Quarters = sum(Recession, na.rm = TRUE),
    Total_Quarters = n(),
    Recession_Share = Total_Recession_Quarters / Total_Quarters
  ) %>%
  left_join(n_terms_by_ideology, by = "Ideologi") %>%
  mutate(
    Average_Recession_Quarters_per_4year = Recession_Share * 16,
    Average_Recession_Quarters_per_term  = Recession_Share * Average_Term_Length * 4
  )

# ===========================================
# STEP 6.2: Classical SE for Recession Share
# ===========================================

recession_se <- data_terms_split %>%
  group_by(Ideologi) %>%
  summarise(
    SE_Recession = sd(Recession, na.rm = TRUE) / sqrt(n_distinct(Term_ID))
  )

# ===========================================
# STEP 6.3: Newey-West SEs
# ===========================================

nw_se_recession <- data_terms_split %>%
  group_by(Ideologi) %>%
  summarise(
    NW_SE_Recession = sqrt(diag(vcovHAC(lm(Recession ~ 1, data = cur_data()))))[1]
  )

term_data <- data_terms_split %>%
  group_by(Ideologi, Term_ID) %>%
  summarise(Recession_Quarters = sum(Recession, na.rm = TRUE), .groups = "drop")

nw_se_recession_term <- term_data %>%
  group_by(Ideologi) %>%
  summarise(
    NW_SE_Recession_per_term = sqrt(diag(vcovHAC(lm(Recession_Quarters ~ 1, data = cur_data()))))[1]
  )

# ===========================================
# STEP 6.4: Merge Recession Statistics
# ===========================================

recession_stats <- recession_stats %>%
  left_join(recession_se, by = "Ideologi") %>%
  left_join(nw_se_recession, by = "Ideologi") %>%
  left_join(nw_se_recession_term, by = "Ideologi")

# ===========================================
# STEP 7: Merge GDP & Recession Stats
# ===========================================

table_1 <- gdp_stats %>%
  left_join(recession_stats, by = "Ideologi")

# ===========================================
# STEP 8: Compute Differences & SEs
# ===========================================

diff_gdp <- table_1$Avg_GDP_Growth[table_1$Ideologi == "Sosialistisk"] - 
  table_1$Avg_GDP_Growth[table_1$Ideologi == "Konservativ"]

diff_recession_4year <- table_1$Average_Recession_Quarters_per_4year[table_1$Ideologi == "Sosialistisk"] - 
  table_1$Average_Recession_Quarters_per_4year[table_1$Ideologi == "Konservativ"]

diff_recession_term <- table_1$Average_Recession_Quarters_per_term[table_1$Ideologi == "Sosialistisk"] - 
  table_1$Average_Recession_Quarters_per_term[table_1$Ideologi == "Konservativ"]

se_diff_gdp <- sqrt(
  table_1$Standard_Error[table_1$Ideologi == "Sosialistisk"]^2 +
    table_1$Standard_Error[table_1$Ideologi == "Konservativ"]^2
)

se_diff_gdp_nw <- sqrt(
  table_1$NW_SE[table_1$Ideologi == "Sosialistisk"]^2 +
    table_1$NW_SE[table_1$Ideologi == "Konservativ"]^2
)

se_diff_recession <- sqrt(
  table_1$SE_Recession[table_1$Ideologi == "Sosialistisk"]^2 +
    table_1$SE_Recession[table_1$Ideologi == "Konservativ"]^2
)

se_diff_recession_4year_nw <- sqrt(
  table_1$NW_SE_Recession[table_1$Ideologi == "Sosialistisk"]^2 +
    table_1$NW_SE_Recession[table_1$Ideologi == "Konservativ"]^2
)

se_diff_recession_term_nw <- sqrt(
  table_1$NW_SE_Recession_per_term[table_1$Ideologi == "Sosialistisk"]^2 +
    table_1$NW_SE_Recession_per_term[table_1$Ideologi == "Konservativ"]^2
)

# ===========================================
# STEP 9: T-Tests (p-values) using NW SEs
# ===========================================

t_stat_gdp_nw <- diff_gdp / se_diff_gdp_nw
t_stat_recession_4year_nw <- diff_recession_4year / se_diff_recession_4year_nw
t_stat_recession_term_nw <- diff_recession_term / se_diff_recession_term_nw

p_value_gdp_nw <- 2 * pt(-abs(t_stat_gdp_nw), df = Inf)
p_value_recession_4year_nw <- 2 * pt(-abs(t_stat_recession_4year_nw), df = Inf)
p_value_recession_term_nw <- 2 * pt(-abs(t_stat_recession_term_nw), df = Inf)

# ===========================================
# STEP 10: Final Table Output
# ===========================================

table_1_final <- data.frame(
  Variable = c("BNP (VR)", "Kvartaler i resesjon (Stortingsperiode)", "Kvartaler i resesjon (Gj.snitt regjeringsperiode)"),
  
  Sosialistisk = c(
    paste0(round(table_1$Avg_GDP_Growth[table_1$Ideologi == "Sosialistisk"], 2), 
           " (", round(table_1$Standard_Error[table_1$Ideologi == "Sosialistisk"], 2), 
           ") [", round(table_1$NW_SE[table_1$Ideologi == "Sosialistisk"], 2), "]"),
    
    paste0(round(table_1$Average_Recession_Quarters_per_4year[table_1$Ideologi == "Sosialistisk"], 2), 
           " (", round(table_1$SE_Recession[table_1$Ideologi == "Sosialistisk"], 2),
           ") [", round(table_1$NW_SE_Recession[table_1$Ideologi == "Sosialistisk"], 2), "]"),
    
    paste0(round(table_1$Average_Recession_Quarters_per_term[table_1$Ideologi == "Sosialistisk"], 2), 
           " (", round(table_1$SE_Recession[table_1$Ideologi == "Sosialistisk"], 2),
           ") [", round(table_1$NW_SE_Recession_per_term[table_1$Ideologi == "Sosialistisk"], 2), "]")
  ),
  
  Konservativ = c(
    paste0(round(table_1$Avg_GDP_Growth[table_1$Ideologi == "Konservativ"], 2), 
           " (", round(table_1$Standard_Error[table_1$Ideologi == "Konservativ"], 2), 
           ") [", round(table_1$NW_SE[table_1$Ideologi == "Konservativ"], 2), "]"),
    
    paste0(round(table_1$Average_Recession_Quarters_per_4year[table_1$Ideologi == "Konservativ"], 2), 
           " (", round(table_1$SE_Recession[table_1$Ideologi == "Konservativ"], 2),
           ") [", round(table_1$NW_SE_Recession[table_1$Ideologi == "Konservativ"], 2), "]"),
    
    paste0(round(table_1$Average_Recession_Quarters_per_term[table_1$Ideologi == "Konservativ"], 2), 
           " (", round(table_1$SE_Recession[table_1$Ideologi == "Konservativ"], 2),
           ") [", round(table_1$NW_SE_Recession_per_term[table_1$Ideologi == "Konservativ"], 2), "]")
  ),
  
  Difference = c(
    paste0(round(diff_gdp, 2), " (", round(se_diff_gdp, 2), ") [", round(se_diff_gdp_nw, 2), "]"), 
    paste0(round(diff_recession_4year, 2), " (", round(se_diff_recession, 2), ") [", round(se_diff_recession_4year_nw, 2), "]"),
    paste0(round(diff_recession_term, 2), " (", round(se_diff_recession, 2), ") [", round(se_diff_recession_term_nw, 2), "]")
  ),
  
  `p.value` = c(
    round(p_value_gdp_nw, 3),
    round(p_value_recession_4year_nw, 3),
    round(p_value_recession_term_nw, 3)
  )
)

# ===========================================
# STEP 11: Plot Table
# ===========================================

table_1_final %>%
  gt() %>%
  tab_header(
    title = ""
  ) %>%
  cols_label(
    Variable = "Variabel",
    Sosialistisk = "Sosialistisk",
    Konservativ = "Konservativ",
    Difference = "Forskjell",
    `p.value` = "p-verdi"
  ) %>%
  fmt_number(
    columns = c(`p.value`),
    decimals = 3
  ) %>%
  tab_options(
    table.font.size = 12,
    column_labels.font.weight = "bold",
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12,
    table.border.top.style = "solid",
    table.border.bottom.style = "none",
    table_body.border.bottom.style = "none"
  )


# --------------------------------------------
# Inspect intermediate values like Blinder & Watson
# --------------------------------------------

# Step A: Compute recession share and intermediate term-based metrics
recession_check <- data_terms_split %>%
  group_by(Ideologi) %>%
  summarise(
    Total_Quarters = n(),
    Total_Recession_Quarters = sum(Recession, na.rm = TRUE)
  ) %>%
  left_join(n_terms_by_ideology, by = "Ideologi") %>%
  mutate(
    Recession_Share = Total_Recession_Quarters / Total_Quarters,
    Recession_per_term_check = Recession_Share * Average_Term_Length * 4  # Convert years to quarters
  )

# Step B: Display nicely
print(recession_check)

library(lmtest)
library(sandwich)

str(data_clean)

data_clean <- data_clean %>%
  mutate(
    `BNP vekstrate` = as.numeric(`BNP vekstrate`),
    Recession = as.numeric(Recession),
    Ideologi = as.factor(Ideologi)
  )

model_gdp <- lm(`BNP vekstrate` ~ Ideologi, data = data_clean)
bptest(model_gdp)
dwtest(model_gdp)

model_recession <- lm(Recession ~ Ideologi, data = data_clean)
bptest(model_recession)
dwtest(model_recession)

term_data <- data_clean %>%
  group_by(Ideologi, Statsminister) %>%
  summarise(Recession_Quarters = sum(Recession, na.rm = TRUE), .groups = "drop")

model_term <- lm(Recession_Quarters ~ Ideologi, data = term_data)
bptest(model_term)
dwtest(model_term)


# BNP vekstrate
bp_gdp <- bptest(model_gdp)
dw_gdp <- dwtest(model_gdp)

# Recession (quarterly)
bp_recession <- bptest(model_recession)
dw_recession <- dwtest(model_recession)

# Recession quarters per term
bp_term <- bptest(model_term)
dw_term <- dwtest(model_term)


# Create a Mini Summary Table

diagnostic_table <- data.frame(
  Variable = c("BNP vekstrate", "Recession (Quarterly)", "Recession Quarters per Term"),
  `BP p-value` = c(round(bp_gdp$p.value, 3), round(bp_recession$p.value, 3), round(bp_term$p.value, 3)),
  `DW p-value` = c(round(dw_gdp$p.value, 3), round(dw_recession$p.value, 3), round(dw_term$p.value, 3))
)

print(diagnostic_table)


# Load sandwich package if not already loaded
library(sandwich)

# Example for the GDP growth model (Sosialistisk)
gdp_model_sos <- lm(`BNP vekstrate` ~ 1, data = filter(data_terms_split, Ideologi == "Sosialistisk"))
bw_gdp_sos <- bwNeweyWest(gdp_model_sos)
cat("Newey-West lags used for Sosialistisk GDP:", bw_gdp_sos, "\n")

# Example for the GDP growth model (Konservativ)
gdp_model_kon <- lm(`BNP vekstrate` ~ 1, data = filter(data_terms_split, Ideologi == "Konservativ"))
bw_gdp_kon <- bwNeweyWest(gdp_model_kon)
cat("Newey-West lags used for Konservativ GDP:", bw_gdp_kon, "\n")

# Example for recession share (Sosialistisk)
rec_model_sos <- lm(Recession ~ 1, data = filter(data_terms_split, Ideologi == "Sosialistisk"))
bw_rec_sos <- bwNeweyWest(rec_model_sos)
cat("Newey-West lags used for Sosialistisk Recession:", bw_rec_sos, "\n")

# Example for recession share (Konservativ)
rec_model_kon <- lm(Recession ~ 1, data = filter(data_terms_split, Ideologi == "Konservativ"))
bw_rec_kon <- bwNeweyWest(rec_model_kon)
cat("Newey-West lags used for Konservativ Recession:", bw_rec_kon, "\n")


getAnywhere("bwNeweyWest")



getAnywhere("biweight_kernel")

biweight_kernel







                       
