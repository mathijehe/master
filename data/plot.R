data <- readRDS("data.rds")
data2 <- readRDS("data2.rds")
library(tidyverse)
library(ggthemes)
library(broom)
library(moments)
library(knitr)
library(gt)
library(DT)


mean(data$BNP, na.rm = TRUE)  


data_venstre <- data[data$Regjering != "Høyre", ]

data_høyre <- data[data$Regjering != "Venstre", ]

mean(data_venstre$BNP, na.rm = TRUE)  

mean(data_høyre$BNP, na.rm = TRUE)  

data %>% 
  slice(1:(n() - 3)) %>% 
  slice(4:n()) %>% 
  group_by(...2) %>% 
  mutate(mean = mean(BNP)) %>% 
  arrange(year) %>% 
  ggplot(aes(x = reorder(...2, year), y = mean, fill = Regjering)) +
  geom_col(position = "dodge") +
  labs(title = "Gjennomsnittlig BNP etter statsminister",
       x = NULL,  # Fjerner tekst på x-aksen
       y = NULL) + # Fjerner tekst på y-aksen
  theme_few() +
  scale_fill_manual(values = c("Høyre" = "blue2", "Venstre" = "red3")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)) +  # Roterer teksten 45 grader
  scale_y_continuous(breaks = c(1,2,3,4,5))





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
   
 
 
  
 


 
 