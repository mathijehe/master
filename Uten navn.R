mean(data$BNP, na.rm = TRUE)  


data_venstre <- data2[data2$Regjering != "Høyre", ]

data_høyre <- data2[data2$Regjering != "Venstre", ]

mean(data_venstre$BNP, na.rm = TRUE)  

mean(data_høyre$BNP, na.rm = TRUE)  





data2 <-  data2 %>% 
  select(year, kvartal,"BNP"="Bruttonasjonalprodukt, markedsverdi",
         "BNP_fast" = "Bruttonasjonalprodukt Fastlands-Norge, markedsverdi", Regjering)

Q1 <- quantile(data2$BNP, 0.25, na.rm = TRUE)  # Første kvartil
Q3 <- quantile(data2$BNP, 0.75, na.rm = TRUE)  # Tredje kvartil
IQR <- Q3 - Q1                               # Interkvartilavstand

# Sett grenser for å identifisere outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR


data3 <- data2 %>%
  mutate(BNP = pmin(pmax(BNP, lower_bound), upper_bound))   

data_venstre <- data3[data3$Regjering != "Høyre", ]

data_høyre <- data3[data3$Regjering != "Venstre", ]

mean(data_venstre$BNP, na.rm = TRUE)  

mean(data_høyre$BNP, na.rm = TRUE)  