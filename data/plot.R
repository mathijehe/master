data <- readRDS("data.rds")

library(tidyverse)
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
  theme_bw() +
  scale_fill_manual(values = c("Høyre" = "blue2", "Venstre" = "red3")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)) +  # Roterer teksten 45 grader
  scale_y_continuous(breaks = c(1,2,3,4,5))
