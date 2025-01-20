data <- readRDS("data.rds")

library(tidyverse)
library(ggthemes)
library(broom)
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


ll <- data %>%
  slice(1:(n() - 3)) %>% 
  slice(4:n()) %>% 
  group_by(Regjering) %>%
  summarise(
    avg_gdp_growth = mean(BNP),
    sd_gdp_growth = sd(BNP),
    avg_unemployment = mean(arbeidsledighet),
    sd_unemployment = sd(arbeidsledighet)
  )

t_test_gdp <- t.test(BNP ~ Regjering, data = data)
t_test_unemployment <- t.test(arbeidsledighet ~ Regjering, data = data)

# Vis resultatene
results <- bind_rows(
  tidy(t_test_gdp) %>% mutate(variable = "BNP"),
  tidy(t_test_unemployment) %>% mutate(variable = "Arbeidsledighet")
)

