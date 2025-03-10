library(tidyverse)
library(vars)
library(svars)
library(readxl)
library(lubridate)
library(stringr)
library(urca)
library(tseries)


#data olje

olje <- read_excel("~/Downloads/RBRTEm.xls")

olje <- olje %>%
  mutate(year = floor_date(ymd(year), "quarter")) %>%  # Sett dato til første dag i kvartalet
  group_by(year) %>%
  summarise(pris = mean(pris, na.rm = TRUE)) %>%  # Aggregerer oljeprisen per kvartal
  arrange(year) %>%
  mutate(pris = log(pris) - log(lag(pris, 1)))


EXR <- read_delim("~/Downloads/EXR.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)


EXR <-EXR %>% 
  dplyr::select(year=TIME_PERIOD, exr=OBS_VALUE) %>% 
  mutate(
    year = paste0(year, "-01"),          
    year = ymd(year)) %>% 
  mutate(
    year = floor_date(year, "quarter")  # Runder ned til første måned i kvartalet
  ) %>%
  group_by(year) %>%
  summarise(
    exr = mean(exr, na.rm = TRUE),  # Gjennomsnitt per kvartal
    .groups = "drop"
  ) 
  
bnp <- read_csv("~/Downloads/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_GROWTH_OECD,1.0+Q..NOR...B1GQ......G1..csv")
bnp <- bnp %>% 
  dplyr::select("year"="TIME_PERIOD", "bnp"="OBS_VALUE") %>% 
  arrange(year) %>% 
  mutate(
    year = str_replace(year, "Q1", "-01-01"),
    year = str_replace(year, "Q2", "-04-01"),
    year = str_replace(year, "Q3", "-07-01"),
    year = str_replace(year, "Q4", "-10-01"),
    year = ymd(year)  # Konverterer til datoformat
  )

data5 <- list(olje,EXR, bnp)
data5 <-  reduce(data5, full_join, by="year") 

data5 <- data5 %>% 
  arrange(year) %>% 
  drop_na()




#data rente

kpi <-  read_excel("~/Downloads/08183_20250213-103118.xlsx", 
                   col_names = FALSE)
kpi <-  kpi %>%
  rename(year = `...1`, kpi = `...2`) %>% 
  slice(7:n()) %>% 
  mutate(
    year = str_replace(year, "M", "-"),
    year = paste0(year, "-01"),          
    year = ymd(year)  # Convert to date
  ) %>% 
  drop_na() %>%
  mutate(kpi = as.numeric(kpi)) %>% 
  mutate(
    year = floor_date(year, "quarter")  # Round down to first month of the quarter
  ) %>%
  group_by(year) %>%
  summarise(
    kpi = mean(kpi, na.rm = TRUE),  # Quarterly average
    .groups = "drop"
  ) %>% 
  mutate(kpi = log(kpi) - log(lag(kpi, 1))) %>% 
  drop_na()

#styringsrente

styringsrente <- read_delim("~/Downloads/IR (3).csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)


styringsrente <- styringsrente %>% 
  dplyr::select(TIME_PERIOD, OBS_VALUE) %>% 
  rename(year=`TIME_PERIOD`, rente=`OBS_VALUE`) %>%
  mutate(rente = as.numeric(str_replace(rente, ",", ".")))


styringsrente_2 <- read_excel("~/Downloads/IR (3).xlsx")

styringsrente_2 <- styringsrente_2 %>% 
  slice(5:n()) %>% 
  slice(1:(n() - 25)) %>% 
  rename(year="FREQ", rente="M") %>% 
  mutate(rente = as.numeric(rente))

styringsrente3 <- list(styringsrente, styringsrente_2)
styringsrente3 <-  reduce(styringsrente3, full_join, by=c("rente", "year"))

styringsrente3 <- styringsrente3 %>% 
  arrange(year) %>% 
  mutate(
    year = paste0(year, "-01"),          
    year = ymd(year)) %>% 
  mutate(
    year = floor_date(year, "quarter")  # Runder ned til første måned i kvartalet
  ) %>%
  group_by(year) %>%
  summarise(
    rente = mean(rente, na.rm = TRUE),  # Gjennomsnitt per kvartal
    .groups = "drop"
  ) %>% 
  mutate(rente = rente - lag(rente, 1)) %>% 
  drop_na()


bnp <- read_csv("~/Downloads/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_GROWTH_OECD,1.0+Q..NOR...B1GQ......G1..csv")
bnp <- bnp %>% 
  dplyr::select("year"="TIME_PERIOD", "bnp"="OBS_VALUE") %>% 
  arrange(year) %>% 
  mutate(
    year = str_replace(year, "Q1", "-01-01"),
    year = str_replace(year, "Q2", "-04-01"),
    year = str_replace(year, "Q3", "-07-01"),
    year = str_replace(year, "Q4", "-10-01"),
    year = ymd(year)  # Konverterer til datoformat
  )

data4 <- list(styringsrente3, bnp, kpi)
data4 <-  reduce(data4, full_join, by="year") 

data4 <- data4 %>% 
  arrange(year) %>% 
  drop_na()



#internasjonale sjokk

bnp_int <- read_excel("~/Downloads/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_GROWTH_OECD,1.0,filtered,2025-02-25 13-51-00.xlsx", 
                      col_names = FALSE)
bnp_int <- bnp_int %>% 
  slice(6:n()) 

colnames(bnp_int) <- as.character(unlist(bnp_int[1, ]))
bnp_int <- bnp_int %>% 
  dplyr::select(year="Reference area", "Belgium", "Canada","Denmark","France",
                "Germany","Italy","Japan","Netherlands",  
                "Sweden","United Kingdom","United States" ) %>% 
  slice(3:n()) %>% 
  slice(1:(n() - 2)) %>%
  arrange(year) %>% 
  mutate(
    year = str_replace(year, "Q1", "-01-01"),
    year = str_replace(year, "Q2", "-04-01"),
    year = str_replace(year, "Q3", "-07-01"),
    year = str_replace(year, "Q4", "-10-01"),
    year = ymd(year)  # Konverterer til datoformat
  ) 

bnp <- read_csv("~/Downloads/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_GROWTH_OECD,1.0+Q..NOR...B1GQ......G1..csv")
bnp <- bnp %>% 
  dplyr::select("year"="TIME_PERIOD", "bnp"="OBS_VALUE") %>% 
  arrange(year) %>% 
  mutate(
    year = str_replace(year, "Q1", "-01-01"),
    year = str_replace(year, "Q2", "-04-01"),
    year = str_replace(year, "Q3", "-07-01"),
    year = str_replace(year, "Q4", "-10-01"),
    year = ymd(year)  # Konverterer til datoformat
  )

bnp_int <- list(bnp_int, bnp)
bnp_int <-  reduce(bnp_int, full_join, by="year") 

bnp_int <- bnp_int %>% 
  arrange(year) %>% 
  drop_na()
  

#produktivitet 

prod <- read_excel("~/Downloads/09174_20250228-133910.xlsx", 
                   col_names = FALSE)

prod <- prod %>% 
  slice(5:n()) %>%
  dplyr::select(year=...2, prod=...3) %>% 
  drop_na() %>% 
  mutate(prod = as.character(prod)) %>%
  arrange(year) %>% 
  mutate(across(everything(), as.numeric))

bnp_vekst <- readRDS("bnp_vekst.rds")
bnp_vekst <- bnp_vekst %>% 
  dplyr::select("year"="...2", "BNP"="...5", "BNP fastlands Norge"="...6") %>% 
  drop_na()%>% 
  mutate(across(everything(), as.numeric))

dataset6 <-  list(bnp_vekst,prod)
data6 <-  reduce(dataset6, full_join, by="year")


 
#forbrukerforventninger

url <- "https://static.dwcdn.net/data/oDF05.csv?v=1738923780000"

# Les CSV-filen direkte fra URL
forvent <- read_csv(url)

forvent <-forvent %>%
  dplyr::select(year= `Year-Quarter`, trend=`Justert trendindicator`) %>% 
  arrange(year) %>% 
  mutate(
    year = str_replace(year, "Q1", "-01-01"),
    year = str_replace(year, "Q2", "-04-01"),
    year = str_replace(year, "Q3", "-07-01"),
    year = str_replace(year, "Q4", "-10-01"),
    year = ymd(year)  # Konverterer til datoformat
  ) %>% 
  mutate(trend = log(trend) - log(lag(trend, 1))) %>% 
  drop_na()


bnp <- read_csv("~/Downloads/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_GROWTH_OECD,1.0+Q..NOR...B1GQ......G1..csv")
bnp <- bnp %>% 
  dplyr::select("year"="TIME_PERIOD", "bnp"="OBS_VALUE") %>% 
  arrange(year) %>% 
  mutate(
    year = str_replace(year, "Q1", "-01-01"),
    year = str_replace(year, "Q2", "-04-01"),
    year = str_replace(year, "Q3", "-07-01"),
    year = str_replace(year, "Q4", "-10-01"),
    year = ymd(year)  # Konverterer til datoformat
  )

data8 <- list(forvent, bnp)
data8 <-  reduce(data8, full_join, by="year") 

data8 <- data8 %>% 
  arrange(year) %>% 
  drop_na()

#finanspolitikk 
finans <- read_excel("~/Downloads/09186_20250305-154337.xlsx")

finans <- finans %>% 
  dplyr::select(year=...2, finans=...3) %>% 
  arrange(year) %>% 
  mutate(
    year = str_replace(year, "K1", "-01-01"),
    year = str_replace(year, "K2", "-04-01"),
    year = str_replace(year, "K3", "-07-01"),
    year = str_replace(year, "K4", "-10-01"),
    year = ymd(year)  # Konverterer til datoformat
  ) 


bnp <- read_csv("~/Downloads/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_GROWTH_OECD,1.0+Q..NOR...B1GQ......G1..csv")
bnp <- bnp %>% 
  dplyr::select("year"="TIME_PERIOD", "bnp"="OBS_VALUE") %>% 
  arrange(year) %>% 
  mutate(
    year = str_replace(year, "Q1", "-01-01"),
    year = str_replace(year, "Q2", "-04-01"),
    year = str_replace(year, "Q3", "-07-01"),
    year = str_replace(year, "Q4", "-10-01"),
    year = ymd(year)  # Konverterer til datoformat
  )

data9 <- list(finans, bnp)
data9 <-  reduce(data9, full_join, by="year") 

data9 <- data9 %>% 
  arrange(year) %>% 
  drop_na()

data9$finans <- as.numeric(data9$finans) 
  
  

