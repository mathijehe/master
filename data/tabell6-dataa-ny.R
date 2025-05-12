library(readxl)


# data

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



ravarepriser <- read_excel("~/Desktop/råvarepriser.xlsx")

ravarepriser <- ravarepriser %>% 
  mutate(
    year = str_replace(year, "M", "-"),
    year = paste0(year, "-01"),          
    year = ymd(year)) %>% 
  mutate(
    year = floor_date(year, "quarter")  # Round down to first month of the quarter
  ) %>%
  group_by(year) %>%
  summarise(
    oil = mean(oil, na.rm = TRUE),
    index = mean(index, na.rm = TRUE),# Quarterly average
    .groups = "drop"
  ) %>% 
  mutate(
    oil = log(oil) - log(lag(oil, 1)),
    index = log(index) - log(lag(index, 1)) 
  ) %>%  
  drop_na()


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
  mutate(kpi = c( NA,diff(kpi))) %>% 
  drop_na()



bnp_int <- read_excel("~/Downloads/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_GROWTH_OECD,1.0,filtered,2025-02-25 13-51-00.xlsx", 
                      col_names = FALSE)
bnp_int <- bnp_int %>% 
  slice(6:n()) 

colnames(bnp_int) <- as.character(unlist(bnp_int[1, ]))
bnp_int <- bnp_int %>% 
  dplyr::select(year="Reference area", "Belgium", "Canada","Denmark","France",
                "Germany","Italy","Japan","Netherlands",  
                "Sweden","United Kingdom","United States" ) %>% 
  slice(6:n()) %>% 
  slice(1:(n() - 2)) %>%
  arrange(year) %>% 
  mutate(
    year = str_replace(year, "Q1", "-01-01"),
    year = str_replace(year, "Q2", "-04-01"),
    year = str_replace(year, "Q3", "-07-01"),
    year = str_replace(year, "Q4", "-10-01"),
    year = ymd(year)  # Konverterer til datoformat
  ) %>% 
  mutate(across(-year, as.numeric))




var_data <- bnp_int %>%
  dplyr::select(Belgium, Denmark, France, Germany, 
                 Netherlands, Sweden, 
                `United Kingdom`, `United States`) %>%
  drop_na()

vekter <- read_excel("~/Downloads/10174_20250407-161008.xlsx")

vekter <- vekter %>% 
  pivot_longer(
    cols = -year,
    names_to = "Land",
    values_to = "Eksport") %>% 
  group_by(Land) %>%
  summarise(Gjennomsnitt = mean(Eksport, na.rm = TRUE)) %>%
  mutate(Vekt = Gjennomsnitt / sum(Gjennomsnitt))


weights <- vekter$Vekt
names(weights) <- vekter$Land

bnp_int$intl_bnp_growth <- rowSums(var_data[, names(weights)] * weights[names(weights)])


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

finans$finans <- as.numeric(finans$finans) 




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



#forbrukerforventninger


forvent <- read_excel("~/Desktop/Kopi av Tabell Forventningsbarometeret Q4 2024.xlsx", 
                      sheet = "Ark2")

forvent <-forvent %>%
  arrange(year) %>% 
  mutate(
    year = str_replace(year, "Q1", "01-01"),
    year = str_replace(year, "Q2", "04-01"),
    year = str_replace(year, "Q3", "07-01"),
    year = str_replace(year, "Q4", "10-01"),
    year = ymd(year)) %>% 
  arrange(year) %>%
  mutate(trend = trend - lag(trend)) %>% 
  drop_na()


prod <- read_excel("~/Downloads/09174_20250228-133910.xlsx", 
                   col_names = FALSE)
prod <- prod %>% 
  slice(5:n()) %>%
  dplyr::select(year=...2, prod=...3) %>% 
  drop_na() %>% 
  mutate(prod = as.character(prod)) %>%
  arrange(year) %>% 
  mutate(across(everything(), as.numeric))


# Lag kvartals-datoer
quarters <- expand.grid(
  year = 1971:2024,
  quarter = 1:4
) %>%
  arrange(year, quarter) %>%
  mutate(
    date = yq(paste0(year, " Q", quarter))
  )

# === Step-interpoler ===
quarterly_prod <- quarters %>%
  left_join(prod, by = "year") %>% 
  dplyr::select(year=date, prod)


quarterly_prod <- quarterly_prod %>%
  arrange(year) %>%
  mutate(
    # Konverter årlig vekst til kvartalsvis vekst
    prod = (1 + prod / 100)^(1 / 4) - 1,     # → kvartalsvis vekstrate i desimaler
    prod = prod * 100                     # → gjør det om til prosent
  )


data_regjering <- read_excel("~/Desktop/data-regjering.xlsx")
data_regjering <- data_regjering %>%
  mutate(year = ymd(year))





