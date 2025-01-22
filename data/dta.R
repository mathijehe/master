library(readxl)
library(tidyverse)


#bnp_vekst <- read_excel("~/Downloads/09189_20250114-141213.xlsx")
saveRDS(bnp_vekst, "bnp_vekst.rds")
bnp_vekst <- readRDS("bnp_vekst.rds")
bnp_vekst <- bnp_vekst %>% 
  select("year"="...2", "BNP"="...5", "BNP fastlands Norge"="...6") %>% 
  drop_na()%>% 
  mutate(across(everything(), as.numeric))


#inflasjon <- read_excel("~/Downloads/03014_20250114-143534.xlsx",
                        #col_names = FALSE)

saveRDS(inflasjon, "inflasjon.rds")
inflasjon <- readRDS("inflasjon.rds")

inflasjon <- inflasjon %>% 
  select("year"="...1", "inflasjonsvekst"="...2") %>% 
  drop_na()%>% 
  mutate(across(everything(), as.numeric))


#produktivitet <- read_excel("~/Downloads/09170_20250114-144926.xlsx", 
       #                     col_names = FALSE)
saveRDS(produktivitet, "produktivitet.rds")
produktivitet <- readRDS("produktivitet.rds")

produktivitet <- produktivitet %>% 
  select("year"="...2", "produktivitet"="...3") %>% 
  drop_na()%>% 
  mutate(across(everything(), as.numeric))



#abreidsledighet <- read_excel("~/Downloads/08517_20250114-145811.xlsx", 
    #                          col_names = FALSE)
saveRDS(abreidsledighet, "abreidsledighet.rds")
abreidsledighet <- readRDS("abreidsledighet.rds")

abreidsledighet <- abreidsledighet %>% 
  select("year"="...2", "arbeidsledighet"="...3") %>% 
  drop_na()%>% 
  mutate(across(everything(), as.numeric))



#bnp_predeksjoner <- read_excel("~/Downloads/12880_20250114-150706.xlsx", 
 #                              col_names = FALSE)
saveRDS(bnp_predeksjoner, "bnp_predeksjoner.rds")
bnp_predeksjoner <- readRDS("bnp_predeksjoner.rds")

bnp_predeksjoner <- bnp_predeksjoner %>% 
  select("year"="...1","BNP_pred"="...2","BNP_fastland_pred"="...3") %>% 
  drop_na() %>% 
  mutate(across(everything(), as.numeric))




#handelsbalanse <- read_excel("~/Downloads/08792_20250115-114601.xlsx", 
                  #           col_names = FALSE)

saveRDS(handelsbalanse, "handelsbalanse.rds")
handelsbalanse <- readRDS("handelsbalanse.rds")

handelsbalanse <- handelsbalanse %>% 
  select("year"="...1", "import"="...2", "eksport"="...3") %>% 
  drop_na() %>% 
  mutate(year = substr(year, 1, 4)) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  group_by(year) %>%                    
  summarize(
    import = sum(import, na.rm = TRUE),  
    eksport = sum(eksport, na.rm = TRUE)) %>% 
  mutate("handelbalanse"=(eksport-import))



#bnp_utland <- read_excel("~/Downloads/P_Data_Extract_From_World_Development_Indicators-2.xlsx", 
    #                     col_names = FALSE)

saveRDS(bnp_utland, "bnp_utland.rds")
bnp_utland <- readRDS("bnp_utland.rds")

bnp_utland <- bnp_utland %>% 
  select("year"="...3","Euro_erea"="...5","USA"="...6", "Sweden"="...7") %>% 
  mutate(across(everything(), as.numeric)) %>% 
  drop_na() %>% 
  mutate(USA = round(USA, 1),
         Sweden = round(Sweden,1),
         Euro_erea = round(Euro_erea,1))



#oil <- read_csv("~/Downloads/Brent Oil Futures Historical Data.csv")
saveRDS(oil, "oil.rds")
oil <- readRDS("oil.rds")

oil <- oil %>%
  mutate(growth = str_remove(`Change %`, "%"),  
         growth = as.numeric(`Change %`)) %>% 
  mutate("year" = substr(`Date`, 7, 10)) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  group_by(year) %>%                    
  summarize(
    Price = mean(Price, na.rm = TRUE),
    Open = mean(Open, na.rm = TRUE),
    High = mean(High, na.rm = TRUE),
    Low = mean(Low, na.rm = TRUE))

oil1 <- oil %>% 
  select(year, Price)


#regjering <- read_excel("~/Desktop/data-regjering.xlsx")
saveRDS(regjering, "regjering.rds")
regjering <- readRDS("regjering.rds")

regjering$year <- as.numeric(regjering$year)  

#bnp_kvartal <- read_excel("~/Downloads/09190_20250121-112748.xlsx", 
 #                         col_names = FALSE)
saveRDS(bnp_kvartal, "bnp_kvartal.rds")
bnp_kvartal <- readRDS("bnp_kvartal.rds")

bnp_kvartal <-  bnp_kvartal %>% 
  slice(4:n())
 
bnp_kvartal[is.na(bnp_kvartal)] <- "year"
colnames(bnp_kvartal) <- bnp_kvartal[1, ]  
bnp_kvartal <-  bnp_kvartal %>% 
  slice(3:n()) %>% 
  slice(1:(n() - 58)) %>% 
  separate(year, into = c("year", "kvartal"), sep = "K") %>% 
  mutate(across(everything(), as.numeric))


dataset <- list(bnp_vekst, abreidsledighet, inflasjon, produktivitet,
                bnp_predeksjoner, bnp_utland, handelsbalanse, oil1, regjering)
data <- reduce(dataset, full_join, by="year")

#bnp_capita <- read_csv("~/Downloads/P_Data_Extract_From_World_Development_Indicators-2/9d5b2de6-efe2-4ae2-915c-933706c14c5d_Data.csv")
saveRDS(bnp_capita, "bnp_capita.rds")
bnp_capita <- readRDS("bnp_capita.rds")

bnp_capita <-  bnp_capita %>% 
  select("year"="Time", "norway"="Norway [NOR]","EU"="Euro area [EMU]", "USA"="United States [USA]", "sweden"="Sweden [SWE]" ) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  drop_na() %>% 
  mutate(USA = round(USA, 1),
         sweden = round(sweden,1),
         EU = round(EU,1),
         norway = round(norway,1))

dataset2 <-  list(bnp_kvartal,regjering)
data2 <-  reduce(dataset2, full_join, by="year")

saveRDS(data, "data.rds")
saveRDS(data2, "data2.rds")

