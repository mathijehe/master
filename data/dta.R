library(readxl)
library(tidyverse)


#bnp_vekst <- read_excel("~/Downloads/09189_20250114-141213.xlsx")
#saveRDS(bnp_vekst, "bnp_vekst.rds")
bnp_vekst <- readRDS("bnp_vekst.rds")
bnp_vekst <- bnp_vekst %>% 
  select("year"="...2", "BNP"="...5", "BNP fastlands Norge"="...6") %>% 
  drop_na()%>% 
  mutate(across(everything(), as.numeric))


#inflasjon <- read_excel("~/Downloads/03014_20250114-143534.xlsx",
                        #col_names = FALSE)

#saveRDS(inflasjon, "inflasjon.rds")
inflasjon <- readRDS("inflasjon.rds")

inflasjon <- inflasjon %>% 
  select("year"="...1", "inflasjonsvekst"="...2") %>% 
  drop_na()%>% 
  mutate(across(everything(), as.numeric))


#produktivitet <- read_excel("~/Downloads/09170_20250114-144926.xlsx", 
       #                     col_names = FALSE)
#saveRDS(produktivitet, "produktivitet.rds")
produktivitet <- readRDS("produktivitet.rds")

produktivitet <- produktivitet %>% 
  select("year"="...2", "produktivitet"="...3") %>% 
  drop_na()%>% 
  mutate(across(everything(), as.numeric))



#abreidsledighet <- read_excel("~/Downloads/08517_20250114-145811.xlsx", 
    #                          col_names = FALSE)
#saveRDS(abreidsledighet, "abreidsledighet.rds")
abreidsledighet <- readRDS("abreidsledighet.rds")

abreidsledighet <- abreidsledighet %>% 
  select("year"="...2", "arbeidsledighet"="...3") %>% 
  drop_na()%>% 
  mutate(across(everything(), as.numeric)) %>% 
  mutate(arbeidsledighet_endring = lag(arbeidsledighet) - arbeidsledighet)


#bnp_predeksjoner <- read_excel("~/Downloads/12880_20250114-150706.xlsx", 
 #                              col_names = FALSE)
#saveRDS(bnp_predeksjoner, "bnp_predeksjoner.rds")
bnp_predeksjoner <- readRDS("bnp_predeksjoner.rds")

bnp_predeksjoner <- bnp_predeksjoner %>% 
  select("year"="...1","BNP_pred"="...2","BNP_fastland_pred"="...3") %>% 
  drop_na() %>% 
  mutate(across(everything(), as.numeric))




#handelsbalanse <- read_excel("~/Downloads/08792_20250115-114601.xlsx", 
                  #           col_names = FALSE)

#saveRDS(handelsbalanse, "handelsbalanse.rds")
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

#saveRDS(bnp_utland, "bnp_utland.rds")
bnp_utland <- readRDS("bnp_utland.rds")

bnp_utland <- bnp_utland %>% 
  select("year"="...3","Euro_erea"="...5","USA"="...6", "Sweden"="...7") %>% 
  mutate(across(everything(), as.numeric)) %>% 
  drop_na() %>% 
  mutate(USA = round(USA, 1),
         Sweden = round(Sweden,1),
         Euro_erea = round(Euro_erea,1))



#oil <- read_csv("~/Downloads/Brent Oil Futures Historical Data.csv")
#saveRDS(oil, "oil.rds")
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
#saveRDS(regjering, "regjering.rds")
regjering <- readRDS("regjering.rds")

regjering$year <- as.numeric(regjering$year)  

#bnp_kvartal <- read_excel("~/Downloads/09190_20250121-112748.xlsx", 
 #                         col_names = FALSE)
#saveRDS(bnp_kvartal, "bnp_kvartal.rds")
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




#bnp_capita <- read_csv("~/Downloads/P_Data_Extract_From_World_Development_Indicators-2/9d5b2de6-efe2-4ae2-915c-933706c14c5d_Data.csv")
#saveRDS(bnp_capita, "bnp_capita.rds")
bnp_capita <- readRDS("bnp_capita.rds")

bnp_capita <-  bnp_capita %>% 
  select("year"="Time", "norway"="Norway [NOR]","EU"="Euro area [EMU]", "USA"="United States [USA]", "sweden"="Sweden [SWE]" ) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  drop_na() %>% 
  mutate(USA = round(USA, 1),
         sweden = round(sweden,1),
         EU = round(EU,1),
         norway = round(norway,1))

#sysselsatte <- read_excel("~/Downloads/05111_20250122-123248.xlsx", 
                      #    col_names = FALSE)

#saveRDS(sysselsatte, "sysselsatte.rds")
sysselsatte <- readRDS("sysselsatte.rds")

sysselsatte <- sysselsatte %>% 
  select("year"="...1", "arbeidstyrke"="...2", "sysselsatte"="...3") %>% 
  slice(7:n()) %>% 
  slice(1:(n() - 54)) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  mutate(syssel= (arbeidstyrke-sysselsatte)) %>% 
  mutate(sysselsettning_endring = (lag(syssel)-syssel)/syssel) %>% 
  mutate(sysselsettning_endring = round(sysselsettning_endring, 1)) %>% 
  select(year, sysselsettning_endring)

#vderiskapning_sektor <- read_excel("~/Downloads/09170_20250123-102730.xlsx", 
 #                                  col_names = FALSE)
#saveRDS(vderiskapning_sektor, "vderiskapning_sektor.rds")
vderiskapning_sektor <- readRDS("vderiskapning_sektor.rds")

vderiskapning_sektor <-  vderiskapning_sektor %>% 
  slice(4:n()) %>%
  slice(1:(n() - 49)) %>% 
  mutate(across(everything(), ~ifelse(is.na(.), "year", .)))
colnames(vderiskapning_sektor) <- vderiskapning_sektor[1, ] 
vderiskapning_sektor <-  vderiskapning_sektor %>% 
  mutate(across(everything(), as.numeric)) %>% 
  slice(5:n())



#styringsrente <- read_delim("~/Downloads/IR (1).csv", 
      #                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
#saveRDS(styringsrente, "styringsrente.rds")
styringsrente <- readRDS("styringsrente.rds")

styringsrente <-  styringsrente %>% 
  select("year"="TIME_PERIOD", "STYRINGSRENTE"=OBS_VALUE) %>% 
  mutate(across(everything(), ~as.numeric(gsub(",", ".", .)))) %>% 
  slice(3:n()) %>% 
  slice(1:(n() - 43))


#styringsrente2 <- read_excel("~/Downloads/IR.xlsx")

#saveRDS(styringsrente2, "styringsrente2.rds")
styringsrente2 <- readRDS("styringsrente2.rds")

styringsrente2 <-  styringsrente2 %>% 
  select("year"="FREQ", "STYRINGSRENTE"="A") %>% 
  slice(5:n()) %>%
  mutate(across(everything(), as.numeric)) %>% 
  slice(1:(n() - 32))

styringsrente <- list(styringsrente, styringsrente2)
styringsrente <-  reduce(styringsrente, full_join, by=c("STYRINGSRENTE", "year"))

styringsrente <- styringsrente %>% 
  arrange(year)

dataset <- list(bnp_vekst, abreidsledighet, inflasjon, produktivitet,
                bnp_predeksjoner, bnp_utland, handelsbalanse, oil1, regjering,
                sysselsatte,styringsrente)
data <- reduce(dataset, full_join, by="year")


#næring <- read_excel("~/Downloads/09170_20250128-115600.xlsx", 
                #     col_names = FALSE)
#saveRDS(næring, "næring.rds")
næring <- readRDS("næring.rds")

næring <- næring %>% 
  slice(4:n()) %>% 
  slice(1:(n() - 49)) %>% 
  mutate(across(everything(), ~ifelse(is.na(.), "year", .)))
colnames(næring) <- næring[1, ] 
næring <- næring %>% 
  slice(3:n()) %>% 
  mutate(across(everything(), as.numeric)) 

#aksje <- read_excel("~/Desktop/aksje.xlsx")
#saveRDS(aksje, "aksje.rds")
aksje <- readRDS("aksje.rds")

aksje <-aksje %>% 
  rename(year="År")

dataset2 <-  list(bnp_kvartal,regjering)
data2 <-  reduce(dataset2, full_join, by="year")

dataset3 <-  list(bnp_capita,regjering, næring, abreidsledighet, sysselsatte,
                  handelsbalanse,inflasjon, produktivitet, styringsrente, aksje)
data3 <-  reduce(dataset3, full_join, by="year")

dataset4 <-  list(bnp_predeksjoner,bnp_vekst,regjering)
data4 <-  reduce(dataset4, full_join, by="year")

saveRDS(data, "data.rds")
saveRDS(data2, "data2.rds")
saveRDS(data3, "data3.rds")
saveRDS(data4, "data4.rds")
str(bnp_capita)
