# 2. faza: Uvoz podatkov

# Funkcija, ki uvozi občine iz Wikipedije
# uvozi.obcine <- function() {
#   link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
#   stran <- html_session(link) %>% read_html()
#   tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
#     .[[1]] %>% html_table(dec = ",")
#   colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
#                         "ustanovitev", "pokrajina", "regija", "odcepitev")
#   tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
#   tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
#   tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
#   for (col in colnames(tabela)) {
#     tabela[tabela[[col]] == "-", col] <- NA
#   }
#   for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
#     if (is.numeric(tabela[[col]])) {
#       next()
#     }
#     tabela[[col]] <- gsub("[.*]", "", tabela[[col]]) %>% as.numeric()
#   }
#   for (col in c("obcina", "pokrajina", "regija")) {
#     tabela[[col]] <- factor(tabela[[col]])
#   }
#   return(tabela)
# }
# 
# # Funkcija, ki uvozi podatke iz datoteke druzine.csv
# uvozi.druzine <- function(obcine) {
#   data <- read_csv2("podatki/druzine.csv", col_names = c("obcina", 1:4),
#                     locale = locale(encoding = "Windows-1250"))
#   data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
#     strapplyc("([^ ]+)") %>% sapply(paste, collapse = " ") %>% unlist()
#   data$obcina[data$obcina == "Sveti Jurij"] <- "Sveti Jurij ob Ščavnici"
#   data <- data %>% melt(id.vars = "obcina", variable.name = "velikost.druzine",
#                         value.name = "stevilo.druzin")
#   data$velikost.druzine <- as.numeric(data$velikost.druzine)
#   data$obcina <- factor(data$obcina, levels = obcine)
#   return(data)
# }
# 
# # Zapišimo podatke v razpredelnico obcine
# obcine <- uvozi.obcine()
# 
# # Zapišimo podatke v razpredelnico druzine.
# druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.


# JAN REMS, ANALIZA REVŠČINE
#uvoz knjižnjic in paketov
require ("xlsx")
require("dplyr")
require("readr")
# Uvoz csv tabele iz eurostata o stopnji ogroženih pred transferji skupno starosti in spolu.pokojnine izključene.

#uvozi.tabelo.pred.transferji <- function(){
stolpci <- c("Leto","Država","Kategorija","Spol","Starost","Delež","Opombe")
tabela_pred_transferji <- read_csv("podatki/ilc_li10_1_Data.csv",
                                   col_names = c("Leto","Drzava","Kategorija","Spol","Starost","Delez","Opombe"),
                                   locale = locale(encoding = "Windows-1250"),
                                   skip = 1,
                                   na= c("",":"))

tabela_pred_transferji$Kategorija <- NULL
tabela_pred_transferji$Opombe <- NULL

row.has.na <- apply(tabela_pred_transferji, 1, function(x){any(is.na(x))})
tabela_pred_transferji <- tabela_pred_transferji[!row.has.na,]

izbris <- (tabela_pred_transferji$Drzava == "European Union (28 countries)" )|(tabela_pred_transferji$Spol == "Total") | (tabela_pred_transferji$Starost == "Total")
tabela_pred_transferji <- tabela_pred_transferji[!izbris,]

tabela_pred_transferji$Spol <- as.factor(tabela_pred_transferji$Spol)
tabela_pred_transferji$Starost <- as.factor(tabela_pred_transferji$Starost)

levels(tabela_pred_transferji$Spol) <- c("Zenske", "Moski")
levels(tabela_pred_transferji$Starost) <- c("Nad 65 let", "Od 18 do 64 let", "Pod 18 let")

#return(tabela_pred_transferji)
  
#}  

# Funkcija uvozi tabelo po transferjih
#uvozi.tabelo.po.transferji <- function(){
tabela_po_transferjih <- read_csv("podatki/ilc_li02_1_Data.csv",
                                  col_names = c("Leto","Drzava","Kategorija","krneki","Spol","Starost","Delez","Opombe"),
                                  locale = locale(encoding = "Windows-1250"),
                                  skip = 1,
                                  na= c("",":"))


tabela_po_transferjih$Kategorija <- NULL
tabela_po_transferjih$Opombe <- NULL
tabela_po_transferjih$krneki <- NULL


row.has.na <- apply(tabela_po_transferjih, 1, function(x){any(is.na(x))})
tabela_po_transferjih <- tabela_po_transferjih[!row.has.na,]

izbris_po <- (tabela_po_transferjih$Drzava == "European Union (28 countries)") |(tabela_po_transferjih$Spol == "Total") | (tabela_po_transferjih$Starost == "Total")
tabela_po_transferjih <- tabela_po_transferjih[!izbris_po,]

tabela_po_transferjih$Spol <- as.factor(tabela_po_transferjih$Spol)
tabela_po_transferjih$Starost <- as.factor(tabela_po_transferjih$Starost)

levels(tabela_po_transferjih$Spol) <- c("Zenske", "Moski")
levels(tabela_po_transferjih$Starost) <- c("Nad 65 let", "Od 18 do 64 let", "Pod 18 let")

#}


#Uvoz deleža bdp namenjenega socialnim transferjem
tabela_delez_bdp <- read_csv("podatki/spr_exp_sum_1_Data.csv",
                                  col_names = c("Leto","Drzava","Kategorija","enota","Delez","Opombe"),
                                  locale = locale(encoding = "Windows-1250"),
                                  skip = 1,
                                  na= c("",":"))





#Uvoz html datotek














