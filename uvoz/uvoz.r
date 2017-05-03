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

uvozi.tabelo.pred.transferji <- function(){
  stolpci <- c("Leto","Država","Kategorija","Spol","Starost","Delež","Opombe")
  tabela_pred_transferji <- read.csv("podatki/ilc_li10_1_Data.csv", 
                                     header = FALSE,
                                     skip = 1,
                                     encoding = "Windows-1250", 
                                     na= c("",":"))
  
  tabela_pred_transferji$V3 <- NULL
  tabela_pred_transferji$V7 <- NULL
  
  row.has.na <- apply(tabela_pred_transferji, 1, function(x){any(is.na(x))})
  tabela_pred_transferji <- tabela_pred_transferji[!row.has.na,]
  
  izbris <- tabela_pred_transferji$V2 == "European Union (28 countries)"
  tabela_pred_transferji <- tabela_pred_transferji[!izbris,]
  
  izbris2 <- tabela_pred_transferji$v4 != "Total" & tabela_pred_transferji$v5 != "Total"
  return(tabela_pred_transferji)
  
}  

# Funkcija uvozi tabelo po transferjih
uvozi.tabelo.po.transferji <- function(){
  tabela_po_transferjih <- read.csv("podatki/ilc_li02_1_Data.csv", 
                                    header = FALSE,
                                    skip = 1,
                                    encoding = "Windows-1250", 
                                    na= c("",":"))
  tabela_po_transferjih$V8 <- NULL
  tabela_po_transferjih$V4 <- NULL
  tabela_po_transferjih$V3 <- NULL
  
  row.has.na <- apply(tabela_po_transferjih, 1, function(x){any(is.na(x))})
  tabela_po_transferjih <- tabela_po_transferjih[!row.has.na,]
  
  izbris_po <- tabela_po_transferjih$V2 == "European Union (28 countries)"
  tabela_po_transferjih <- tabela_po_transferjih[!izbris_po,]
  
  
}