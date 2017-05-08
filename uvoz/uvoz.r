


# JAN REMS, ANALIZA REVä»INE
# 2. faza - uvoz podatkov

#uvoz knjiénjic in paketov


require ("xlsx")
require("dplyr")
require("readr")

# Uvoz csv tabele iz eurostata o stopnji ogroéenih pred transferji skupno starosti in spolu. pokojnine izkljuËene.


stolpci <- c("Leto","Dr≈æava","Kategorija","Spol","Starost","Dele≈æ","Opombe")
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


  

# Uvoz tabele pred transferji


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



#Uvoz deleûa bdp namenjenega socialnim transferjem
tabela_delez_bdp <- read_csv("podatki/spr_exp_sum_1_Data.csv",
                                  col_names = c("Leto","Drzava","Kategorija","enota","Delez","Opombe"),
                                  locale = locale(encoding = "Windows-1250"),
                                  skip = 1,
                                  na= c("",":"))

tabela_delez_bdp$Opombe <- NULL
total_expenditure_in_procenti <- (tabela_delez_bdp$Kategorija =="Total expenditure") & (tabela_delez_bdp$enota == "Percentage of gross domestic product (GDP)") & !grepl("^Eur.*",tabela_delez_bdp$Drzava) & (!is.na(tabela_delez_bdp$Delez)) 
tabela_delez_bdp <- tabela_delez_bdp[total_expenditure_in_procenti,]

tabela_delez_bdp$Kategorija <- NULL
tabela_delez_bdp$enota <- NULL





#Uvoz html datotek

library('xml2')
library('rvest')
library(reshape)
library(varhandle)


link <- "http://ec.europa.eu/eurostat/tgm/table.do?tab=table&init=1&language=en&pcode=tec00114&plugin=1"
stran <- html_session(link) %>% read_html()

tabela <- stran %>% 
          
          html_nodes(xpath="//table[@id='contenttable' ]") %>%
          .[[1]] %>% 
          html_table(dec = ",", header = FALSE)


leta <- stran %>%
  html_nodes(xpath="//table[@id='headtable' ]") %>%
  .[[1]]%>%
  html_table(dec = ",")

drzave <- stran %>%
  html_nodes(xpath="//table[@id='fixtable' ]") %>%
  .[[1]]%>%
  html_table(dec = ",", header = FALSE)

colnames(tabela) <- colnames(leta)    
bdp_indeks <- cbind(drzave, tabela)


bdp_indeks <- melt(bdp_indeks, id = "X1")

colnames(bdp_indeks) <- c("Drzava", "Leto", "Indeks")
bdp_indeks <- bdp_indeks[c( "Leto","Drzava", "Indeks")]

for (col in c("Leto", "Indeks")) {
  bdp_indeks[[col]] <- parse_number(bdp_indeks[[col]], na = ":")
}

bdp_indeks <- bdp_indeks %>% 
  filter(bdp_indeks$Drzava != "Liechtenstein"
        & bdp_indeks$Drzava !="Montenegro"
        & bdp_indeks$Drzava !="Albania"
        & bdp_indeks$Drzava !="Bosnia and Herzegovina"
        & bdp_indeks$Drzava !="United States"
        & bdp_indeks$Drzava !="Japan"
        & bdp_indeks$Leto > 2006 
        & !grepl( "^E[Uu].*", bdp_indeks$Drzava))







#Uvoz csv datoteke o ginijevem dokumentu

tabela_gini <- read_csv("podatki/ilc_di12_1_Data.csv",
                                            col_names = c("Leto","Drzava","Kategorija","Delez","Opombe"),
                                            locale = locale(encoding = "Windows-1250"),
                                            skip = 1, 
                                            na = c("",":"))

tabela_gini$Kategorija <- NULL
tabela_gini$Opombe <- NULL

row.has.na <- apply(tabela_gini, 1, function(x){any(is.na(x))})
tabela_gini <- tabela_gini[!row.has.na,]

le_drzave <- !grepl("^Eur.*",tabela_gini$Drzava)
tabela_gini <- tabela_gini[le_drzave,]




