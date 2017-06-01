


# JAN REMS, ANALIZA REVSCINE
# 2. faza - uvoz podatkov






# Uvoz csv tabele iz eurostata o stopnji ogrozenih pred transferji skupno starosti in spolu. pokojnine so izu transferjev izkljucene .


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

izbris <- (tabela_pred_transferji$Spol != "Total" & tabela_pred_transferji$Starost != "Total") |(tabela_pred_transferji$Drzava == "European Union (28 countries)")
tabela_pred_transferji <- tabela_pred_transferji[!izbris,]

tabela_pred_transferji$Spol <- as.factor(tabela_pred_transferji$Spol)
tabela_pred_transferji$Starost <- as.factor(tabela_pred_transferji$Starost)
tabela_pred_transferji$Drzava <- as.factor(tabela_pred_transferji$Drzava)

levels(tabela_pred_transferji$Spol) <- c("Zenske", "Moski","Skupaj")
levels(tabela_pred_transferji$Starost) <- c("Nad 65 let", "Od 18 do 64 let", "Pod 18 let","Skupaj")
levels(tabela_pred_transferji$Drzava)[12] <- "Germany"
levels(tabela_pred_transferji$Drzava)[10] <- "Macedonia"
  

# Uvoz tabele po transferjih


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

izbris_po <- (tabela_po_transferjih$Spol != "Total" & tabela_po_transferjih$Starost != "Total") |(tabela_po_transferjih$Drzava == "European Union (28 countries)")
tabela_po_transferjih <- tabela_po_transferjih[!izbris_po,]

tabela_po_transferjih$Spol <- as.factor(tabela_po_transferjih$Spol)
tabela_po_transferjih$Starost <- as.factor(tabela_po_transferjih$Starost)
tabela_po_transferjih$Drzava <- as.factor(tabela_po_transferjih$Drzava)

levels(tabela_po_transferjih$Spol) <- c("Zenske", "Moski","Skupaj")
levels(tabela_po_transferjih$Starost) <- c("Nad 65 let", "Od 18 do 64 let", "Pod 18 let","Skupaj")
levels(tabela_po_transferjih$Drzava)[12] <- "Germany"
levels(tabela_po_transferjih$Drzava)[10] <- "Macedonia"



#Uvoz deleza bdp namenjenega socialnim transferjem

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

tabela_delez_bdp$Drzava <- as.factor(tabela_delez_bdp$Drzava)
levels(tabela_delez_bdp$Drzava)[11] <- "Germany"






#Uvoz html datotek



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
  filter(Drzava %in% tabela_po_transferjih$Drzava 
        & Leto > 2006 
        & !grepl( "^E[Uu].*", Drzava))

bdp_indeks$Drzava <- as.factor(bdp_indeks$Drzava)






#Uvoz csv datoteke o ginijevem koeficientu

tabela_gini <- read_csv("podatki/ilc_di12_1_Data.csv",
                                            col_names = c("Leto","Drzava","Kategorija","Koeficient","Opombe"),
                                            locale = locale(encoding = "Windows-1250"),
                                            skip = 1, 
                                            na = c("",":"))

tabela_gini$Kategorija <- NULL
tabela_gini$Opombe <- NULL

row.has.na <- apply(tabela_gini, 1, function(x){any(is.na(x))})
tabela_gini <- tabela_gini[!row.has.na,]

le_drzave <- !grepl("^Eur.*",tabela_gini$Drzava)
tabela_gini <- tabela_gini[le_drzave,]

tabela_gini$Drzava <- as.factor(tabela_gini$Drzava)

levels(tabela_gini$Drzava)[10] <- "Macedonia"
levels(tabela_gini$Drzava)[12] <- "Germany"

# Uvoz stevila prebivalcev po letih in drzavah

tabela_prebivalci <- read_csv("podatki/nama_10_pe_1_Data.csv",
                        col_names = c("Leto","Drzava","Kategorija","Koeficient","Populacija","Opombe"),
                        locale = locale(encoding = "Windows-1250"),
                        skip = 1, 
                        na = c("",":"))

tabela_prebivalci$Kategorija <- NULL
tabela_prebivalci$Koeficient <- NULL
tabela_prebivalci$Opombe <- NULL

row.has.na <- apply(tabela_prebivalci, 1, function(x){any(is.na(x))})
tabela_prebivalci <- tabela_prebivalci[!row.has.na,]

tabela_prebivalci$Drzava <- as.factor(tabela_prebivalci$Drzava)
levels(tabela_prebivalci$Drzava)[10] <- "Macedonia"
levels(tabela_prebivalci$Drzava)[12] <- "Germany"


grcija <- tabela_pred_transferji %>%
  filter(Drzava == "Greece", Spol == "Skupaj", Starost == "Skupaj")


#Uvoz tabele za ID drzav

link2 <- "https://en.wikipedia.org/wiki/ISO_3166-1"
stran2 <- html_session(link) %>% read_html()

tabela_id <- stran2 %>% 
  html_nodes(xpath="//table[@class='wikitable sortable']") %>%
  .[[3]] %>% 
    html_table(dec = ",", header = FALSE)



