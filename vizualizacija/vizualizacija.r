# 3. faza: Vizualizacija podatkov


#iskanje ustreznih podatkov

##skupni podatki za evropo

### Gibanje deleza socialno ogroženih pred transferji v Evropi skozi leta
evro <- inner_join(tabela_pred_transferji,tabela_prebivalci) %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj") %>%
  group_by(Leto) %>%
  summarize(delez = sum(Delez * Populacija)/sum(Populacija))

#### Povprecje in odklon
povp.pred <- mean(evro$delez)

odklon.pred <- sd(evro$delez)

### Gibanje delezasocialno ogroženih po transferjih v Evropi skozi leta
evro_po <- inner_join(tabela_po_transferjih,tabela_prebivalci) %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj") %>%
  group_by(Leto) %>%
  summarize(delez = sum(Delez * Populacija)/sum(Populacija))

#### Povprecje in odklon
povp.po <- mean(evro_po$delez)

odklon.po <- sd(evro_po$delez)

## Analiza starosti socialno ogroženih v Evropi

### Delez socialno ogrozenih pred transferji v Evropi po starostnih kategorijah
eu_starost_pred <- inner_join(tabela_pred_transferji,tabela_prebivalci) %>%
  filter(Spol == "Skupaj"& Starost != "Skupaj") %>%
  group_by(Starost) %>%
  summarize(Delez = mean(sum(Delez * Populacija)/sum(Populacija)))

### Delez socialno ogrozenih po transferjih v Evropi po starostnih kategorijah
eu_starost_po <- inner_join(tabela_po_transferjih,tabela_prebivalci) %>%
  filter(Spol == "Skupaj"& Starost != "Skupaj") %>%
  group_by(Starost) %>%
  summarize(Delez = mean(sum(Delez * Populacija)/sum(Populacija)))

#### Zdruzeni tabeli
starost_zdruzeni <- inner_join(eu_starost_pred %>% rename(delez.pred = Delez), eu_starost_po %>% rename(delez.po = Delez)) %>%
  transmute(Starost, delez.pred, delez.po, razlika = delez.pred-delez.po)
colnames(starost_zdruzeni)[c(2:4)] <- c("Delez pred transferji", "Delez po transferjih", "Razlika")

## Podatki ločeno po državah, gledano za povprečje let

### Delez socialno ogrozenih po drzavah pred transferji
drzave_pred <- tabela_pred_transferji %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez))

#### Povprecje in odklon  
povprecje.drzave.pred <- mean(drzave_pred$Delez)
odklon.drzave.pred <- sd(drzave_pred$Delez)

### Delez socialno ogrozenih po drzavah pred transferjih
drzave_po <- tabela_po_transferjih %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez))

#### Povprecje in odklon  
povprecje.drzave.po <- mean(drzave_po$Delez)
odklon.drzave.po <- sd(drzave_po$Delez)

### Razlika med socialno ogrozenimi pred in po transferjih po
najboljse_drzave <- left_join(tabela_pred_transferji %>% rename(Delez.Pred = Delez),
                       tabela_po_transferjih %>% rename(Delez.Po = Delez)) %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez.Pred - Delez.Po)) %>%
  arrange(desc(Delez))
  


## Analiza socialno ogrozenih glede na spol za povprecje let

### Tabela socialno ogrozenih moskih po drzavah pred transferji
moski_pred <- tabela_pred_transferji %>%
  filter(Spol == "Moski" & Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez)) 

### Tabela socialno ogrozenih moskih po drzavah po transferjih
moski_po <- tabela_po_transferjih %>%
  filter(Spol == "Moski" & Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez))

###Tabela socialno ogrozenih zensk po drzavah pred transferji
zenske_pred <- tabela_pred_transferji %>%
  filter(Spol == "Zenske" & Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez)) 

### Tabela socialno ogrozenih zensk po drzavah po transferjih
zenske_po <- tabela_po_transferjih %>%
  filter(Spol == "Zenske" & Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez))

### Razlika deleza socialno ogrozenih moskih pred in po transferjih po drzavah
razlika_m <- inner_join(moski_po %>% rename(Delez.Po = Delez), moski_pred %>% rename(Delez.Pred = Delez)) %>%
  transmute(Drzava, Delez = Delez.Pred - Delez.Po)

### Razlika med socialno ogrozenostjo zensk pred in po transferjih po drzavah 
razlika_z <- inner_join(zenske_po %>% rename(Delez.Po = Delez), zenske_pred %>% rename(Delez.Pred = Delez)) %>%
  transmute(Drzava, Delez = Delez.Pred - Delez.Po)
#### Razlika med uspesnostjo transferjev med spoloma
razlika_mz <- inner_join(razlika_m %>% rename(Delez.m = Delez), razlika_z %>% rename(Delez.z = Delez)) %>%
  transmute(Drzava, Delez = Delez.m - Delez.z)
#### Povprecje
ralika_mz.pov <- mean(razlika_mz$Delez)


### Razlika med socialno ogrozenostjo moskih in zensk pred transferji
razlika_mz_pred <- inner_join(moski_pred %>% rename(delez.m = Delez), zenske_pred %>% rename(delez.z = Delez)) %>%
  transmute(Drzava, Delez = delez.m -delez.z)
  
#### Povprecje
razlika.spol.pov <- mean(razlika_mz_pred$Delez)


# Grafi

## Graf prikazuje za koliko drzave zmanjsajo delez socialno ogrozenih
graf_najboljsi <- ggplot(data = najboljse_drzave) + 
  aes(x = Drzava, y = Delez)+ 
  geom_bar(stat="identity",fill ="cornflowerblue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Drzava") + 
  ylab("Razlika delezov") +
  ggtitle("Zmanjsanje deleza socialno ogrozenih preko socialnih transferjev")

  
## Graf razlike socialne ogrozenosti med spoloma

skupaj_pred <- rbind(moski_pred %>% mutate(Spol = "M"),
                     zenske_pred %>% mutate(Spol = "Z"))

primerjava <- inner_join(moski_pred,zenske_pred)
  
graf_spol <- skupaj_pred %>%
  ggplot(aes(x = Drzava, y = Delez, fill = Spol)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Drzava") + 
  ylab("Delez socialno ogroženih pred transferji") +
  ggtitle("Razlika med socialno ogroženostjo moskih in zensk pred transferji")


## Graf socialnega stanja na Irskem skozi leta

irska <- left_join(tabela_pred_transferji %>% rename(delez.pred = Delez), tabela_po_transferjih %>% rename(delez.po = Delez)) %>%
  filter(Drzava == "Ireland", Spol == "Skupaj", Starost == "Skupaj") %>%
  transmute(Leto,  delez.pred, delez.po, razlika = delez.pred - delez.po)

irska <- melt(irska, id = "Leto")

colnames(irska) <- c("Leto", "Kategorija", "Delez")

levels(irska$Kategorija) <- c("Pred transferji", "Po transferjih", "Razlika")

graf_irska <- ggplot(data = irska, aes(x = Leto, y = Delez, fill = Kategorija)) + 
  geom_bar(stat = "identity", position = "dodge")

graf_irska <- graf_irska + xlab("Leto") + 
  ylab("Delez") +
  ggtitle("Socialna ogroženost in uspešnost socialne politike na Irskem med leti 2007-2015")
  
#Zemljevid socialne ogrozenosti pred transferji po drzavah

pred_for_map <- inner_join(drzave_pred, tabela_id)


evropa <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                          "ne_50m_admin_0_countries", 
                          encoding = "UTF-8") %>%
  pretvori.zemljevid() %>% 
  filter(continent == "Europe" | sovereignt %in% c("Turkey", "Cyprus") & sovereignt != "Russia",long > -30)

pred_for_map1 <- left_join(evropa, pred_for_map, by = c("iso_a2" = "ID"))


zem_e <-ggplot() +
  geom_polygon(data = pred_for_map1, aes(x = long, y = lat, group = group, fill = Delez)) +
  coord_map(xlim = c(-25,40), ylim = c(32,72))


##Ti podatki so se izkazali za dokaj neuporabne
# leto_7_moski_pred  <- tabela_pred_transferji %>%
#   filter(Spol == "Moski" & Starost == "Skupaj" & Leto == 2007) %>%
#   group_by(Drzava) %>%
#   select(Drzava, Delez)
# 
# leto_7_zensk_pred <- tabela_pred_transferji %>%
#   filter(Spol == "Zenske" & Starost == "Skupaj" & Leto == 2007) %>%
#   group_by(Drzava) %>%
#   select(Drzava, Delez)
# 
# leto_7_moski_po <- tabela_po_transferjih %>%
#   filter(Spol == "Moski" & Starost == "Skupaj" & Leto == 2007) %>%
#   group_by(Drzava) %>%
#   select(Drzava, Delez)
# 
# leto_7_zensk_po <- tabela_po_transferjih %>%
#   filter(Spol == "Zenske" & Starost == "Skupaj" & Leto == 2007) %>%
#   group_by(Drzava) %>%
#   select(Drzava, Delez)
 