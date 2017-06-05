# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip",
                             "OB/OB", encoding = "Windows-1250")
levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels = levels(obcine$obcina))
zemljevid <- pretvori.zemljevid(zemljevid)

# Izračunamo povprečno velikost družine
povprecja <- druzine %>% group_by(obcina) %>%
  summarise(povprecje = sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))


#pred teansferi povprecna
pred_for_map <- tabela_pred_transferji %>%
  filter(Spol == "Skupaj", Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez))

pred_for_map <- inner_join(pred_for_map, tabela_id)

library(rworldmap)
library(maps)


evropa <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                          "ne_50m_admin_0_countries", encoding = "UTF-8") %>%
  pretvori.zemljevid() %>% filter(continent == "Europe" | sovereignt %in% c("Turkey", "Cyprus") & sovereignt != "Russia",
                                  long > -30)



pred_for_map1 <- left_join(evropa, pred_for_map, by = c("iso_a2" = "ID"))



zem_e <-ggplot() +
  geom_polygon(data = pred_for_map1, aes(x = long, y = lat, group = group, fill = Delez)) +
  coord_map(xlim = c(-25,40), ylim = c(32,72))
  

print(zem_e)










# 3. faza: Vizualizacija podatkov

#iskanje ustreznih podatkov

#skupni podatki za evropo

evro <- inner_join(tabela_pred_transferji,tabela_prebivalci) %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj") %>%
  group_by(Leto) %>%
  summarize(delez = sum(Delez * Populacija)/sum(Populacija))


evro_po <- inner_join(tabela_po_transferjih,tabela_prebivalci) %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj") %>%
  group_by(Leto) %>%
  summarize(delez = sum(Delez * Populacija)/sum(Populacija))


#Eu starost
eu_starost_pred <- inner_join(tabela_pred_transferji,tabela_prebivalci) %>%
  filter(Spol == "Skupaj"& Starost != "Skupaj") %>%
  group_by(Leto, Starost) %>%
  summarize(Delez = sum(Delez * Populacija)/sum(Populacija))

eu_starost_pred.povp <- eu_starost_pred %>%
  group_by(Starost) %>%
  summarise(Delez = mean(Delez))

eu_starost_po <- inner_join(tabela_po_transferjih,tabela_prebivalci) %>%
  filter(Spol == "Skupaj"& Starost != "Skupaj") %>%
  group_by(Leto, Starost) %>%
  summarize(Delez = sum(Delez * Populacija)/sum(Populacija))

eu_starost_po.povp <- eu_starost_po %>%
  group_by(Starost) %>%
  summarise(Delez = mean(Delez))




#podatki za leto 2007

leto7_pred <- tabela_pred_transferji %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj",Leto == 2007) %>%
  group_by(Drzava) %>%
  select(Drzava, Delez)
  
povprecje_leto7_pred <- mean(leto7_pred$Delez)
odklon_leto7_pred <- sd(leto7_pred$Delez)

leto7_po <- tabela_po_transferjih %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj",Leto == 2007) %>%
  group_by(Drzava) %>%
  select(Drzava, Delez)

povprecje_leto7_po <- mean(leto7_po$Delez)
odklon_leto7_po <- sd(leto7_po$Delez)


najboljsi_7 <- left_join(tabela_pred_transferji %>% rename(Delez.Pred = Delez),
                       tabela_po_transferjih %>% rename(Delez.Po = Delez)) %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez.Pred - Delez.Po)) %>%
  arrange(desc(Delez))

najboljsi_13 <- left_join(tabela_pred_transferji %>% rename(Delez.Pred = Delez),
                          tabela_po_transferjih %>% rename(Delez.Po = Delez)) %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj",Leto == 2013) %>%
  transmute(Drzava,  Delez = Delez.Pred - Delez.Po) %>%
  arrange(desc(Delez))

razlika_najboljsi <- left_join(najboljsi_7 %>% rename(delez.7 = Delez),najboljsi_13 %>% rename(delez.13 = Delez)) %>%
  group_by(Drzava) %>%
  summarize(Delez = sum(delez.13, -delez.7))
razlika_najboljsi$Delez <- round(razlika_najboljsi$Delez, digits = 2)

razlika.povp <- mean(razlika_najboljsi$Delez)

#dost brezveze
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
  
# leto 2015

leto15_pred <- tabela_pred_transferji %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj",Leto == 2015) %>%
  group_by(Drzava) %>%
  select(Drzava, Delez)

povprecje_leto15_pred <- mean(leto15_pred$Delez)
odklon_leto15_pred <- sd(leto15_pred$Delez)

# povprečje let

moski_pred <- tabela_pred_transferji %>%
  filter(Spol == "Moski" & Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez)) 

moski_po <- tabela_po_transferjih %>%
  filter(Spol == "Moski" & Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez))

zenske_pred <- tabela_pred_transferji %>%
  filter(Spol == "Zenske" & Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez)) 

zenske_po <- tabela_po_transferjih %>%
  filter(Spol == "Zenske" & Starost == "Skupaj") %>%
  group_by(Drzava) %>%
  summarise(Delez = mean(Delez))

razlika_m <- inner_join(moski_po %>% rename(Delez.Po = Delez), moski_pred %>% rename(Delez.Pred = Delez)) %>%
  transmute(Drzava, Delez = Delez.Pred - Delez.Po)

razlika_z <- inner_join(zenske_po %>% rename(Delez.Po = Delez), zenske_pred %>% rename(Delez.Pred = Delez)) %>%
  transmute(Drzava, Delez = Delez.Pred - Delez.Po)

razlika_mz <- inner_join(razlika_m %>% rename(Delez.m = Delez), razlika_z %>% rename(Delez.z = Delez)) %>%
  transmute(Drzava, Delez = Delez.m - Delez.z)
ralika_mz.pov <- mean(razlika_mz$Delez)
#razlike med zmanjšanjem revnih praktično ni minimalno na strani ž. torej raje glejmo pred

razlika_mz_pred <- inner_join(moski_pred %>% rename(delez.m = Delez), zenske_pred %>% rename(delez.z = Delez)) %>%
  transmute(Drzava, Delez = delez.m -delez.z)
  
razlika_mz_po.pov <- mean(razlika_mz_pred$Delez)


#starost

starost_pred <- tabela_pred_transferji %>%
  filter(Spol == "Skupaj", Starost != "Skupaj") %>%
  group_by(Drzava, Starost) %>%
  summarise(Povprecni_delez = mean(Delez))

#grafi

graf_najboljsi <- ggplot(data = najboljsi_7) + aes(x = Drzava, y = Delez) + geom_bar(stat="identity",fill ="cornflowerblue") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
graf_najboljsi <- graf_najboljsi + xlab("Drzava") + ylab("Razlika delezov") +ggtitle("Zmanjsanje deleza socialno ogrozenih preko socialnih transferjev")
print(graf_najboljsi)


odnosi <- inner_join(tabela_pred_transferji, tabela_gini) %>% 
  inner_join(bdp_indeks) %>%
  group_by(Drzava) %>%
  summarise(Delez = round(mean(Delez),1), Koeficient = round(mean(Koeficient),1), Indeks = round(mean(Indeks),1))

graf_odnosi <- ggplot() + 
  geom_point(data=odnosi, mapping=aes(x = Koeficient, y = Delez, size = Indeks), colour = 'red4', fill = 'white')
print(graf_odnosi)
  
  
  
  
spremembe <- tabela_pred_transferji %>%
  filter(Starost == "Skupaj", Spol == "Skupaj")
  


skupaj_pred <- rbind(moski_pred %>% mutate(Spol = "M"),
                     zenske_pred %>% mutate(Spol = "Z"))

primerjava <- inner_join(moski_pred,zenske_pred)
  
graf_spol <- skupaj_pred %>%
  ggplot(aes(x = Drzava, y = Delez, fill = Spol)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
graf_spol <- graf_spol + xlab("Drzava") + ylab("Delez socialno ogroženih pred transferji") +ggtitle("Razlika med socialno ogroženostjo moskih in zensk pred transferji")
print(graf_spol)

povprecje_spol <- skupaj_pred %>%
  group_by(Spol) %>%
  summarise(Razlika = mean(Delez))

irska <- left_join(tabela_pred_transferji %>% rename(delez.pred = Delez), tabela_po_transferjih %>% rename(delez.po = Delez)) %>%
  filter(Drzava == "Ireland", Spol == "Skupaj", Starost == "Skupaj") %>%
  transmute(Leto, Drzava, delez.pred, delez.po, razlika = delez.pred - delez.po)

  
  grcija <- tabela_pred_transferji %>%
  filter(Drzava == "Greece", Spol == "Skupaj", Starost == "Skupaj")