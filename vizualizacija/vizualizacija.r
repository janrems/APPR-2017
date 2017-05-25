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
odklon_leto7_pred <- (1/length(leto7_pred)*sum((leto7_pred$Delez - povprecje_leto7_pred)^2))^0.5

leto7_po <- tabela_po_transferjih %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj",Leto == 2007) %>%
  group_by(Drzava) %>%
  select(Drzava, Delez)

povprecje_leto7_po <- mean(leto7_po$Delez)
odklon_leto7_po <- (1/length(leto7_po)*sum((leto7_po$Delez - povprecje_leto7_po)^2))^0.5


najboljsi_7 <- left_join(tabela_pred_transferji %>% rename(Delez.Pred = Delez),
                       tabela_po_transferjih %>% rename(Delez.Po = Delez)) %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj",Leto == 2007) %>%
  transmute(Drzava,  Delez = Delez.Pred - Delez.Po) %>%
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
odklon_leto15_pred <- (1/length(leto15_pred)*sum((leto15_pred$Delez - povprecje_leto15_pred)^2))^0.5

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

graf_najboljsi <- najboljsi_7 %>% 
  ggplot(aes(x = Drzava, y = Delez)) + geom_bar()



