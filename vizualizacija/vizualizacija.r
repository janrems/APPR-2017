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


najboljsi <- left_join(tabela_pred_transferji %>% rename(Delez.Pred = Delez),
                       tabela_po_transferjih %>% rename(Delez.Po = Delez)) %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj",Leto == 2007) %>%
  transmute(Drzava,  Delez = Delez.Pred - Delez.Po) %>%
  arrange(desc(Delez))

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
