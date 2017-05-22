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

test <- right_join(tabela_pred_transferji, tabela_po_transferjih)

najboljsi <- left_join(tabela_pred_transferji %>% rename(Delez.Pred = Delez),
                       tabela_po_transferjih %>% rename(Delez.Po = Delez)) %>%
  filter(Spol == "Skupaj"& Starost == "Skupaj",Leto == 2007) %>%
  transmute(Drzava,  Delez = Delez.Pred - Delez.Po) %>%
  arrange(desc(Delez))

spol_pred  <- tabela_pred_transferji %>%
  filter(Spol != "Skupaj" & Starost == "Skupaj" $ Leto == 2007) %>%
  transmute(Drzava, Razlika = )

