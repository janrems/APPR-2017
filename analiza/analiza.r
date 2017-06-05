# 4. faza: Analiza podatkov

podatki <- obcine %>% transmute(obcina, povrsina, gostota,
                                gostota.naselij = naselja/povrsina) %>%
  left_join(povprecja, by = "obcina")
row.names(podatki) <- podatki$obcina
podatki$obcina <- NULL

# Število skupin
n <- 5
skupine <- hclust(dist(scale(podatki))) %>% cutree(n)

#ZA NAPREDNO ANALIZO
# odnosi <- inner_join(tabela_pred_transferji, tabela_gini) %>% 
#   inner_join(bdp_indeks) %>%
#   group_by(Drzava) %>%
#   summarise(Delez = round(mean(Delez),1), Koeficient = round(mean(Koeficient),1), Indeks = round(mean(Indeks),1))
# 
# graf_odnosi <- ggplot() + 
#   geom_point(data=odnosi, mapping=aes(x = Koeficient, y = Delez, size = Indeks), colour = 'red4', fill = 'white')