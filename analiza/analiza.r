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


odnosi <- inner_join(tabela_pred_transferji, tabela_gini) %>%
  inner_join(bdp_indeks) %>%
  group_by(Drzava) %>%
  summarise(Delez = round(mean(Delez),1), Koeficient = round(mean(Koeficient),1), Indeks = round(mean(Indeks),1))

graf_odnosi <- ggplot() +
  geom_point(data=odnosi, mapping=aes(x = Koeficient, y = Delez, size = Indeks), colour = 'red4', fill = 'white') + 
  geom_text(aes(x, y,label=odnosi$Drzava),hjust=0, vjust=0)

graf_odnosi2 <- ggplot(odnosi, aes(x= Koeficient, y= Delez, size = Indeks, colour="green", label=Drzava)) +
  geom_point() +
  geom_text(aes(label=Drzava),hjust=0, vjust=0)


odnosi_po <-inner_join(tabela_po_transferjih, tabela_gini) %>%
  inner_join(bdp_indeks) %>% 
  inner_join(tabela_delez_bdp %>% rename(Bdp = Delez)) %>%
  group_by(Drzava) %>%
  summarise(Delez = round(mean(Delez),1), Koeficient = round(mean(Koeficient),1), Indeks = round(mean(Indeks),1), Bdp =round(mean(bdp),1))

graf_odnosi_po <- ggplot(odnosi_po, aes(x= Koeficient, y= Delez, size = Koeficient, colour="green", label=Drzava)) +
  geom_point() +
  geom_text(aes(label=Drzava),hjust=0, vjust=0) +
  geom_smooth(method = "lm")

lin <- lm(data = odnosi_po, Delez ~ bdp + I(bdp^2))
npaka <- sum(lin$residuals^2)

odnosi.norm <- odnosi %>% select(-Drzava) %>% scale()
k <- kmeans(odnosi.norm, 5, nstart = 1000)
  

skupine <- data.frame(Drzava = odnosi$Drzava, skupina = factor(k$cluster))
skupine <- inner_join(odnosi,skupine) %>%
  group_by(skupina) %>%
  summarise(Delez = mean(Delez), Koeficient = mean(Koeficient), Indeks = mean(Indeks))

graf_skupine <-ggplot(skupine, aes(x= Koeficient, y= Delez, size = Indeks, colour="green", label=skupina)) +
  geom_point() +
  geom_text(aes(label=skupina),hjust=0, vjust=0)
  

skupna <- inner_join(odnosi %>% rename(Pred = Delez), tabela_po_transferjih%>% rename(Po = Delez)) %>%
  inner_join(tabela_delez_bdp %>% rename(Delez_bdp = Delez)) %>%
  group_by(Drzava) %>%
  summarise(Po = round(mean(Po),1), Pred = round(mean(Pred),1), Koeficient = round(mean(Koeficient),1), Indeks = round(mean(Indeks),1), Delez_bdp = round(mean(Delez_bdp),1))


grafi <- list()
i <- 0
for (kategorija in colnames(skupna)[c(2,3)]) {
  for (var in colnames(skupna)[c(4:6)]){
    i <- i + 1
    u <- list()
    graf <- ggplot(skupna, aes(x= skupna$var, y= skupna$kategorija)) +
      geom_point()
    u[1] <- graf
    funkcija <- lm(data = skupna, skupna$kategorija ~ skupna$var + I(skupna$var^2))
    u[2] <- funkcija$coefficients
    napaka <- sum(funkcija$residuals^2)
    u[3] <- napaka
    grafi[i] <- paste(kategorija)
    assign(paste(kategorija, var, sep = "."),u)
  } 
}
  