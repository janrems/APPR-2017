

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
  inner_join(tabela_delez_bdp %>% rename(bdp = Delez)) %>%
  group_by(Drzava) %>%
  summarise(Delez = round(mean(Delez),1), Koeficient = round(mean(Koeficient),1), Indeks = round(mean(Indeks),1), bdp =round(mean(bdp),1))

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

# 
# grafi <- list()
# i <- 0
# for (kategorija in colnames(skupna)[c(2,3)]) {
#   for (var in colnames(skupna)[c(4:6)]){
#     i <- i + 1
#     u <- list()
#     graf <- ggplot(skupna, aes(x= skupna$var, y= skupna$kategorija)) +
#       geom_point()
#     u[1] <- graf
#     funkcija <- lm(data = skupna, skupna$kategorija ~ skupna$var + I(skupna$var^2))
#     u[2] <- funkcija$coefficients
#     napaka <- sum(funkcija$residuals^2)
#     u[3] <- napaka
#     grafi[i] <- paste(kategorija)
#     assign(paste(kategorija, var, sep = "."),u)
#   } 
# }

# Seznam vseh grafov odvisnosti

stx <- c("Koeficient", "Indeks", "Delez_bdp")
sty <- c("Pred", "Po")
stolpci <- skupna[1,] %>% melt(measure.vars = stx, variable.name = "x", value.name = "valx") %>%
  melt(measure.vars = sty, variable.name = "y", value.name = "valy") %>% select(x, y)
grafi <- apply(stolpci, 1, . %>% {
  ggplot(skupna, aes_string(x = .[1], y = .[2])) + geom_point() +
    geom_smooth(method = "lm", formula = y ~ x + I(x^2))
})
ggmatrix(grafi, 2, 3, xAxisLabels = stx, yAxisLabels = sty)
 

# Matrika modelov odvisnosti

modeli <- apply(stolpci, 1, . %>% {
  lm(data = skupna, sprintf("%s ~ %s + I(%s^2)", .[2], .[1], .[1]) %>% as.formula()) %>%
  { c(.$coefficients, round(sum(.$residuals^2),0)) }
}) %>% t()

matrika_odnosov <- cbind(stolpci, modeli)
matrika_odnosov <- matrika_odnosov[c("y","x",3,2,1,4)]
colnames(matrika_odnosov) <- c("Delez ogrozenih", "Spremenljivka", "Kvadratni clen", "Linearni clen", "Prosti clen", "Residual")

korealcija <- apply(stolpci, 1, . %>% {
  cor(skupna$.[1], skupna$.[2])
})
