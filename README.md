# Analiza podatkov s programom R, 2016/17

## Analiza revščine in vpliv socialnih transferjev v EU
Avtor: Jan Rems

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2016/17

## Tematika

V projektu bom analiziral delež prebivalstva, na pragu revščine, pred socialnimi transferji in po njih, glede na geografsko poreklo, starost in spol. Prav tako bom analiziral vpliv socialnizh transferjev na zmanjšanje tveganja revščine. Podatke bom analiziral za obdobje od 2007 do 2015. 

Tabela 1 bo vsebovala države za vsako leto in naslednje stolpce:

* delež socialno ogroženih pred transferji

* delež socialno ogroženih po transferjih

* BDP 

* delež BDP-ja namenjen za socialne transferje

Tabela 2 bo vsebovala države za vsa leta v stolpcih pa bo struktura ogroženega prebivalstva pred socialnimi transferji po kategoriji starosti. Stoplpci bodo starostne kategorije.

Tabela 3 bo vsebovala države za vsa leta v stolpcih pa bo struktura ogroženega prebivalstva po socialnih transferjih po kategoriji starosti. Stoplpci bodo starostne kategorije.

Cilj naloge je ugotoviti, v prvem planu stopnjo revščine oz. socialne ogroženosti prebivalstva različnih držav, glede na geografsko poreklo in BDP, nato pa še učinkovitost socialnih politik teh držav. Na koncu bi naredil še napoved kako se bo gibala stopnja revščine v posameznih državah in v EU v prihodnje.

Podatke sem dobil na naslednjih povezavah:

* http://ec.europa.eu/eurostat/web/products-datasets/-/ilc_li10

* http://ec.europa.eu/eurostat/web/products-datasets/-/tipslc20

* http://ec.europa.eu/eurostat/web/products-datasets/-/tespm050

* http://ec.europa.eu/eurostat/web/products-datasets/-/tps00102

Podatki so v CSV obliki.


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
