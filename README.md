# Analiza podatkov s programom R, 2016/17

## Analiza revščinr in vpliv socialnih transferjev v EU
Avtor: Jan Rems

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2016/17

## Tematika

V projektu bom analiziral delež prebivalstva, na pragu revščine, pred socialnimi transferji in po njih, glede na geografsko poreklo, starost in spol. Prav tako bom analiziral vpliv socialnizh transferjev na zmanjšanje tveganja revščine. Podatke bom analiziral za obdobje od 2007 do 2015(mogoče dodam še glede na socialni status, izobrazbo...).

Podatke sem dobil na naslednjih povezavah:

* http://ec.europa.eu/eurostat/web/products-datasets/-/ilc_li10

* http://ec.europa.eu/eurostat/web/products-datasets/-/tipslc20

* http://ec.europa.eu/eurostat/web/products-datasets/-/tespm050

* http://ec.europa.eu/eurostat/web/products-datasets/-/tps00102

Podatki so v CSV obliki.

Izbrali si boste temo, s katero se bo vaš projekt ukvarjal. Tukaj boste napisali, kje ste dobili podatke, ter kakšen je vaš cilj.

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
