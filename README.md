Európai Infláció Monitor
================
Ferenci Tamás (www.medstat.hu)

A weboldal elérhetősége: [http://87.229.84.127:3838/EUInflacioMonitor/](http://87.229.84.127:3838/EUInflacioMonitor/).

## Technikai részletek

A weboldal [R Shiny](https://shiny.rstudio.com/) segítségével készült, [R statisztikai környezet](https://www.youtube.com/@FerenciTamas/playlists?view=50&sort=dd&shelf_id=2) alatt. A vizualizáláshoz a [Highcharts](https://www.highcharts.com/) könyvtárat használtam (R alatt a [highcharter](https://jkunst.com/highcharter/) csomag segítségével), illetve a [ggplot2](https://ggplot2.tidyverse.org/) csomagot.

A weboldalhoz a következő fájlok tartoznak:
- [app.R](https://github.com/tamas-ferenci/EUInflacioMonitor/blob/main/app.R): Az oldal forráskódja.
- [InflacioPrepare.R](https://github.com/tamas-ferenci/EUInflacioMonitor/blob/main/InflacioPrepare.R): Az adatokat előkészítő szkript.
- COICOPData.rds, RawDataCountryWeights.rds, RawDataInflation.rds: Az előkészített adatok.
- Structure EN-GA-HR-HU-IT-LT-LV-MT.xlsx: A COICOP-hierachia adatforrása

Az inflációs adatok forrása az Eurostat [prc_hicp_manr](https://ec.europa.eu/eurostat/databrowser/view/prc_hicp_manr/default/table?lang=en) adattáblája, az országok súlyai (amik alapján az összehasonlításnál az országcsoportok adatai számolhatóak a [prc_hicp_cow](https://ec.europa.eu/eurostat/databrowser/view/prc_hicp_cow/default/table?lang=en) táblából kerültek ki.

A robusztus mutató számításakor a MAD már tartalmazza az 1,48-as szorzót (hogy normális eloszlásra azonos legyen az értéke a szórással).
