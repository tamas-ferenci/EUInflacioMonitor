library(data.table)

RawDataCountryWeights <- as.data.table(eurostat::get_eurostat("prc_hicp_cow"))
RawDataCountryWeights$year <- lubridate::year(RawDataCountryWeights$time)
RawDataCountryWeights <- rbindlist(lapply(
  1999:max(RawDataCountryWeights$year),
  function(yr)
    rbind(RawDataCountryWeights[year==yr&statinfo=="COWEU27_2020"&!geo%in%c("EU27_2020", "EA"),
                                .(geo, year, values)],
          RawDataCountryWeights[year==yr&statinfo=="COWEU28"&geo=="UK", .(geo, year, values)],
          RawDataCountryWeights[year==yr&statinfo=="COWEA"&geo!="EA",
                                .(geo, year, values = values*RawDataCountryWeights[
                                  year==yr&statinfo=="COWEU"&geo=="EA"]$values/1000)])))
names(RawDataCountryWeights)[names(RawDataCountryWeights)=="values"] <- "weight"
RawDataCountryWeights$geo <- countrycode::countrycode(RawDataCountryWeights$geo, "eurostat", "cldr.name.hu")
saveRDS(RawDataCountryWeights, "RawDataCountryWeights.rds")

RawData <- as.data.table(eurostat::get_eurostat("prc_hicp_manr"))
RawData <- RawData[geo%in%c(eurostat::eu_countries$code, "UK")&time>="1999-01-01"]
RawData$year <- lubridate::year(RawData$time)
RawData$geo <- countrycode::countrycode(RawData$geo, "eurostat", "cldr.name.hu")
RawData <- RawData[, .(geo, time, coicop, values)]
RawData <- RawData[order(geo, time)]
saveRDS(RawData, "RawDataInflation.rds")

COICOPData <- as.data.table(readxl::read_excel("Structure EN-GA-HR-HU-IT-LT-LV-MT.xlsx"))
COICOPData <- unique(COICOPData[, .(coicop = Code, COICOPname = `Hulladékszállítás`)][
  !is.na(coicop)&!coicop%in%c("A", "B")])
COICOPData$COICOPname <- stringr::str_to_sentence(COICOPData$COICOPname)
COICOPData$coicop <- ifelse(substring(COICOPData$coicop, 1, 1)%in%(0:9),
                            paste0("CP", gsub(".", "", COICOPData$coicop, fixed = TRUE)), COICOPData$coicop)
COICOPData <- rbind(
  COICOPData,
  data.table(coicop = c("AP", "APF", "APM", "AP_NNRG", "AP_NRG", "CP00", "CP0531_0532", "CP0612_0613",
                        "CP0621_0623", "CP0712-0714", "CP082_083", "CP0921_0922", "CP0934_0935",
                        "CP0953_0954", "CP1212_1213", "TOT_X_AP", "TOT_X_APF", "TOT_X_APM"),
             COICOPname = c("Administered prices", "Fully administered prices",
                            "Mainly administered prices","Administered prices, non-energy",
                            "Administered prices, energy", "Minden elem",
                            paste0("Elektromos és nem elektromos tartós háztartási gépek és készülékek, és",
                                   "elektromos háztartási kisgépek"),
                            "Egyéb egészségügyi termékek, és gyógyászati segédeszközök és készülékek",
                            "Egészségügyi szolgáltatások, és paramedicinális szolgáltatások",
                            "Motorkerékpár, kerékpár, és állati erővel vontatott járművek",
                            "Telefon- és telefaxberendezések, és telefon- és telefaxszolgáltatások",
                            paste0("Kültéri szabadidős tevékenységet és kempingezést szolgáló tartós",
                                   "fogyasztási cikkek, és hangszerek és beltéri szabadidős tevékenységet",
                                   "szolgáló tartós fogyasztási cikkek"),
                            paste0("Hobbiállatok és tartásukkal kapcsolatos termékek, és állatorvosi és",
                                   "egyéb szolgáltatások hobbiállatok számára"),
                            "Egyéb nyomdai termékek, és papír és írószer, rajzeszköz",
                            paste0("Egyéb testápolási eszközök és termékek, és egyéb testápolási eszközök",
                                   "és termékek"), "Overall index excluding administered prices",
                            "Overall index excluding fully administered prices",
                            "Overall index excluding mainly administered prices")))
COICOPData$COICOPlevel <- ifelse(substring(COICOPData$coicop, 1, 2)=="CP", nchar(COICOPData$coicop)-2, -1)
COICOPData[coicop=="CP00"]$COICOPlevel <- 1
COICOPData <- COICOPData[order(COICOPlevel==-1, COICOPlevel)]
saveRDS(COICOPData, "COICOPData.rds")