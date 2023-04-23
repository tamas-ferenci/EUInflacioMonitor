library(shiny)
library(data.table)
library(highcharter)

RawData <- readRDS("RawDataInflation.rds")
COICOPData <- readRDS("COICOPData.rds")
RawDataCountryWeights <- readRDS("RawDataCountryWeights.rds")

hcoptslang <- getOption("highcharter.lang")
hcoptslang$contextButtonTitle <- "Helyi menü"
hcoptslang$exitFullscreen <- "Kilépés a teljes képernyős módból"
hcoptslang$hideData <- "Adatok elrejtése"
hcoptslang$loading <- "Betöltés..."
hcoptslang$mainBreadcrumb <- "Fő ábra"
hcoptslang$noData <- "Nincs megjeleníthető adat"
hcoptslang$printChart <- "Ábra nyomtatása"
hcoptslang$viewData <- "Adatok megtekintése"
hcoptslang$viewFullscreen <- "Teljes képernyős nézet"
hcoptslang$months <- c("január", "február", "március", "április", "május","június", "július", "augusztus",
                       "szeptember", "október", "november", "december")
hcoptslang$shortMonths <- c("jan", "febr", "márc", "ápr", "máj", "jún", "júl", "aug", "szept", "okt",
                            "nov", "dec")
hcoptslang$weekdays <- c("vasárnap", "hétfő", "kedd", "szerda", "csütörtök", "péntek", "szombat")
hcoptslang$shortWeekdays <- c("Vas", "Hét", "Ked", "Sze", "Csü", "Pén", "Szo", "Vas")
hcoptslang$exportButtonTitle <- "Exportál"
hcoptslang$printButtonTitle <- "Importál"
hcoptslang$rangeSelectorFrom <- "ettől"
hcoptslang$rangeSelectorTo <- "eddig"
hcoptslang$rangeSelectorZoom <- "mutat:"
hcoptslang$downloadPNG <- "Letöltés PNG képként"
hcoptslang$downloadJPEG <- "Letöltés JPEG képként"
hcoptslang$downloadPDF <- "Letöltés PDF dokumentumként"
hcoptslang$downloadSVG <- "Letöltés SVG formátumban"
hcoptslang$downloadCSV <- "Letöltés CSV formátumú táblázatként"
hcoptslang$downloadXLS <- "Letöltés XLS formátumú táblázatként"
hcoptslang$resetZoom <- "Nagyítás alaphelyzetbe állítása"
hcoptslang$resetZoomTitle <- "Nagyítás alaphelyzetbe állítása"
hcoptslang$thousandsSep <- " "
hcoptslang$decimalPoint <- ","
options(highcharter.lang = hcoptslang)

eucountries <- list("EU28" = unique(RawData$geo),
                    "V4" = c("Magyarország", "Csehország", "Szlovákia", "Lengyelország"),
                    "EU15" = c("Ausztria", "Belgium", "Dánia", "Finnország", "Franciaország", "Németország",
                               "Görögország", "Írország", "Olaszország", "Luxemburg", "Hollandia",
                               "Portugália", "Spanyolország", "Svédország", "Egyesült Királyság"),
                    "EU11" = c("Csehország", "Észtország", "Magyarország", "Lettország", "Litvánia",
                               "Lengyelország", "Szlovákia", "Szlovénia", "Bulgária", "Románia",
                               "Horvátország"))

desctext <- paste0("Az Európai Unió országainak inflációit bemutató és Magyarországgal való ",
                   "összehasonlítását lehetővé tevő alkalmazás. Írta: Ferenci Tamás.")
urlpre <- "http://87.229.84.127:3838/"

ui <- fluidPage(
  
  tags$head(
    tags$meta(name = "description", content = desctext),
    tags$meta(property = "og:title", content = "Európai Infláció Monitor"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:locale", content = "hu_HU"),
    tags$meta(property = "og:url", content = paste0(urlpre, "EUInflacioMonitor/")),
    tags$meta(property = "og:image", content = paste0(urlpre, "EUInflacioMonitor_Pelda.png")),
    tags$meta(property = "og:image:width", content = 1200),
    tags$meta(property = "og:image:height", content = 630),
    tags$meta(property = "og:description", content = desctext),
    tags$meta(name = "DC.Title", content = "Európai Infláció Monitor"),
    tags$meta(name = "DC.Creator", content = "Ferenci Tamás"),
    tags$meta(name = "DC.Subject", content = "közgazdaságtan"),
    tags$meta(name = "DC.Description", content = desctext),
    tags$meta(name = "DC.Publisher", content = paste0(urlpre, "EUInflacioMonitor/")),
    tags$meta(name = "DC.Contributor", content = "Ferenci Tamás"),
    tags$meta(name = "DC.Language", content = "hu_HU"),
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:title", content = "Európai Infláció Monitor"),
    tags$meta(name = "twitter:description", content = desctext),
    tags$meta(name = "twitter:image", content = paste0(urlpre, "EUInflacioMonitor_Pelda.png"))
  ),
  
  tags$div(id = "fb-root"),
  tags$script(async = NA, defer = NA, crossorigin = "anonymous",
              src = "https://connect.facebook.net/hu_HU/sdk.js#xfbml=1&version=v16.0", nonce = "Ae71wgRn"),
  
  titlePanel("Európai Infláció Monitor"),
  
  p( "A program használatát részletesen bemutató súgó, valamint a technikai részletek",
     a( "itt", href = "https://github.com/tamas-ferenci/EUInflacioMonitor",
        target = "_blank" ), "olvashatóak el. Írta: ",
     a("Ferenci Tamás", href = "http://www.medstat.hu/", target = "_blank",
       .noWS = "outside"), "."),
  
  div(style = "line-height: 13px;",
      div(class = "fb-share-button",
          "data-href" = "http://87.229.84.127:3838/EUInflacioMonitor/",
          "data-layout" = "button_count", "data-size" = "small",
          a("Megosztás", target = "_blank",
            href = paste0("https://www.facebook.com/sharer/sharer.php?u=",
                          "http%3A%2F%2F87.229.84.127%3A3838%2FEUInflacioMonitor%2F&amp;src=sdkpreparse"),
            class = "fb-xfbml-parse-ignore")),
      
      a("Tweet", href = "https://twitter.com/share?ref_src=twsrc%5Etfw", class = "twitter-share-button",
        "data-show-count" = "true"),
      includeScript("http://platform.twitter.com/widgets.js", async = NA, charset = "utf-8")),
  
  p(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("task", "Feladat",
                  c("Infláció abszolút mértéke (elemként)" = "AbsIndiv",
                    "Infláció abszolút mértéke (összkép COICOP-főcsoport szinten)" = "AbsGroup",
                    "Infláció viszonyítása más országokhoz (elemenként)" = "RelIndiv",
                    "Infláció viszonyítása más országokhoz (összkép COICOP-főcsoport szinten)" =
                      "RelGroup")),
      conditionalPanel("input.task=='RelIndiv' | input.task=='RelGroup'",
                       selectInput("relmethod", "Viszonyítás módszere",
                                   c("z-score" = "zscore", "Különbség" = "meandiff"))),
      conditionalPanel("input.task=='AbsIndiv' | input.task=='RelIndiv'",
                       selectInput("item", "Vizsgált elem", with(COICOPData[COICOPlevel%in%c(-1, 1, 2)],
                                                                 setNames(coicop, COICOPname)), "CP00"),
                       conditionalPanel("input.item.indexOf('CP') > -1 & input.item!='CP00'",
                                        selectInput("csoport", "COICOP-csoport", NULL)),
                       conditionalPanel("!!input.csoport & input.csoport!='Mindegyik'",
                                        selectInput("alcsoport", "COICOP-alcsoport", NULL)),
                       conditionalPanel("!!input.alcsoport & input.alcsoport!='Mindegyik'",
                                        selectInput("kategoria", "COICOP-kategória", NULL))
      ),
      sliderInput("daterange", "Vizsgált időszak", min(RawData$time), max(RawData$time),
                  c(as.Date("2015-01-01"), max(RawData$time)), timeFormat = "%Y. %m."),
      conditionalPanel("input.task=='AbsIndiv' | input.task=='AbsGroup' |
                       (input.task=='RelIndiv'&input.relmethod=='meandiff') |
                       (input.task=='RelGroup'&input.relmethod=='meandiff')",
                       selectInput("countries", "Összehasonlításként megjelenített országok",
                                   c("Teljes EU és Egyesült Királyság" = "EU28", "Visegrádi négyek" = "V4",
                                     "Posztszocialista országok (EU11)" = "EU11", "EU15" = "EU15"))),
      conditionalPanel("(input.task=='RelIndiv' | input.task=='RelGroup')&input.relmethod=='zscore'",
                       checkboxInput("robust", "Robusztus mutató megjelenítése")),
      selectInput("metric", "Inflációs mutató", c("Éves" = "annual", "Havi" = "monthly"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Eredmények",
                 conditionalPanel("input.task=='AbsIndiv' | input.task=='RelIndiv'",
                                  shinycssloaders::withSpinner(highchartOutput(
                                    "IndivPlot", height = "450px"))),
                 conditionalPanel("input.task=='AbsGroup' | input.task=='RelGroup'",
                                  shinycssloaders::withSpinner(plotOutput("GroupPlot", height = "450px")))),
        tabPanel("Magyarázat", includeMarkdown("explanation.md"))
      )
      
    )
  ),
  hr(),
  h4("Írta: Ferenci Tamás, v0.05"),
  
  tags$script(HTML("var sc_project=12872814; 
                      var sc_invisible=1; 
                      var sc_security=\"bcb68b34\";
                      var sc_remove_link=1;"),
              type = "text/javascript"),
  tags$script(src="https://www.statcounter.com/counter/counter.js", async = NA, type = "text/javascript")
)

server <- function(input, output) {
  
  observeEvent(input$item, {
    freezeReactiveValue(input, "csoport")
    updateSelectInput(inputId = "csoport",
                      choices = with(COICOPData[(substring(coicop, 1, nchar(input$item))==input$item)&
                                                  (COICOPlevel==nchar(input$item)-1)],
                                     setNames(c("Mindegyik", coicop), c("Mindegyik", COICOPname))))
  })
  
  observeEvent(input$csoport, {
    freezeReactiveValue(input, "alcsoport")
    updateSelectInput(inputId = "alcsoport",
                      choices = with(COICOPData[(substring(coicop, 1, nchar(input$csoport))==input$csoport)&
                                                  (COICOPlevel==nchar(input$csoport)-1)],
                                     setNames(c("Mindegyik", coicop), c("Mindegyik", COICOPname))))
  })
  
  observeEvent(input$alcsoport, {
    freezeReactiveValue(input, "kategoria")
    updateSelectInput(inputId = "kategoria",
                      choices = with(COICOPData[(substring(coicop, 1,
                                                           nchar(input$alcsoport))==input$alcsoport)&
                                                  (COICOPlevel==nchar(input$alcsoport)-1)],
                                     setNames(c("Mindegyik", coicop), c("Mindegyik", COICOPname))))
  })
  
  output$IndivPlot <- renderHighchart({
    switch(input$task,
           "AbsIndiv" = {
             code <- input$item
             if(input$csoport!="Mindegyik") code <- input$csoport
             if(input$alcsoport!="Mindegyik") code <- input$alcsoport
             if(input$kategoria!="Mindegyik") code <- input$kategoria
             temp <- RawData[coicop==code&time>=input$daterange[1]&time<=input$daterange[2]&
                               geo%in%c("Magyarország", eucountries[[input$countries]])&variable==input$metric]
             if(nrow(temp)==0) return(NULL)
             p <- highchart() |>
               hc_add_series(temp[geo!="Magyarország"], "line", hcaes(x = time, y = value, group = geo),
                             color = "gray", lineWidth = 0.2, marker = list(enabled = FALSE)) |>
               hc_add_series(temp[geo=="Magyarország"], "line", hcaes(x = time, y = value), lineWidth = 2,
                             marker = list(enabled = FALSE), name = "Magyarország") |>
               hc_yAxis(title = list(text = paste0(switch(input$metric, "annual" = "Éves", "monthly" = "Havi"), " infláció [%]")))
           },
           "RelIndiv" = {
             code <- input$item
             if(input$csoport!="Mindegyik") code <- input$csoport
             if(input$alcsoport!="Mindegyik") code <- input$alcsoport
             if(input$kategoria!="Mindegyik") code <- input$kategoria
             temp <- RawData[coicop==code&time>=input$daterange[1]&time<=input$daterange[2]&variable==input$metric]
             if(nrow(temp)==0) return(NULL)
             
             temp2 <- rbindlist(lapply(eucountries, function(countries) {
               merge(temp[geo=="Magyarország"],
                     merge(
                       temp[geo%in%countries&geo!="Magyarország"][
                         , .(geo, year = lubridate::year(time), time, value)],
                       RawDataCountryWeights, by = c("geo", "year"))[
                         , .(mu = matrixStats::weightedMean(value, weight),
                             sigma = matrixStats::weightedSd(value, weight),
                             med = matrixStats::weightedMedian(value, weight),
                             wtmad = matrixStats::weightedMad(value, weight)), .(time)], by = "time")[
                               , .(time, coicop, mu, zscore = (value-mu)/sigma,
                                   robzscore = (value-med)/wtmad)]
             }), idcol = "eustate")
             
             switch(input$relmethod,
                    "zscore" = {
                      p <- highchart() |>
                        hc_add_series(temp2, "line",
                                      hcaes(x = time, y = zscore, group = eustate), marker = list(enabled = FALSE),
                                      visible = sort(unique(temp2$eustate))=="EU28") |>
                        hc_yAxis(title = list(text = "z-score"),
                                 plotLines = list(list(value = 0, color = "red", width = 2))) |>
                        hc_colors(c('#2f7ed8', '#0d233a', '#8bbc21', '#910000'))
                      
                      if(input$robust) p <- p |>
                          hc_add_series(temp2, "line", hcaes(x = time, y = robzscore, group = eustate),
                                        marker = list(enabled = FALSE), dashStyle = "Dash",
                                        visible = sort(unique(temp2$eustate))=="EU28")
                    },
                    "meandiff" = {
                      temp2 <- merge(temp, temp2[eustate==input$countries], by = c("time", "coicop"))[
                        geo%in%eucountries[[input$countries]]]
                      
                      p <- highchart() |>
                        hc_add_series(temp2[geo!="Magyarország"], "line", hcaes(x = time, y = value-mu, group = geo),
                                      color = "gray", lineWidth = 0.2, marker = list(enabled = FALSE)) |>
                        hc_add_series(temp2[geo=="Magyarország"], "line", hcaes(x = time, y = value-mu), lineWidth = 2,
                                      marker = list(enabled = FALSE), name = "Magyarország") |>
                        hc_yAxis(title = list(text = paste0(switch(input$metric, "annual" = "Éves", "monthly" = "Havi"),
                                                            " infláció eltérése a viszonyítási országok átlagától [%]")),
                                 plotLines = list(list(value = 0, color = "red", width = 2)))
                      
                    })
           })
    if(input$task%in%c("AbsIndiv", "RelIndiv")) {
      p <- p |>
        hc_title(text = COICOPData[coicop==code]$COICOPname) |>
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = "%Y", month = "%Y. %m.")) |>
        hc_tooltip(dateTimeLabelFormats = list(day = "%Y. %B", month = "%Y. %B"),
                   valueDecimals = switch(input$task, "AbsIndiv" = 1, "RelIndiv" = 2)) |>
        hc_subtitle(text = "Ferenci Tamás, medstat.hu", align = "left", verticalAlign = "bottom") |>
        hc_add_theme(hc_theme(chart = list(backgroundColor = "white"))) |>
        hc_credits(enabled = TRUE) |>
        hc_exporting(enabled = TRUE, chartOptions = list(legend = list(enabled = FALSE)))
      
      p
    }
  })
  
  output$GroupPlot <- renderPlot({
    switch(input$task,
           "AbsGroup" = {
             temp <- RawData[nchar(coicop)==4&substring(coicop, 1, 2)=="CP"&time>=input$daterange[1]&
                               time<=input$daterange[2]&
                               geo%in%c("Magyarország", eucountries[[input$countries]])&variable==input$metric]
             temp <- merge(temp, COICOPData, by = "coicop")
             temp$COICOPname <- factor(temp$COICOPname,
                                       levels = unique(temp$COICOPname[order(temp$coicop)]))
             ggplot2::ggplot() +
               ggplot2::geom_line(data = temp[geo!="Magyarország"],
                                  ggplot2::aes(x = time, y = value, group = geo), color = "gray",
                                  linewidth = 0.2) +
               ggplot2::geom_line(data = temp[geo=="Magyarország"],
                                  ggplot2::aes(x = time, y = value), color = "#2f7ed8", linewidth = 1) +
               ggplot2::facet_wrap(~COICOPname, scales = "free",
                                   labeller = ggplot2::label_wrap_gen(width = 36)) +
               ggplot2::labs(y = paste0(switch(input$metric, "annual" = "Éves", "monthly" = "Havi"), " infláció [%]"),
                             x = "", tag = "Ferenci Tamás, medstat.hu") +
               ggplot2::theme(plot.tag.position = c(0.15, 0))
           },
           "RelGroup" = {
             temp <- RawData[nchar(coicop)==4&substring(coicop, 1, 2)=="CP"&
                               time>=input$daterange[1]&time<=input$daterange[2]&variable==input$metric]
             temp2 <- rbindlist(lapply(eucountries, function(countries) {
               merge(temp[geo=="Magyarország"],
                     merge(
                       temp[geo%in%countries&geo!="Magyarország"][
                         , .(geo, year = lubridate::year(time), time, value, coicop)],
                       RawDataCountryWeights, by = c("geo", "year"))[
                         , .(mu = matrixStats::weightedMean(value, weight),
                             sigma = matrixStats::weightedSd(value, weight),
                             med = matrixStats::weightedMedian(value, weight),
                             wtmad = matrixStats::weightedMad(value, weight)), .(time, coicop)],
                     by = c("time", "coicop"))[, .(time, coicop, mu, zscore = (value-mu)/sigma,
                                                   robzscore = (value-med)/wtmad)]
             }), idcol = "eustate")
             temp2 <- merge(temp2, COICOPData, by = "coicop")
             temp2$COICOPname <- factor(temp2$COICOPname,
                                        levels = unique(temp2$COICOPname[order(temp2$coicop)]))
             
             switch (input$relmethod,
                     "zscore" = {
                       ggplot2::ggplot() +
                         ggplot2::geom_line(data = temp2,
                                            ggplot2::aes(x = time, y = zscore, group = eustate, color = eustate)) +
                         {if(input$robust)  ggplot2::geom_line(data = temp2,
                                                               ggplot2::aes(x = time, y = robzscore, group = eustate,
                                                                            color = eustate), linetype = "dashed")} +
                         ggplot2::facet_wrap(~COICOPname, scales = "free") +
                         ggplot2::geom_hline(yintercept = 0, color = "red") +
                         ggplot2::labs(y = "z-score", x = "", tag = "Ferenci Tamás, medstat.hu") +
                         ggplot2::theme(plot.tag.position = c(0.15, 0), legend.position = "bottom",
                                        legend.title = ggplot2::element_blank())
                     },
                     "meandiff" = {
                       temp2 <- merge(temp, temp2[eustate==input$countries], by = c("time", "coicop"))[
                         geo%in%eucountries[[input$countries]]]
                       
                       ggplot2::ggplot() +
                         ggplot2::geom_line(data = temp2[geo!="Magyarország"],
                                            ggplot2::aes(x = time, y = value-mu, group = geo), color = "gray",
                                            linewidth = 0.2) +
                         ggplot2::geom_line(data = temp2[geo=="Magyarország"],
                                            ggplot2::aes(x = time, y = value-mu), color = "#2f7ed8", linewidth = 1) +
                         ggplot2::facet_wrap(~COICOPname, scales = "free",
                                             labeller = ggplot2::label_wrap_gen(width = 36)) +
                         ggplot2::geom_hline(yintercept = 0, color = "red") +
                         ggplot2::labs(y = paste0(switch(input$metric, "annual" = "Éves", "monthly" = "Havi"),
                                                  " infláció eltérése a viszonyítási országok átlagától [%]"),
                                       x = "", tag = "Ferenci Tamás, medstat.hu") +
                         ggplot2::theme(plot.tag.position = c(0.15, 0))
                     }
             )
           })
  })
}

shinyApp(ui = ui, server = server)