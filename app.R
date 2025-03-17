library(shiny)
library(tidyverse)

# Adatok betöltése

s <- read.csv2("EUSILCH20052023legszukebb.csv")
medianok <- read.csv2("medianok.csv")

ui <- fluidPage(
  tags$h4('Az EU-SILC teljes magyarországi mintájának jövedelemadatai'),
  
  fluidRow(
    column(1, 
           selectInput(inputId = "ev",
                       label = "Választott év:",
                       choices = 2005:2023,
                       selected = 2021)
    ),
    column(11, 
           plotOutput("HX090Hist", width = "100%")
    )
  ),
  
  fluidRow(
    column(12, 
           uiOutput("szoveg")
    )
  ),
  
  tags$style(HTML("
    .selectize-input {
      margin-bottom: 5px;
      width: 80px; /* Állítsd be az ideális szélességet */
    }
  "))
)

server <- function(input, output){
  HX090_hist <- function(ev){
    kuszob <- medianok$AROP60tresh[medianok$y == ev]
    teteje <- medianok$also90[medianok$y == ev]
    
    hist <- hist(s$HX090[s$y == ev & s$HX090 < teteje[1] & s$HX090 > -500],
                 plot = FALSE,
                 breaks = seq(-500, round(teteje[1] / 100 + 1) * 100, 100)) 
    
    highlight_ranges <- list(
      "2017" = list(c(-500,0)),
      "2018" = list(c(3200, 3300)),
      "2019" = list(c(3500,3800)),
      "2020" = list(c(3800,4000)),
      "2021" = list(c(4000,4100)),                  
      "2022" = list(c(0,1)),
      "2023" = list(c(7800,7900),c(4400,4500))
    )
    
    colors <- rep("#999999", length(hist$mids))
    colors[hist$mids < kuszob[1]] <- "#984ea3"
    
    ranges <- highlight_ranges[[as.character(ev)]]
    if (!is.null(ranges)) {
      for (range in ranges) {
        colors[hist$mids >= range[1] & hist$mids <= range[2]] <- "#ca0020"
      }
    }
    
    plot(hist, col = colors, main=NULL,
         xlab = "Egy fogyasztási egységre jutó jövedelem, 100 eurós sávokban", 
         ylab="Esetek száma a 100 eurós sávban")
    
    abline(v = kuszob[1], col = "#377eb8", lwd=2)
    text(x = kuszob[1], 
         y = max(hist$counts) * 0.85, 
         labels = paste("szegénységi küszöb", round(kuszob[1]),"€"), 
         pos = 2, col = "#377eb8",cex=1.4)
    
    mu <- medianok$HX090median[medianok$y == ev]
    sigma <- sd(s$HX090[s$y == ev & s$HX090 < teteje & s$HX090 > -500])
    x_vals <- seq(0, teteje[1], length.out = 100)
    y_vals <- dnorm(x_vals, mean = mu, sd = sigma)
    y_vals <- y_vals * quantile(hist$counts,probs=seq(0,1,0.05))["95%"]  / max(y_vals)  
    lines(x_vals, y_vals, col = "darkgreen", lwd = 2)
  }
  
  output$HX090Hist <- renderPlot({
    HX090_hist(ev = input$ev)
  })
  
  szovegek <- data.frame(ev= 2005:2023, szoveg=character(19),stringsAsFactors=F)
  szovegek$szoveg[szovegek$ev %in% 2005:2016] <- 
    "2005-2016 között az eloszlás a jövedelemeloszlásra általában jellemző, 
  klasszikus mintázatot mutat."
  szovegek$szoveg[szovegek$ev==2017] <- 
    "2017-ben a minta 1%-ának negatív jövedelme volt, vagyis az adatok szerint több 
  adót fizettek be, mint amennyi jövedelmük volt. Negatív jövedelmek mindig és mindenhol 
  előforul(hat)nak, de az 1% körüli arány példa nélküli a magyarországi adatok történetében.
  Ettől eltekintve az eloszlás szabályos mintázatot mutat, megfelel a jövedelemeloszlás 
  klasszikus mintájának, amit a zöld vonal is jelez."
  szovegek$szoveg[szovegek$ev==2018] <- 
    "2018-ban a szegénységi küszöb értéke 3254 euró, közvetlenül ezen érték körül 
  kb. 600 fő jövedelemértéke csoportosul (ami közel kétszerese a következő 
  legmagasabb értéknek). Ez az oszlop egyben az eloszlás leggyakrabban előforduló 
  értéke, ami nem jellemző a jövedelemeloszlásra. A zöld vonal a jövedelemeloszlás 
  klasszikus formáját vetíti a 2018. évi adatokra, megjelenítve ezzel a 2005-2016 
  közötti időszak alapján várható mintázatot. "
  szovegek$szoveg[szovegek$ev==2019] <- 
    "2019-ben közvetlenül a szegénységi küszöb fölötti 300 eurós sávban a jóval 
  nagyobb a megfigyelések hányada, mint a 2016-ot megelőző években. Ez a 3600-3700 
  euró közötti sáv (középső piros oszlop) egyben az eloszlás leggyakrabban előforduló 
  értéke is, ami nem jellemző a jövedelemeloszlásra. A zöld vonal a jövedelemeloszlás 
  klasszikus formáját vetíti a 2019 évi adatokra, megjelenítve ezzel a 2005-2016 
  közötti időszak alapján várható mintázatot."
  szovegek$szoveg[szovegek$ev==2020] <- 
    "2020-ban a szegénységi küszöb környékén (alatta és felette is) jelentős a torlódás. 
  Ez az oszlop egyben az eloszlás leggyakrabban előforduló értéke is, ami nem jellemző a 
  jövedelemeloszlásra. A zöld vonal a jövedelemeloszlás klasszikus formáját vetíti a 
  2020 évi adatokra, megjelenítve ezzel a 2005-2016 közötti időszak alapján várható 
  mintázatot."
  szovegek$szoveg[szovegek$ev==2021] <- 
    "2021-ben a szegénységi küszöb felett jelentős a torlódás. A zöld vonal a 
  jövedelemeloszlás klasszikus formáját vetíti a 2021 évi adatokra, megjelenítve ezzel 
  a 2005-2016 közötti időszak alapján várható mintázatot."
  szovegek$szoveg[szovegek$ev==2022] <- 
    "2022-ben a szegénységi küszöb alatt van, a korábbiaknál kevésbé jelentős a torlódás. 
  A zöld vonal a jövedelemeloszlás klasszikus formáját vetíti a 2022 évi adatokra, 
  megjelenítve ezzel a 2005-2016 közötti időszak alapján várható mintázatot."
  szovegek$szoveg[szovegek$ev==2023] <- 
    "2023-ban kiugróan magas a legalacsonyabb jövedelmi sávokba tartozó személyek aránya. 
  Emellett a szegénységi küszöb értéke felett kicsivel, illetve 7800 eurónál látható 
  torlódás. A zöld vonal a jövedelemeloszlás klasszikus formáját vetíti a 2023 évi 
  adatokra, megjelenítve ezzel a 2005-2016 közötti időszak alapján várható mintázatot."

  output$szoveg <- renderUI({
    p(strong(szovegek$szoveg[szovegek$ev == input$ev]))
  })
}

shinyApp(ui = ui, server = server)
