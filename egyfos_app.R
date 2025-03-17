library(shiny)
library(tidyverse)

# Adatok betöltése

sEGYFO <- read.csv2("EUSILCH20052023legszukebbEGYFO.csv")
medianok <- read.csv2("medianok.csv")

ui <- fluidPage(
  tags$h4('Az egyfős háztartások jövedelemeloszlása'),
  
  fluidRow(
    column(1, 
           selectInput(inputId = "ev",
                       label = "Választott év:",
                       choices = 2005:2023,
                       selected = 2020)
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
  HX090_hist_minimalber_egyfos_teljeseloszlas <- function(ev){
    kuszob <- medianok$AROP60tresh[medianok$y == ev]
    minimalber <- medianok$minimalber[medianok$y == ev]
    teteje <- medianok$also90[medianok$y == ev]
    #Hisztogram elmentése, hogy lehessen szerkeszteni
    hist <- hist(sEGYFO$HX090[sEGYFO$y == ev & sEGYFO$HX090 < teteje[1] & sEGYFO$HX090 > -500],
                 plot = FALSE,
                 breaks = seq(-500,
                              round(teteje[1] / 100 + 1) * 100,
                              100))
    # Egyedi színezési tartományok év szerint
    highlight_ranges <- list(
      "2017" = list(c(-500,0)),
      "2018" = list(c(0, 1)),
      "2019" = list(c(0,1)),
      "2020" = list(c(3800,4000)),
      "2021" = list(c(0,1)),                  
      "2022" = list(c(0,1)),
      "2023" = list(c(4400,4500),c(5800,6000)))
    # Alapértelmezett színezés: minden szürke
    colors <- rep("#999999", length(hist$mids))
    # Szegénységi küszöb alatti oszlopok kékre állítása
    colors[hist$mids < kuszob[1]] <- "#984ea3"
    # Kiemelt tartományok pirosra állítása
    ranges <- highlight_ranges[[as.character(ev)]]
    if (!is.null(ranges)) {
      for (range in ranges) {
        colors[hist$mids >= range[1] & hist$mids <= range[2]] <- "#ca0020"
      }
    }
    #Ábrázolás
    plot(hist, col = colors, main = NULL,
         xlab = "Éves nettó összjövedelem euróban, 100 eurós sávokban",
         yla="Esetek száma a 100 eurós sávban")
    abline(v = kuszob[1], col = "#377eb8", lwd=2)
    text(x = ifelse(minimalber[1]>-5000,minimalber[1],kuszob[1]),
         y = max(hist$counts) * 0.75, 
         labels = paste("küszöbérték", round(kuszob[1]),"€"), 
         pos = 2, col = "#377eb8",cex=0.8)
    abline(v= minimalber[1],col="darkgreen",lty=2)
    # Minimálbér felirat csak akkor, ha a minimálbér nagyobb, mint -5000
    if (minimalber[1] > -5000) {
      text(x = minimalber[1],
           y = max(hist$counts) * 0.7,
           labels = paste("minimálbér", round(minimalber[1]), "€"),
           pos = 2, col = "darkgreen", cex = 0.8)
    }
    # normálgörbe rárajzolása
    mu <- median(sEGYFO$HX090[sEGYFO$y == ev & sEGYFO$HX090 < teteje[1] & sEGYFO$HX090 > -500],na.rm=T) # medián legyen a várható érték
    sigma <- sigma <- sd(sEGYFO$HX090[sEGYFO$y == ev & sEGYFO$HX090 < teteje[1] & sEGYFO$HX090 > -500],na.rm=T) #szórás
    x_vals <- seq(0, teteje[1], length.out = 100) # számsorozat a vonal generálásához
    y_vals <- dnorm(x_vals, mean = mu, sd = sigma) #normális eloszlás sűrűségfüggvénye
    y_vals <- y_vals * quantile(hist$counts,probs=seq(0,1,0.05))["95%"]  / max(y_vals) # felszorzott ávltozat picit emelve
    lines(x_vals, y_vals, col = "darkgreen", lwd = 2)
  }
  
  
  output$HX090Hist <- renderPlot({
    HX090_hist_minimalber_egyfos_teljeseloszlas(ev = input$ev)
  })
  
  szovegek <- data.frame(ev= 2005:2023, szoveg=character(19),stringsAsFactors=F)
  szovegek$szoveg[szovegek$ev %in% 2005:2016] <- 
    "2005-2016 között az eloszlás a jövedelemeloszlásra általában jellemző, 
  klasszikus mintázatot mutat."
  szovegek$szoveg[szovegek$ev==2017] <- 
    "2017-ben jelentős a negatív jövedelműek száma, vagyis azoké, akik több adót 
  fizettek be, mint amennyi jövedelmük volt. Negatív jövedelmek mindig és mindenhol 
  előforul(hat)nak, de ez a magas szám példa nélküli a magyarországi adatok történetében. 
  Ettől eltekintve az eloszlás szabályos, a jövedelemeloszlásra általában jellemző formát 
  mutat."
  szovegek$szoveg[szovegek$ev %in% 2018:2019] <- 
    "2018-ban és 2019-ben a 2005-2016 közötti időszakhoz hasonlóan az eloszlás a jövedelemeloszlásra 
  általában jellemző, klasszikus formát mutat."
  szovegek$szoveg[szovegek$ev==2020] <- 
    "2020-ban közvetlenül a szegénységi küszöb fölött olyan jelentős jövedelemcsoportosulást 
  látunk, ami sem általában a jövedelemeloszláshoz kapcsolódó okkal, sem társadalmi 
  folyamatokkal, sem adminisztratív intézkedésekkel nem magyarázható. Ez az oszlop 
  egyben az eloszlás leggyakrabban előforduló értéke is, ami nem jellemző a 
  jövedelemeloszlásra. A zöld vonal a jövedelemeloszlás klasszikus formáját 
  vetíti a 2020 évi adatokra, megjelenítve ezzel a 2005-2016 közötti időszak 
  alapján várható mintázatot. "
  szovegek$szoveg[szovegek$ev==2021] <- 
    "2021-ben a szegénységi küszöb alatt van kisebb torlódás, ezzel párhuzamosan a 
  3300-3500 euró közötti sáv gyakorlatilag üres. A zöld vonal a jövedelemeloszlás 
  klasszikus formáját vetíti a 2021 évi adatokra, megjelenítve ezzel a 
  2005-2016 közötti időszak alapján várható mintázatot."
  szovegek$szoveg[szovegek$ev==2022] <- 
    "2022-ben a minimálbér alatti 400 eurós sáv gyakorlatilag üres. 
  A zöld vonala jövedelemeloszlás klasszikus formáját vetíti a 2022 évi adatokra, 
  megjelenítve ezzel a 2005-2016 közötti időszak alapján várható mintázatot."
  szovegek$szoveg[szovegek$ev==2023] <- 
    "2023-ban két kiugrást is láthatunk a jövedelemadatokban, egyet közvetlenül a 
  szegénységi küszöb felett (4400 eurónál), egy másikat pedig 5800-6000 euró között. 
  Emellett a teljes 2000 euró alatti sáv gyakorlatilag üres, mintha nem lennének olyan 
  egyfős háztartások Magyarországon, ahol a jövedelem nem éri el a nettó évi 2000 eurót. 
  A 2020-2022 közötti időszak éveiben az egyfős háztartások 4-8%-a esett ebbe a jövedelmi
  sávba. A zöld vonal a jövedelemeloszlás klasszikus formáját vetíti a 2023 évi adatokra, 
  megjelenítve ezzel a 2005-2016 közötti időszak alapján várható mintázatot."

  output$szoveg <- renderUI({
    p(strong(szovegek$szoveg[szovegek$ev == input$ev]))
  })
}

shinyApp(ui = ui, server = server)
