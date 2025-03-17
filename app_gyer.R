library(shiny)
library(tidyverse)

# Adatok betöltése

sGYER <- read.csv2("EUSILCH20052023legszukebbGYER.csv")
medianok <- read.csv2("medianok.csv")

ui <- fluidPage(
  tags$h4('A gyermekes háztartások jövedelemeloszlása'),
  
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
  HX090_hist_GYER <- function(ev){
    kuszob <- medianok$AROP60tresh[medianok$y == ev]
    teteje <- medianok$also90[medianok$y == ev]
    #Hisztogram elmentése, hogy lehessen szerkeszteni
    hist <- hist(sGYER$HX090[sGYER$y == ev & sGYER$HX090 < teteje[1] & sGYER$HX090 > -500],
                 plot = FALSE,
                 breaks = seq(-500,
                              round(teteje[1] / 100 + 1) * 100,
                              100))
    # Egyedi színezési tartományok év szerint
    highlight_ranges <- list(
      "2017" = list(c(-500,0)),
      "2018" = list(c(3200,3300)),
      "2019" = list(c(3500,3700)),
      "2020" = list(c(3800,3900)),
      "2021" = list(c(4000,4100)),                  
      "2022" = list(c(0,1)),
      "2023" = list(c(0,1)))
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
         xlab = "Egy fogyasztási egységre jutó jövedelem euróban, 100 eurós sávokban",
         ylab = "Esetek száma a 100 eurós sávban")
    abline(v = kuszob[1], col = "#377eb8", lwd=2)
    text(x = kuszob[1], 
         y = max(hist$counts) * 0.925, 
         labels = paste("küszöbérték", round(kuszob[1]),"€"), 
         pos = 2, col = "#377eb8")
    # normálgörbe rárajzolása
    mu <- median(sGYER$HX090[sGYER$y == ev & sGYER$HX090 < teteje[1] & sGYER$HX090 > -500],na.rm=T) # medián legyen a várható érték
    sigma <- sigma <- sd(sGYER$HX090[sGYER$y == ev & sGYER$HX090 < teteje[1] & sGYER$HX090 > -500],na.rm=T) #szórás
    x_vals <- seq(-500, teteje[1], length.out = 100) # számsorozat a vonal generálásához
    y_vals <- dnorm(x_vals, mean = mu, sd = sigma) #normális eloszlás sűrűségfüggvénye
    y_vals <- y_vals * quantile(hist$counts,probs=seq(0,1,0.05))["95%"]  / max(y_vals) # felszorzott ávltozat picit emelve
    
    
    lines(x_vals, y_vals, col = "darkgreen", lwd = 2)
    
  }
  
  
  
  output$HX090Hist <- renderPlot({
    HX090_hist_GYER(ev = input$ev)
  })
  
  szovegek <- data.frame(ev= 2005:2023, szoveg=character(19),stringsAsFactors=F)
  szovegek$szoveg[szovegek$ev %in% 2005:2016] <- 
    "2005-2016 között az eloszlás a jövedelemeloszlásra általában jellemző mintázatot mutat."
  szovegek$szoveg[szovegek$ev==2017] <- 
    "2017-ben látványosan csökkent az eloszlás 2400 euró alatti részében a megfigyelések 
  száma. Ezzel párhuzamosan kisebb torlódás van közvetlenül a szegénységi küszöb alatt. 
  Ettől eltekintve az eloszlás szabályos, a jövedelemeloszlás klasszikus formájához 
  hasonló formát mutat."
  szovegek$szoveg[szovegek$ev==2018] <- 
    "2018-ban a vártnál legalább kétszer több gyermekkel élő személyt látunk közvetlenül 
  a szegénységi küszöb értékét is tartalmazó 100 eurós sávban. Ettől eltekintve az eloszlás
  szabályos, a jövedelemeloszlás klasszikus formájához hasonló formát mutat."
  szovegek$szoveg[szovegek$ev==2019] <- 
    "2019-ben a szegénységi küszöb fölötti 200 eurós sávba tartozók száma 
  lényegesen nagyobb a vártnál. Ezzel párhuzamosan közvetlenül a küszöb alatti 
  terület üresebb a vártnál. A zöld vonal a jövedelemeloszlás klasszikus formáját 
  vetíti a 2019 évi adatokra, megjelenítve ezzel a 2005-2016 közötti időszak 
  alapján várható mintázatot."
  szovegek$szoveg[szovegek$ev==2020] <- 
    "2020-ban közvetlenül a szegénységi küszöb alatt olyan jelentős jövedelemcsoportosulást 
  látunk, ami sem általában a jövedelemeloszláshoz kapcsolódó okkal, sem társadalmi 
  folyamatokkal, sem adminisztratív intézkedésekkel nem magyarázható. Ez az oszlop 
  egyben az eloszlás legnagyobb gyakoriságot mutató tartománya is, ami nem jellemző 
  a jövedelemeloszlásra. A zöld vonal a jövedelemeloszlás klasszikus formáját vetíti 
  a 2020 évi adatokra."
  szovegek$szoveg[szovegek$ev==2021] <- 
    "2021-ben közvetlenül a szegénységi küszöb felett olyan jelentős jövedelemcsoportosulást
  látunk, ami sem általában a jövedelemeloszláshoz kapcsolódó okkal, sem társadalmi 
  folyamatokkal, sem adminisztratív intézkedésekkel nem magyarázható. Ez az oszlop 
  egyben az eloszlás legnagyobb gyakoriságot mutató tartománya is, ami nem jellemző 
  a jövedelemeloszlásra. A zöld vonal a jövedelemeloszlás klasszikus formáját vetíti
  a 2021 évi adatokra."
  szovegek$szoveg[szovegek$ev==2022] <- 
    "2022-ben közvetlenül a szegénységi küszöb feletti tartományban alacsonyabb a 
  gyakoriság az elvártnál. A zöld vonal a jövedelemeloszlás klasszikus formáját 
  vetíti a 2022 évi adatokra, megjelenítve ezzel a 2005-2016 közötti időszak alapján 
  várható mintázatot."
  szovegek$szoveg[szovegek$ev==2023] <- 
    "2023-ban a szegényégi küszöb alatti eloszlásrészbe tartozók többsége az 1800 euró 
  alatti sávban   tömörül, Ez példa nélküli a korábbi évek történetben és ellentétes a 
  jövedelemeloszlás klasszikus mintázatával, amit zöld vonal jelenít meg az ábrán."
  
  output$szoveg <- renderUI({
    p(strong(szovegek$szoveg[szovegek$ev == input$ev]))
  })
}

shinyApp(ui = ui, server = server)
