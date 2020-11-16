## app.R ##
library(shiny)
library(shinydashboard)

## ui.R ##
ui <- dashboardPage(dashboardHeader(title = "YourInterest"),
                    dashboardSidebar(
                      sidebarMenu(
                        id = "tabs",
                        menuItem("Home", tabName = "Home", icon = icon("home")),
                        
                        menuItem("Deine Eingaben", tabName = "Input", icon = icon("pen")),
                        
                        menuItem("Dein Ergebnis", tabName = "DeinErgebnis", icon = icon("check-circle")),
                        
                        menuItem("Komponenten",icon = icon("puzzle-piece"),startExpanded = TRUE, 
                                 
                                 menuSubItem("Eigenkapitalkosten", tabName = "Eigenkapitalkosten"),
                                 
                                 menuSubItem("Fremdkapitalkosten", tabName = "Fremdkapitalkosten"),
                                 
                                 menuSubItem("Ausfallkosten", tabName = "Ausfallkosten"),
                                 
                                 menuSubItem("Betriebskosten", tabName = "Betriebskosten"))
                        
                      )),
                    
                    dashboardBody(
                      tabItems(
                        
                        #Tab Home
                        tabItem(tabName = "Home",
                                
                                h1("Herzlich Willkommen bei YourInterest !", align= "center", style = "font-size:500%"),
                                
                                h2("Dein Partner bei der Bestimmung Deiner", tags$strong("Intercompany"),"Darlehenszinsen.", align = "center",style = "font-size:300%"),
                                
                                img(height = 350,
                                    width = 400,
                                    src ="Logo.png", 
                                    style="display: block;margin: 0px auto;"),
                                
                                
                                h3("Du brauchst Hilfe bei der Bestimmung Deiner", tags$strong("Intercompany"),"Darlehenszinsen?", align = "center", style = "font-size:230%"),
                                
                                tags$div(tags$p()),
                                
                                actionButton("startbutton", "Los gehts!",alt="",style="display: block;margin: 0px auto;height: 100px; width: 200px; font-size:180%")
                        ),
                        
                        #Tab Input
                        tabItem(
                          tabName = "Input",
                                
                                h1(tags$strong("Bitte mache hier deine Eingaben:"), align= "center", style = "font-size:300%"),
                                
                                fluidRow(
                                  box(
                                    title = "Kreditvolumen ",
                                    numericInput("kv", "Wie hoch ist der Kredit? ", 0.0, min = 0, max = 10000000000, )),
                                  
                                  box(
                                    title = "Laufzeit des Darlehens ",
                                    numericInput("dl", "Wie viele Jahre beträgt die Laufzeit? ", 0.0, min = 0, max = 10000000))),
                                
                                fluidRow(
                                  box(
                                    title = "Eigenkapitalkostensatz ",
                                    numericInput("eks", "Wie hoch ist der Eigenkapitalkostensatz? (Bitte Dezimalzahl eingeben!) ", 0.0, min = 0, max = 1000)),
                                  
                                  box(
                                    title = "Rating",
                                    selectInput("r", label = "Welches Rating liegt vor? ", choices = c("AAA", "AA+", "AA","AA-","A+","A","A-","BBB+","BBB",
                                                                                                       "BBB-","BB+", "BB", "BB-", "B+", "B", "B-","CCC+",
                                                                                                       "CCC", "CCC-", "CC","C", "D"), selectize = TRUE))),
                                
                                
                                fluidRow(box(
                                  title = "Betriebskosten ",
                                  numericInput("bk", "Wie hoch sind die Betriebskosten? (Bitte Dezimalzahl eingeben!) ", 0.01, min = 0, max = 1000))),
                                
                                
                                tags$div(tags$p()),
                                
                                fluidRow(
                                  h3(tags$strong("Bist du fertig mit deiner Eingabe? Dann geht es hier zu deinem Ergebnis!"), align = "center", style = "font-size:150%")),
                                
                                tags$div(tags$p()),
                                
                                actionButton("ergebnisbutton", "Dein Ergebnis!",alt="", 
                                             style="display: block;margin: 0px auto;height: 85px; width: 220px; font-size:120%")
                        ),
                        
                        #Tab Dein Ergebnis
                        tabItem(
                          tabName = "DeinErgebnis",
                          
                                fluidRow(
                                  column(width = 3),
                                  column(width = 4, h1(tags$strong("Dein Ergebnis"), align= "left", style = "font-size:300%"))
                                ),
                                
                                
                                fluidRow(
                                  (column(width = 1)),
                                  (column(width = 4, 
                                          img(height = 120,
                                              width = 900,
                                              src ="Gesamt.png")))
                                ),
                                
                                tags$div(tags$p()),
                                
                                fluidRow(valueBoxOutput("Ergebnis")),
                                
                                tags$div(tags$p()),
                                
                                h3(tags$strong("Setzt sich aus folgenden Komponenten zusammen: "), align = "left", style = "font-size:170%"),
                                
                                fluidRow(
                                  valueBoxOutput("Eigenkapitalkosten"),
                                  valueBoxOutput("Fremdkapitalkosten")
                                ),
                                
                                fluidRow(
                                  (column(width = 4, actionButton("ekbutton", "Mehr Infos!",alt="", 
                                                                  style="height: 30px; width: 80px; font-size:80%"))),
                                  (column(width = 4, actionButton("fkbutton", "Mehr Infos!",alt="", 
                                                                  style="height: 30px; width: 80px; font-size:80%")))
                                ),
                                
                                tags$div(tags$p()),
                                
                                fluidRow(
                                  
                                  valueBoxOutput("Ausfallkosten"),
                                  valueBoxOutput("Betriebskosten")
                                ),
                                
                                fluidRow(
                                  
                                  (column(width = 4, actionButton("akbutton", "Mehr Infos!",alt="", 
                                                                  style="height: 30px; width: 80px; font-size:80%"))),
                                  (column(width = 4, actionButton("bkbutton", "Mehr Infos!",alt="", 
                                                                  style="height: 30px; width: 80px; font-size:80%")))
                                )),
                        
                        #Tab Eigenkapitalkosten        
                        tabItem(
                          tabName = "Eigenkapitalkosten",
                          
                          fluidRow(
                            (column(width = 2)),
                            (column(width = 4, h1(tags$strong("Deine Eigenkapitalkosten"), align= "left", style = "font-size:300%")))
                          ),
                          
                          
                          tags$div(tags$p()),
                          
                          fluidRow(
                            (column(width = 1)),
                            (column(width = 4, 
                                    img(height = 120,
                                        width = 800,
                                        src ="Eigenkapital.png")))
                          ),
                          
                          tags$div(tags$p()),
                          
                          fluidRow(valueBoxOutput("Eigenkapitalkosten2")),
                          
                          tags$div(tags$p()),
                          
                          h3(tags$strong("Setzt sich aus folgenden Komponenten zusammen:"), align = "left", style = "font-size:170%"),
                          
                          tags$div(tags$p()),
                          
                          fluidRow(
                            
                            valueBoxOutput("Eigenkapitalquote"),
                            
                            valueBoxOutput("Eigenkapitalkostensatz")
                          ),
                          
                          tags$div(tags$p()),
                          
                          fluidRow(
                            column(width = 4, actionButton("zurueck1button", "<< Zurück zum Ergebnis",alt="", 
                                                           style="height: 30px; width: 300px; font-size:100%")),
                            column(width = 2),
                            column(width = 4, actionButton("weiter1button", "Weiter zu den Fremdkapitalkosten >>",alt="",align="left",
                                                           style="height: 30px; width: 300px; font-size:100%")))
                        ),
                        
                        
                        #Tab Fremdkapitalkosten
                        
                        tabItem(
                          tabName = "Fremdkapitalkosten",
                          
                          fluidRow(
                            (column(width = 2)),
                            (column(width = 4, h1(tags$strong("Deine Fremdkapitalkosten"), align= "left", style = "font-size:300%")))
                          ),
                          
                          tags$div(tags$p()),
                          
                          fluidRow(
                            (column(width = 1)),
                            (column(width = 4, 
                                    img(height = 100,
                                        width = 900,
                                        src ="Fremdkapital.png",alt="", 
                                        style="display: block;margin: 0px auto;")))),
                          
                          tags$div(tags$p()),
                          
                          fluidRow(valueBoxOutput("Fremdkapitalkosten2")),
                          
                          tags$div(tags$p()),
                          
                          h3(tags$strong("Setzt sich aus folgenden Komponenten zusammen:"), align = "left", style = "font-size:170%"),
                          
                          tags$div(tags$p()),
                          
                          fluidRow(
                            
                            valueBoxOutput("Fremdkapitalquote"),
                            
                            valueBoxOutput("Fremdkapitalkostensatz")
                          ),
                          
                          tags$div(tags$p()),
                          
                          
                          fluidRow(
                            column(width = 4,
                                   actionButton("zurueck2button", "<<Zurück zu den Eigenkapitalkosten",alt="", 
                                                style="height: 30px; width: 300px; font-size:100%")),
                            column(width = 2),
                            column(width = 4,
                                   actionButton("weiter2button", "Weiter zu den Ausfallkosten>>",alt="", align="left",
                                                style="height: 30px; width: 300px; font-size:100%")))),
                        
                        # Ausfallkosten
                        tabItem(
                          tabName = "Ausfallkosten",
                          fluidRow(
                            (column(width = 2)),
                            (column(width = 4, h1(tags$strong("Deine Ausfallkosten"), align= "left", style = "font-size:300%")))),
                          
                          tags$div(tags$p()),
                          
                          fluidRow(
                            (column(width = 1)),
                            (column(width = 4,img(height = 120,
                                                  width = 800,
                                                  src ="Ausfallkosten.png",alt="", 
                                                  style="display: block;margin: 0px auto;")))),
                          
                          tags$div(tags$p()),
                          
                          fluidRow(valueBoxOutput("Ausfallkosten2")),
                          
                          tags$div(tags$p()),
                          
                          h3(tags$strong("Setzt sich aus folgenden Komponenten zusammen:"), align = "left", style = "font-size:150%"),
                          
                          tags$div(tags$p()),
                          
                          fluidRow(
                            
                            valueBoxOutput("PD"),
                            
                            valueBoxOutput("LGD")
                         
                          ),
                          
                          tags$div(tags$p()),
                          
                          fluidRow(
                            column(width = 4,
                                   actionButton("zurueck3button", "<<Zurück zu den Fremdkapitalkosten",alt="",
                                                style="height: 30px; width: 300px; font-size:100%")),
                            column(width = 2),
                            column(width = 4,
                                   actionButton("weiter3button", "Weiter zu den Betriebskosten>>",alt="",
                                                style="height: 30px; width: 300px; font-size:100%")))),
                        
                        #Betriebskosten
                        tabItem(
                          tabName = "Betriebskosten",
                          fluidRow(
                            (column(width = 2)),
                            (column(width = 4, h1(tags$strong("Deine Betriebskosten"), align= "left", style = "font-size:300%")))),
                          
                          tags$div(tags$p()),
                          
                          fluidRow(
                            (column(width = 1)),
                            (column(width = 4,img(height = 120,
                                                  width = 800,
                                                  src ="Betriebskosten.png",alt="", 
                                                  style="display: block;margin: 0px auto;")))),
                          
                          tags$div(tags$p()),
                          
                          fluidRow(valueBoxOutput("Betriebskosten2")),
                          
                          tags$div(tags$p()),
                          
                          
                          tags$div(tags$p()),
                          fluidRow(
                            
                            column(width = 4,
                                   actionButton("zurueck4button", "<<Zurück zu den Ausfallkosten",alt="",
                                                style="height: 30px; width: 300px; font-size:100%")),
                            column(width = 2),
                            column(width = 4,
                                   actionButton("weiter4button", "Weiter zum Ergebnis>>",alt="", align = "left",
                                                style="height: 30px; width: 300px; font-size:100%"))))
                        
                      )))


server <- function(input, output, session) {
  
  ##Definition der Variablen
  
  #Definition der Inputvariablen
  
  num_KV      <-   reactive({input$kv})
  num_DL      <-   reactive({input$dl})
  num_R       <-   reactive({input$r})
  num_EKS_1   <-   reactive({input$eks})
  num_BK_1    <-   reactive({input$bk})
  
  
  ##Definition der fixen Variablen
  
  num_SolVK   <- reactive({0.08})
  num_LGD_1   <- reactive({0.50})
  
  
  #Formatierung Loss Given Default(LGD)
  
  num_LGD_2   <- reactive({num_LGD_1() * 100})
  
  
  #Formatierung der Betriebskosten
  
  num_BK_2    <- reactive({num_BK_1() * 100})
  
  #Formatierung Eigenkapitalkostensatz (EKS)
  
  num_EKS_2   <- reactive({num_EKS_1() * 100})
  
  
  ##Tabellen einlesen fuer die Variablen aus den Tabellen PD, Risikogewicht, Risikozins
  
  #Tabelle einlesen "PD"
  
  data_pd_1 <- read.csv("PD.csv")
  data_pd_2 <- data.frame(do.call("rbind", strsplit(as.character(data_pd_1$x), ";", fixed = TRUE)))
  
  #Auslesen der CSV-Datei "PD"
  
  num_pd_var_1 <- reactive(  for (i in data_pd_2[,1]){
    
    
    if (i == num_R()) {
      
      data_i <- subset(data_pd_2, X1 == i)
      
      rv <- as.factor(data_i[1,2])
      
      droplevels(rv)
      
      rv_u <- as.numeric(levels(rv))[rv]
      
      return(rv_u)
      
    }})
  
  
  num_pd_var_2 <- reactive({num_pd_var_1() * 100})
  
  num_pd_var_3 <- reactive({round(num_pd_var_2(), digits = 4)})
  
  
  #Es kann nun mit "num_pd_var" als Variable gerechnet werden
  
  #Tabelle einlesen "Risikogewicht"
  
  
  data_rg_1 <- read.csv("Risikogewicht.csv")
  data_rg_2 <- data.frame(do.call("rbind", strsplit(as.character(data_rg_1$x), ";", fixed = TRUE)))
  
  
  #Auslesen der CSV-Datei "Risikogewicht"
  
  
  num_rg_var <- reactive(  for (i in data_rg_2[,1]){
    
    
    if (i == num_R()) {
      
      data_j <- subset(data_rg_2, X1 == i)
      
      rvv <- as.factor(data_j[1,2])
      
      droplevels(rvv)
      
      rv_uu <- as.numeric(levels(rvv))[rvv]
      
      return(rv_uu)
      
    }})
  
  #Es kann nun mit "num_rg_var" gerechnet werden
  
  #Tabelle einlesen "Risikozins"
  
  data_rz_1 <- read.csv("Risikozins.csv")
  data_rz_2 <- data.frame(do.call("rbind", strsplit(as.character(data_rz_1$x), ";", fixed = TRUE)))
  
  
  #Auslesen der CSV-Datei "Risikozins"
  
  num_rz_var_1 <- reactive( for (i in data_rz_2[,1]){
    
    if (i == num_DL()) {
      
      data_k <- subset(data_rz_2, X1 == i)
      
      rvvv <- as.factor(data_k[1,2])
      
      droplevels(rvvv)
      
      rv_uuu <- as.numeric(levels(rvvv))[rvvv]
      
      return(rv_uuu)
      
    }})
  
  num_rz_var_2 <- reactive({num_rz_var_1() * 100})
  
  #Es kann nun mit "num_rz_var" gerechnet werden
  
  ##Prozess zur Berechnung der Eigenkapitalkosten - Dashboard ("Dein Ergebnis/Eigenkapitalkosten")
  
  #Berechnung des Eigenkapitals fuer Kreditrisiken (EK1)
  
  num_EK_1 <- reactive(num_KV() * num_rg_var() * num_SolVK())
  
  
  #Berechnung der Eigenkapitalquote (EKQ)
  
  num_EKQ_1 <- reactive({num_EK_1()/num_KV()})
  
  #Formatierung der Eigenkapitalquote(EKQ)
  
  num_EKQ_2 <- reactive({num_EKQ_1() * 100})
  
  num_EKQ_3 <- reactive({round(num_EKQ_2(), digits = 2)})
  
  
  #Berechnung der Eigenkapitalkosten (EKK)
  
  num_EKK_1 <- reactive({num_EKQ_1() * num_EKS_1()})
  
  #Berechnung der Eigenkapitalkosten (EKK)
  
  num_EKK_2 <- reactive({num_EKK_1() * 100})
  
  num_EKK_3 <- reactive({round(num_EKK_2(), digits = 2)})
  
  
  ##Prozess zur Berechnung der Fremdkapitalkosten - Dashboard("Dein Ergebnis/Fremdkapitalkosten")
  
  #Berechnung der Fremdkapitalquote (FKQ)
  
  num_FKQ_1 <- reactive({1 - num_EKQ_1()})
  
  num_FKQ_2 <- reactive({num_FKQ_1() * 100})
  
  
  #Berechnung der Fremdkapitalkosten (FKK)
  
  num_FKK_1 <- reactive({num_FKQ_1() * num_rz_var_1()})
  
  num_FKK_2 <- reactive({num_FKK_1() * 100})
  
  num_FKK_3 <- reactive({round(num_FKK_2(), digits = 2)})
  
  
  ##Prozess zur Berechnung der Ausfallkosten - Dashboard("Dein Ergebnis/Ausfallkosten")
  
  
  #Ausfallkosten 1
  
  num_AK_1 <- reactive({  
    
    AK_FOR_1 <- 0
    
    for (i in 1:num_DL()){
      
      AK_FOR_1 <- AK_FOR_1 + (1-num_pd_var_1())**(i-1)*num_pd_var_1()*(1-num_LGD_1())}
    
    return(AK_FOR_1)})
  
  
  #Ausfallkosten 2
  
  num_AK_2 <- reactive({
    
    AK_FOR_2 <- 0
    
    for (i in 1:num_DL()){
      
      AK_FOR_2 <- AK_FOR_2 + (1+num_rz_var_1())**i}
    
    return(AK_FOR_2)})
  
  
  #Ausfallkosten 3
  
  num_AK_3 <- reactive({
    
    AK_FOR_3 <- 0
    
    for(i in 1:num_DL()){
      
      AK_FOR_3 <- AK_FOR_3 + ((1-num_pd_var_1())**(i-1)*num_pd_var_1()*(1-num_LGD_1()))/((1+num_rz_var_1())**i)}
    
    return(AK_FOR_3)})
  
  #Berechnung der Ausfallkosten 4
  
  
  num_AK_4 <- reactive({(1-num_pd_var_1())**num_DL()})
  
  
  #Berechnung der Ausfallkosten 5
  
  
  num_AK_5 <- reactive({(1 + num_rz_var_1())**num_DL()})  
  
  
  #Berechnunmg der Ausfallkosten 6
  
  num_AK_6 <- reactive({num_AK_4()/num_AK_5()})
  
  
  #Berechnung der Ausfallkosten 7
  
  num_AK_7 <- reactive({
    
    AK_FOR_4 <- 0
    
    for(i in 1:num_DL()){
      
      AK_FOR_4 <- AK_FOR_4 + (((1-num_pd_var_1())**i+(1-num_pd_var_1())**(i-1)*num_pd_var_1()*((1-num_LGD_1())))/(1+num_rz_var_1())^i)}
    
    return(AK_FOR_4)})
  
  
  #Berechnung der Ausfallkosten 8
  
  
  num_AK_8 <- reactive({(1-num_AK_3()-num_AK_6())/num_AK_7()})
  
  #Berechnung der Ausfallkosten 9
  
  num_AK_9.1 <- reactive({num_AK_8() - num_rz_var_1()})
  
  num_AK_9.2 <- reactive({num_AK_9.1() * 100})
  
  num_AK_9.3 <- reactive({round(num_AK_9.2(), digits = 2)})
  
  
  #Berechnung des Endergebnis
  
  num_Z_1 <- reactive({num_EKK_1() + num_FKK_1() + num_AK_9.1() + num_BK_1()})
  
  num_Z_2 <- reactive({num_Z_1() * 100})
  
  num_Z_3 <- reactive({round(num_Z_2(), digits = 2)})
  
  ##Erstellung der Buttons fuer die einzelnen Dashboards
  
  #Erstellen des Startbuttons
  
  observeEvent(input$startbutton, {
    
    updateTabItems(session,"tabs",selected = "Input") })
  
  #Erstellen des Ergebnisbuttons
  
  observeEvent(input$ergebnisbutton, {
    
    updateTabItems(session,"tabs",selected = "DeinErgebnis") })
  
  #Erstellen der WeitereInfobuttons
  
  observeEvent(input$ekbutton, {
    
    updateTabItems(session,"tabs",selected = "Eigenkapitalkosten") })
  
  observeEvent(input$fkbutton, {
    
    updateTabItems(session,"tabs",selected = "Fremdkapitalkosten") })
  
  observeEvent(input$akbutton, {
    
    updateTabItems(session,"tabs",selected = "Ausfallkosten") })
  
  observeEvent(input$bkbutton, {
    
    updateTabItems(session,"tabs",selected = "Betriebskosten") })
  
  #Erstellen der Weiterbuttons
  observeEvent(input$weiter1button, {
    
    updateTabItems(session,"tabs",selected = "Fremdkapitalkosten") })
  
  observeEvent(input$weiter2button, {
    
    updateTabItems(session,"tabs",selected = "Ausfallkosten") })
  
  observeEvent(input$weiter3button, {
    
    updateTabItems(session,"tabs",selected = "Betriebskosten") })
  
  observeEvent(input$weiter4button, {
    
    updateTabItems(session,"tabs",selected = "DeinErgebnis") })
  
  #Erstellen der Zurueckbuttons
  observeEvent(input$zurueck1button, {
    
    updateTabItems(session,"tabs",selected = "DeinErgebnis") })
  
  observeEvent(input$zurueck2button, {
    
    updateTabItems(session,"tabs",selected = "Eigenkapitalkosten") })
  
  observeEvent(input$zurueck3button, {
    
    updateTabItems(session,"tabs",selected = "Fremdkapitalkosten") })
  
  observeEvent(input$zurueck4button, {
    
    updateTabItems(session,"tabs",selected = "Ausfallkosten") })
  
  
  
  ##Erstellen der Outputboxen 
  
  # Ergebnis
  output$Ergebnis <- renderValueBox({valueBox(paste(num_Z_3(),"%"), "Deine fremdueblichen Zinsen", icon = icon("check-circle"),
                                              color = "black")})
  
  output$Eigenkapitalkosten <- renderValueBox({valueBox(paste(num_EKK_3(),"%"), "Eigenkapitalkosten", icon = icon("puzzle-piece"),
                                                        color = "blue")})
  
  output$Fremdkapitalkosten <- renderValueBox({valueBox(paste(num_FKK_3(),"%"), "Fremdkapitalkosten", icon = icon("puzzle-piece"),
                                                        color = "lime")})
  
  output$Ausfallkosten <- renderValueBox({valueBox(paste(num_AK_9.3(),"%"), "Ausfallkosten", icon = icon("puzzle-piece"),
                                                   color = "navy")})
  
  output$Betriebskosten <- renderValueBox({valueBox(paste(num_BK_2(), "%"), "Betriebskosten", icon = icon("puzzle-piece"),
                                                    color = "olive")})
  # Eigenkapitalkosten
  output$Eigenkapitalkosten2 <- renderValueBox({valueBox(paste(num_EKK_3(),"%"), "Eigenkapitalkosten", icon = icon("puzzle-piece"),
                                                         color = "blue")})
  
  output$Eigenkapitalquote <- renderValueBox({valueBox(paste(num_EKQ_3(),"%"), "Eigenkapitalquote", icon = icon("puzzle-piece"),
                                                       color = "blue")})
  
  output$Eigenkapitalkostensatz <- renderValueBox({valueBox(paste(num_EKS_2(),"%"), "Eigenkapitalkostensatz", icon = icon("puzzle-piece"),
                                                            color = "blue")})
  # Fremdkapitalkosten
  output$Fremdkapitalkosten2 <- renderValueBox({valueBox(paste(num_FKK_3(),"%"), "Fremdkapitalkosten", icon = icon("puzzle-piece"),
                                                         color = "lime")})
  
  output$Fremdkapitalquote <- renderValueBox({valueBox(paste(num_FKQ_2(),"%"), "Fremdkapitalquote", icon = icon("puzzle-piece"),
                                                       color = "lime")})
  
  output$Fremdkapitalkostensatz <- renderValueBox({valueBox(paste(num_rz_var_2(),"%"), "Fremdkapitalkostensatz", icon = icon("puzzle-piece"),
                                                            color = "lime")})
  # Ausfallkosten
  output$Ausfallkosten2 <- renderValueBox({valueBox(paste(num_AK_9.3(),"%"), "Ausfallkosten", icon = icon("puzzle-piece"),
                                                    color = "navy")})
  
  output$PD <- renderValueBox({valueBox(paste(num_pd_var_3(), "%"), "Probability of Default", icon = icon("puzzle-piece"),
                                        color = "navy")})
  
  output$LGD <- renderValueBox({valueBox(paste(num_LGD_2(), "%"), "Loss Given Default", icon = icon("puzzle-piece"),
                                         color = "navy")})

  #Betriebskosten
  output$Betriebskosten2 <- renderValueBox({valueBox(paste(num_BK_2(), "%"), "Betriebskosten", icon = icon("puzzle-piece"),
                                                     color = "olive")})
}

shinyApp(ui, server)