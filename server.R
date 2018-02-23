library(ggplot2)
library(dplyr)

#library(rgdal)


#library(osrm)
#library(RCurl)

datimugello<-read.csv("Dati_Mugello.csv")
datimugello$Data_accettazione<-as.Date(datimugello$Data_accettazione,"%d/%m/%Y")

ospedaliU <-read.table("indirizzi_ospedali_ripuliti.csv", sep=";", 
                       quote="'", header=TRUE)

datimugello$sett<-as.factor(strftime(datimugello$Data_accettazione, format="%W"))
datimugello$Esito_slashn50<-as.factor(
    unlist(
      lapply(
        strwrap(datimugello$Esito,width=50,simplify = FALSE)
        ,paste
        ,collapse='\n'
      )
    ))

function(input,output,session){
  
  
  dataf <- reactive({
    m<-datimugello%>% 
      filter(Data_accettazione %in% seq(input$date_range[1], input$date_range[2],by="1 day") &
               DesComune %in% input$comuni &
               Capitolo_princ %in% input$cap)
    m<-as.data.frame(m)
    m
    })
  # definizione degli input
  output$comuniControl <- renderUI({
    comuni_choices <- levels(datimugello$DesComune)
    selectInput("Comuni", inputId = "comuni",
                choices = comuni_choices,
                selected = comuni_choices, multiple = TRUE)
  })
  
  output$capitoloControl <- renderUI({
    cap_choices<-levels(datimugello$Capitolo_princ)
    selectInput("Capitolo diagnosi principale", inputId = "cap",
              choices = cap_choices,
              selected = cap_choices, 
              multiple = TRUE)
  })
  
  ## Leggo le coordinate degli ospedali

  #Preparo le icone
  #icon_id<-makeIcon("marker_red.png", iconWidth = 20,iconHeight = 20)
  icon_osp<-makeIcon("marker_h.png", iconWidth = 25,
                   iconHeight = 25)
  
  ######## Rappresentazione   ############################
  
  output$map <- renderLeaflet(
    leaflet()
      %>% addTiles()
      
      %>% fitBounds(min(datimugello$LONGITUDINE), min(datimugello$LATITUDINE),max(datimugello$LONGITUDINE), max(datimugello$LATITUDINE))
      %>%addMarkers (data=ospedaliU, lng=~X.LONGITUDINE
                , lat=~X.LATITUDINE, icon=icon_osp)
      %>% addWMSTiles(
        "http://www502.regione.toscana.it/wmsraster/com.rt.wms.RTmap/wms?map=wmsambamm&",
        layers = "rt_ambamm.idcomuni.rt.poly",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        tileOptions(tms = TRUE),
        attribution = "prova")
        
  )
  
  observe({
    map<-leafletProxy("map",data=dataf())
    map%>%clearGroup(group="cdata")
    
    if (nrow(dataf())>0)
    {
      map%>% addCircleMarkers(~LONGITUDINE
                        ,~LATITUDINE
                        ,color = "#b10026", radius = 5
                        , stroke = FALSE, fillOpacity = 0.5
                        , popup = ~Data_accettazione
                        ,group="cdata"
                    )
    }  
  })
  
  output$numberBox <- renderValueBox({
    df <-dataf()
    valueBox(
      format(nrow(df), format="d", big.mark=","), "Numero eventi", icon = NULL,
      color = "purple")
  })
  
  output$meanageBox <- renderValueBox({
    df <-dataf()
    valueBox(
      format(mean(df$Etagg), digit=0, big.mark=","), "Età media", icon = NULL,
      color = "purple")
  })
  
  output$settcount<-renderPlot(
    {
      per_S<-dataf()%>%
        group_by(sett, Colore_ingresso)%>%
        summarise(n=n())
      if (nrow(dataf())==0){
        gg<-ggplot(per_S)+geom_point()
        print(gg)
      }
      else
      {
        gg<-ggplot(per_S,aes(x=sett,y=n,fill=Colore_ingresso))+
          geom_bar(stat="identity")+
          scale_fill_manual(values=c('#FFB347','#FF6961'))+
          labs(
            x="Numero settimana"
            , y="N. Accessi")
        print(gg)
      }  
    })                                              

  output$comunecount<-renderPlot(
    {
      per_S<-dataf()%>%
        group_by(DesComune, Colore_ingresso)%>%
        summarise(n=n())
      if (nrow(dataf())==0){
        gg<-ggplot(per_S)+geom_point()
        print(gg)
      }
      else
      {
        gg<-ggplot(per_S,aes(x=DesComune,y=n,fill=Colore_ingresso))+
          geom_bar(stat="identity")+
          scale_fill_manual(values=c('#FFB347','#FF6961'))+
          labs(
            x="Comune"
            , y="N. Accessi")+
        coord_flip()
        print(gg)
      }  
    })                                              
  
  output$esitocount<-renderPlot(
    {
      per_S<-dataf()%>%
        group_by(Esito_slashn50, Colore_ingresso)%>%
        summarise(n=n())
      if (nrow(dataf())==0){
        gg<-ggplot(per_S)+geom_point()
        print(gg)
      }
      else
      {
        #xlabel.wrap<-lapply(strwrap(per_S$Esito,width=50,simplify = FALSE),paste,collapse='\n')
    
        gg<-ggplot(per_S,aes(x=Esito_slashn50,y=n,fill=Colore_ingresso))+
          geom_bar(stat="identity")+
          scale_fill_manual(values=c('#FFB347','#FF6961'))+
          labs(
            x="Esito"
            , y="N. Accessi")+
          #scale_x_discrete(labels=xlabel.wrap)+
          coord_flip()
        print(gg)
      }  
    })
  output$table <- renderDataTable(dataf()%>%
                    select('Data accettazione'= Data_accettazione,
                           'Ora accetazione'= Ora_accettazione,
                           'Settimana'=sett,
                           'Comune'=DesComune,
                           'Colore ingresso'=Colore_ingresso,
                           'Capitolo diagnosi principale'=Capitolo_princ,
                           'Esito'=Esito
#                           ,'Tempo attesa minimo'=tempo_attesa_min
#                           ,'Età'=Etagg
                    )
        #, rownames = TRUE
        , options = list(
    pageLength = 10, autoWidth = FALSE))
  
}
  