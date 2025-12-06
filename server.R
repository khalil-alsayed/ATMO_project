library(shiny)
library(leafsync)
library(leaflet)
library(data.table)
library(ggplot2)
library(dplyr)
library(base)
library(leaflet.extras)
library(leaflet.extras2)
library(corrplot)
library(ggcorrplot)
library(stringr)
library(magrittr)
library(stats)
library(htmlwidgets)
library(stringi)
library(RColorBrewer)
library(RPostgres) # pour écrire des requêtes PostgresSQL dans R.
library(DBI) # afin de se connecter à la base de données.
library(devtools)
library(xml2)
library(XML)
library(plotly)
library(DT)
library(viridis)
library(ggiraph)
library(MASS)
library(sp)
library(usethis)
library(viridisLite)
#library(robustbase)
load("Carte_shape.Rdata") #shape surfacique
load("Carte_shape2.Rdata") #shape ponctuel
load("Carte_shape3.Rdata") #shape ligne
load("Carte_epci.Rdata") #shape des epci
#Relier avec la base de donnees-------------
base <- dbConnect(drv = RPostgres::Postgres(),
                  host = "172.16.38.156",
                  port = "5433",
                  dbname = "icare_3.2",
                  user = "postgres",
                  password = "icare")


#Preparer les parties snap et napfue pour declaration consommation----------------
snap_napfue_tc_conso<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct snap,napfue FROM agricole_test.declarations_consommations x
WHERE x.niveau IN ('commune')")))
snap_napfue_annee_decla_conso<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct snap,napfue,annee_ref FROM agricole_test.declarations_consommations x WHERE x.niveau IN ('commune') order by annee_ref")))
snap_napfue_tc_conso[, ID := .I]
snap_napfue_annee_decla_conso[,ID := .I]


#Preparer les parties cheptel pour cheptel_saa----------------------
cheptel<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct cheptel FROM agricole_test.cheptel_saa_commune_complet_pcit2 x
WHERE x.annee_ref IN ('2008','2010','2012','2015','2018','2020')")))
cheptel_annee<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct cheptel, annee_ref  FROM agricole_test.cheptel_saa_commune_complet_pcit2 x WHERE x.annee_ref IN ('2008','2010','2012','2015','2018','2020') order by annee_ref")))

#Preparer les combustible pour W_conso_install----------------------
combustible<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct combustible FROM agricole_test.W_conso_install x WHERE x.an IN ('2008','2010','2012','2015','2018')")))

#Preparer les parties polluantes pour w_src_etab_emi-------------------------------------------
w_src_etab_emi_annee<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct id_polluant, an FROM agricole_test.w_src_etab_emi x WHERE (x.an IN (2008,2010,2012,2015,2018)) AND (x.id_polluant IN (19,49,101,102,111)) order by an")))

#Preparer les parties id_polluant et combustible pour w_film_emi_install--------------------------------------------------------------
polluant_combustible<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct id_polluant,combustible FROM agricole_test.w_film_emi_install x WHERE x.an IN (2008,2010,2012,2015,2018)")))

#Preparer les parties snap et napfue pour a_tc_conso_corrigees----------------
snap_napfue_a_tc_conso<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct snap,napfue FROM agricole_test.a_tc_conso_corrigees")))
snap_napfue_a_tc_conso[, ID := .I]
snap_napfue_annee_a_tc_conso_corrigees<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct snap,napfue,annee_ref FROM agricole_test.a_tc_conso_corrigees order by annee_ref")))
snap_napfue_annee_a_tc_conso_corrigees[,ID := .I]

#Preparer les parties culture et culture_rga pour cultures_saa_commune_pcit2----------------
culture_culture_rga<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct culture,culture_rga FROM agricole_test.cultures_saa_commune_pcit2")))
culture_culture_rga_annee<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct culture,culture_rga,annee_ref FROM agricole_test.cultures_saa_commune_pcit2 order by annee_ref")))
culture_culture_rga_annee[,ID := .I]

#Preparer les parties code_napfue et pcaet pour emi_total_v2020_v4----------------
partie<-data.table(dbGetQuery(conn=base,paste0("SELECT * FROM (SELECT distinct code_napfue, pcaet,code_snap  FROM agricole_test.emi_total_a2008_v2020_v4) AS w1
                                                INTERSECT
                                                SELECT * FROM (SELECT distinct code_napfue , pcaet,code_snap FROM agricole_test.emi_total_a2010_v2020_v4) AS w2
                                                INTERSECT
                                                SELECT * FROM (SELECT distinct code_napfue , pcaet,code_snap FROM agricole_test.emi_total_a2012_v2020_v4) AS w3
                                                INTERSECT
                                                SELECT * FROM (SELECT distinct code_napfue , pcaet,code_snap FROM agricole_test.emi_total_a2015_v2020_v4) AS w4
                                                INTERSECT
                                                SELECT * FROM (SELECT distinct code_napfue , pcaet,code_snap FROM agricole_test.emi_total_a2018_v2020_v4) AS w5
                                               ")))
#Preparer les parties importance et cat_admin pour evolution_trafic_tmja_2020----------------
importance_cat_admin<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct importance,cat_admin FROM agricole_test.evolution_trafic_tmja_2020")))

#Server--------------
shinyServer(function(input, output,session) {
  
  
  #Les observation pour declaration consommation--------------------------------------------------------------------------------------------------------------------------------------------------------------------
  observe({updateSelectInput(inputId = "select11",label = "napfue",choices = snap_napfue_tc_conso[snap==input$select1]$napfue,selected = snap_napfue_tc_conso[snap==input$select1]$napfue[1])})
  observe({updateSelectInput(inputId = "select111",label = "annee 1",choices = snap_napfue_annee_decla_conso[snap==input$select1 & napfue==input$select11]$annee_ref)})
  observe({updateSelectInput(inputId = "select1111",label = "annee 2",choices = snap_napfue_annee_decla_conso[snap==input$select1 & napfue==input$select11]$annee_ref)})
  observe({updateSelectInput(inputId = "select11111",label = "annee",choices = snap_napfue_annee_decla_conso[snap==input$select1 & napfue==input$select11]$annee_ref)})
  
  #Les observation pour cheptel_saa----------------------------------------
  observe({updateSelectInput(inputId = "select33",label = "annee 1",choices = cheptel_annee[cheptel==input$select3]$annee_ref)})
  observe({updateSelectInput(inputId = "select333",label = "annee 2",choices = cheptel_annee[cheptel==input$select3]$annee_ref)})
  
  #Les observation pour w_src_etab_emi----------------------------------------
  observe({updateSelectInput(inputId = "select55",label = "annee 1",choices = w_src_etab_emi_annee[id_polluant==input$select5]$an)})
  observe({updateSelectInput(inputId = "select555",label = "annee 2",choices = w_src_etab_emi_annee[id_polluant==input$select5]$an)})
  observe({updateSelectInput(inputId = "select5555",label = "annee",choices = w_src_etab_emi_annee[id_polluant==input$select5]$an)})
  #Les observation pour w_film_emi_install--------------------------------------------------------------------------------------------------------------------------------------------------------------------
  observe({updateSelectInput(inputId = "select66",label = "combustible",choices = polluant_combustible[id_polluant==input$select6]$combustible)})
  
  #Les observation pour a_tc_conso_corrigees--------------------------------------------------------------------------------------------------------------------------------------------------------------------
  observe({updateSelectInput(inputId = "select99",label = "napfue",choices = snap_napfue_a_tc_conso[snap==input$select9]$napfue,selected = snap_napfue_a_tc_conso[snap==input$select9]$napfue[1])})
  #Les observation pour cultures_saa_commune_pcit2--------------------------------------------------------------------------------------------------------------------------------------------------------------------
  observe({updateSelectInput(inputId = "select1010",label = "culture",choices = culture_culture_rga[culture_rga==input$select10]$culture)})
  observe({updateSelectInput(inputId = "select101010",label = "annee 1",choices = culture_culture_rga_annee[culture_rga==input$select10 & culture==input$select1010]$annee_ref)})
  observe({updateSelectInput(inputId = "select10101010",label = "annee 2",choices = culture_culture_rga_annee[culture_rga==input$select10 & culture==input$select1010]$annee_ref)})
  observe({updateSelectInput(inputId = "select1010101010",label = "annee",choices = culture_culture_rga_annee[culture_rga==input$select10 & culture==input$select1010]$annee_ref)})
  
  #Les observation pour emi_total--------------------------------------------------------------------------------------------------------------------------------------------------------------------
  observe({updateSelectInput(inputId = "select121212",label = "pcaet",choices =partie[code_napfue==input$select1212]$pcaet,selected = partie[code_napfue==input$select1212]$pcaet[1])})
  observe({updateSelectInput(inputId = "select12121212",label = "code_snap",choices =partie[code_napfue==input$select1212 & pcaet==input$select121212]$code_snap,selected = partie[code_napfue==input$select1212 & pcaet==input$select121212]$code_snap[1])})
  
  #Les observation pour evolution_trafic_tmja_2020--------------------------------------------------------------------------------------------------------------------------------------------------------------------
  observe({updateSelectInput(inputId = "select1313",label = "cat_admin",choices =importance_cat_admin[importance==input$select13]$cat_admin,selected = importance_cat_admin[importance==input$select13]$cat_admin[1])})
  
  #Les observation pour evolution_trafic_pl_2020--------------------------------------------------------------------------------------------------------------------------------------------------------------------
  observe({updateSelectInput(inputId = "select1414",label = "cat_admin",choices =importance_cat_admin[importance==input$select14]$cat_admin,selected = importance_cat_admin[importance==input$select14]$cat_admin[1])})
  
  #data_decla_conso()-----------------------
  data_decla_conso<-eventReactive(input$go,{
    
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    ##Au debut telechargement puis fusionnement des communes
    dbSendQuery(conn=base,paste0(str_glue("create temp table tc_cons as select * from agricole_test.declarations_consommations tcc where snap in ('{input$select1}') AND napfue in ('{input$select11}') AND niveau IN ('commune')")))
    dbSendQuery(conn=base,paste0("create  temp table communes_fusionnees as select * from agricole_test.communes_fusionnees tcc where annee_ref = 2020 "))
    dbSendQuery(conn=base,paste0("ALTER TABLE communes_fusionnees RENAME COLUMN numcom to numcom_new"))
    dbSendQuery(conn=base,paste0("update tc_cons set cog= numcom_new from communes_fusionnees where tc_cons.cog=communes_fusionnees.numcom_ancien"))
    declarations_consommations<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM tc_cons "))))
    ##Preparation carte
    carte1<-Carte
    carte1_data<-data.table(carte1@data)
    w <-data.table(dbGetQuery(conn = base, paste0("SELECT SUBSTRING(CONCAT(w1.cog, w2.cog, w3.cog,w4.cog,w5.cog, w6.cog),0,6) as cog, conso_2008, conso_2010, conso_2012, conso_2015, conso_2018, conso_2020 FROM
(SELECT cog,SUM(conso) AS conso_2008 FROM tc_cons  WHERE  annee_ref=2008   GROUP BY cog) AS w1 FULL JOIN
(SELECT cog,SUM(conso) AS conso_2010 FROM tc_cons  WHERE  annee_ref=2010   GROUP BY cog) AS w2 ON w1.cog=w2.cog FULL JOIN
(SELECT cog,SUM(conso) AS conso_2012 FROM tc_cons  WHERE  annee_ref=2012   GROUP BY cog) AS w3 ON COALESCE(w1.cog,w2.cog)=w3.cog FULL JOIN
(SELECT cog,SUM(conso) AS conso_2015 FROM tc_cons  WHERE  annee_ref=2015   GROUP BY cog) AS w4 ON COALESCE(w1.cog,w2.cog,w3.cog)=w4.cog FULL JOIN
(SELECT cog,SUM(conso) AS conso_2018 FROM tc_cons  WHERE  annee_ref=2018   GROUP BY cog) AS w5 ON COALESCE(w1.cog,w2.cog,w3.cog,w4.cog)=w5.cog FULL JOIN
(SELECT cog,SUM(conso) AS conso_2020 FROM tc_cons  WHERE  annee_ref=2020   GROUP BY cog) AS w6 ON COALESCE(w1.cog,w2.cog,w3.cog,w4.cog,w5.cog)=w6.cog")))
    idx <- match(carte1@data$INSEE_COM , w$cog)
    carte1_data <- data.table(w[idx, ])
    func_etat<-function(a,b,c,d,e,f){
      cc<-c(a,b,c,d,e,f)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*1.5
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm=TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm=TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e) & is.na(f)){
        return(NA)
      }else if((ab3/max(cc3,na.rm = TRUE)>3 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>3 && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>1.5 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>1.5 && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    carte1_data[,Etat:=mapply(func_etat,conso_2008,conso_2010,conso_2012,conso_2015,conso_2018,conso_2020)]
    
    
    carte1$conso_2008<-carte1_data$conso_2008
    carte1$conso_2010<-carte1_data$conso_2010
    carte1$conso_2012<-carte1_data$conso_2012
    carte1$conso_2015<-carte1_data$conso_2015
    carte1$conso_2018<-carte1_data$conso_2018
    carte1$conso_2020<-carte1_data$conso_2020
    carte1$Etat<-carte1_data$Etat
    save(list=c("carte1","declarations_consommations"),file="carte1.Rdata",compress = F)
    
    #leaflet
    pal<-colorFactor("viridis",domain = carte1$Etat)
    leaflet(carte1)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  color="white",
                  fillOpacity =1 ,
                  fillColor = ~pal(Etat),
                  label=~INSEE_COM,
                  layerId = ~INSEE_COM,
                  highlightOptions =highlightOptions(
                    color="white",
                    weight=3,
                    fillColor = NA),
                  popup = paste0("<strong> Commune: </strong>",
                                 carte1$NOM_M_COM,
                                 "<br>",
                                 "<strong> Numero commune: </strong>",
                                 carte1$INSEE_COM,
                                 "<br>",
                                 "<strong> conso_2008: </strong>",
                                 carte1$conso_2008,
                                 "<br>",
                                 "<strong> conso_2010: </strong>",
                                 carte1$conso_2010,
                                 "<br>",
                                 "<strong> conso_2012: </strong>",
                                 carte1$conso_2012,
                                 "<br>",
                                 "<strong> conso_2015: </strong>",
                                 carte1$conso_2015,
                                 "<br>",
                                 "<strong> conso_2018: </strong>",
                                 carte1$conso_2018,
                                 "<br>",
                                 "<strong> conso_2020: </strong>",
                                 carte1$conso_2020,
                                 "<br>",
                                 "<strong> Etat: </strong>",
                                 carte1$Etat),
                  group = "carte1")%>%
      addLegend(pal=pal,
                values = ~Etat,
                title = "Etat",
                opacity = 1)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "carte1",
                        options = searchFeaturesOptions(zoom = 12,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    
    
    
  })
  
  
  #data_decla_consoc()------------------
  data_decla_consoc<-eventReactive(input$go1,{
    load("carte1.Rdata")
    a<-input$select11111
    c<-carte1
    c@data<-carte1@data[c("ID","NOM_COM","NOM_M_COM","INSEE_COM","STATUT","POPULATION","NOM_EPCI","NATURE_EPC","CODE_SIREN","INSEE_CAN","INSEE_ARR","INSEE_DEP","NOM_DEP","NOM_M_DEP","INSEE_REG","NOM_REG","NOM_M_REG",str_glue("conso_{a}"),"Etat")]
    names(c)[18]<-'conso'
    pal<-colorBin("YlOrRd", domain = c$conso)
    leaflet(c)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  color="white",
                  fillOpacity =1 ,
                  fillColor = ~pal(conso),
                  label=~INSEE_COM,
                  layerId = ~INSEE_COM,
                  highlightOptions =highlightOptions(
                    color="white",
                    weight=3,
                    fillColor = NA),
                  popup = paste0("<strong> Commune: </strong>",
                                 c$NOM_M_COM,
                                 "<br>",
                                 "<strong> Numero commune: </strong>",
                                 c$INSEE_COM,
                                 "<br>",
                                 "<strong> conso: </strong>",
                                 c$conso
                  ),
                  group = "c")%>%
      addLegend(pal=pal,
                values = ~conso,
                title = "conso",
                opacity = 1)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "c",
                        options = searchFeaturesOptions(zoom = 12,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    
  })
  
  
  
  #mouse_capture--------------------
  rv_shape<-reactiveVal(FALSE)
  rv_location<-reactiveValues(id=NULL,lat=NULL,lng=NULL,code_etab=NULL,zoom=NULL)
  
  #data_graphe_decla_conso()--------------------
  
  data_graphe_decla_conso<-eventReactive(input$mape_decla_conso_shape_click,{
    mape_decla_conso_shape_click_info <- input$mape_decla_conso_shape_click
    rv_location$id <- mape_decla_conso_shape_click_info$id
    load("carte1.Rdata")
    h1<-data.table(carte1@data)[INSEE_COM==rv_location$id]$conso_2008
    h2<-data.table(carte1@data)[INSEE_COM==rv_location$id]$conso_2010
    h3<-data.table(carte1@data)[INSEE_COM==rv_location$id]$conso_2012
    h4<-data.table(carte1@data)[INSEE_COM==rv_location$id]$conso_2015
    h5<-data.table(carte1@data)[INSEE_COM==rv_location$id]$conso_2018
    h6<-data.table(carte1@data)[INSEE_COM==rv_location$id]$conso_2020
    h<-c(h1,h2,h3,h4,h5,h6)
    hh<-c(2008,2010,2012,2015,2018,2020)
    hhh<-data.table(numcom=rv_location$id,conso=h,annee=hh)
    ggplot(hhh,aes(x=annee,y=conso,color=annee))+geom_line()+geom_point()
    
    
  })
  
  #data_correlation_decla_conso-----------------------
  
  data_correlation_decla_conso<-eventReactive(input$go,{
    
    load("carte1.Rdata")
    correlation_decla_conso<-data.table(carte1@data)[,list(conso_2008,conso_2010,conso_2012,conso_2015,conso_2018,conso_2020)]
    
    if(length(correlation_decla_conso[is.na(conso_2008)==TRUE]$conso_2008)==length(correlation_decla_conso$conso_2008)){
      correlation_decla_conso[,':='(conso_2008=NULL)]
    }
    if(length(correlation_decla_conso[is.na(conso_2010)==TRUE]$conso_2010)==length(correlation_decla_conso$conso_2010)){
      correlation_decla_conso[,':='(conso_2010=NULL)]
    }
    if(length(correlation_decla_conso[is.na(conso_2012)==TRUE]$conso_2012)==length(correlation_decla_conso$conso_2012)){
      correlation_decla_conso[,':='(conso_2012=NULL)]
    }
    if(length(correlation_decla_conso[is.na(conso_2015)==TRUE]$conso_2015)==length(correlation_decla_conso$conso_2015)){
      correlation_tc_conso[,':='(conso_2015=NULL)]
    }
    if(length(correlation_decla_conso[is.na(conso_2018)==TRUE]$conso_2018)==length(correlation_decla_conso$conso_2018)){
      correlation_decla_conso[,':='(conso_2018=NULL)]
    }
    if(length(correlation_decla_conso[is.na(conso_2020)==TRUE]$conso_2020)==length(correlation_decla_conso$conso_2020)){
      correlation_decla_conso[,':='(conso_2020=NULL)]
    }
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corr<-cor(correlation_decla_conso,use="pairwise.complete.obs")
    p.mat<-cor_pmat(correlation_decla_conso)
    corrplot(corr,
             col=col(200),
             method="color",
             title = title(main=str_glue("Correlogramme pour snap={input$select1} et napfue = {input$select11}"),cex.main=0.6,line=-1,adj=0.7),
             addCoef.col = "black",
             type = "lower",
             outline.col = "white",
             p.mat = p.mat,
             sig.level = 0.01,
             tl.srt=90,
             tl.cex=0.8,
             tl.col = "red",
             diag = TRUE)
  })
  
  #data_table_decla_conso()----------------------------------
  
  data_table_decla_conso<-eventReactive(input$mape_decla_conso_shape_click,{
    load("carte1.Rdata")
    mape_decla_conso_shape_click_info <- input$mape_decla_conso_shape_click
    rv_location$id <- mape_decla_conso_shape_click_info$id
    A<-declarations_consommations[snap==input$select1 & napfue==input$select11 & cog==rv_location$id]
    tc_copie<-A
    tc_copie[,ID := .I]
    doublons<-which(duplicated(tc_copie$annee_ref))
    annee<-tc_copie$annee_ref
    if (length(doublons)!=0){
      annee<-tc_copie[-doublons]$annee_ref}
    cc<-data.table(conso=0,annee=annee)
    for (i in cc$annee){
      cc[annee==i]$conso<-sum(tc_copie[annee_ref==i]$conso,na.rm = TRUE)
      
    }
    
    col_fort<-c()
    col_faible<-c()
    
    ccc<-cc$conso
    Q1<-quantile(ccc,0.25,na.rm=TRUE)
    Q3<-quantile(ccc,0.75,na.rm=TRUE)
    s1<-(Q3-Q1)*1.5
    ab3<- max(ccc-Q3,na.rm=TRUE)+Q3
    ab1<- -max(Q1-ccc,na.rm=TRUE)+Q1
    cc3<-data.table(ccc)
    cc3[, ID := .I]
    a3<-cc3[abs(ccc-ab3)== min(cc3[,.(abs(ccc-ab3))]$V1) ]$ID
    cc3<-cc3[-a3,]$ccc
    
    cc1<-data.table(ccc)
    cc1[, ID := .I]
    a1<-cc1[abs(ccc-ab1)== min(cc1[,.(abs(ccc-ab1))]$V1) ]$ID
    cc1<-cc1[-a1,]$ccc
    
    if((min(cc1,na.rm = TRUE)/ab1>1.5 && min(cc1,na.rm = TRUE)/ab1 <3 && Q1-ab1>s1)){
      
      col_faible[[1]]<-A[annee_ref == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>1.5 && ab3/max(cc3,na.rm = TRUE)<3  && ab3-Q3>s1)){
      
      col_faible[[2]]<-A[annee_ref == cc[a3,]$annee]$ID
    }
    
    if((min(cc1,na.rm = TRUE)/ab1>3 && Q1-ab1>s1)){
      
      col_fort[[1]]<-A[annee_ref == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>3 && ab3-Q3>s1)){
      
      col_fort[[2]]<-A[annee_ref == cc[a3,]$annee]$ID
    }
    
    A[,':='(ID=NULL)]
    if(length(col_fort)!=0 && length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(c("niveau","cog","annee_ref","snap","napfue","conso","source"),color=styleRow(col_fort,"red"))%>%
        formatStyle(c("niveau","cog","annee_ref","snap","napfue","conso","source"),color=styleRow(col_faible,"blue"))
      
    }else if(length(col_fort)!=0){
      datatable(A)%>%
        formatStyle(c("niveau","cog","annee_ref","snap","napfue","conso","source"),color=styleRow(col_fort,"red"))
      
      
    }else if(length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(c("niveau","cog","annee_ref","snap","napfue","conso","source"),color=styleRow(col_faible,"blue"))
      
    }else{
      datatable(A)
    }
  })
  
  #data_decla_conso_reg--------------------------------
  
  data_decla_conso_reg<-eventReactive(input$go11,{
    load("carte1.Rdata")
    
    DD<-data.frame(carte1@data)
    g<-ggplot(data=DD,aes(x=DD[,str_glue("conso_{input$select111}")],y=DD[,str_glue("conso_{input$select1111}")],color=Etat))+
      geom_point(aes(text=paste("numcom:",
                                DD[,"INSEE_COM"],
                                "<br>",
                                str_glue("conso_{input$select111}:"),
                                DD[,str_glue("conso_{input$select111}")],
                                "<br>",
                                str_glue("conso_{input$select1111}:"),
                                DD[,str_glue("conso_{input$select1111}")],
                                "<br>",
                                "Etat:",
                                DD[,"Etat"]
      )))+
      stat_smooth(method = "rlm", col = "black")+
      labs(title=str_glue("Graphe de regression lineaire de declaration consommation pour {input$select111}-{input$select1111}"),x=str_glue("conso de {input$select111}"),y=str_glue("conso de {input$select1111}"))
    
    ggplotly(g,tooltip = "text")
    
    
    
  })
  
  #data_decla_conso_histo()----------------------------------------------
  
  data_decla_conso_histo<-eventReactive(input$go,{
    load("carte1.Rdata")
    D<-data.table(annee=snap_napfue_annee_decla_conso[snap==input$select1 & napfue==input$select11]$annee_ref,conso=as.numeric())
    for (i in D$annee){
      #D[i,]$conso<-sum(tc_conso_corrigees[annee_ref==D[i,]$annee & snap==as.numeric(input$select1) & napfue==as.numeric(input$select11)]$conso)
      D[annee==i]$conso<-sum(declarations_consommations[annee_ref==i & snap==input$select1 & napfue==input$select11]$conso,na.rm = TRUE)
    }
    ggplot(data=D,aes(x=as.character(annee),y=conso,fill=as.character(annee),tooltip = conso ))+geom_histogram_interactive(stat='identity',position = 'dodge')+labs(title="Histogramme de la consommation total des communes",x="annee",y="conso en kg",fill="annee")+theme(axis.text.x = element_text(face="bold",size=10,angle = 90))+geom_text(aes(label=as.integer(conso)),position = position_dodge(.9), vjust = "top") 
    
  })
  
  #data_src_emi()--------------------------
  data_src_emi<-eventReactive(input$go2,{
    
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    #Au debut telechargement des données
    SRC_install_emi<-data.table(dbGetQuery(conn=base,paste0("SELECT x.* FROM agricole_test.src_install_emi x WHERE (x.an IN (2008,2010,2012,2015,2018,2020)) AND (x.id_polluant IN (19,49,101,102,111,131))")))
    
    
    ##preparation carte 
    carte2<-Carte2
    carte2_data<-data.table(gid=str_split(carte2@data$gid, pattern = "_",simplify = TRUE)[,2])
    
    func_etat<-function(a,b,c,d,e,f){
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e) & is.na(f)){
        return(NA)
      }
      cc<-c(a,b,c,d,e,f)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*1.5
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm = TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm = TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if((ab3/max(cc3,na.rm = TRUE)>3 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>3 && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>1.5 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>1.5 && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    func_install<-function(etab,install){
      "aa<-SRC_install_emi[code_install==install & code_etab==etab & an==2008 & id_polluant==input$select2 ]$val
      a<-sum(aa)
      bb<-SRC_install_emi[code_install==install & code_etab==etab & an==2010 & id_polluant==input$select2 ]$val
      b<-sum(bb)
      Cc<-SRC_install_emi[code_install==install & code_etab==etab & an==2012 & id_polluant==input$select2 ]$val
      c<-sum(Cc)
      dd<-SRC_install_emi[code_install==install & code_etab==etab & an==2015 & id_polluant==input$select2 ]$val
      d<-sum(dd)
      ee<-SRC_install_emi[code_install==install & code_etab==etab & an==2018 & id_polluant==input$select2 ]$val
      e<-sum(ee)
      ff<-SRC_install_emi[code_install==install & code_etab==etab & an==2020 & id_polluant==input$select2 ]$val
      f<-sum(ff)
      if(length(aa)==0){
        a<-NA
      }
      if(length(bb)==0){
        b<-NA
      }
      if(length(Cc)==0){
        c<-NA
      }
      if(length(dd)==0){
        d<-NA
      }
      if(length(ee)==0){
        e<-NA
      }
      if(length(ff)==0){
        f<-NA
      }"
      if (is.na(install)){
        aa<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.src_install_emi WHERE an = 2008 AND id_polluant = {input$select2} AND code_etab = '{etab}' AND code_install is NULL "))))$val
        a<-sum(aa)
        bb<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.src_install_emi WHERE an = 2010 AND id_polluant = {input$select2} AND code_etab = '{etab}' AND code_install is NULL "))))$val
        b<-sum(bb)
        Cc<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.src_install_emi WHERE an = 2012 AND id_polluant = {input$select2} AND code_etab = '{etab}' AND code_install is NULL "))))$val
        c<-sum(Cc)
        dd<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.src_install_emi WHERE an = 2015 AND id_polluant = {input$select2} AND code_etab = '{etab}' AND code_install is NULL "))))$val
        d<-sum(dd)
        ee<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.src_install_emi WHERE an = 2018 AND id_polluant = {input$select2} AND code_etab = '{etab}' AND code_install is NULL "))))$val
        e<-sum(ee)
        ff<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.src_install_emi WHERE an = 2020 AND id_polluant = {input$select2} AND code_etab = '{etab}' AND code_install is NULL "))))$val
        f<-sum(ff)
        if(length(aa)==0){
          a<-NA
        }
        if(length(bb)==0){
          b<-NA
        }
        if(length(Cc)==0){
          c<-NA
        }
        if(length(dd)==0){
          d<-NA
        }
        if(length(ee)==0){
          e<-NA
        }
        if(length(ff)==0){
          f<-NA
        }
      }else{
        aa<-SRC_install_emi[code_install==install & code_etab==etab & an==2008 & id_polluant==input$select2]$val
        a<-sum(aa)
        bb<-SRC_install_emi[code_install==install & code_etab==etab & an==2010 & id_polluant==input$select2 ]$val
        b<-sum(bb)
        Cc<-SRC_install_emi[code_install==install & code_etab==etab & an==2012 & id_polluant==input$select2 ]$val
        c<-sum(Cc)
        dd<-SRC_install_emi[code_install==install & code_etab==etab & an==2015 & id_polluant==input$select2 ]$val
        d<-sum(dd)
        ee<-SRC_install_emi[code_install==install & code_etab==etab & an==2018 & id_polluant==input$select2 ]$val
        e<-sum(ee)
        ff<-SRC_install_emi[code_install==install & code_etab==etab & an==2020 & id_polluant==input$select2]$val
        f<-sum(ff)
        if(length(aa)==0){
          a<-NA
        }
        if(length(bb)==0){
          b<-NA
        }
        if(length(Cc)==0){
          c<-NA
        }
        if(length(dd)==0){
          d<-NA
        }
        if(length(ee)==0){
          e<-NA
        }
        if(length(ff)==0){
          f<-NA
        }
      }
      return(func_etat(a,b,c,d,e,f))
    }
    func_src_etat<-function(etab){
      D<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT distinct code_install FROM agricole_test.src_install_emi WHERE (an IN (2008,2010,2012,2015,2018,2020)) AND id_polluant= {input$select2} AND code_etab='{etab}' "))))
      if (length(D$code_install)==0){
        return(NA)
      }
      D[,resultat:=mapply(func_install,etab,code_install)]
      if ("Aberrante Forte" %in% D$resultat){
        return("Aberrante Forte")
      }
      else if ("Aberrante Faible" %in% D$resultat){
        return("Aberrante Faible")
      }
      else if("Normal" %in% D$resultat){
        return("Normal")
      }
      else if("Null" %in% D$resultat){
        return("Null")
      }
      else{
        return(NA)
      }
      
      
    }
    
    carte2_data[,Etat:=mapply(func_src_etat,gid)]
    carte2$gid<-str_split(carte2$gid, pattern = "_",simplify = TRUE)[,2]
    carte2$Etat<-carte2_data$Etat
    
    
    ##leaflet 
    pal<-colorFactor("viridis",domain = carte2$Etat)
    l<-leaflet(carte2)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addCircleMarkers(stroke = FALSE,
                       radius = 5,
                       color="white",
                       fillOpacity =1 ,
                       fillColor = ~pal(Etat),
                       label=~gid,
                       layerId = ~gid,
                       
                       popup = paste0("<strong> code_etab: </strong>",
                                      carte2$gid,
                                      "<br>",
                                      "<strong> Etat: </strong>",
                                      carte2$Etat))%>%
      addMarkers(label=~gid,
                 group = "carte2",
                 icon = makeIcon(
                   iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                   iconWidth = 0.01,
                   iconHeight = 0.01
                 ))%>%
      addLegend(pal=pal,
                values = ~Etat,
                title = "Etat",
                opacity = 1)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "carte2",
                        options = searchFeaturesOptions(zoom = 50,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    esri <- grep("^CartoDB", providers, value = TRUE)
    esri<-esri[2]
    esri1<-grep("^Esri", providers, value = TRUE)
    esri1<-esri1[5]
    esri2<-c(esri1,esri)
    for (provider in esri2) {
      l <- l %>% addProviderTiles(provider, group = provider)
    }
    
    l%>%
      addLayersControl(baseGroups = names(esri2),
                       options = layersControlOptions(collapsed = TRUE),position = "topleft")
    
    
  })
  
  
  #data_table_src_emi()----------------------------------------------------
  
  data_table_src_emi<-eventReactive(input$mape_src_emi_marker_click,{
    SRC_install_emi<-data.table(dbGetQuery(conn=base,paste0("SELECT x.* FROM agricole_test.src_install_emi x WHERE (x.an IN (2008,2010,2012,2015,2018,2020)) AND (x.id_polluant IN (19,49,101,102,111,131))")))
    mape_src_emi_marker_click_info <- input$mape_src_emi_marker_click
    rv_location$id <- mape_src_emi_marker_click_info$id
    func_val<-function(annee,install){
      if (is.na(install)){
        aa<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.src_install_emi WHERE an = {annee} AND id_polluant = {input$select2} AND code_etab = '{rv_location$id}' AND code_install is NULL "))))$val
        a<-sum(aa)
        if(length(aa)==0){
          a<-NA
        }
        return(a)
      }else{
        aa<-SRC_install_emi[code_install==install & code_etab==rv_location$id & an==annee & id_polluant==input$select2]$val
        #aa<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT val FROM agricole_test.src_install_emi  WHERE an={annee} AND id_polluant={input$select2} AND code_etab='{rv_location$id}' AND code_install={install})"))))
        a<-sum(aa)
        if(length(aa)==0){
          a<-NA
        }
        return(a)
      }
      
      
    }
    D<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT distinct code_install FROM agricole_test.src_install_emi WHERE (an IN (2008,2010,2012,2015,2018,2020)) AND id_polluant= {input$select2} AND code_etab='{rv_location$id}' "))))
    if (length(D$code_install)!=0){
      D[,c("val_2008","val_2010","val_2012","val_2015","val_2018","val_2020"):=list(mapply(func_val,2008,code_install),mapply(func_val,2010,code_install),mapply(func_val,2012,code_install),mapply(func_val,2015,code_install),mapply(func_val,2018,code_install),mapply(func_val,2020,code_install))]
      DD<-D$code_install
      D[,code_install:=NULL]
      D%>%datatable(rownames = paste("install",DD))
    }else{
      D<-data.table(val_2008=numeric(),val_2010=numeric(),val_2012=numeric(),val_2015=numeric(),val_2018=numeric(),val_2020=numeric())
      datatable(D)
    }
    
    
  })
  #data_cheptel_saa_commune_complet_pcit2()-----------------------
  data_cheptel_saa_commune_complet_pcit2<-eventReactive(input$go3,{
    
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    ##Au debut telechargement puis fusionnement des communes
    communes_fusionnees<-data.table(dbGetQuery(conn=base,paste0('SELECT x.* FROM agricole_test.communes_fusionnees x WHERE x.annee_ref IN (2020)')))
    #cheptel_saa<-data.table(dbGetQuery(conn=base,paste0("SELECT x.* FROM agricole_test.cheptel_saa_commune_complet_pcit2 x WHERE x.annee_ref IN ('2008','2010','2012','2015','2018','2020')")))
    x0<-print(gsub("'", "''", input$select3) )
    cheptel_saa<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT x.* FROM agricole_test.cheptel_saa_commune_complet_pcit2 x WHERE (x.annee_ref IN ('2008','2010','2012','2015','2018','2020')) AND (x.cheptel IN ('{x0}'))"))))
    setnames(cheptel_saa,"numcom","numcom_ancien")
    cheptel_saa[communes_fusionnees, numcom_ancien:=numcom,on = .(numcom_ancien)]
    setnames(cheptel_saa,"numcom_ancien","numcom")
    ##Preparation carte
    carte3<-Carte
    carte3_data<-data.table(carte3@data)
    func<-function(x,annee){
      aa<-cheptel_saa[annee_ref==annee & cheptel==input$select3 & numcom==x]$valeur
      a<-sum(aa)
      if(length(aa)==0){
        a<-NA
      }
      return(a)
    }
    carte3_data[ , c("valeur_2008","valeur_2010","valeur_2012","valeur_2015","valeur_2018","valeur_2020"):= list(mapply(func,INSEE_COM,"2008"),mapply(func,INSEE_COM,"2010"),mapply(func,INSEE_COM,"2012"), mapply(func,INSEE_COM,"2015"),mapply(func,INSEE_COM,"2018"),mapply(func,INSEE_COM,"2020"))]
    func_etat<-function(a,b,c,d,e,f){
      cc<-c(a,b,c,d,e,f)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*1.5
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm=TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm=TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e) & is.na(f)){
        return(NA)
      }else if((ab3/max(cc3,na.rm = TRUE)>3 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>3 && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>1.5 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>1.5 && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    carte3_data[,Etat:=mapply(func_etat,valeur_2008,valeur_2010,valeur_2012,valeur_2015,valeur_2018,valeur_2020)]
    
    
    carte3$valeur_2008<-carte3_data$valeur_2008
    carte3$valeur_2010<-carte3_data$valeur_2010
    carte3$valeur_2012<-carte3_data$valeur_2012
    carte3$valeur_2015<-carte3_data$valeur_2015
    carte3$valeur_2018<-carte3_data$valeur_2018
    carte3$valeur_2020<-carte3_data$valeur_2020
    carte3$Etat<-carte3_data$Etat
    save(list=c("carte3","cheptel_saa"),file="carte3.Rdata",compress = F)
    
    #leaflet
    pal<-colorFactor("viridis",domain = carte3$Etat)
    leaflet(carte3)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  color="white",
                  fillOpacity =1 ,
                  fillColor = ~pal(Etat),
                  label=~INSEE_COM,
                  layerId = ~INSEE_COM,
                  highlightOptions =highlightOptions(
                    color="white",
                    weight=3,
                    fillColor = NA),
                  popup = paste0("<strong> Commune: </strong>",
                                 carte3$NOM_M_COM,
                                 "<br>",
                                 "<strong> Numero commune: </strong>",
                                 carte3$INSEE_COM,
                                 "<br>",
                                 "<strong> valeur_2008: </strong>",
                                 carte3$valeur_2008,
                                 "<br>",
                                 "<strong> valeur_2010: </strong>",
                                 carte3$valeur_2010,
                                 "<br>",
                                 "<strong> valeur_2012: </strong>",
                                 carte3$valeur_2012,
                                 "<br>",
                                 "<strong> valeur_2015: </strong>",
                                 carte3$valeur_2015,
                                 "<br>",
                                 "<strong> valeur_2018: </strong>",
                                 carte3$valeur_2018,
                                 "<br>",
                                 "<strong> valeur_2020: </strong>",
                                 carte3$valeur_2020,
                                 "<br>",
                                 "<strong> Etat: </strong>",
                                 carte3$Etat),
                  group = "carte3")%>%
      addLegend(pal=pal,
                values = ~Etat,
                title = "Etat",
                opacity = 1)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "carte3",
                        options = searchFeaturesOptions(zoom = 12,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    
    
    
  })
  
  
  
  #data_cheptel_saa_comparaison()------------------
  data_cheptel_saa_comparaison<-eventReactive(input$go333,{
    load("carte3.Rdata")
    a<-input$select3333
    c<-carte3
    #c@data<-carte3@data[c("ID","NOM_COM","NOM_M_COM","INSEE_COM","STATUT","POPULATION","NOM_EPCI","NATURE_EPC","CODE_SIREN","INSEE_CAN","INSEE_ARR","INSEE_DEP","NOM_DEP","NOM_M_DEP","INSEE_REG","NOM_REG","NOM_M_REG",str_glue("conso_{a}"),"Etat")]
    c@data<-carte3@data[c(names(c@data)[c(1:17)],str_glue("valeur_{a}"),"Etat")]
    names(c)[18]<-'valeur'
    pal<-colorBin("YlOrRd", domain = c$valeur)
    leaflet(c)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  color="white",
                  fillOpacity =1 ,
                  fillColor = ~pal(valeur),
                  label=~INSEE_COM,
                  layerId = ~INSEE_COM,
                  highlightOptions =highlightOptions(
                    color="white",
                    weight=3,
                    fillColor = NA),
                  popup = paste0("<strong> Commune: </strong>",
                                 c$NOM_M_COM,
                                 "<br>",
                                 "<strong> Numero commune: </strong>",
                                 c$INSEE_COM,
                                 "<br>",
                                 "<strong> valeur: </strong>",
                                 c$valeur
                  ),
                  group = "c")%>%
      addLegend(pal=pal,
                values = ~valeur,
                title = "valeur",
                opacity = 1)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "c",
                        options = searchFeaturesOptions(zoom = 12,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    
  })
  #data_cheptel_saa_totale()--------------------
  
  data_cheptel_saa_totale<-eventReactive(input$go33333,{
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    ##Au debut telechargement puis fusionnement des communes
    communes_fusionnees<-data.table(dbGetQuery(conn=base,paste0('SELECT x.* FROM agricole_test.communes_fusionnees x WHERE x.annee_ref IN (2020)')))
    cheptel_saa<-data.table(dbGetQuery(conn=base,paste0("SELECT x.* FROM agricole_test.cheptel_saa_commune_complet_pcit2 x WHERE (x.annee_ref IN ('2008','2010','2012','2015','2018','2020'))")))
    setnames(cheptel_saa,"numcom","numcom_ancien")
    cheptel_saa[communes_fusionnees, numcom_ancien:=numcom,on = .(numcom_ancien)]
    setnames(cheptel_saa,"numcom_ancien","numcom")
    ##Preparation carte
    carte0<-Carte
    carte0_data<-data.table(carte0@data)
    func<-function(x,annee){
      aa<-cheptel_saa[annee_ref==annee & numcom==x]$valeur
      a<-sum(aa,na.rm = TRUE)
      if(length(aa)==0){
        a<-NA
      }
      return(a)
    }
    carte0_data[ , c("valeur_2008","valeur_2010","valeur_2012","valeur_2015","valeur_2018","valeur_2020"):= list(mapply(func,INSEE_COM,"2008"),mapply(func,INSEE_COM,"2010"),mapply(func,INSEE_COM,"2012"), mapply(func,INSEE_COM,"2015"),mapply(func,INSEE_COM,"2018"),mapply(func,INSEE_COM,"2020"))]
    func_etat<-function(a,b,c,d,e,f){
      cc<-c(a,b,c,d,e,f)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*(input$seuil_3+1.5)
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm=TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm=TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e) & is.na(f)){
        return(NA)
      }else if((ab3/max(cc3,na.rm = TRUE)>(3+input$seuil_3) && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>(3+input$seuil_3) && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>(1.5+input$seuil_3) && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>(1.5+input$seuil_3) && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    carte0_data[,Etat:=mapply(func_etat,valeur_2008,valeur_2010,valeur_2012,valeur_2015,valeur_2018,valeur_2020)]
    
    
    carte0$valeur_2008<-carte0_data$valeur_2008
    carte0$valeur_2010<-carte0_data$valeur_2010
    carte0$valeur_2012<-carte0_data$valeur_2012
    carte0$valeur_2015<-carte0_data$valeur_2015
    carte0$valeur_2018<-carte0_data$valeur_2018
    carte0$valeur_2020<-carte0_data$valeur_2020
    carte0$Etat<-carte0_data$Etat
   
    
    
    #leaflet
    pal<-colorFactor("viridis",domain = carte0$Etat)
    leaflet(carte0)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  color="white",
                  fillOpacity =1 ,
                  fillColor = ~pal(Etat),
                  label=~INSEE_COM,
                  layerId = ~INSEE_COM,
                  highlightOptions =highlightOptions(
                    color="white",
                    weight=3,
                    fillColor = NA),
                  popup = paste0("<strong> Commune: </strong>",
                                 carte0$NOM_M_COM,
                                 "<br>",
                                 "<strong> Numero commune: </strong>",
                                 carte0$INSEE_COM,
                                 "<br>",
                                 "<strong> valeur_2008: </strong>",
                                 carte0$valeur_2008,
                                 "<br>",
                                 "<strong> valeur_2010: </strong>",
                                 carte0$valeur_2010,
                                 "<br>",
                                 "<strong> valeur_2012: </strong>",
                                 carte0$valeur_2012,
                                 "<br>",
                                 "<strong> valeur_2015: </strong>",
                                 carte0$valeur_2015,
                                 "<br>",
                                 "<strong> valeur_2018: </strong>",
                                 carte0$valeur_2018,
                                 "<br>",
                                 "<strong> valeur_2020: </strong>",
                                 carte0$valeur_2020,
                                 "<br>",
                                 "<strong> Etat: </strong>",
                                 carte0$Etat),
                  group = "carte0")%>%
      addLegend(pal=pal,
                values = ~Etat,
                title = "Etat",
                opacity = 1)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "carte0",
                        options = searchFeaturesOptions(zoom = 12,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    
    
    
  })
  
  
  
  
  
  
  
  #data_graphe_cheptel_saa()--------------------
  data_graphe_cheptel_saa<-eventReactive(input$mape_cheptel_saa_shape_click,{
    mape_cheptel_saa_shape_click_info <- input$mape_cheptel_saa_shape_click
    rv_location$id <- mape_cheptel_saa_shape_click_info$id
    load("carte3.Rdata")
    h1<-data.table(carte3@data)[INSEE_COM==rv_location$id]$valeur_2008
    h2<-data.table(carte3@data)[INSEE_COM==rv_location$id]$valeur_2010
    h3<-data.table(carte3@data)[INSEE_COM==rv_location$id]$valeur_2012
    h4<-data.table(carte3@data)[INSEE_COM==rv_location$id]$valeur_2015
    h5<-data.table(carte3@data)[INSEE_COM==rv_location$id]$valeur_2018
    h6<-data.table(carte3@data)[INSEE_COM==rv_location$id]$valeur_2020
    h<-c(h1,h2,h3,h4,h5,h6)
    hh<-c(2008,2010,2012,2015,2018,2020)
    hhh<-data.table(numcom=rv_location$id,valeur=h,annee=hh)
    ggplot(hhh,aes(x=annee,y=valeur,color=annee))+geom_line()+geom_point()
    
    
  })
  
  #data_table_cheptel_saa()----------------------------------
  data_table_cheptel_saa<-eventReactive(input$mape_cheptel_saa_shape_click,{
    load("carte3.Rdata")
    mape_cheptel_saa_shape_click_info <- input$mape_cheptel_saa_shape_click
    rv_location$id <- mape_cheptel_saa_shape_click_info$id
    A<-cheptel_saa[cheptel==input$select3 & numcom==rv_location$id]
    tc_copie<-A
    tc_copie[,ID := .I]
    doublons<-which(duplicated(tc_copie$annee_ref))
    annee<-tc_copie$annee_ref
    if (length(doublons)!=0){
      annee<-tc_copie[-doublons]$annee_ref}
    cc<-data.table(valeur=0,annee=annee)
    for (i in cc$annee){
      cc[annee==i]$valeur<-sum(tc_copie[annee_ref==i]$valeur,na.rm = TRUE)
      
    }
    
    col_fort<-c()
    col_faible<-c()
    
    ccc<-cc$valeur
    Q1<-quantile(ccc,0.25,na.rm=TRUE)
    Q3<-quantile(ccc,0.75,na.rm=TRUE)
    s1<-(Q3-Q1)*1.5
    ab3<- max(ccc-Q3,na.rm=TRUE)+Q3
    ab1<- -max(Q1-ccc,na.rm=TRUE)+Q1
    cc3<-data.table(ccc)
    cc3[, ID := .I]
    a3<-cc3[abs(ccc-ab3)== min(cc3[,.(abs(ccc-ab3))]$V1) ]$ID
    cc3<-cc3[-a3,]$ccc
    
    cc1<-data.table(ccc)
    cc1[, ID := .I]
    a1<-cc1[abs(ccc-ab1)== min(cc1[,.(abs(ccc-ab1))]$V1) ]$ID
    cc1<-cc1[-a1,]$ccc
    
    if((min(cc1,na.rm = TRUE)/ab1>1.5 && min(cc1,na.rm = TRUE)/ab1 <3 && Q1-ab1>s1)){
      
      col_faible[[1]]<-A[annee_ref == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>1.5 && ab3/max(cc3,na.rm = TRUE)<3  && ab3-Q3>s1)){
      
      col_faible[[2]]<-A[annee_ref == cc[a3,]$annee]$ID
    }
    
    if((min(cc1,na.rm = TRUE)/ab1>3 && Q1-ab1>s1)){
      
      col_fort[[1]]<-A[annee_ref == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>3 && ab3-Q3>s1)){
      
      col_fort[[2]]<-A[annee_ref == cc[a3,]$annee]$ID
    }
    
    A[,':='(ID=NULL)]
    if(length(col_fort)!=0 && length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(c("numcom","numdep","annee_ref","annee_source","cheptel","valeur","unites","source","numreg"),color=styleRow(col_fort,"red"))%>%
        formatStyle(c("numcom","numdep","annee_ref","annee_source","cheptel","valeur","unites","source","numreg"),color=styleRow(col_faible,"blue"))
      
    }else if(length(col_fort)!=0){
      datatable(A)%>%
        formatStyle(c("numcom","numdep","annee_ref","annee_source","cheptel","valeur","unites","source","numreg"),color=styleRow(col_fort,"red"))
      
      
    }else if(length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(c("numcom","numdep","annee_ref","annee_source","cheptel","valeur","unites","source","numreg"),color=styleRow(col_faible,"blue"))
      
    }else{
      datatable(A)
    }
  })
  
  #data_correlation_cheptel_saa-----------------------
  
  data_correlation_cheptel_saa<-eventReactive(input$go3,{
    
    load("carte3.Rdata")
    correlation_cheptel_saa<-data.table(carte3@data)[,list(valeur_2008,valeur_2010,valeur_2012,valeur_2015,valeur_2018,valeur_2020)]
    
    if(length(correlation_cheptel_saa[is.na(valeur_2008)==TRUE]$valeur_2008)==length(correlation_cheptel_saa$valeur_2008)){
      correlation_cheptel_saa[,':='(conso_2008=NULL)]
    }
    if(length(correlation_cheptel_saa[is.na(valeur_2010)==TRUE]$valeur_2010)==length(correlation_cheptel_saa$valeur_2010)){
      correlation_cheptel_saa[,':='(conso_2010=NULL)]
    }
    if(length(correlation_cheptel_saa[is.na(valeur_2012)==TRUE]$valeur_2012)==length(correlation_cheptel_saa$valeur_2012)){
      correlation_cheptel_saa[,':='(conso_2012=NULL)]
    }
    if(length(correlation_cheptel_saa[is.na(valeur_2015)==TRUE]$valeur_2015)==length(correlation_cheptel_saa$valeur_2015)){
      correlation_cheptel_saa[,':='(conso_2015=NULL)]
    }
    if(length(correlation_cheptel_saa[is.na(valeur_2018)==TRUE]$valeur_2018)==length(correlation_cheptel_saa$valeur_2018)){
      correlation_cheptel_saa[,':='(conso_2018=NULL)]
    }
    if(length(correlation_cheptel_saa[is.na(valeur_2020)==TRUE]$valeur_2020)==length(correlation_cheptel_saa$valeur_2020)){
      correlation_cheptel_saa[,':='(conso_2020=NULL)]
    }
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corr<-cor(correlation_cheptel_saa,use="pairwise.complete.obs")
    p.mat<-cor_pmat(correlation_cheptel_saa)
    corrplot(corr,
             col=col(200),
             method="color",
             title = title(main=str_glue("Correlogramme pour cheptel={input$select3}"),cex.main=0.6,line=-1,adj=0.7),
             addCoef.col = "black",
             type = "lower",
             outline.col = "white",
             p.mat = p.mat,
             sig.level = 0.01,
             tl.srt=90,
             tl.cex=0.8,
             tl.col = "red",
             diag = TRUE)
  })
  #data_cheptel_saa_reg--------------------------------
  data_cheptel_saa_reg<-eventReactive(input$go33,{
    load("carte3.Rdata")
    
    DD<-data.frame(carte3@data)
    g<-ggplot(data=DD,aes(x=DD[,str_glue("valeur_{input$select33}")],y=DD[,str_glue("valeur_{input$select333}")],color=Etat))+
      geom_point(aes(text=paste("numcom:",
                                DD[,"INSEE_COM"],
                                "<br>",
                                str_glue("valeur_{input$select33}:"),
                                DD[,str_glue("valeur_{input$select33}")],
                                "<br>",
                                str_glue("valeur_{input$select333}:"),
                                DD[,str_glue("valeur_{input$select333}")],
                                "<br>",
                                "Etat:",
                                DD[,"Etat"]
      )))+
      stat_smooth(method = "rlm", col = "black")+
      labs(title=str_glue("Graphe de regression lineaire de cheptel_saa pour {input$select33}-{input$select333}"),x=str_glue("valeur de {input$select33}"),y=str_glue("valeur de {input$select333}"))
    
    ggplotly(g,tooltip = "text")
    
    
    
  })
  #data_cheptel_saa_histo()----------------------------------------------
  
  data_cheptel_saa_histo<-eventReactive(input$go3,{
    load("carte3.Rdata")
    D<-data.table(annee=cheptel_annee[cheptel==input$select3]$annee_ref,valeur=as.numeric())
    for (i in D$annee){
      #D[i,]$conso<-sum(tc_conso_corrigees[annee_ref==D[i,]$annee & snap==as.numeric(input$select1) & napfue==as.numeric(input$select11)]$conso)
      D[annee==i]$valeur<-sum(cheptel_saa[annee_ref==i & cheptel==input$select3]$valeur,na.rm = TRUE)
    }
    ggplot(data=D,aes(x=as.character(annee),y=valeur,fill=as.character(annee),tooltip = valeur ))+geom_histogram_interactive(stat='identity',position = 'dodge')+labs(title="Histogramme de la valeur total des communes",x="annee",y="valeur en tetes",fill="annee")+theme(axis.text.x = element_text(face="bold",size=10,angle = 90))+geom_text(aes(label=as.integer(valeur)),position = position_dodge(.9), vjust = "top") 
    
  })
  
  #data_w_conso_install()--------------------------
  data_w_conso_install<-eventReactive(input$go4,{
    
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    #Au debut telechargement des données
    W_conso_install<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT x.* FROM agricole_test.w_conso_install x WHERE x.combustible IN ('{input$select4}')"))))
    
    
    ##preparation carte 
    carte4<-Carte2
    carte4_data<-data.table(gid=str_split(carte4@data$gid, pattern = "_",simplify = TRUE)[,2])
    
    func_etat<-function(a,b,c,d,e){
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e)){
        return(NA)
      }
      cc<-c(a,b,c,d,e)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*1.5
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm = TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm = TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if((ab3/max(cc3,na.rm = TRUE)>3 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>3 && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>1.5 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>1.5 && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    func_install<-function(etab,install){
      if (is.na(install)){
        aa<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.W_conso_install WHERE an = 2008 AND combustible = '{input$select4}' AND code_etab = '{etab}' AND id_install is NULL "))))$conso_gj
        a<-sum(aa)
        bb<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.W_conso_install WHERE an = 2010 AND combustible = '{input$select4}' AND code_etab = '{etab}' AND id_install is NULL"))))$conso_gj
        b<-sum(bb)
        Cc<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.W_conso_install WHERE an = 2012 AND combustible = '{input$select4}' AND code_etab = '{etab}' AND id_install is NULL"))))$conso_gj
        c<-sum(Cc)
        dd<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.w_conso_install WHERE an = 2015 AND combustible = '{input$select4}' AND code_etab = '{etab}' AND id_install is NULL"))))$conso_gj
        d<-sum(dd)
        ee<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.w_conso_install WHERE an = 2018 AND combustible = '{input$select4}' AND code_etab = '{etab}' AND id_install is NULL"))))$conso_gj
        e<-sum(ee)
        
        if(length(aa)==0){
          a<-NA
        }
        if(length(bb)==0){
          b<-NA
        }
        if(length(Cc)==0){
          c<-NA
        }
        if(length(dd)==0){
          d<-NA
        }
        if(length(ee)==0){
          e<-NA
        }
      }else{
        aa<-W_conso_install[id_install==install & code_etab==etab & an==2008 & combustible==input$select4]$conso_gj
        a<-sum(aa)
        bb<-W_conso_install[id_install==install & code_etab==etab & an==2010 & combustible==input$select4 ]$conso_gj
        b<-sum(bb)
        Cc<-W_conso_install[id_install==install & code_etab==etab & an==2012 & combustible==input$select4 ]$conso_gj
        c<-sum(Cc)
        dd<-W_conso_install[id_install==install & code_etab==etab & an==2015 & combustible==input$select4 ]$conso_gj
        d<-sum(dd)
        ee<-W_conso_install[id_install==install & code_etab==etab & an==2018 & combustible==input$select4 ]$conso_gj
        e<-sum(ee)
        if(length(aa)==0){
          a<-NA
        }
        if(length(bb)==0){
          b<-NA
        }
        if(length(Cc)==0){
          c<-NA
        }
        if(length(dd)==0){
          d<-NA
        }
        if(length(ee)==0){
          e<-NA
        }
      }
      return(func_etat(a,b,c,d,e))
    }
    func_src_etat<-function(etab){
      D<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT distinct id_install FROM agricole_test.W_conso_install WHERE (an IN (2008,2010,2012,2015,2018)) AND combustible= '{input$select4}' AND code_etab='{etab}' "))))
      if (length(D$id_install)==0){
        return(NA)
      }
      D[,resultat:=mapply(func_install,etab,id_install)]
      if ("Aberrante Forte" %in% D$resultat){
        return("Aberrante Forte")
      }
      else if ("Aberrante Faible" %in% D$resultat){
        return("Aberrante Faible")
      }
      else if("Normal" %in% D$resultat){
        return("Normal")
      }
      else if("Null" %in% D$resultat){
        return("Null")
      }
      else{
        return(NA)
      }
      
      
    }
    
    carte4_data[,Etat:=mapply(func_src_etat,gid)]
    carte4$gid<-str_split(carte4$gid, pattern = "_",simplify = TRUE)[,2]
    carte4$Etat<-carte4_data$Etat
    
    
    ##leaflet 
    pal<-colorFactor("viridis",domain = carte4$Etat)
    l<-leaflet(carte4)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addCircleMarkers(stroke = FALSE,
                       radius = 5,
                       color="white",
                       fillOpacity =1 ,
                       fillColor = ~pal(Etat),
                       label=~gid,
                       layerId = ~gid,
                       
                       popup = paste0("<strong> code_etab: </strong>",
                                      carte4$gid,
                                      "<br>",
                                      "<strong> Etat: </strong>",
                                      carte4$Etat))%>%
      addMarkers(label=~gid,
                 group = "carte4",
                 icon = makeIcon(
                   iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                   iconWidth = 0.01,
                   iconHeight = 0.01
                 ))%>%
      addLegend(pal=pal,
                values = ~Etat,
                title = "Etat",
                opacity = 1)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "carte4",
                        options = searchFeaturesOptions(zoom = 50,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    esri <- grep("^CartoDB", providers, value = TRUE)
    esri<-esri[2]
    esri1<-grep("^Esri", providers, value = TRUE)
    esri1<-esri1[5]
    esri2<-c(esri1,esri)
    for (provider in esri2) {
      l <- l %>% addProviderTiles(provider, group = provider)
    }
    
    l%>%
      addLayersControl(baseGroups = names(esri2),
                       options = layersControlOptions(collapsed = TRUE),position = "topleft")
    
    
    
  })
  
  #data_table_w_conso_install()----------------------------------------------------
  
  data_table_w_conso_install<-eventReactive(input$mape_w_conso_install_marker_click,{
    w_conso_install<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT x.* FROM agricole_test.W_conso_install x WHERE (x.an IN (2008,2010,2012,2015,2018)) AND (x.combustible IN ('{input$select4}'))"))))
    mape_w_conso_install_marker_click_info <- input$mape_w_conso_install_marker_click
    rv_location$id <- mape_w_conso_install_marker_click_info$id
    func_val<-function(annee,install){
      if (is.na(install)){
        aa<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.W_conso_install WHERE an = {annee} AND combustible = '{input$select4}' AND code_etab = '{rv_location$id}' AND id_install is NULL "))))$conso_gj
        a<-sum(aa)
        if(length(aa)==0){
          a<-NA
        }
        return(a)
      }else{
        aa<-w_conso_install[id_install==install & code_etab==rv_location$id & an==annee & combustible==input$select4]$conso_gj
        a<-sum(aa)
        if(length(aa)==0){
          a<-NA
        }
        return(a)
      }
      
      
    }
    D<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT distinct id_install FROM agricole_test.W_conso_install WHERE (an IN (2008,2010,2012,2015,2018)) AND combustible= '{input$select4}' AND code_etab='{rv_location$id}' "))))
    if (length(D$id_install)!=0){
      D[,c("conso_gj_2008","conso_gj_2010","conso_gj_2012","conso_gj_2015","conso_gj_2018"):=list(mapply(func_val,2008,id_install),mapply(func_val,2010,id_install),mapply(func_val,2012,id_install),mapply(func_val,2015,id_install),mapply(func_val,2018,id_install))]
      DD<-D$id_install
      D[,id_install:=NULL]
      D%>%datatable(rownames = paste("install",DD))
    }else{
      D<-data.table(conso_gj_2008=numeric(),conso_gj_2010=numeric(),conso_gj_2012=numeric(),conso_gj_2015=numeric(),conso_gj_2018=numeric())
      datatable(D)
    }
    
    
  })
  #data_w_src_etab_emi()-----------------------
  data_w_src_etab_emi<-eventReactive(input$go5,{
    
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    ##Au debut telechargement puis fusionnement des etablissements
    w_src_etab_emi<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT x.* FROM agricole_test.w_src_etab_emi x WHERE (x.an IN (2008,2010,2012,2015,2018)) AND (x.id_polluant IN (19,49,101,102,111))"))))
    ##Preparation carte
    carte5<-Carte2
    carte5_data<-data.table(gid=str_split(carte5@data$gid, pattern = "_",simplify = TRUE)[,2])
    func<-function(x,annee){
      aa<-w_src_etab_emi[an==annee & id_polluant==input$select5 & code_etab==x]$val
      a<-sum(aa)
      if(length(aa)==0){
        a<-NA
      }
      return(a)
    }
    carte5_data[ , c("val_2008","val_2010","val_2012","val_2015","val_2018"):= list(mapply(func,gid,2008),mapply(func,gid,2010),mapply(func,gid,2012), mapply(func,gid,2015),mapply(func,gid,2018))]
    func_etat<-function(a,b,c,d,e){
      cc<-c(a,b,c,d,e)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*1.5
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm=TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm=TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e)){
        return(NA)
      }else if((ab3/max(cc3,na.rm = TRUE)>3 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>3 && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>1.5 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>1.5 && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    carte5_data[,Etat:=mapply(func_etat,val_2008,val_2010,val_2012,val_2015,val_2018)]
    
    
    carte5$val_2008<-carte5_data$val_2008
    carte5$val_2010<-carte5_data$val_2010
    carte5$val_2012<-carte5_data$val_2012
    carte5$val_2015<-carte5_data$val_2015
    carte5$val_2018<-carte5_data$val_2018
    carte5$Etat<-carte5_data$Etat
    carte5$gid<-str_split(carte5$gid, pattern = "_",simplify = TRUE)[,2]
    
    save(list=c("carte5","w_src_etab_emi"),file="carte5.Rdata",compress = F)
    
    #leaflet
    pal<-colorFactor("viridis",domain = carte5$Etat)
    l<-leaflet(carte5)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addCircleMarkers(stroke = FALSE,
                       radius = 5,
                       color="white",
                       fillOpacity =1 ,
                       fillColor = ~pal(Etat),
                       label=~gid,
                       layerId = ~gid,
                       
                       popup = paste0("<strong> code_etab: </strong>",
                                      carte5$gid,
                                      "<br>",
                                      "<strong> val_2008: </strong>",
                                      carte5$val_2008,
                                      "<br>",
                                      "<strong> val_2010: </strong>",
                                      carte5$val_2010,
                                      "<br>",
                                      "<strong> val_2012: </strong>",
                                      carte5$val_2012,
                                      "<br>",
                                      "<strong> val_2015: </strong>",
                                      carte5$val_2015,
                                      "<br>",
                                      "<strong> val_2018: </strong>",
                                      carte5$val_2018,
                                      "<br>",
                                      "<strong> Etat: </strong>",
                                      carte5$Etat))%>%
      addMarkers(label=~gid,
                 group = "carte5",
                 icon = makeIcon(
                   iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                   iconWidth = 0.01,
                   iconHeight = 0.01
                 ))%>%
      addLegend(pal=pal,
                values = ~Etat,
                title = "Etat",
                opacity = 1)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "carte5",
                        options = searchFeaturesOptions(zoom = 50,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    esri <- grep("^CartoDB", providers, value = TRUE)
    esri<-esri[2]
    esri1<-grep("^Esri", providers, value = TRUE)
    esri1<-esri1[5]
    esri2<-c(esri1,esri)
    for (provider in esri2) {
      l <- l %>% addProviderTiles(provider, group = provider)
    }
    
    l%>%
      addLayersControl(baseGroups = names(esri2),
                       options = layersControlOptions(collapsed = TRUE),position = "topleft")
    
    
    
  })
  
  #data_w_src_etab_emi_comparaison()------------------
  data_w_src_etab_emi_comparaison<-eventReactive(input$go555,{
    load("carte5.Rdata")
    a<-input$select5555
    c<-carte5
    c@data<-carte5@data[c(names(c@data)[c(1:1)],str_glue("val_{a}"),"Etat")]
    names(c)[2]<-'val'
    pal<-colorBin("YlOrRd", domain = c$val)
    l<-leaflet(c)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addCircleMarkers(stroke = FALSE,
                       radius = 5,
                       color="white",
                       fillOpacity =1 ,
                       fillColor = ~pal(val),
                       label=~gid,
                       layerId = ~gid,
                       
                       popup = paste0("<strong> code_etab: </strong>",
                                      c$gid,
                                      "<br>",
                                      "<strong> val: </strong>",
                                      c$val))%>%
      addMarkers(label=~gid,
                 group = "c",
                 icon = makeIcon(
                   iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                   iconWidth = 0.01,
                   iconHeight = 0.01
                 ))%>%
      addLegend(pal=pal,
                values = ~val,
                title = "valeur",
                opacity = 1)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "c",
                        options = searchFeaturesOptions(zoom = 50,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    esri <- grep("^CartoDB", providers, value = TRUE)
    esri<-esri[2]
    esri1<-grep("^Esri", providers, value = TRUE)
    esri1<-esri1[5]
    esri2<-c(esri1,esri)
    for (provider in esri2) {
      l <- l %>% addProviderTiles(provider, group = provider)
    }
    
    l%>%
      addLayersControl(baseGroups = names(esri2),
                       options = layersControlOptions(collapsed = TRUE),position = "topleft")
    
  })
  #data_graphe_w_src_etab_emi()--------------------
  
  data_graphe_w_src_etab_emi<-eventReactive(input$mape_w_src_etab_emi_marker_click,{
    mape_w_src_etab_emi_marker_click_info <- input$mape_w_src_etab_emi_marker_click
    rv_location$id <- mape_w_src_etab_emi_marker_click_info$id
    load("carte5.Rdata")
    h1<-data.table(carte5@data)[gid==rv_location$id]$val_2008
    h2<-data.table(carte5@data)[gid==rv_location$id]$val_2010
    h3<-data.table(carte5@data)[gid==rv_location$id]$val_2012
    h4<-data.table(carte5@data)[gid==rv_location$id]$val_2015
    h5<-data.table(carte5@data)[gid==rv_location$id]$val_2018
    h<-c(h1,h2,h3,h4,h5)
    hh<-c(2008,2010,2012,2015,2018)
    hhh<-data.table(numcom=rv_location$id,val=h,annee=hh)
    ggplot(hhh,aes(x=annee,y=val,color=annee))+geom_line()+geom_point()
    
    
  })
  #data_table_w_src_etab_emi()----------------------------------
  
  data_table_w_src_etab_emi<-eventReactive(input$mape_w_src_etab_emi_marker_click,{
    load("carte5.Rdata")
    mape_w_src_etab_emi_marker_click_info <- input$mape_w_src_etab_emi_marker_click
    rv_location$id <- mape_w_src_etab_emi_marker_click_info$id
    A<-w_src_etab_emi[id_polluant==input$select5 & code_etab==rv_location$id]
    tc_copie<-A
    tc_copie[,ID := .I]
    doublons<-which(duplicated(tc_copie$an))
    annee<-tc_copie$an
    if (length(doublons)!=0){
      annee<-tc_copie[-doublons]$an}
    cc<-data.table(val=0,annee=annee)
    for (i in cc$annee){
      cc[annee==i]$val<-sum(tc_copie[an==i]$val,na.rm = TRUE)
      
    }
    
    col_fort<-c()
    col_faible<-c()
    
    ccc<-cc$val
    Q1<-quantile(ccc,0.25,na.rm=TRUE)
    Q3<-quantile(ccc,0.75,na.rm=TRUE)
    s1<-(Q3-Q1)*1.5
    ab3<- max(ccc-Q3,na.rm=TRUE)+Q3
    ab1<- -max(Q1-ccc,na.rm=TRUE)+Q1
    cc3<-data.table(ccc)
    cc3[, ID := .I]
    a3<-cc3[abs(ccc-ab3)== min(cc3[,.(abs(ccc-ab3))]$V1) ]$ID
    cc3<-cc3[-a3,]$ccc
    
    cc1<-data.table(ccc)
    cc1[, ID := .I]
    a1<-cc1[abs(ccc-ab1)== min(cc1[,.(abs(ccc-ab1))]$V1) ]$ID
    cc1<-cc1[-a1,]$ccc
    
    if((min(cc1,na.rm = TRUE)/ab1>1.5 && min(cc1,na.rm = TRUE)/ab1 <3 && Q1-ab1>s1)){
      
      col_faible[[1]]<-A[an == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>1.5 && ab3/max(cc3,na.rm = TRUE)<3  && ab3-Q3>s1)){
      
      col_faible[[2]]<-A[an == cc[a3,]$annee]$ID
    }
    
    if((min(cc1,na.rm = TRUE)/ab1>3 && Q1-ab1>s1)){
      
      col_fort[[1]]<-A[an== cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>3 && ab3-Q3>s1)){
      
      col_fort[[2]]<-A[an == cc[a3,]$annee]$ID
    }
    
    A[,':='(ID=NULL)]
    if(length(col_fort)!=0 && length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(c("id","id_version","code_etab","an","id_polluant","code_methode","val","masse_accidentelle","fiche_calcul","commentaire"),color=styleRow(col_fort,"red"))%>%
        formatStyle(c("id","id_version","code_etab","an","id_polluant","code_methode","val","masse_accidentelle","fiche_calcul","commentaire"),color=styleRow(col_faible,"blue"))
      
    }else if(length(col_fort)!=0){
      datatable(A)%>%
        formatStyle(c("id","id_version","code_etab","an","id_polluant","code_methode","val","masse_accidentelle","fiche_calcul","commentaire"),color=styleRow(col_fort,"red"))
      
      
    }else if(length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(c("id","id_version","code_etab","an","id_polluant","code_methode","val","masse_accidentelle","fiche_calcul","commentaire"),color=styleRow(col_faible,"blue"))
      
    }else{
      datatable(A)
    }
  })
  
  #data_w_src_etab_emi_reg()--------------------------------
  
  data_w_src_etab_emi_reg<-eventReactive(input$go55,{
    load("carte5.Rdata")
    
    DD<-data.frame(carte5@data)
    g<-ggplot(data=DD,aes(x=DD[,str_glue("val_{input$select55}")],y=DD[,str_glue("val_{input$select555}")],color=Etat))+
      geom_point(aes(text=paste("code_etab:",
                                DD[,"gid"],
                                "<br>",
                                str_glue("val_{input$select55}:"),
                                DD[,str_glue("val_{input$select55}")],
                                "<br>",
                                str_glue("val_{input$select555}:"),
                                DD[,str_glue("val_{input$select555}")],
                                "<br>",
                                "Etat:",
                                DD[,"Etat"]
      )))+
      stat_smooth(method = "rlm", col = "black")+
      labs(title=str_glue("Graphe de regression lineaire robuste de w_src_etab_emi pour {input$select55}-{input$select555}"),x=str_glue("val de {input$select55}"),y=str_glue("val de {input$select555}"))
    
    ggplotly(g,tooltip = "text")
    
    
    
  })
  
  #data_correlation_w_src_etab_emi()-----------------------
  
  data_correlation_w_src_etab_emi<-eventReactive(input$go5,{
    
    load("carte5.Rdata")
    correlation_w_src_etab_emi<-data.table(carte5@data)[,list(val_2008,val_2010,val_2012,val_2015,val_2018)]
    
    if(length(correlation_w_src_etab_emi[is.na(val_2008)==TRUE]$val_2008)==length(correlation_w_src_etab_emi$val_2008)){
      correlation_w_src_etab_emi[,':='(val_2008=NULL)]
    }
    if(length(correlation_w_src_etab_emi[is.na(val_2010)==TRUE]$val_2010)==length(correlation_w_src_etab_emi$val_2010)){
      correlation_w_src_etab_emi[,':='(val_2010=NULL)]
    }
    if(length(correlation_w_src_etab_emi[is.na(val_2012)==TRUE]$val_2012)==length(correlation_w_src_etab_emi$val_2012)){
      correlation_w_src_etab_emi[,':='(val_2012=NULL)]
    }
    if(length(correlation_w_src_etab_emi[is.na(val_2015)==TRUE]$val_2015)==length(correlation_w_src_etab_emi$val_2015)){
      correlation_w_src_etab_emi[,':='(val_2015=NULL)]
    }
    if(length(correlation_w_src_etab_emi[is.na(val_2018)==TRUE]$val_2018)==length(correlation_w_src_etab_emi$val_2018)){
      correlation_w_src_etab_emi[,':='(val_2018=NULL)]
    }
    
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corr<-cor(correlation_w_src_etab_emi,use="pairwise.complete.obs")
    p.mat<-cor_pmat(correlation_w_src_etab_emi)
    corrplot(corr,
             col=col(200),
             method="color",
             title = title(main=str_glue("Correlogramme pour id_polluant={input$select5}"),cex.main=0.6,line=-1,adj=0.7),
             addCoef.col = "black",
             type = "lower",
             outline.col = "white",
             p.mat = p.mat,
             sig.level = 0.01,
             tl.srt=90,
             tl.cex=0.8,
             tl.col = "red",
             diag = TRUE)
  })
  
  #data_w_src_etab_emi_histo()----------------------------------------------
  
  data_w_src_etab_emi_histo<-eventReactive(input$go5,{
    load("carte5.Rdata")
    D<-data.table(an=w_src_etab_emi_annee[id_polluant==input$select5]$an,val=as.numeric())
    for (i in D$an){
      D[an==i]$val<-sum(w_src_etab_emi[an==i & id_polluant==input$select5]$val,na.rm = TRUE)
    }
    ggplot(data=D,aes(x=as.character(an),y=val,fill=as.character(an),tooltip = val ))+geom_histogram_interactive(stat='identity',position = 'dodge')+labs(title="Histogramme de w_src_etab_emi_annee",x="an",y="val",fill="an")+theme(axis.text.x = element_text(face="bold",size=10,angle = 90))+geom_text(aes(label=as.integer(val)),position = position_dodge(.9), vjust = "top") 
    
  })
  
  #data_w_film_emi_install()--------------------------
  data_w_film_emi_install<-eventReactive(input$go6,{
    
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    #Au debut telechargement des données
    w_film_emi_install<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT x.* FROM agricole_test.w_film_emi_install x WHERE (x.an IN (2008,2010,2012,2015,2018)) AND (x.id_polluant IN ({input$select6})) AND (x.combustible IN ('{input$select66}'))"))))
    
    
    ##preparation carte 
    carte6<-Carte2
    carte6_data<-data.table(gid=str_split(carte6@data$gid, pattern = "_",simplify = TRUE)[,2])
    
    func_etat<-function(a,b,c,d,e){
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e)){
        return(NA)
      }
      cc<-c(a,b,c,d,e)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*1.5
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm = TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm = TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if((ab3/max(cc3,na.rm = TRUE)>3 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>3 && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>1.5 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>1.5 && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    func_install<-function(etab,install){
      if (is.na(install)){
        aa<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.W_conso_install WHERE an = 2008 AND id_polluant = {input$select6} AND combustible = '{input$select66}' AND code_etab = '{etab}' AND id_install is NULL "))))$val
        a<-sum(aa)
        bb<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.W_conso_install WHERE an = 2010 AND id_polluant = {input$select6} AND combustible = '{input$select66}' AND code_etab = '{etab}' AND id_install is NULL "))))$val
        b<-sum(bb)
        Cc<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.W_conso_install WHERE an = 2012 AND id_polluant = {input$select6} AND combustible = '{input$select66}' AND code_etab = '{etab}' AND id_install is NULL "))))$val
        c<-sum(Cc)
        dd<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.W_conso_install WHERE an = 2015 AND id_polluant = {input$select6} AND combustible = '{input$select66}' AND code_etab = '{etab}' AND id_install is NULL "))))$val
        d<-sum(dd)
        ee<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.W_conso_install WHERE an = 2018 AND id_polluant = {input$select6} AND combustible = '{input$select66}' AND code_etab = '{etab}' AND id_install is NULL "))))$val
        e<-sum(ee)
        
        if(length(aa)==0){
          a<-NA
        }
        if(length(bb)==0){
          b<-NA
        }
        if(length(Cc)==0){
          c<-NA
        }
        if(length(dd)==0){
          d<-NA
        }
        if(length(ee)==0){
          e<-NA
        }
      }else{
        aa<-w_film_emi_install[id_install==install & code_etab==etab & an==2008 & combustible==input$select66 & id_polluant==input$select6]$val
        a<-sum(aa)
        bb<-w_film_emi_install[id_install==install & code_etab==etab & an==2010 & combustible==input$select66 & id_polluant==input$select6]$val
        b<-sum(bb)
        Cc<-w_film_emi_install[id_install==install & code_etab==etab & an==2012 & combustible==input$select66 & id_polluant==input$select6]$val
        c<-sum(Cc)
        dd<-w_film_emi_install[id_install==install & code_etab==etab & an==2015 & combustible==input$select66 & id_polluant==input$select6]$val
        d<-sum(dd)
        ee<-w_film_emi_install[id_install==install & code_etab==etab & an==2018 & combustible==input$select66 & id_polluant==input$select6]$val
        e<-sum(ee)
        if(length(aa)==0){
          a<-NA
        }
        if(length(bb)==0){
          b<-NA
        }
        if(length(Cc)==0){
          c<-NA
        }
        if(length(dd)==0){
          d<-NA
        }
        if(length(ee)==0){
          e<-NA
        }
      }
      return(func_etat(a,b,c,d,e))
    }
    func_src_etat<-function(etab){
      D<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT distinct id_install FROM agricole_test.w_film_emi_install WHERE (an IN (2008,2010,2012,2015,2018)) AND combustible= '{input$select66}' AND code_etab='{etab}' AND id_polluant= {input$select6} "))))
      if (length(D$id_install)==0){
        return(NA)
      }
      D[,resultat:=mapply(func_install,etab,id_install)]
      if ("Aberrante Forte" %in% D$resultat){
        return("Aberrante Forte")
      }
      else if ("Aberrante Faible" %in% D$resultat){
        return("Aberrante Faible")
      }
      else if("Normal" %in% D$resultat){
        return("Normal")
      }
      else if("Null" %in% D$resultat){
        return("Null")
      }
      else{
        return(NA)
      }
      
      
    }
    
    carte6_data[,Etat:=mapply(func_src_etat,gid)]
    carte6$gid<-str_split(carte6$gid, pattern = "_",simplify = TRUE)[,2]
    carte6$Etat<-carte6_data$Etat
    
    
    ##leaflet 
    pal<-colorFactor("viridis",domain = carte6$Etat)
    l<-leaflet(carte6)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addCircleMarkers(stroke = FALSE,
                       radius = 5,
                       color="white",
                       fillOpacity =1 ,
                       fillColor = ~pal(Etat),
                       label=~gid,
                       layerId = ~gid,
                       
                       popup = paste0("<strong> code_etab: </strong>",
                                      carte6$gid,
                                      "<br>",
                                      "<strong> Etat: </strong>",
                                      carte6$Etat))%>%
      addMarkers(label=~gid,
                 group = "carte6",
                 icon = makeIcon(
                   iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
                   iconWidth = 0.01,
                   iconHeight = 0.01
                 ))%>%
      addLegend(pal=pal,
                values = ~Etat,
                title = "Etat",
                opacity = 1)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "carte6",
                        options = searchFeaturesOptions(zoom = 50,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    esri <- grep("^CartoDB", providers, value = TRUE)
    esri<-esri[2]
    esri1<-grep("^Esri", providers, value = TRUE)
    esri1<-esri1[5]
    esri2<-c(esri1,esri)
    for (provider in esri2) {
      l <- l %>% addProviderTiles(provider, group = provider)
    }
    
    l%>%
      addLayersControl(baseGroups = names(esri2),
                       options = layersControlOptions(collapsed = TRUE),position = "topleft")
    
    
    
  })
  
  #data_table_w_film_emi_install()----------------------------------------------------
  
  data_table_w_film_emi_install<-eventReactive(input$mape_w_film_emi_install_marker_click,{
    w_film_emi_install<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT x.* FROM agricole_test.w_film_emi_install x WHERE (x.an IN (2008,2010,2012,2015,2018)) AND (x.combustible IN ('{input$select66}')) AND (x.id_polluant IN ({input$select6}))"))))
    mape_w_film_emi_install_marker_click_info <- input$mape_w_film_emi_install_marker_click
    rv_location$id <- mape_w_film_emi_install_marker_click_info$id
    func_val<-function(annee,install){
      if (is.na(install)){
        aa<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.w_film_emi_install WHERE an = {annee} AND combustible = '{input$select66}' AND AND (x.id_polluant IN ({input$select6})) AND code_etab = '{rv_location$id}' AND id_install is NULL "))))$val
        a<-sum(aa)
        if(length(aa)==0){
          a<-NA
        }
        return(a)
      }else{
        aa<-w_film_emi_install[id_install==install & code_etab==rv_location$id & an==annee & combustible==input$select66 & id_polluant==input$select6]$val
        a<-sum(aa)
        if(length(aa)==0){
          a<-NA
        }
        return(a)
      }
      
      
    }
    D<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT distinct id_install FROM agricole_test.w_film_emi_install WHERE (an IN (2008,2010,2012,2015,2018)) AND combustible= '{input$select66}' AND code_etab='{rv_location$id}' AND id_polluant = {input$select6} "))))
    if (length(D$id_install)!=0){
      D[,c("val_2008","val_2010","val_2012","val_2015","val_2018"):=list(mapply(func_val,2008,id_install),mapply(func_val,2010,id_install),mapply(func_val,2012,id_install),mapply(func_val,2015,id_install),mapply(func_val,2018,id_install))]
      DD<-D$id_install
      D[,id_install:=NULL]
      D%>%datatable(rownames = paste("install",DD))
    }else{
      D<-data.table(val_2008=numeric(),val_2010=numeric(),val_2012=numeric(),val_2015=numeric(),val_2018=numeric())
      datatable(D)
    }
    
    
  })
  
  #data_rs_tmja()-----------------------
  data_rs_tmja<-eventReactive(input$go7,{
    
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    
    func_carte<-function(x){
      if (x=="02"){
        return(Carte_02)
      }else if(x=="59"){
        return(Carte_59)
      }else if(x=="60"){
        return(Carte_60)
      }else if(x=="62"){
        return(Carte_62)
      }else if(x=="80"){
        return(Carte_80)
      }
    }
    rs<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT t1.id,t1.tmja2008_bourg,t2.tmja2010_bourg,t3.tmja2012_bourg,t4.tmja2015_bourg,t5.tmja2018_bourg,t1.pctpl2008_bourg,t2.pctpl2010_bourg,t3.pctpl2012_bourg,t4.pctpl2015_bourg,t5.pctpl2018_bourg from agricole_test.rs_{input$select7}_tmja_2008_2010 t1 inner join agricole_test.rs_{input$select7}_tmja_2008_2010 t2 on t1.id=t2.id inner join agricole_test.rs_{input$select7}_tmja_join_r_zone_2012_hdf t3 on t2.id=t3.id inner join agricole_test.rs_{input$select7}_tmja_join_r_zone_2015_hdf t4 on t3.id=t4.id inner join agricole_test.rs_{input$select7}_tmja_join_r_zone_2018_hdf t5 on t4.id=t5.id"))))
    
    ##Preparation carte
    carte_rs<-func_carte(input$select7)
    T<-data.frame(rs)
    idx <- match(carte_rs@data$id, rs$id)
    carte_rs_data<-data.table(T[idx,c(colnames(rs)[-1])])
    func_etat<-function(a,b,c,d,e){
      cc<-c(a,b,c,d,e)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*(1.5)
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm=TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm=TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e)){
        return(NA)
      }else if((ab3/max(cc3,na.rm = TRUE)>3 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>3 && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>1.5 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>1.5 && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    
    carte_rs$tmja2008_bourg<-carte_rs_data$tmja2008_bourg
    carte_rs$tmja2010_bourg<-carte_rs_data$tmja2010_bourg
    carte_rs$tmja2012_bourg<-carte_rs_data$tmja2012_bourg
    carte_rs$tmja2015_bourg<-carte_rs_data$tmja2015_bourg
    carte_rs$tmja2018_bourg<-carte_rs_data$tmja2018_bourg
    
    carte_rs$pctpl2008_bourg<-carte_rs_data$pctpl2008_bourg
    carte_rs$pctpl2010_bourg<-carte_rs_data$pctpl2010_bourg
    carte_rs$pctpl2012_bourg<-carte_rs_data$pctpl2012_bourg
    carte_rs$pctpl2015_bourg<-carte_rs_data$pctpl2015_bourg
    carte_rs$pctpl2018_bourg<-carte_rs_data$pctpl2018_bourg
    
    
    if (input$select77=="tmja"){
      carte_rs_data[,Etat:=mapply(func_etat,tmja2008_bourg,tmja2010_bourg,tmja2012_bourg,tmja2015_bourg,tmja2018_bourg)]
      carte_rs$Etat<-carte_rs_data$Etat
      pal<-colorFactor("viridis", domain = carte_rs$Etat)
      
      leaflet(carte_rs)%>%
        setView(lat=50,lng=2.74,zoom=7)%>%
        addTiles()%>%
        addPolylines(fillOpacity =0.5,
                     weight = 1,
                     color = ~pal(Etat),
                     label=~id,
                     layerId = ~id,
                     popup = paste0("<strong> route: </strong>",
                                    carte_rs$id,
                                    "<br>",
                                    "<strong> tmja2008_bourg: </strong>",
                                    carte_rs$tmja2008_bourg,
                                    "<br>",
                                    "<strong> tmja2010_bourg: </strong>",
                                    carte_rs$tmja2010_bourg,
                                    "<br>",
                                    "<strong> tmja2012_bourg: </strong>",
                                    carte_rs$tmja2012_bourg,
                                    "<br>",
                                    "<strong> tmja2015_bourg: </strong>",
                                    carte_rs$tmja2015_bourg,
                                    "<br>",
                                    "<strong> tmja2018_bourg: </strong>",
                                    carte_rs$tmja2018_bourg,
                                    "<br>",
                                    "<strong> Etat: </strong>",
                                    carte_rs$Etat
                     ))%>%
        addLegend(pal=pal,
                  values = ~Etat,
                  title = "Etat",
                  opacity = 1)%>%
        addProviderTiles(providers$CartoDB.Positron)%>%
        addResetMapButton()%>%
        addSearchFeatures(targetGroups = "carte_rs",
                          options = searchFeaturesOptions(zoom = 12,
                                                          openPopup = TRUE,
                                                          firstTipSubmit = TRUE,
                                                          autoCollapse = TRUE,
                                                          hideMarkerOnCollapse=TRUE))
      
      
    }else if(input$select77=="pctpl"){
      carte_rs_data[,Etat:=mapply(func_etat,pctpl2008_bourg,pctpl2010_bourg,pctpl2012_bourg,pctpl2015_bourg,pctpl2018_bourg)]
      carte_rs$Etat<-carte_rs_data$Etat
      pal<-colorFactor("viridis", domain = carte_rs$Etat)
      
      leaflet(carte_rs)%>%
        setView(lat=50,lng=2.74,zoom=7)%>%
        addTiles()%>%
        addPolylines(fillOpacity =0.5,
                     weight = 1,
                     color = ~pal(Etat),
                     label=~id,
                     layerId = ~id,
                     popup = paste0("<strong> route: </strong>",
                                    carte_rs$id,
                                    "<br>",
                                    "<strong> tmja2008_bourg: </strong>",
                                    carte_rs$pctpl2008_bourg,
                                    "<br>",
                                    "<strong> tmja2010_bourg: </strong>",
                                    carte_rs$pctpl2010_bourg,
                                    "<br>",
                                    "<strong> tmja2012_bourg: </strong>",
                                    carte_rs$pctpl2012_bourg,
                                    "<br>",
                                    "<strong> tmja2015_bourg: </strong>",
                                    carte_rs$pctpl2015_bourg,
                                    "<br>",
                                    "<strong> tmja2018_bourg: </strong>",
                                    carte_rs$pctpl2018_bourg,
                                    "<br>",
                                    "<strong> Etat: </strong>",
                                    carte_rs$Etat
                     ))%>%
        addLegend(pal=pal,
                  values = ~Etat,
                  title = "Etat",
                  opacity = 1)%>%
        addProviderTiles(providers$CartoDB.Positron)%>%
        addResetMapButton()%>%
        addSearchFeatures(targetGroups = "carte_rs",
                          options = searchFeaturesOptions(zoom = 12,
                                                          openPopup = TRUE,
                                                          firstTipSubmit = TRUE,
                                                          autoCollapse = TRUE,
                                                          hideMarkerOnCollapse=TRUE))
    }
    
  })
  #data_table_rs_tmja()----------------------------------
  
  data_table_rs_tmja<-eventReactive(input$mape_a_tc_conso_corrigees_line_click,{
    load("carte9.Rdata")
    mape_a_tc_conso_corrigees_shape_click_info <- input$mape_a_tc_conso_corrigees_shape_click
    rv_location$id <- mape_a_tc_conso_corrigees_shape_click_info$id
    A<-a_tc_conso_corrigees[snap==input$select9 & napfue==input$select99 & numcom==rv_location$id]
    tc_copie<-A
    tc_copie[,ID := .I]
    doublons<-which(duplicated(tc_copie$annee_ref))
    annee<-tc_copie$annee_ref
    if (length(doublons)!=0){
      annee<-tc_copie[-doublons]$annee_ref}
    cc<-data.table(conso=0,annee=annee)
    for (i in cc$annee){
      cc[annee==i]$conso<-sum(tc_copie[annee_ref==i]$conso,na.rm = TRUE)
      
    }
    
    col_fort<-c()
    col_faible<-c()
    
    ccc<-cc$conso
    Q1<-quantile(ccc,0.25,na.rm=TRUE)
    Q3<-quantile(ccc,0.75,na.rm=TRUE)
    s1<-(Q3-Q1)*(1.5+input$seuil_9)
    ab3<- max(ccc-Q3,na.rm=TRUE)+Q3
    ab1<- -max(Q1-ccc,na.rm=TRUE)+Q1
    cc3<-data.table(ccc)
    cc3[, ID := .I]
    a3<-cc3[abs(ccc-ab3)== min(cc3[,.(abs(ccc-ab3))]$V1) ]$ID
    cc3<-cc3[-a3,]$ccc
    
    cc1<-data.table(ccc)
    cc1[, ID := .I]
    a1<-cc1[abs(ccc-ab1)== min(cc1[,.(abs(ccc-ab1))]$V1) ]$ID
    cc1<-cc1[-a1,]$ccc
    
    if((min(cc1,na.rm = TRUE)/ab1>(1.5+input$seuil_9) && min(cc1,na.rm = TRUE)/ab1 <(3+input$seuil_9) && Q1-ab1>s1)){
      
      col_faible[[1]]<-A[annee_ref == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>(1.5+input$seuil_9) && ab3/max(cc3,na.rm = TRUE)<(3+input$seuil_9)  && ab3-Q3>s1)){
      
      col_faible[[2]]<-A[annee_ref == cc[a3,]$annee]$ID
    }
    
    if((min(cc1,na.rm = TRUE)/ab1>(3+input$seuil_9) && Q1-ab1>s1)){
      
      col_fort[[1]]<-A[annee_ref == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>(3+input$seuil_9) && ab3-Q3>s1)){
      
      col_fort[[2]]<-A[annee_ref == cc[a3,]$annee]$ID
    }
    
    A[,':='(ID=NULL)]
    if(length(col_fort)!=0 && length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(names(a_tc_conso_corrigees),color=styleRow(col_fort,"red"))%>%
        formatStyle(names(a_tc_conso_corrigees),color=styleRow(col_faible,"blue"))
      
    }else if(length(col_fort)!=0){
      datatable(A)%>%
        formatStyle(names(a_tc_conso_corrigees),color=styleRow(col_fort,"red"))
      
      
    }else if(length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(names(a_tc_conso_corrigees),color=styleRow(col_faible,"blue"))
      
    }else{
      datatable(A)
    }
  })
  # data_r_tc_conso_corrigees--------------------------------------
  data_r_tc_conso_corrigees<-eventReactive(input$go8,{
    
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    ## telechargement du fichier
    r_tc_conso_corrigees<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT x.* FROM agricole_test.r_tc_conso_corrigees x WHERE x.napfue IN ('{input$select8}')"))))
    communes_fusionnees<-data.table(dbGetQuery(conn=base,paste0('SELECT x.* FROM agricole_test.communes_fusionnees x WHERE x.annee_ref IN (2020)')))
    setnames(r_tc_conso_corrigees,"numcom","numcom_ancien")
    r_tc_conso_corrigees[communes_fusionnees, numcom_ancien:=numcom,on = .(numcom_ancien)]
    setnames(r_tc_conso_corrigees,"numcom_ancien","numcom")
    ##Preparation carte
    carte8<-Carte
    carte8_data<-data.table(carte8@data)
    func<-function(x,annee){
      aa<-r_tc_conso_corrigees[annee_ref==annee & numcom==x]$conso_climat_reel
      a<-sum(aa)
      if(length(aa)==0){
        a<-NA
      }
      return(a)
    }
    carte8_data[ , c("conso_2008","conso_2010","conso_2012","conso_2015","conso_2018"):= list(mapply(func,INSEE_COM,2008),mapply(func,INSEE_COM,2010),mapply(func,INSEE_COM,2012), mapply(func,INSEE_COM,2015),mapply(func,INSEE_COM,2018))]
    func_etat<-function(a,b,c,d,e){
      cc<-c(a,b,c,d,e)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*1.5
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm=TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm=TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e)){
        return(NA)
      }else if((ab3/max(cc3,na.rm = TRUE)>3 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>3 && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>1.5 && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>1.5 && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    carte8_data[,Etat:=mapply(func_etat,conso_2008,conso_2010,conso_2012,conso_2015,conso_2018)]
    
    
    carte8$conso_2008<-carte8_data$conso_2008
    carte8$conso_2010<-carte8_data$conso_2010
    carte8$conso_2012<-carte8_data$conso_2012
    carte8$conso_2015<-carte8_data$conso_2015
    carte8$conso_2018<-carte8_data$conso_2018
    carte8$Etat<-carte8_data$Etat
    
    save(list=c("carte8","r_tc_conso_corrigees"),file="carte8.Rdata",compress = TRUE)
    
    #leaflet
    pal<-colorFactor("viridis",domain = carte8$Etat)
    leaflet(carte8)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  color="white",
                  fillOpacity =1 ,
                  fillColor = ~pal(Etat),
                  label=~INSEE_COM,
                  layerId = ~INSEE_COM,
                  highlightOptions =highlightOptions(
                    color="white",
                    weight=3,
                    fillColor = NA),
                  popup = paste0("<strong> Commune: </strong>",
                                 carte8$NOM_M_COM,
                                 "<br>",
                                 "<strong> Numero commune: </strong>",
                                 carte8$INSEE_COM,
                                 "<br>",
                                 "<strong> conso_2008: </strong>",
                                 carte8$conso_2008,
                                 "<br>",
                                 "<strong> conso_2010: </strong>",
                                 carte8$conso_2010,
                                 "<br>",
                                 "<strong> conso_2012: </strong>",
                                 carte8$conso_2012,
                                 "<br>",
                                 "<strong> conso_2015: </strong>",
                                 carte8$conso_2015,
                                 "<br>",
                                 "<strong> conso_2018: </strong>",
                                 carte8$conso_2018,
                                 "<br>",
                                 "<strong> Etat: </strong>",
                                 carte8$Etat),
                  group = "carte8")%>%
      addLegend(pal=pal,
                values = ~Etat,
                title = "Etat",
                opacity = 1)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "carte8",
                        options = searchFeaturesOptions(zoom = 12,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    
    
    
  })
  
  #data_graphe_r_tc_conso_corrigees()--------------------
  
  data_graphe_r_tc_conso_corrigees<-eventReactive(input$mape_r_tc_conso_corrigees_shape_click,{
    mape_r_tc_conso_corrigees_shape_click_info <- input$mape_r_tc_conso_corrigees_shape_click
    rv_location$id <- mape_r_tc_conso_corrigees_shape_click_info$id
    load("carte8.Rdata")
    h1<-data.table(carte8@data)[INSEE_COM==rv_location$id]$conso_2008
    h2<-data.table(carte8@data)[INSEE_COM==rv_location$id]$conso_2010
    h3<-data.table(carte8@data)[INSEE_COM==rv_location$id]$conso_2012
    h4<-data.table(carte8@data)[INSEE_COM==rv_location$id]$conso_2015
    h5<-data.table(carte8@data)[INSEE_COM==rv_location$id]$conso_2018
    h<-c(h1,h2,h3,h4,h5)
    hh<-c(2008,2010,2012,2015,2018)
    hhh<-data.table(numcom=rv_location$id,conso=h,annee=hh)
    ggplot(hhh,aes(x=annee,y=conso,color=annee))+geom_line()+geom_point()
    
    
  })
  
  #data_table_r_tc_conso_corrigees()----------------------------------
  
  data_table_r_tc_conso_corrigees<-eventReactive(input$mape_r_tc_conso_corrigees_shape_click,{
    load("carte8.Rdata")
    mape_r_tc_conso_corrigees_shape_click_info <- input$mape_r_tc_conso_corrigees_shape_click
    rv_location$id <- mape_r_tc_conso_corrigees_shape_click_info$id
    A<-r_tc_conso_corrigees[numcom==rv_location$id]
    tc_copie<-A
    tc_copie[,ID := .I]
    doublons<-which(duplicated(tc_copie$annee_ref))
    annee<-tc_copie$annee_ref
    if (length(doublons)!=0){
      annee<-tc_copie[-doublons]$annee_ref}
    cc<-data.table(valeur=0,annee=annee)
    for (i in cc$annee){
      cc[annee==i]$valeur<-sum(tc_copie[annee_ref==i]$valeur,na.rm = TRUE)
      
    }
    
    col_fort<-c()
    col_faible<-c()
    
    ccc<-cc$valeur
    Q1<-quantile(ccc,0.25,na.rm=TRUE)
    Q3<-quantile(ccc,0.75,na.rm=TRUE)
    s1<-(Q3-Q1)*1.5
    ab3<- max(ccc-Q3,na.rm=TRUE)+Q3
    ab1<- -max(Q1-ccc,na.rm=TRUE)+Q1
    cc3<-data.table(ccc)
    cc3[, ID := .I]
    a3<-cc3[abs(ccc-ab3)== min(cc3[,.(abs(ccc-ab3))]$V1) ]$ID
    cc3<-cc3[-a3,]$ccc
    
    cc1<-data.table(ccc)
    cc1[, ID := .I]
    a1<-cc1[abs(ccc-ab1)== min(cc1[,.(abs(ccc-ab1))]$V1) ]$ID
    cc1<-cc1[-a1,]$ccc
    
    if((min(cc1,na.rm = TRUE)/ab1>1.5 && min(cc1,na.rm = TRUE)/ab1 <3 && Q1-ab1>s1)){
      
      col_faible[[1]]<-A[annee_ref == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>1.5 && ab3/max(cc3,na.rm = TRUE)<3  && ab3-Q3>s1)){
      
      col_faible[[2]]<-A[annee_ref == cc[a3,]$annee]$ID
    }
    
    if((min(cc1,na.rm = TRUE)/ab1>3 && Q1-ab1>s1)){
      
      col_fort[[1]]<-A[annee_ref == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>3 && ab3-Q3>s1)){
      
      col_fort[[2]]<-A[annee_ref == cc[a3,]$annee]$ID
    }
    
    A[,':='(ID=NULL)]
    if(length(col_fort)!=0 && length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(c("annee_ref","numreg","numcom","numiris","napfue","usage","snap","achl","catl","chfl","typl","type_equipement","modele_equipement","conso_climat_reel","conso_climat_normal","gid"),color=styleRow(col_fort,"red"))%>%
        formatStyle(c("annee_ref","numreg","numcom","numiris","napfue","usage","snap","achl","catl","chfl","typl","type_equipement","modele_equipement","conso_climat_reel","conso_climat_normal","gid"),color=styleRow(col_faible,"blue"))
      
    }else if(length(col_fort)!=0){
      datatable(A)%>%
        formatStyle(c("annee_ref","numreg","numcom","numiris","napfue","usage","snap","achl","catl","chfl","typl","type_equipement","modele_equipement","conso_climat_reel","conso_climat_normal","gid"),color=styleRow(col_fort,"red"))
      
      
    }else if(length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(c("annee_ref","numreg","numcom","numiris","napfue","usage","snap","achl","catl","chfl","typl","type_equipement","modele_equipement","conso_climat_reel","conso_climat_normal","gid"),color=styleRow(col_faible,"blue"))
      
    }else{
      datatable(A)
    }
  })
  
  #data_a_tc_conso_corrigees()-----------------------
  data_a_tc_conso_corrigees<-eventReactive(input$go9,{
    
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    ##Au debut telechargement puis fusionnement des communes
    dbSendQuery(conn=base,paste0(str_glue("create temp table tc_cons as select * from agricole_test.a_tc_conso_corrigees tcc where snap in ('{input$select9}') AND napfue in ('{input$select99}')")))
    dbSendQuery(conn=base,paste0("create  temp table communes_fusionnees as select * from agricole_test.communes_fusionnees tcc where annee_ref = 2020 "))
    dbSendQuery(conn=base,paste0("ALTER TABLE communes_fusionnees RENAME COLUMN numcom to numcom_new"))
    dbSendQuery(conn=base,paste0("update tc_cons set numcom= numcom_new from communes_fusionnees where tc_cons.numcom=communes_fusionnees.numcom_ancien"))
    a_tc_conso_corrigees<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM tc_cons "))))
    ##Preparation carte
    carte9<-Carte
    carte9_data<-data.table(carte9@data)
    w <-data.table(dbGetQuery(conn = base, paste0("SELECT SUBSTRING(CONCAT(w1.numcom, w2.numcom, w3.numcom,w4.numcom,w5.numcom, w6.numcom),0,6) as numcom, conso_2008, conso_2010, conso_2012, conso_2015, conso_2018, conso_2020 FROM
(SELECT numcom,SUM(conso) AS conso_2008 FROM tc_cons  WHERE  annee_ref=2008   GROUP BY numcom) AS w1 FULL JOIN
(SELECT numcom,SUM(conso) AS conso_2010 FROM tc_cons  WHERE  annee_ref=2010   GROUP BY numcom) AS w2 ON w1.numcom=w2.numcom FULL JOIN
(SELECT numcom,SUM(conso) AS conso_2012 FROM tc_cons  WHERE  annee_ref=2012   GROUP BY numcom) AS w3 ON COALESCE(w1.numcom,w2.numcom)=w3.numcom FULL JOIN
(SELECT numcom,SUM(conso) AS conso_2015 FROM tc_cons  WHERE  annee_ref=2015   GROUP BY numcom) AS w4 ON COALESCE(w1.numcom,w2.numcom,w3.numcom)=w4.numcom FULL JOIN
(SELECT numcom,SUM(conso) AS conso_2018 FROM tc_cons  WHERE  annee_ref=2018   GROUP BY numcom) AS w5 ON COALESCE(w1.numcom,w2.numcom,w3.numcom,w4.numcom)=w5.numcom FULL JOIN
(SELECT numcom,SUM(conso) AS conso_2020 FROM tc_cons  WHERE  annee_ref=2020   GROUP BY numcom) AS w6 ON COALESCE(w1.numcom,w2.numcom,w3.numcom,w4.numcom,w5.numcom)=w6.numcom")))
    
    idx <- match(carte9@data$INSEE_COM , w$numcom)
    carte9_data <- data.table(w[idx, ])
    func_etat<-function(a,b,c,d,e,f){
      cc<-c(a,b,c,d,e,f)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*(1.5+input$seuil_9)
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm=TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm=TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e) & is.na(f)){
        return(NA)
      }else if((ab3/max(cc3,na.rm = TRUE)>(3+input$seuil_9) && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>(3+input$seuil_9) && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>(1.5+input$seuil_9) && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>(1.5+input$seuil_9) && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    carte9_data[,Etat:=mapply(func_etat,conso_2008,conso_2010,conso_2012,conso_2015,conso_2018,conso_2020)]
    
    
    carte9$conso_2008<-carte9_data$conso_2008
    carte9$conso_2010<-carte9_data$conso_2010
    carte9$conso_2012<-carte9_data$conso_2012
    carte9$conso_2015<-carte9_data$conso_2015
    carte9$conso_2018<-carte9_data$conso_2018
    carte9$conso_2020<-carte9_data$conso_2020
    carte9$Etat<-carte9_data$Etat
    save(list=c("carte9","a_tc_conso_corrigees"),file="carte9.Rdata",compress = F)
    
    #leaflet
    pal<-colorFactor("viridis",domain = carte9$Etat)
    leaflet(carte9)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  color="white",
                  fillOpacity =1 ,
                  fillColor = ~pal(Etat),
                  label=~INSEE_COM,
                  layerId = ~INSEE_COM,
                  highlightOptions =highlightOptions(
                    color="white",
                    weight=3,
                    fillColor = NA),
                  popup = paste0("<strong> Commune: </strong>",
                                 carte9$NOM_M_COM,
                                 "<br>",
                                 "<strong> Numero commune: </strong>",
                                 carte9$INSEE_COM,
                                 "<br>",
                                 "<strong> conso_2008: </strong>",
                                 carte9$conso_2008,
                                 "<br>",
                                 "<strong> conso_2010: </strong>",
                                 carte9$conso_2010,
                                 "<br>",
                                 "<strong> conso_2012: </strong>",
                                 carte9$conso_2012,
                                 "<br>",
                                 "<strong> conso_2015: </strong>",
                                 carte9$conso_2015,
                                 "<br>",
                                 "<strong> conso_2018: </strong>",
                                 carte9$conso_2018,
                                 "<br>",
                                 "<strong> conso_2020: </strong>",
                                 carte9$conso_2020,
                                 "<br>",
                                 "<strong> Etat: </strong>",
                                 carte9$Etat),
                  group = "carte9")%>%
      addLegend(pal=pal,
                values = ~Etat,
                title = "Etat",
                opacity = 1)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "carte9",
                        options = searchFeaturesOptions(zoom = 12,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    
    
    
  })
  
  #data_graphe_a_tc_conso_corrigees()--------------------
  
  data_graphe_a_tc_conso_corrigees<-eventReactive(input$mape_a_tc_conso_corrigees_shape_click,{
    mape_a_tc_conso_corrigees_shape_click_info <- input$mape_a_tc_conso_corrigees_shape_click
    rv_location$id <- mape_a_tc_conso_corrigees_shape_click_info$id
    load("carte9.Rdata")
    h1<-data.table(carte9@data)[INSEE_COM==rv_location$id]$conso_2008
    h2<-data.table(carte9@data)[INSEE_COM==rv_location$id]$conso_2010
    h3<-data.table(carte9@data)[INSEE_COM==rv_location$id]$conso_2012
    h4<-data.table(carte9@data)[INSEE_COM==rv_location$id]$conso_2015
    h5<-data.table(carte9@data)[INSEE_COM==rv_location$id]$conso_2018
    h6<-data.table(carte9@data)[INSEE_COM==rv_location$id]$conso_2020
    h<-c(h1,h2,h3,h4,h5,h6)
    hh<-c(2008,2010,2012,2015,2018,2020)
    hhh<-data.table(numcom=rv_location$id,conso=h,annee=hh)
    ggplot(hhh,aes(x=annee,y=conso,color=annee))+geom_line()+geom_point()
    
    
  })
  
  #data_table_a_tc_conso_corrigees()----------------------------------
  
  data_table_a_tc_conso_corrigees<-eventReactive(input$mape_a_tc_conso_corrigees_shape_click,{
    load("carte9.Rdata")
    mape_a_tc_conso_corrigees_shape_click_info <- input$mape_a_tc_conso_corrigees_shape_click
    rv_location$id <- mape_a_tc_conso_corrigees_shape_click_info$id
    A<-a_tc_conso_corrigees[snap==input$select9 & napfue==input$select99 & numcom==rv_location$id]
    tc_copie<-A
    tc_copie[,ID := .I]
    doublons<-which(duplicated(tc_copie$annee_ref))
    annee<-tc_copie$annee_ref
    if (length(doublons)!=0){
      annee<-tc_copie[-doublons]$annee_ref}
    cc<-data.table(conso=0,annee=annee)
    for (i in cc$annee){
      cc[annee==i]$conso<-sum(tc_copie[annee_ref==i]$conso,na.rm = TRUE)
      
    }
    
    col_fort<-c()
    col_faible<-c()
    
    ccc<-cc$conso
    Q1<-quantile(ccc,0.25,na.rm=TRUE)
    Q3<-quantile(ccc,0.75,na.rm=TRUE)
    s1<-(Q3-Q1)*(1.5+input$seuil_9)
    ab3<- max(ccc-Q3,na.rm=TRUE)+Q3
    ab1<- -max(Q1-ccc,na.rm=TRUE)+Q1
    cc3<-data.table(ccc)
    cc3[, ID := .I]
    a3<-cc3[abs(ccc-ab3)== min(cc3[,.(abs(ccc-ab3))]$V1) ]$ID
    cc3<-cc3[-a3,]$ccc
    
    cc1<-data.table(ccc)
    cc1[, ID := .I]
    a1<-cc1[abs(ccc-ab1)== min(cc1[,.(abs(ccc-ab1))]$V1) ]$ID
    cc1<-cc1[-a1,]$ccc
    
    if((min(cc1,na.rm = TRUE)/ab1>(1.5+input$seuil_9) && min(cc1,na.rm = TRUE)/ab1 <(3+input$seuil_9) && Q1-ab1>s1)){
      
      col_faible[[1]]<-A[annee_ref == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>(1.5+input$seuil_9) && ab3/max(cc3,na.rm = TRUE)<(3+input$seuil_9)  && ab3-Q3>s1)){
      
      col_faible[[2]]<-A[annee_ref == cc[a3,]$annee]$ID
    }
    
    if((min(cc1,na.rm = TRUE)/ab1>(3+input$seuil_9) && Q1-ab1>s1)){
      
      col_fort[[1]]<-A[annee_ref == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>(3+input$seuil_9) && ab3-Q3>s1)){
      
      col_fort[[2]]<-A[annee_ref == cc[a3,]$annee]$ID
    }
    
    A[,':='(ID=NULL)]
    if(length(col_fort)!=0 && length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(names(a_tc_conso_corrigees),color=styleRow(col_fort,"red"))%>%
        formatStyle(names(a_tc_conso_corrigees),color=styleRow(col_faible,"blue"))
      
    }else if(length(col_fort)!=0){
      datatable(A)%>%
        formatStyle(names(a_tc_conso_corrigees),color=styleRow(col_fort,"red"))
      
      
    }else if(length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(names(a_tc_conso_corrigees),color=styleRow(col_faible,"blue"))
      
    }else{
      datatable(A)
    }
  })
  
  #data_correlation_a_tc_conso_corrigees-----------------------
  
  data_correlation_a_tc_conso_corrigees<-eventReactive(input$go9,{
    
    load("carte9.Rdata")
    correlation_a_tc_conso_corrigees<-data.table(carte9@data)[,list(conso_2008,conso_2010,conso_2012,conso_2015,conso_2018,conso_2020)]
    
    if(length(correlation_a_tc_conso_corrigees[is.na(conso_2008)==TRUE]$conso_2008)==length(correlation_a_tc_conso_corrigees$conso_2008)){
      correlation_a_tc_conso_corrigees[,':='(conso_2008=NULL)]
    }
    if(length(correlation_a_tc_conso_corrigees[is.na(conso_2010)==TRUE]$conso_2010)==length(correlation_a_tc_conso_corrigees$conso_2010)){
      correlation_a_tc_conso_corrigees[,':='(conso_2010=NULL)]
    }
    if(length(correlation_a_tc_conso_corrigees[is.na(conso_2012)==TRUE]$conso_2012)==length(correlation_a_tc_conso_corrigees$conso_2012)){
      correlation_a_tc_conso_corrigees[,':='(conso_2012=NULL)]
    }
    if(length(correlation_a_tc_conso_corrigees[is.na(conso_2015)==TRUE]$conso_2015)==length(correlation_a_tc_conso_corrigees$conso_2015)){
      correlation_a_tc_conso_corrigees[,':='(conso_2015=NULL)]
    }
    if(length(correlation_a_tc_conso_corrigees[is.na(conso_2018)==TRUE]$conso_2018)==length(correlation_a_tc_conso_corrigees$conso_2018)){
      correlation_a_tc_conso_corrigees[,':='(conso_2018=NULL)]
    }
    if(length(correlation_a_tc_conso_corrigees[is.na(conso_2020)==TRUE]$conso_2020)==length(correlation_a_tc_conso_corrigees$conso_2020)){
      correlation_a_tc_conso_corrigees[,':='(conso_2020=NULL)]
    }
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corr<-cor(correlation_a_tc_conso_corrigees,use="pairwise.complete.obs")
    p.mat<-cor_pmat(correlation_a_tc_conso_corrigees)
    corrplot(corr,
             col=col(200),
             method="color",
             title = title(main=str_glue("Correlogramme pour snap={input$select9} et napfue = {input$select99}"),cex.main=0.6,line=-1,adj=0.7),
             addCoef.col = "black",
             type = "lower",
             outline.col = "white",
             p.mat = p.mat,
             sig.level = 0.01,
             tl.srt=90,
             tl.cex=0.8,
             tl.col = "red",
             diag = TRUE)
  })
  #data_a_tc_conso_corrigees_reg--------------------------------
  
  data_a_tc_conso_corrigees_reg<-eventReactive(input$go99,{
    load("carte9.Rdata")
    
    DD<-data.frame(carte9@data)
    g<-ggplot(data=DD,aes(x=DD[,str_glue("conso_{input$select999}")],y=DD[,str_glue("conso_{input$select9999}")],color=Etat))+
      geom_point(aes(text=paste("numcom:",
                                DD[,"INSEE_COM"],
                                "<br>",
                                str_glue("conso_{input$select999}:"),
                                DD[,str_glue("conso_{input$select999}")],
                                "<br>",
                                str_glue("conso_{input$select9999}:"),
                                DD[,str_glue("conso_{input$select9999}")],
                                "<br>",
                                "Etat:",
                                DD[,"Etat"]
      )))+
      stat_smooth(method = "rlm", col = "black")+
      labs(title=str_glue("Graphe de regression lineaire de declaration consommation pour {input$select999}-{input$select9999}"),x=str_glue("conso de {input$select999}"),y=str_glue("conso de {input$select9999}"))
    
    ggplotly(g,tooltip = "text")
    
    
    
  })
  
  #data_a_tc_conso_corrigees_comparaison()------------------
  data_a_tc_conso_corrigees_comparaison<-eventReactive(input$go999,{
    load("carte9.Rdata")
    a<-input$select99999
    c<-carte9
    c@data<-carte9@data[c(names(c@data)[c(1:17)],str_glue("conso_{a}"),"Etat")]
    names(c)[18]<-'conso'
    pal<-colorBin("YlOrRd", domain = c$conso)
    l<-leaflet(c)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  color="white",
                  fillOpacity =0.5 ,
                  fillColor = ~pal(conso),
                  label=~INSEE_COM,
                  layerId = ~INSEE_COM,
                  popup = paste0("<strong> Commune: </strong>",
                                 c$NOM_M_COM,
                                 "<br>",
                                 "<strong> Numero commune: </strong>",
                                 c$INSEE_COM,
                                 "<br>",
                                 "<strong> conso: </strong>",
                                 c$conso
                  ),
                  group = "c")%>%
      addLegend(pal=pal,
                values = ~conso,
                title = "conso",
                opacity = 0.5)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "c",
                        options = searchFeaturesOptions(zoom = 12,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    esri <- grep("^CartoDB", providers, value = TRUE)
    esri<-esri[2]
    esri1<-grep("^Esri", providers, value = TRUE)
    esri1<-esri1[5]
    esri2<-c(esri1,esri)
    for (provider in esri2) {
      l <- l %>% addProviderTiles(provider, group = provider)
    }
    
    l%>%
      addLayersControl(baseGroups = names(esri2),
                       options = layersControlOptions(collapsed = TRUE),position = "topleft")
    
  })
  #data_a_tc_conso_corrigees_histo()----------------------------------------------
  
  data_a_tc_conso_corrigees_histo<-eventReactive(input$go9,{
    load("carte9.Rdata")
    D<-data.table(annee=snap_napfue_annee_a_tc_conso_corrigees[snap==input$select9 & napfue==input$select99]$annee_ref,conso=as.numeric())
    for (i in D$annee){
      D[annee==i]$conso<-sum(a_tc_conso_corrigees[annee_ref==i & snap==input$select9 & napfue==input$select99]$conso,na.rm = TRUE)
    }
    ggplot(data=D,aes(x=as.character(annee),y=conso,fill=as.character(annee),tooltip = conso ))+geom_histogram_interactive(stat='identity',position = 'dodge')+labs(title="Histogramme de la consommation total des communes",x="annee",y="conso en kg",fill="annee")+theme(axis.text.x = element_text(face="bold",size=10,angle = 90))+geom_text(aes(label=as.integer(conso)),position = position_dodge(.9), vjust = "top") 
    
  })
  
  #data_cultures_saa_commune_pcit2()-----------------------
  data_cultures_saa_commune_pcit2<-eventReactive(input$go10,{
    
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    ##Au debut telechargement puis fusionnement des communes
    communes_fusionnees<-data.table(dbGetQuery(conn=base,paste0('SELECT x.* FROM agricole_test.communes_fusionnees x WHERE x.annee_ref IN (2020)')))
    x1<-print(gsub("'", "''", input$select10) )
    x2<-print(gsub("'", "''", input$select1010) )
    cultures_saa_commune_pcit2<-data.table(dbGetQuery(conn=base,paste0(str_glue("SELECT x.* FROM agricole_test.cultures_saa_commune_pcit2 x WHERE (x.culture_rga IN ('{x1}')) AND (x.culture IN ('{x2}'))"))))
    setnames(cultures_saa_commune_pcit2,"numcom","numcom_ancien")
    cultures_saa_commune_pcit2[communes_fusionnees, numcom_ancien:=numcom,on = .(numcom_ancien)]
    setnames(cultures_saa_commune_pcit2,"numcom_ancien","numcom")

    ##Preparation carte
    carte10<-Carte
    carte10_data<-data.table(carte10@data)
    func<-function(x,annee){
      aa<-cultures_saa_commune_pcit2[annee_ref==annee & numcom==x]$surface
      a<-sum(aa)
      if(length(aa)==0){
        a<-NA
      }
      return(a)
    }
    carte10_data[ , c("surface_2008","surface_2010","surface_2012","surface_2015","surface_2018","surface_2020"):= list(mapply(func,INSEE_COM,"2008"),mapply(func,INSEE_COM,"2010"),mapply(func,INSEE_COM,"2012"), mapply(func,INSEE_COM,"2015"),mapply(func,INSEE_COM,"2018"),mapply(func,INSEE_COM,"2020"))]
    func_etat<-function(a,b,c,d,e,f){
      cc<-c(a,b,c,d,e,f)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*(1.5+input$seuil_10)
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm=TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm=TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e) & is.na(f)){
        return(NA)
      }else if((ab3/max(cc3,na.rm = TRUE)>(3+input$seuil_10) && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>(3+input$seuil_10) && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>(1.5+input$seuil_10) && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>(1.5+input$seuil_10) && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    carte10_data[,Etat:=mapply(func_etat,surface_2008,surface_2010,surface_2012,surface_2015,surface_2018,surface_2020)]
    
    
    carte10$surface_2008<-carte10_data$surface_2008
    carte10$surface_2010<-carte10_data$surface_2010
    carte10$surface_2012<-carte10_data$surface_2012
    carte10$surface_2015<-carte10_data$surface_2015
    carte10$surface_2018<-carte10_data$surface_2018
    carte10$surface_2020<-carte10_data$surface_2020
    carte10$Etat<-carte10_data$Etat
    save(list=c("carte10","cultures_saa_commune_pcit2"),file="carte10.Rdata",compress = F)
    
    #leaflet
    pal<-colorFactor("viridis",domain = carte10$Etat)
    leaflet(carte10)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  color="white",
                  fillOpacity =1 ,
                  fillColor = ~pal(Etat),
                  label=~INSEE_COM,
                  layerId = ~INSEE_COM,
                  highlightOptions =highlightOptions(
                    color="white",
                    weight=3,
                    fillColor = NA),
                  popup = paste0("<strong> Commune: </strong>",
                                 carte10$NOM_M_COM,
                                 "<br>",
                                 "<strong> Numero commune: </strong>",
                                 carte10$INSEE_COM,
                                 "<br>",
                                 "<strong> surface_2008: </strong>",
                                 carte10$surface_2008,
                                 "<br>",
                                 "<strong> surface_2010: </strong>",
                                 carte10$surface_2010,
                                 "<br>",
                                 "<strong> surface_2012: </strong>",
                                 carte10$surface_2012,
                                 "<br>",
                                 "<strong> surface_2015: </strong>",
                                 carte10$surface_2015,
                                 "<br>",
                                 "<strong> surface_2018: </strong>",
                                 carte10$surface_2018,
                                 "<br>",
                                 "<strong> surface_2020: </strong>",
                                 carte10$surface_2020,
                                 "<br>",
                                 "<strong> Etat: </strong>",
                                 carte10$Etat),
                  group = "carte10")%>%
      addLegend(pal=pal,
                values = ~Etat,
                title = "Etat",
                opacity = 1)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "carte10",
                        options = searchFeaturesOptions(zoom = 12,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    
    
    
  })
  
  #data_graphe_cultures_saa_commune_pcit2()--------------------
  
  data_graphe_cultures_saa_commune_pcit2<-eventReactive(input$mape_cultures_saa_commune_pcit2_shape_click,{
    mape_cultures_saa_commune_pcit2_shape_click_info <- input$mape_cultures_saa_commune_pcit2_shape_click
    rv_location$id <- mape_cultures_saa_commune_pcit2_shape_click_info$id
    load("carte10.Rdata")
    h1<-data.table(carte10@data)[INSEE_COM==rv_location$id]$surface_2008
    h2<-data.table(carte10@data)[INSEE_COM==rv_location$id]$surface_2010
    h3<-data.table(carte10@data)[INSEE_COM==rv_location$id]$surface_2012
    h4<-data.table(carte10@data)[INSEE_COM==rv_location$id]$surface_2015
    h5<-data.table(carte10@data)[INSEE_COM==rv_location$id]$surface_2018
    h6<-data.table(carte10@data)[INSEE_COM==rv_location$id]$surface_2020
    h<-c(h1,h2,h3,h4,h5,h6)
    hh<-c(2008,2010,2012,2015,2018,2020)
    hhh<-data.table(numcom=rv_location$id,surface=h,annee=hh)
    ggplot(hhh,aes(x=annee,y=surface,color=annee))+geom_line()+geom_point()
    
    
  })
  
  #data_table_cultures_saa_commune_pcit2()----------------------------------
  
  data_table_cultures_saa_commune_pcit2<-eventReactive(input$mape_cultures_saa_commune_pcit2_shape_click,{
    load("carte10.Rdata")
    mape_cultures_saa_commune_pcit2_shape_click_info <- input$mape_cultures_saa_commune_pcit2_shape_click
    rv_location$id <- mape_cultures_saa_commune_pcit2_shape_click_info$id
    A<-cultures_saa_commune_pcit2[culture_rga==input$select10 & culture==input$select1010 & numcom==rv_location$id]
    tc_copie<-A
    tc_copie[,ID := .I]
    doublons<-which(duplicated(tc_copie$annee_ref))
    annee<-tc_copie$annee_ref
    if (length(doublons)!=0){
      annee<-tc_copie[-doublons]$annee_ref}
    cc<-data.table(surface=0,annee=annee)
    for (i in cc$annee){
      cc[annee==i]$surface<-sum(tc_copie[annee_ref==i]$surface,na.rm = TRUE)
      
    }
    
    col_fort<-c()
    col_faible<-c()
    
    ccc<-cc$surface
    Q1<-quantile(ccc,0.25,na.rm=TRUE)
    Q3<-quantile(ccc,0.75,na.rm=TRUE)
    s1<-(Q3-Q1)*(1.5+input$seuil_10)
    ab3<- max(ccc-Q3,na.rm=TRUE)+Q3
    ab1<- -max(Q1-ccc,na.rm=TRUE)+Q1
    cc3<-data.table(ccc)
    cc3[, ID := .I]
    a3<-cc3[abs(ccc-ab3)== min(cc3[,.(abs(ccc-ab3))]$V1) ]$ID
    cc3<-cc3[-a3,]$ccc
    
    cc1<-data.table(ccc)
    cc1[, ID := .I]
    a1<-cc1[abs(ccc-ab1)== min(cc1[,.(abs(ccc-ab1))]$V1) ]$ID
    cc1<-cc1[-a1,]$ccc
    
    if((min(cc1,na.rm = TRUE)/ab1>(1.5+input$seuil_10) && min(cc1,na.rm = TRUE)/ab1 <(3+input$seuil_10) && Q1-ab1>s1)){
      
      col_faible[[1]]<-A[annee_ref == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>(1.5+input$seuil_10) && ab3/max(cc3,na.rm = TRUE)<(3+input$seuil_10)  && ab3-Q3>s1)){
      
      col_faible[[2]]<-A[annee_ref == cc[a3,]$annee]$ID
    }
    
    if((min(cc1,na.rm = TRUE)/ab1>(3+input$seuil_10) && Q1-ab1>s1)){
      
      col_fort[[1]]<-A[annee_ref == cc[a1,]$annee]$ID
    }
    if((ab3/max(cc3,na.rm = TRUE)>(3+input$seuil_10) && ab3-Q3>s1)){
      
      col_fort[[2]]<-A[annee_ref == cc[a3,]$annee]$ID
    }
    
    A[,':='(ID=NULL)]
    if(length(col_fort)!=0 && length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(names(cultures_saa_commune_pcit2),color=styleRow(col_fort,"red"))%>%
        formatStyle(names(cultures_saa_commune_pcit2),color=styleRow(col_faible,"blue"))
      
    }else if(length(col_fort)!=0){
      datatable(A)%>%
        formatStyle(names(cultures_saa_commune_pcit2),color=styleRow(col_fort,"red"))
      
      
    }else if(length(col_faible)!=0){
      datatable(A)%>%
        formatStyle(names(cultures_saa_commune_pcit2),color=styleRow(col_faible,"blue"))
      
    }else{
      datatable(A)
    }
  })
  #data_cultures_saa_commune_pcit2_reg--------------------------------
  
  data_cultures_saa_commune_pcit2_reg<-eventReactive(input$go1010,{
    load("carte10.Rdata")
    
    DD<-data.frame(carte10@data)
    g<-ggplot(data=DD,aes(x=DD[,str_glue("surface_{input$select101010}")],y=DD[,str_glue("surface_{input$select10101010}")],color=Etat))+
      geom_point(aes(text=paste("numcom:",
                                DD[,"INSEE_COM"],
                                "<br>",
                                str_glue("surface_{input$select101010}:"),
                                DD[,str_glue("surface_{input$select101010}")],
                                "<br>",
                                str_glue("surface_{input$select10101010}:"),
                                DD[,str_glue("surface_{input$select10101010}")],
                                "<br>",
                                "Etat:",
                                DD[,"Etat"]
      )))+
      stat_smooth(method = "rlm", col = "black")+
      labs(title=str_glue("Graphe de regression lineaire de cultures_saa_commune_pcit2 pour {input$select101010}-{input$select10101010}"),x=str_glue("surface de {input$select101010}"),y=str_glue("surface de {input$select10101010}"))
    
    ggplotly(g,tooltip = "text")
    
    
    
  })
  
  #data_correlation_cultures_saa_commune_pcit2-----------------------
  
  data_correlation_cultures_saa_commune_pcit2<-eventReactive(input$go10,{
    
    load("carte10.Rdata")
    correlation_cultures_saa_commune_pcit2<-data.table(carte10@data)[,list(surface_2008,surface_2010,surface_2012,surface_2015,surface_2018,surface_2020)]
    
    if(length(correlation_cultures_saa_commune_pcit2[is.na(surface_2008)==TRUE]$surface_2008)==length(correlation_cultures_saa_commune_pcit2$surface_2008)){
      correlation_cultures_saa_commune_pcit2[,':='(surface_2008=NULL)]
    }
    if(length(correlation_cultures_saa_commune_pcit2[is.na(surface_2010)==TRUE]$surface_2010)==length(correlation_cultures_saa_commune_pcit2$surface_2010)){
      correlation_cultures_saa_commune_pcit2[,':='(surface_2010=NULL)]
    }
    if(length(correlation_cultures_saa_commune_pcit2[is.na(surface_2012)==TRUE]$surface_2012)==length(correlation_cultures_saa_commune_pcit2$surface_2012)){
      correlation_cultures_saa_commune_pcit2[,':='(surface_2012=NULL)]
    }
    if(length(correlation_cultures_saa_commune_pcit2[is.na(surface_2015)==TRUE]$surface_2015)==length(correlation_cultures_saa_commune_pcit2$surface_2015)){
      correlation_cultures_saa_commune_pcit2[,':='(surface_2015=NULL)]
    }
    if(length(correlation_cultures_saa_commune_pcit2[is.na(surface_2018)==TRUE]$surface_2018)==length(correlation_cultures_saa_commune_pcit2$surface_2018)){
      correlation_cultures_saa_commune_pcit2[,':='(surface_2018=NULL)]
    }
    if(length(correlation_cultures_saa_commune_pcit2[is.na(surface_2020)==TRUE]$surface_2020)==length(correlation_cultures_saa_commune_pcit2$surface_2020)){
      correlation_cultures_saa_commune_pcit2[,':='(surface_2020=NULL)]
    }
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    if (ncol(correlation_cultures_saa_commune_pcit2)<3){
      corr<-cor(correlation_cultures_saa_commune_pcit2)
      corrplot(corr,
               col=col(200),
               method="color",
               title = title(main=str_glue("Correlogramme pour culture_rga={input$select10} et culture = {input$select1010}"),cex.main=0.6,line=-1,adj=0.7),
               addCoef.col = "black",
               type = "lower",
               outline.col = "white",
               sig.level = 0.01,
               tl.srt=90,
               tl.cex=0.8,
               tl.col = "red",
               diag = TRUE)
    }else{
      corr<-cor(correlation_cultures_saa_commune_pcit2,use="pairwise.complete.obs")
      p.mat<-cor_pmat(correlation_cultures_saa_commune_pcit2)
      corrplot(corr,
               col=col(200),
               method="color",
               title = title(main=str_glue("Correlogramme pour culture_rga={input$select10} et culture = {input$select1010}"),cex.main=0.6,line=-1,adj=0.7),
               addCoef.col = "black",
               type = "lower",
               outline.col = "white",
               p.mat = p.mat,
               sig.level = 0.01,
               tl.srt=90,
               tl.cex=0.8,
               tl.col = "red",
               diag = TRUE)
    }
    
  })
  #data_cultures_saa_commune_pcit2_comparaison()------------------
  data_cultures_saa_commune_pcit2_comparaison<-eventReactive(input$go101010,{
    load("carte10.Rdata")
    a<-input$select1010101010
    c<-carte10
    c@data<-carte10@data[c(names(c@data)[c(1:17)],str_glue("surface_{a}"),"Etat")]
    names(c)[18]<-'surface'
    pal<-colorBin("YlOrRd", domain = c$surface)
    l<-leaflet(c)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  color="white",
                  fillOpacity =0.5 ,
                  fillColor = ~pal(surface),
                  label=~INSEE_COM,
                  layerId = ~INSEE_COM,
                  popup = paste0("<strong> Commune: </strong>",
                                 c$NOM_M_COM,
                                 "<br>",
                                 "<strong> Numero commune: </strong>",
                                 c$INSEE_COM,
                                 "<br>",
                                 "<strong> surface: </strong>",
                                 c$surface
                  ),
                  group = "c")%>%
      addLegend(pal=pal,
                values = ~surface,
                title = "surface",
                opacity = 0.5)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "c",
                        options = searchFeaturesOptions(zoom = 12,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    esri <- grep("^CartoDB", providers, value = TRUE)
    esri<-esri[2]
    esri1<-grep("^Esri", providers, value = TRUE)
    esri1<-esri1[5]
    esri2<-c(esri1,esri)
    for (provider in esri2) {
      l <- l %>% addProviderTiles(provider, group = provider)
    }
    
    l%>%
      addLayersControl(baseGroups = names(esri2),
                       options = layersControlOptions(collapsed = TRUE),position = "topleft")
    
  })
  
  #data_cultures_saa_commune_pcit2_histo()----------------------------------------------
  
  data_cultures_saa_commune_pcit2_histo<-eventReactive(input$go10,{
    load("carte10.Rdata")
    D<-data.table(annee=culture_culture_rga_annee[culture_rga==input$select10 & culture==input$select1010]$annee_ref,surface=as.numeric())
    for (i in D$annee){
      D[annee==i]$surface<-sum(cultures_saa_commune_pcit2[annee_ref==i & culture_rga==input$select10 & culture==input$select1010]$surface,na.rm = TRUE)
    }
    ggplot(data=D,aes(x=annee,y=surface,fill=annee,tooltip = surface ))+geom_histogram_interactive(stat='identity',position = 'dodge')+labs(title="Histogramme de la surface total des communes",x="annee",y="surface en ha",fill="annee")+theme(axis.text.x = element_text(face="bold",size=10,angle = 90))+geom_text(aes(label=as.integer(surface)),position = position_dodge(.9), vjust = "top") 
    
  })
  #data_cultures_saa_commune_pcit2_totale()--------------------
  
  data_cultures_saa_commune_pcit2_totale<-eventReactive(input$go10101010,{
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    ##Au debut telechargement puis fusionnement des communes
    communes_fusionnees<-data.table(dbGetQuery(conn=base,paste0('SELECT x.* FROM agricole_test.communes_fusionnees x WHERE x.annee_ref IN (2020)')))
    cultures_saa_commune_pcit2<-data.table(dbGetQuery(conn=base,paste0("SELECT x.* FROM agricole_test.cultures_saa_commune_pcit2 x WHERE (x.annee_ref IN ('2008','2010','2012','2015','2018','2020'))")))
    setnames(cultures_saa_commune_pcit2,"numcom","numcom_ancien")
    cultures_saa_commune_pcit2[communes_fusionnees, numcom_ancien:=numcom,on = .(numcom_ancien)]
    setnames(cultures_saa_commune_pcit2,"numcom_ancien","numcom")
    ##Preparation carte
    carte0<-Carte
    carte0_data<-data.table(carte0@data)
    func<-function(x,annee){
      aa<-cultures_saa_commune_pcit2[annee_ref==annee & numcom==x]$surface
      a<-sum(aa,na.rm = TRUE)
      if(length(aa)==0){
        a<-NA
      }
      return(a)
    }
    carte0_data[ , c("surface_2008","surface_2010","surface_2012","surface_2015","surface_2018","surface_2020"):= list(mapply(func,INSEE_COM,"2008"),mapply(func,INSEE_COM,"2010"),mapply(func,INSEE_COM,"2012"), mapply(func,INSEE_COM,"2015"),mapply(func,INSEE_COM,"2018"),mapply(func,INSEE_COM,"2020"))]
    func_etat<-function(a,b,c,d,e,f){
      cc<-c(a,b,c,d,e,f)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*(input$seuil_1010+1.5)
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm=TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm=TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e) & is.na(f)){
        return(NA)
      }else if((ab3/max(cc3,na.rm = TRUE)>(3+input$seuil_1010) && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>(3+input$seuil_1010) && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>(1.5+input$seuil_1010) && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>(1.5+input$seuil_1010) && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    carte0_data[,Etat:=mapply(func_etat,surface_2008,surface_2010,surface_2012,surface_2015,surface_2018,surface_2020)]
    
    
    carte0$surface_2008<-carte0_data$surface_2008
    carte0$surface_2010<-carte0_data$surface_2010
    carte0$surface_2012<-carte0_data$surface_2012
    carte0$surface_2015<-carte0_data$surface_2015
    carte0$surface_2018<-carte0_data$surface_2018
    carte0$surface_2020<-carte0_data$surface_2020
    carte0$Etat<-carte0_data$Etat
    
    
    
    #leaflet
    pal<-colorFactor("viridis",domain = carte0$Etat)
    leaflet(carte0)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  color="white",
                  fillOpacity =1 ,
                  fillColor = ~pal(Etat),
                  label=~INSEE_COM,
                  layerId = ~INSEE_COM,
                  highlightOptions =highlightOptions(
                    color="white",
                    weight=3,
                    fillColor = NA),
                  popup = paste0("<strong> Commune: </strong>",
                                 carte0$NOM_M_COM,
                                 "<br>",
                                 "<strong> Numero commune: </strong>",
                                 carte0$INSEE_COM,
                                 "<br>",
                                 "<strong> surface_2008: </strong>",
                                 carte0$surface_2008,
                                 "<br>",
                                 "<strong> surface_2010: </strong>",
                                 carte0$surface_2010,
                                 "<br>",
                                 "<strong> surface_2012: </strong>",
                                 carte0$surface_2012,
                                 "<br>",
                                 "<strong> surface_2015: </strong>",
                                 carte0$surface_2015,
                                 "<br>",
                                 "<strong> surface_2018: </strong>",
                                 carte0$surface_2018,
                                 "<br>",
                                 "<strong> surface_2020: </strong>",
                                 carte0$surface_2020,
                                 "<br>",
                                 "<strong> Etat: </strong>",
                                 carte0$Etat),
                  group = "carte0")%>%
      addLegend(pal=pal,
                values = ~Etat,
                title = "Etat",
                opacity = 1)%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = "carte0",
                        options = searchFeaturesOptions(zoom = 12,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))
    
    
    
  })
  
  #data_emi_total()-----------------------
  data_emi_total<-eventReactive(input$go12,{
    
    base <- dbConnect(drv = RPostgres::Postgres(),
                      host = "172.16.38.156",
                      port = "5433",
                      dbname = "icare_3.2",
                      user = "postgres",
                      password = "icare")
    carte11<-Carte_epci
    carte11_data<-data.table(carte11@data)
    carte12<-Carte
    carte12_data<-data.table(carte12@data)
    func_polluant<-function(x){
      if (x=="SO2"){
        return('"SO2"')
      }else if (x=="NOx"){
        return('"NOx"')
      }else if (x=="COVNM"){
        return('"COVNM"')
      }else if (x=="PM10"){
        return('"PM10"')
      }else if (x=="PM25"){
        return('"PM25"')
      }else{
        return('"NH3"')
      }
    }
    w <-data.table(dbGetQuery(conn = base, paste0(str_glue("SELECT SUBSTRING(CONCAT(w1.id_geo_epci,w2.id_geo_epci,w3.id_geo_epci,w4.id_geo_epci,w5.id_geo_epci),0,6) AS id_geo_epci, sum_2008, sum_2010, sum_2012, sum_2015, sum_2018 FROM
((SELECT id_geo_epci,SUM({func_polluant(input$select12)}) AS sum_2008 FROM agricole_test.emi_total_a2008_v2020_v4 WHERE code_napfue = '{input$select1212}' AND pcaet='{input$select121212}' AND code_snap='{input$select12121212}' GROUP BY id_geo_epci ) AS w1 FULL JOIN
(SELECT id_geo_epci,SUM({func_polluant(input$select12)}) AS sum_2010 FROM agricole_test.emi_total_a2010_v2020_v4 WHERE code_napfue = '{input$select1212}' AND pcaet='{input$select121212}' AND code_snap='{input$select12121212}' GROUP BY id_geo_epci ) AS w2 ON w1.id_geo_epci=w2.id_geo_epci FULL JOIN
(SELECT id_geo_epci,SUM({func_polluant(input$select12)}) AS sum_2012 FROM agricole_test.emi_total_a2012_v2020_v4 WHERE code_napfue = '{input$select1212}' AND pcaet='{input$select121212}' AND code_snap='{input$select12121212}' GROUP BY id_geo_epci ) AS w3 ON COALESCE(w1.id_geo_epci,w2.id_geo_epci)=w3.id_geo_epci FULL JOIN
(SELECT id_geo_epci,SUM({func_polluant(input$select12)}) AS sum_2015 FROM agricole_test.emi_total_a2015_v2020_v4 WHERE code_napfue = '{input$select1212}' AND pcaet='{input$select121212}' AND code_snap='{input$select12121212}' GROUP BY id_geo_epci ) AS w4 ON COALESCE(w1.id_geo_epci,w2.id_geo_epci,w3.id_geo_epci)=w4.id_geo_epci FULL JOIN
(SELECT id_geo_epci,SUM({func_polluant(input$select12)}) AS sum_2018 FROM agricole_test.emi_total_a2018_v2020_v4 WHERE code_napfue = '{input$select1212}' AND pcaet='{input$select121212}' AND code_snap='{input$select12121212}' GROUP BY id_geo_epci ) AS w5 ON COALESCE(w1.id_geo_epci,w2.id_geo_epci,w3.id_geo_epci,w4.id_geo_epci)=w5.id_geo_epci)"))))
    idx <- match(carte11@data$N_SIREN , w$id_geo_epci)
    carte11_data <- data.table(w[idx, ])
    w1 <-data.table(dbGetQuery(conn = base, paste0(str_glue("SELECT SUBSTRING(CONCAT(w1.code_insee,w2.code_insee,w3.code_insee,w4.code_insee,w5.code_insee),0,6) AS code_insee, sum_2008,sum_2010, sum_2012, sum_2015, sum_2018 FROM
((SELECT code_insee,SUM({func_polluant(input$select12)}) AS sum_2008 FROM agricole_test.emi_total_a2008_v2020_v4 WHERE code_napfue = '{input$select1212}' AND pcaet='{input$select121212}' AND code_snap='{input$select12121212}' GROUP BY code_insee ) AS w1 FULL JOIN
(SELECT code_insee,SUM({func_polluant(input$select12)}) AS sum_2010 FROM agricole_test.emi_total_a2010_v2020_v4 WHERE code_napfue = '{input$select1212}' AND pcaet='{input$select121212}' AND code_snap='{input$select12121212}' GROUP BY code_insee ) AS w2 ON w1.code_insee=w2.code_insee FULL JOIN
(SELECT code_insee,SUM({func_polluant(input$select12)}) AS sum_2012 FROM agricole_test.emi_total_a2012_v2020_v4 WHERE code_napfue = '{input$select1212}' AND pcaet='{input$select121212}' AND code_snap='{input$select12121212}' GROUP BY code_insee ) AS w3 ON COALESCE(w1.code_insee,w2.code_insee)=w3.code_insee FULL JOIN
(SELECT code_insee,SUM({func_polluant(input$select12)}) AS sum_2015 FROM agricole_test.emi_total_a2015_v2020_v4 WHERE code_napfue = '{input$select1212}' AND pcaet='{input$select121212}' AND code_snap='{input$select12121212}' GROUP BY code_insee ) AS w4 ON COALESCE(w1.code_insee,w2.code_insee,w3.code_insee)=w4.code_insee FULL JOIN
(SELECT code_insee,SUM({func_polluant(input$select12)}) AS sum_2018 FROM agricole_test.emi_total_a2018_v2020_v4 WHERE code_napfue = '{input$select1212}' AND pcaet='{input$select121212}' AND code_snap='{input$select12121212}' GROUP BY code_insee ) AS w5 ON COALESCE(w1.code_insee,w2.code_insee,w3.code_insee,w4.code_insee)=w5.code_insee)"))))
    idx1 <- match(carte12@data$INSEE_COM , w1$code_insee)
    carte12_data <- data.table(w1[idx1, ])
    func_etat<-function(a,b,c,d,e){
      cc<-c(a,b,c,d,e)
      Q1<-quantile(cc,0.25,na.rm=TRUE)
      Q3<-quantile(cc,0.75,na.rm=TRUE)
      s1<-(Q3-Q1)*(1.5+input$seuil_12)
      ab3<- max(cc-Q3,na.rm=TRUE)+Q3
      ab1<- -max(Q1-cc,na.rm=TRUE)+Q1
      cc3<-data.table(cc)
      cc3[, ID := .I]
      a3<-cc3[abs(cc-ab3)== min(cc3[,.(abs(cc-ab3))]$V1,na.rm=TRUE) ]$ID
      cc3<-cc3[-a3,]$cc
      cc1<-data.table(cc)
      cc1[, ID := .I]
      a1<-cc1[abs(cc-ab1)== min(cc1[,.(abs(cc-ab1))]$V1,na.rm=TRUE) ]$ID
      cc1<-cc1[-a1,]$cc
      
      if(is.na(a) & is.na(b) & is.na(c) & is.na(d) & is.na(e)){
        return(NA)
      }else if((ab3/max(cc3,na.rm = TRUE)>(3+input$seuil_12) && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>(3+input$seuil_12) && Q1-ab1>s1)){
        return("Aberrante Forte")
      }else if((ab3/max(cc3,na.rm = TRUE)>(1.5+input$seuil_12) && ab3-Q3>s1) | (min(cc1,na.rm = TRUE)/ab1>(1.5+input$seuil_12) && Q1-ab1>s1)){
        return("Aberrante Faible")
      }else if(sum(cc,na.rm=TRUE)==0){
        return("Null")
      }else{
        return("Normal")
      }
    }
    
    carte11_data[,Etat:=mapply(func_etat,sum_2008,sum_2010,sum_2012,sum_2015,sum_2018)]
    carte11$sum_2008<-carte11_data$sum_2008
    carte11$sum_2010<-carte11_data$sum_2010
    carte11$sum_2012<-carte11_data$sum_2012
    carte11$sum_2015<-carte11_data$sum_2015
    carte11$sum_2018<-carte11_data$sum_2018
    carte11$sum_2020<-carte11_data$sum_2020
    carte11$Etat<-carte11_data$Etat
    carte12_data[,Etat:=mapply(func_etat,sum_2008,sum_2010,sum_2012,sum_2015,sum_2018)]
    carte12$sum_2008<-carte12_data$sum_2008
    carte12$sum_2010<-carte12_data$sum_2010
    carte12$sum_2012<-carte12_data$sum_2012
    carte12$sum_2015<-carte12_data$sum_2015
    carte12$sum_2018<-carte12_data$sum_2018
    carte12$sum_2020<-carte12_data$sum_2020
    carte12$Etat<-carte12_data$Etat
    carte13<-carte11
    pal<-colorFactor("viridis",domain = carte11$Etat)
    pal1<-colorFactor("viridis",domain = carte12$Etat)
    leaflet(carte11)%>%
      setView(lat=50,lng=2.74,zoom=7)%>%
      addTiles()%>%
      addPolygons(stroke = TRUE,
                  smoothFactor = 0.3,
                  color="black",
                  weight = 3,
                  fillOpacity =0.6 ,
                  opacity = 10,
                  fillColor = ~pal(Etat),
                  label=~N_SIREN,
                  layerId = ~N_SIREN,
                  highlightOptions =highlightOptions(
                    color="chartreuse",
                    weight=3,
                    bringToFront = TRUE),
                  popup = paste0("<strong> N_SIRN: </strong>",
                                 carte11$N_SIREN,
                                 "<br>",
                                 "<strong> SO2_2008: </strong>",
                                 carte11$sum_2008,
                                 "<br>",
                                 "<strong> SO2_2010: </strong>",
                                 carte11$sum_2010,
                                 "<br>",
                                 "<strong> SO2_2012: </strong>",
                                 carte11$sum_2012,
                                 "<br>",
                                 "<strong> SO2_2015: </strong>",
                                 carte11$sum_2015,
                                 "<br>",
                                 "<strong> SO2_2018: </strong>",
                                 carte11$sum_2018,
                                 "<br>",
                                 "<strong> Etat: </strong>",
                                 carte11$Etat),
                  group = "carte11")%>%
      addPolygons(data = carte12,
                  stroke = FALSE,
                  smoothFactor = 0.3,
                  color="white",
                  fillOpacity =0.6,
                  fillColor = ~pal(Etat),
                  weight = 5,
                  label=~INSEE_COM,
                  layerId = ~INSEE_COM,
                  highlightOptions =highlightOptions(
                    color="white",
                    weight=3,
                    fillColor = NA),
                  popup = paste0("<strong> INSEE_COM: </strong>",
                                 carte12$INSEE_COM,
                                 "<br>",
                                 "<strong> SO2_2008: </strong>",
                                 carte12$sum_2008,
                                 "<br>",
                                 "<strong> SO2_2010: </strong>",
                                 carte12$sum_2010,
                                 "<br>",
                                 "<strong> SO2_2012: </strong>",
                                 carte12$sum_2012,
                                 "<br>",
                                 "<strong> SO2_2015: </strong>",
                                 carte12$sum_2015,
                                 "<br>",
                                 "<strong> SO2_2018: </strong>",
                                 carte12$sum_2018,
                                 "<br>",
                                 "<strong> Etat: </strong>",
                                 carte12$Etat),
                  group = "carte12")%>%
      addLegend("topright",pal=pal,
                values = ~Etat,
                title = "Etat N_SIREN",
                opacity = 1,
                group="carte11")%>%
      addLegend("topright",pal=pal,
                values = ~Etat,
                title = "Etat pour INSEE_COM",
                opacity = 1,
                group="carte12")%>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton()%>%
      addSearchFeatures(targetGroups = c("carte11","carte12"),
                        options = searchFeaturesOptions(zoom = 9,
                                                        openPopup = TRUE,
                                                        firstTipSubmit = TRUE,
                                                        autoCollapse = TRUE,
                                                        hideMarkerOnCollapse=TRUE))%>%
      
      addLayersControl(position = "topleft",
                       baseGroups = "carte11",
                       overlayGroups = "carte12",
                       options = layersControlOptions(collapsed = TRUE)
      )%>%
      hideGroup("carte12")
    
  
    
  })
  
  #data_evolution_trafic_tmja_2020_reg--------------------------------
  
  data_evolution_trafic_tmja_2020_reg<-eventReactive(input$go13,{
    DD<-data.frame(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.evolution_trafic_tmja_2020 WHERE importance = '{input$select13}' AND cat_admin={input$select1313}"))))
    g<-ggplot(data=DD,aes(x=DD[,str_glue("tmja{input$select131313}")],y=DD[,"tmja2020"]))+
      geom_point(aes(text=paste("id:",
                                DD[,"id"],
                                "<br>",
                                str_glue("tmja{input$select131313}:"),
                                DD[,str_glue("tmja{input$select131313}")],
                                "<br>",
                                str_glue("tmja2020"),
                                DD[,"tmja2020"]
      )))+
      stat_smooth(method = "lm", col = "red")+
      labs(title=str_glue("Graphe de regression de tmja pour {input$select131313}-2020"),x=str_glue("tmja de {input$select131313}"),y=str_glue("tmja de 2020"))
    
    ggplotly(g,tooltip = "text")
    
  })
  #data_evolution_trafic_pl_2020_reg--------------------------------
  
  data_evolution_trafic_pl_2020_reg<-eventReactive(input$go14,{
    DD<-data.frame(dbGetQuery(conn=base,paste0(str_glue("SELECT * FROM agricole_test.evolution_trafic_pl_2020 WHERE importance = '{input$select14}' AND cat_admin={input$select1414}"))))
    g<-ggplot(data=DD,aes(x=DD[,str_glue("tpl{input$select141414}")],y=DD[,"tpl2020"]))+
      geom_point(aes(text=paste("id:",
                                DD[,"id"],
                                "<br>",
                                str_glue("tpl{input$select141414}:"),
                                DD[,str_glue("tpl{input$select141414}")],
                                "<br>",
                                str_glue("tpl2020"),
                                DD[,"tpl2020"]
      )))+
      stat_smooth(method = "lm", col = "red")+
      labs(title=str_glue("Graphe de regression de tpl pour {input$select141414}-2020"),x=str_glue("tpl de {input$select141414}"),y=str_glue("tpl de 2020"))
    
    ggplotly(g,tooltip = "text")
    
  })
  
  # Output de declaration consommation-----------------------------------
  output$mape_decla_conso<-renderLeaflet({
    m2<-data_decla_conso()
    m2
  })
  output$graphe_decla_conso <- renderPlot({
    m4<-data_graphe_decla_conso()
    m4
  })
  output$table_decla_conso<-renderDT({
    data_table_decla_conso()
  })
  
  output$decla_conso_reg<-renderPlotly({
    data_decla_conso_reg()
  })
  output$correlation_decla_conso<-renderPlot({
    #ggplotly(data_correlation_tc_conso())
    data_correlation_decla_conso()
  })
  output$decla_conso_histo<-renderggiraph({
    ggiraph(code = print(data_decla_conso_histo()),zoom_max = 10)
  })
  output$mape_decla_consoc<-renderLeaflet({
    m5<-data_decla_consoc()
    m5
  })
  # Output de src_emi--------------------------------
  output$mape_src_emi<-renderLeaflet({
    m3<-data_src_emi()
    m3
  })
  output$table_src_emi<-renderDT({
    data_table_src_emi()
  })
  
  # Output de cheptel_saa_commune_pcit2-----------------------------------
  output$mape_cheptel_saa<-renderLeaflet({
    m2<-data_cheptel_saa_commune_complet_pcit2()
    m2
  })
  output$graphe_cheptel_saa <- renderPlot({
    m4<-data_graphe_cheptel_saa()
    m4
  })
  output$table_cheptel_saa<-renderDT({
    data_table_cheptel_saa()
  })
  output$correlation_cheptel_saa<-renderPlot({
    data_correlation_cheptel_saa()
  })
  output$cheptel_reg<-renderPlotly({
    data_cheptel_saa_reg()
  })
  output$mape_cheptel_saa_comparaison<-renderLeaflet({
    m5<-data_cheptel_saa_comparaison()
    m5
  })
  output$cheptel_saa_histo<-renderggiraph({
    ggiraph(code = print(data_cheptel_saa_histo()),zoom_max = 10)
  })
  output$mape_cheptel_saa_totale<-renderLeaflet({
    m2<-data_cheptel_saa_totale()
    m2
  })
  output$table_cheptel_saa_totale<-renderDT({
    data_table_cheptel_saa_totale()
  })
  # Output de W_conso_install--------------------------------
  output$mape_w_conso_install<-renderLeaflet({
    m3<-data_w_conso_install()
    m3
  })
  output$table_w_conso_install<-renderDT({
    data_table_w_conso_install()
  })
  
  # Output de w_src_etab_emi--------------------------------
  output$mape_w_src_etab_emi<-renderLeaflet({
    m2<-data_w_src_etab_emi()
    m2
  })
  output$graphe_w_src_etab_emi <- renderPlot({
    m4<-data_graphe_w_src_etab_emi()
    m4
  })
  output$table_w_src_etab_emi<-renderDT({
    data_table_w_src_etab_emi()
  })
  output$w_src_etab_emi_reg<-renderPlotly({
    data_w_src_etab_emi_reg()
  })
  output$correlation_w_src_etab_emi<-renderPlot({
    data_correlation_w_src_etab_emi()
  })
  output$mape_w_src_etab_emi_comparaison<-renderLeaflet({
    m5<-data_w_src_etab_emi_comparaison()
    m5
  })
  output$mape_w_src_etab_emi_histo<-renderggiraph({
    ggiraph(code = print(data_w_src_etab_emi_histo()),zoom_max = 10)
  })
  # Output de w_film_emi_install--------------------------------
  output$mape_w_film_emi_install<-renderLeaflet({
    m3<-data_w_film_emi_install()
    m3
  })
  output$table_w_film_emi_install<-renderDT({
    data_table_w_film_emi_install()
  })
  # Output de rs_tmja--------------------------------
  output$mape_rs_tmja<-renderLeaflet({
    m3<-data_rs_tmja()
    m3
  })
  # Output de r_tc_conso_corrigees-----------------------------------
  output$mape_r_tc_conso_corrigees<-renderLeaflet({
    m2<-data_r_tc_conso_corrigees()
    m2
  })
  
  output$graphe_r_tc_conso_corrigees <- renderPlot({
    m4<-data_graphe_r_tc_conso_corrigees()
    m4
  })
  output$table_r_tc_conso_corrigees<-renderDT({
    data_table_r_tc_conso_corrigees()
  })
  
  # Output de a_tc_conso_corrigees-----------------------------------
  output$mape_a_tc_conso_corrigees<-renderLeaflet({
    m2<-data_a_tc_conso_corrigees()
    m2
  })
  output$graphe_a_tc_conso_corrigees <- renderPlot({
    m4<-data_graphe_a_tc_conso_corrigees()
    m4
  })
  output$table_a_tc_conso_corrigees<-renderDT({
    data_table_a_tc_conso_corrigees()
  })
  output$correlation_a_tc_conso_corrigees<-renderPlot({
    #ggplotly(data_correlation_tc_conso())
    data_correlation_a_tc_conso_corrigees()
  })
  output$a_tc_conso_corrigees_reg<-renderPlotly({
    data_a_tc_conso_corrigees_reg()
  })
  output$mape_a_tc_conso_corrigees_comparaison<-renderLeaflet({
    m5<-data_a_tc_conso_corrigees_comparaison()
    m5
  })
  output$mape_a_tc_conso_corrigees_histo<-renderggiraph({
    ggiraph(code = print(data_a_tc_conso_corrigees_histo()),zoom_max = 10)
  })
 
  # Output de cultures_saa_commune_pcit2-----------------------------------
  output$mape_cultures_saa_commune_pcit2<-renderLeaflet({
    m2<-data_cultures_saa_commune_pcit2()
    m2
  })
  output$graphe_cultures_saa_commune_pcit2 <- renderPlot({
    m4<-data_graphe_cultures_saa_commune_pcit2()
    m4
  })
  output$table_cultures_saa_commune_pcit2<-renderDT({
    data_table_cultures_saa_commune_pcit2()
  })
  output$cultures_saa_commune_pcit2_reg<-renderPlotly({
    data_cultures_saa_commune_pcit2_reg()
  })
  output$correlation_cultures_saa_commune_pcit2<-renderPlot({
    data_correlation_cultures_saa_commune_pcit2()
  })
  output$mape_cultures_saa_commune_pcit2_comparaison<-renderLeaflet({
    m5<-data_cultures_saa_commune_pcit2_comparaison()
    m5
  })
  output$mape_cultures_saa_commune_pcit2_histo<-renderggiraph({
    ggiraph(code = print(data_cultures_saa_commune_pcit2_histo()),zoom_max = 10)
  })
  output$mape_cultures_saa_commune_pcit2_totale<-renderLeaflet({
    m2<-data_cultures_saa_commune_pcit2_totale()
    m2
  })
  # Output de emi_total-----------------------------------
  output$mape_emi_total<-renderLeaflet({
    m2<-data_emi_total()
    m2
  })
  # Output de evolution_trafic_tmja_2020-----------------------------------
  output$evolution_trafic_tmja_2020_reg<-renderPlotly({
    data_evolution_trafic_tmja_2020_reg()
  })
  # Output de evolution_trafic_pl_2020-----------------------------------
  output$evolution_trafic_pl_2020_reg<-renderPlotly({
    data_evolution_trafic_pl_2020_reg()
  })
})
