#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
library(RPostgres)
library(DBI)
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
base <- dbConnect(drv = RPostgres::Postgres(),
                  host = "172.16.38.156",
                  port = "5433",
                  dbname = "icare_3.2",
                  user = "postgres",
                  password = "icare")

#Preparer les parties snap et napfue pour declaration consommation----------------
snap_napfue_tc_conso<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct snap,napfue FROM agricole_test.declarations_consommations x
WHERE x.niveau IN ('commune')")))
snap_napfue_tc_conso[, ID := .I]
snap_napfue_annee_decla_conso<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct snap,napfue,annee_ref FROM agricole_test.declarations_consommations x
WHERE x.niveau IN ('commune') order by annee_ref")))
snap_napfue_annee_decla_conso[,ID := .I]


#Preparer les parties cheptel pour cheptel_saa----------------------
cheptel<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct cheptel FROM agricole_test.cheptel_saa_commune_complet_pcit2 x
WHERE x.annee_ref IN ('2008','2010','2012','2015','2018','2020')")))
cheptel_annee<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct cheptel, annee_ref  FROM agricole_test.cheptel_saa_commune_complet_pcit2 x WHERE x.annee_ref IN ('2008','2010','2012','2015','2018','2020') order by annee_ref")))

#Preparer les combustible pour W_conso_install----------------------
combustible<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct combustible FROM agricole_test.W_conso_install x
WHERE x.an IN ('2008','2010','2012','2015','2018')")))

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
#Preparer les parties importance et cat_admin pour evolution_trafic_pl_2020----------------
importance_cat_admin2<-data.table(dbGetQuery(conn=base,paste0("SELECT distinct importance,cat_admin FROM agricole_test.evolution_trafic_pl_2020")))


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$img(height=50,width=300,src="R.png"),
  navbarPage(title="Secteurs",
             #Energie--------------------
             navbarMenu(title = "Secteur Energie",
                        tabPanel(title="declaration_consommation",h3("declaration_consommation"),
                                 sidebarLayout(
                                   sidebarPanel(h4("Carte des valeurs aberrantes"),wellPanel(selectInput(inputId="select1",label = "snap",choices = snap_napfue_tc_conso$snap,selected = snap_napfue_tc_conso[1,]$snap),
                                                                                             selectInput(inputId="select11",label = "napfue",choices = snap_napfue_tc_conso$napfue ),
                                                                                             actionButton(inputId = "go",label = "Play")
                                   ),
                                   plotOutput("graphe_decla_conso"),
                                   h4("Graphe de regression"),
                                   wellPanel(selectInput(inputId="select111",label = "annee 1",choices = snap_napfue_annee_decla_conso$annee_ref),
                                             selectInput(inputId="select1111",label = "annee 2",choices = snap_napfue_annee_decla_conso$annee_ref ),
                                             actionButton(inputId = "go11",label = "Play")),
                                   plotOutput("correlation_decla_conso"),
                                   h4("Carte de comparaison"),
                                   wellPanel(selectInput(inputId="select11111",label = "annee",choices = snap_napfue_annee_decla_conso$annee_ref),
                                             actionButton(inputId = "go1",label = "Play")),
                                   ggiraphOutput("decla_conso_histo")),
                                   mainPanel(leafletOutput("mape_decla_conso",height="80vh"),DTOutput("table_decla_conso"),plotlyOutput("decla_conso_reg"),leafletOutput("mape_decla_consoc",height ="80vh" ))
                                   
                                 )
                                 
                        )
                        
                        
                        
                        
                        
             ),
             #Agricol--------------------
             navbarMenu(title = "Secteur Agricole",
                        tabPanel(title="cheptel_saa_commune_complet_pcit2",h3("cheptel_saa_commune_complet_pcit2"),
                                 sidebarLayout(
                                   sidebarPanel(h4("Carte des valeurs aberrantes"),wellPanel(selectInput(inputId="select3",label = "cheptel",choices = cheptel$cheptel,selected = cheptel[1,]$cheptel),
                                                                                             actionButton(inputId = "go3",label = "Play")
                                   ),
                                   plotOutput("graphe_cheptel_saa"),
                                   h4("Graphe de regression"),
                                   wellPanel(selectInput(inputId="select33",label = "annee 1",choices = cheptel_annee$annee_ref),
                                             selectInput(inputId="select333",label = "annee 2",choices = cheptel_annee$annee_ref ),
                                             actionButton(inputId = "go33",label = "Play")),
                                   plotOutput("correlation_cheptel_saa"),
                                   h4("Carte de comparaison"),
                                   wellPanel(selectInput(inputId="select3333",label = "annee",choices = cheptel_annee$annee_ref),
                                             actionButton(inputId = "go333",label = "Play")),
                                   ggiraphOutput("cheptel_saa_histo"),
                                   h4("Graphe de cheptel totale"),
                                   wellPanel(sliderInput("seuil_3", "Seuil de l'aberation:",
                                                         min = 0, max = 10, value = 0
                                             ),
                                             actionButton(inputId = "go33333",label = "Play"))),
                                   
                                   mainPanel(leafletOutput("mape_cheptel_saa",height="80vh"),DTOutput("table_cheptel_saa"),plotlyOutput("cheptel_reg"),leafletOutput("mape_cheptel_saa_comparaison",height ="80vh" ),leafletOutput("mape_cheptel_saa_totale",height="80vh"))
                                   
                                 )
                                 
                        ),
                        tabPanel(title="tc_conso_corrigees",h3("tc_conso_corrigees"),
                                 sidebarLayout(
                                   sidebarPanel(h4("Carte des valeurs aberrantes"),wellPanel(selectInput(inputId="select9",label = "snap",choices = snap_napfue_a_tc_conso$snap,selected = snap_napfue_a_tc_conso[1,]$snap),
                                                                                             selectInput(inputId="select99",label = "napfue",choices = snap_napfue_a_tc_conso$napfue ),
                                                                                             sliderInput("seuil_9", "Seuil de l'aberration:",
                                                                                                         min = 0, max = 10, value = 0
                                                                                             ),
                                                                                             actionButton(inputId = "go9",label = "Play")
                                   ),
                                   plotOutput("graphe_a_tc_conso_corrigees"),
                                   h4("Graphe de regression"),
                                   wellPanel(selectInput(inputId="select999",label = "annee 1",choices = snap_napfue_annee_a_tc_conso_corrigees$annee_ref),
                                             selectInput(inputId="select9999",label = "annee 2",choices = snap_napfue_annee_a_tc_conso_corrigees$annee_ref),
                                             actionButton(inputId = "go99",label = "Play")),
                                   plotOutput("correlation_a_tc_conso_corrigees"),
                                   h4("Carte de comparaison"),
                                   wellPanel(selectInput(inputId="select99999",label = "annee",choices = snap_napfue_annee_a_tc_conso_corrigees$annee_ref),
                                             actionButton(inputId = "go999",label = "Play")),
                                   ggiraphOutput("mape_a_tc_conso_corrigees_histo")),
                                   mainPanel(leafletOutput("mape_a_tc_conso_corrigees",height="80vh"),DTOutput("table_a_tc_conso_corrigees"),plotlyOutput("a_tc_conso_corrigees_reg"),leafletOutput("mape_a_tc_conso_corrigees_comparaison",height ="80vh" ))
                                   
                                 )
                                 
                        ),
                        tabPanel(title="cultures_saa_commune_pcit2",h3("cultures_saa_commune_pcit2"),
                                 sidebarLayout(
                                   sidebarPanel(h4("Carte des valeurs aberrantes"),wellPanel(selectInput(inputId="select10",label = "culture_rga",choices = culture_culture_rga$culture_rga),
                                                                                             selectInput(inputId="select1010",label = "culture",choices = culture_culture_rga$culture ),
                                                                                             sliderInput("seuil_10", "Seuil de l'aberation:",
                                                                                                         min = 0, max = 10, value = 0
                                                                                             ),
                                                                                             actionButton(inputId = "go10",label = "Play")
                                   ),plotOutput("graphe_cultures_saa_commune_pcit2"),
                                   h4("Graphe de regression"),
                                   wellPanel(selectInput(inputId="select101010",label = "annee 1",choices = culture_culture_rga_annee$annee_ref),
                                             selectInput(inputId="select10101010",label = "annee 2",choices = culture_culture_rga_annee$annee_ref),
                                             actionButton(inputId = "go1010",label = "Play")),
                                   plotOutput("correlation_cultures_saa_commune_pcit2"),
                                   h4("Carte de comparaison"),
                                   wellPanel(selectInput(inputId="select1010101010",label = "annee",choices = culture_culture_rga_annee$annee_ref),
                                             actionButton(inputId = "go101010",label = "Play")),
                                   ggiraphOutput("mape_cultures_saa_commune_pcit2_histo"),
                                   h4("Graphe de cheptel totale"),
                                   wellPanel(sliderInput("seuil_1010", "Seuil de l'aberation:",
                                                         min = 0, max = 10, value = 0
                                   ),
                                   actionButton(inputId = "go10101010",label = "Play"))),
                                   mainPanel(leafletOutput("mape_cultures_saa_commune_pcit2",height="80vh"),DTOutput("table_cultures_saa_commune_pcit2"),plotlyOutput("cultures_saa_commune_pcit2_reg"),leafletOutput("mape_cultures_saa_commune_pcit2_comparaison",height ="80vh" ),leafletOutput("mape_cultures_saa_commune_pcit2_totale",height="80vh"))
                                   
                                 )
                                 
                        )
             ),
             
             
             #industrie--------------------
             navbarMenu(title = "Secteur industrie",
                        tabPanel(title="src_install_emi",h3("src_install_emi"),
                                 sidebarLayout(
                                   sidebarPanel(h4("Carte des valeurs aberrantes"),wellPanel(selectInput(inputId="select2",label = "Polluant",choices = list("19"=19,"49"=49,"101"=101,"102"=102,"111"=111,"131"=131),selected = 19),
                                                                                             actionButton(inputId = "go2",label = "Play"))
                                   ),
                                   mainPanel(leafletOutput("mape_src_emi",height="80vh"),DTOutput("table_src_emi"))
                                   
                                 )
                                 
                        ),
                        tabPanel(title="w_conso_install",h3("w_conso_install"),
                                 sidebarLayout(
                                   sidebarPanel(h4("Carte des valeurs aberrantes"),wellPanel(selectInput(inputId="select4",label = "combustible",choices = combustible$combustible),
                                                                                             actionButton(inputId = "go4",label = "Play"))
                                   ),
                                   mainPanel(leafletOutput("mape_w_conso_install",height="80vh"),DTOutput("table_w_conso_install"))
                                   
                                 )
                                 
                        ),
                        tabPanel(title="w_src_etab_emi",h3("w_src_etab_emi"),
                                 sidebarLayout(
                                   sidebarPanel(h4("Carte des valeurs aberrantes"),wellPanel(selectInput(inputId="select5",label = "id_polluant",choices = list("19"=19,"49"=49,"101"=101,"102"=102,"111"=111),selected = 19),
                                                                                             actionButton(inputId = "go5",label = "Play")),
                                                plotOutput("graphe_w_src_etab_emi"),
                                                h4("Graphe de regression"),
                                                wellPanel(selectInput(inputId="select55",label = "annee 1",choices = w_src_etab_emi_annee$an),
                                                          selectInput(inputId="select555",label = "annee 2",choices = w_src_etab_emi_annee$an ),
                                                          actionButton(inputId = "go55",label = "Play")),
                                                plotOutput("correlation_w_src_etab_emi"),
                                                h4("Carte de comparaison"),
                                                wellPanel(selectInput(inputId="select5555",label = "annee",choices = w_src_etab_emi_annee$an),
                                                          actionButton(inputId = "go555",label = "Play")),
                                                ggiraphOutput("mape_w_src_etab_emi_histo")),
                                   mainPanel(leafletOutput("mape_w_src_etab_emi",height="80vh"),DTOutput("table_w_src_etab_emi"),plotlyOutput("w_src_etab_emi_reg"),leafletOutput("mape_w_src_etab_emi_comparaison",height ="80vh" ))
                                   
                                 )
                                 
                        ),
                        tabPanel(title="w_film_emi_install",h3("w_film_emi_install"),
                                 sidebarLayout(
                                   sidebarPanel(h4("Carte des valeurs aberrantes"),wellPanel(selectInput(inputId="select6",label = "polluant",choices = polluant_combustible$id_polluant),
                                                                                             selectInput(inputId="select66",label = "combustible",choices = polluant_combustible$combustible),
                                                                                             actionButton(inputId = "go6",label = "Play"))
                                   ),
                                   mainPanel(leafletOutput("mape_w_film_emi_install",height="80vh"),DTOutput("table_w_film_emi_install"))
                                   
                                 )
                                 
                        )
                        
             ),
             #Routier--------------------
             navbarMenu(title = "Secteur Routier",
                        tabPanel(title="rs_tmja",h3("rs_tmja"),
                                 sidebarLayout(
                                   sidebarPanel(h4("Carte de comparaison"),wellPanel(selectInput(inputId="select7",label = "departement",choices = list("02"="02","59"="59","60"=60,"62"=62,"80"=80)),
                                                                                     selectInput(inputId="select77",label = "type de variable",choices = list("tmja"="tmja","pctpl"="pctpl")),
                                                                                     actionButton(inputId = "go7",label = "Play")
                                   )),
                                   
                                   mainPanel(leafletOutput("mape_rs_tmja",height="80vh"))
                                   
                                 )
                                 
                        )),
             #Residentiel--------------------
             navbarMenu(title = "Secteur Residentiel",
                        tabPanel(title="tc_conso_corrigees",h3("tc_conso_corrigees"),
                                 sidebarLayout(
                                   sidebarPanel(h4("Carte de comparaison"),wellPanel(selectInput(inputId="select8",label = "napfue",choices = list("102"=102,"111"=111,"204"=204,"301"=301,"303"=303,"901"=901,"902"=902)),
                                                                                     actionButton(inputId = "go8",label = "Play")
                                   ),plotOutput("graphe_r_tc_conso_corrigees")),
                                   
                                   mainPanel(leafletOutput("mape_r_tc_conso_corrigees",height="80vh"),DTOutput("table_r_tc_conso_corrigees"))
                                   
                                 )
                                 
                        )),
             #Bilan---------------------
             navbarMenu(title = "Secteur Bilan",
                        tabPanel(title="emi_total",h3("emi_total_axxxx_v2020_v4"),
                                 sidebarLayout(
                                   sidebarPanel(h4("Carte des valeurs aberrantes"),wellPanel(selectInput(inputId="select12",label = "polluant",choices = list("SO2"="SO2","NOx"="NOx","COVNM"="COVNM","PM10"="PM10","PM25"="PM25","NH3"="NH3")),
                                                                                             selectInput(inputId="select1212",label = "code_napfue",choices = partie$code_napfue),
                                                                                             selectInput(inputId="select121212",label = "pcaet",choices = partie$pcaet),
                                                                                             selectInput(inputId="select12121212",label = "code_snap",choices = partie$code_snap),
                                                                                             sliderInput("seuil_12", "Seuil de l'aberation:",
                                                                                                         min = 0, max = 5, value = 0
                                                                                             ),
                                                                                             actionButton(inputId = "go12",label = "Play")
                                   )),
                                   
                                   mainPanel(leafletOutput("mape_emi_total",height="80vh"))
                                   
                                 )
                                 
                        )),
             #evolution_trafic---------------------
             navbarMenu(title = "Evolution_trafic",
                        tabPanel(title="evolution_trafic_tmja_2020",h3("evolution_trafic_tmja_2020"),
                                 sidebarLayout(
                                   sidebarPanel(h4("La droite de regression"),wellPanel(selectInput(inputId="select13",label = "importance",choices = importance_cat_admin$importance ),
                                                                                        selectInput(inputId="select1313",label = "cat_admin",choices = importance_cat_admin$cat_admin),
                                                                                        selectInput(inputId="select131313",label = "annee",choices = list("2018"=2018,"2019"=2019)),
                                                                                        actionButton(inputId = "go13",label = "Play")
                                   )),
                                   
                                   mainPanel(plotlyOutput("evolution_trafic_tmja_2020_reg"))
                                   
                                 )
                                 
                        ),
                        tabPanel(title="evolution_trafic_pl_2020",h3("evolution_trafic_pl_2020"),
                                 sidebarLayout(
                                   sidebarPanel(h4("La droite de regression"),wellPanel(selectInput(inputId="select14",label = "importance",choices = importance_cat_admin2$importance ),
                                                                                        selectInput(inputId="select1414",label = "cat_admin",choices = importance_cat_admin2$cat_admin),
                                                                                        selectInput(inputId="select141414",label = "annee",choices = list("2018"=2018,"2019"=2019)),
                                                                                        actionButton(inputId = "go14",label = "Play")
                                   )),
                                   
                                   mainPanel(plotlyOutput("evolution_trafic_pl_2020_reg"))
                                   
                                 )
                                 
                        )
                        
                        )
             
             
             
  )))
