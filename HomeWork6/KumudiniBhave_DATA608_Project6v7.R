
# "DATA 608 HomeWork 6 : WildLife Strikes Shiny App"
# "Kumudini Bhave"
# "April 29, 2018"


library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(datasets)
library(tidyr)
library(lubridate)
library(sunburstR)
library(htmlwidgets)
library(stringr)
library(rgdal)

library(maps)
library(sp) 
library(geojsonio)

if(!require("leaflet", character.only = TRUE, quietly = TRUE)) {
     install.packages("leaflet")
     library("leaflet", character.only = TRUE)
}

# adding the shinythemese package
if(!require("shinythemes", character.only = TRUE, quietly = TRUE)) {
     install.packages("shinythemes")
     library("shinythemes", character.only = TRUE)
}


if(!require("plotly", character.only = TRUE, quietly = TRUE)) {
     install.packages("plotly")
      library("plotly", character.only = TRUE)
 }


######################## DATA EXPLORATION ######################################

#https://raw.githubusercontent.com/DoDataDriven/DATA608/master/HomeWork6/faa_data_subset.csv

wildlifestrike <- read.csv("https://raw.githubusercontent.com/DoDataDriven/DATA608/master/HomeWork6/faa_data_subset.csv", encoding='UTF-8', header = TRUE,sep = ",",na.strings = c("","NA"))
colnames(wildlifestrike) <- c("AirportCode","AirportName","OriginState","OriginStateCode","Country","AircraftType","AircraftNumEngines","CollisonDateTime","TimeOfDay","PhaseOfFlight","AmountOfDamage","ImpactToFlight","IndicatedDamage","CostAircraftOutTime","CostDollar","Days","FeetAboveGround","MilesFromAirport","AnimalCategory","SpeciesOrder","SpeciesGroup","Species","SpeciesId","NumberOfStrikes","RecordId")
#View(wildlifestrike)


# Extract year of strike
wildlifestrike$year <- year(as.Date(wildlifestrike$CollisonDateTime,format = "%m/%d/%Y"))

# Extract month (spelled short) of strike
wildlifestrike$month <- format(as.Date(wildlifestrike$CollisonDateTime,format = "%m/%d/%Y"),"%b")

# change the cost dollar to remove commas, change to numeric
wildlifestrike$CostDollar <- as.numeric(str_replace_all(wildlifestrike$CostDollar,',',''))

# Other numeric conversion which are initial factors
wildlifestrike$CostAircraftOutTime <- as.numeric(wildlifestrike$CostAircraftOutTime)
wildlifestrike$FeetAboveGround <- as.numeric(wildlifestrike$FeetAboveGround)

wildlifestrike$NumberOfStrikes <- as.numeric(wildlifestrike$NumberOfStrikes)


#change Take-off Run TakeOff Run as hyphen in Take-ff run disrupts the sunburst level

levels(wildlifestrike$PhaseOfFlight)[levels(wildlifestrike$PhaseOfFlight) == "Take-off run"] <- "Takeoff run"
levels(wildlifestrike$ImpactToFlight)[levels(wildlifestrike$ImpactToFlight) == "Aborted Take-off"] <- "Aborted Takeoff"

 levels(wildlifestrike$PhaseOfFlight)<-c(levels(wildlifestrike$PhaseOfFlight),"Unknown")  #Add the extra level to your factor
wildlifestrike$PhaseOfFlight[is.na(wildlifestrike$PhaseOfFlight)] <- "Unknown"
 
 levels(wildlifestrike$TimeOfDay)<-c(levels(wildlifestrike$TimeOfDay),"Unknown")  #Add the extra level to your factor
 wildlifestrike$TimeOfDay[is.na(wildlifestrike$TimeOfDay)] <- "Unknown"           #Change NA to "None"
 
 levels(wildlifestrike$ImpactToFlight)<-c(levels(wildlifestrike$ImpactToFlight),"Unknown")  #Add the extra level to your factor
 wildlifestrike$ImpactToFlight[is.na(wildlifestrike$ImpactToFlight)] <- "Unknown"           #Change NA to "None"

 
 #######################
 
 
 
 wildlifestrike$OriginState = as.character(wildlifestrike$OriginState)
 wildlifestrike$OriginStateCode = as.character(wildlifestrike$OriginStateCode)
 wildlifestrike$AirportName = as.character(wildlifestrike$AirportName)
 wildlifestrike$AirportCode = as.character(wildlifestrike$AirportCode)
 
 wildlifestrike$AnimalCategory= as.character(wildlifestrike$AnimalCategory)
 wildlifestrike$SpeciesGroup = as.character(wildlifestrike$SpeciesGroup)
 wildlifestrike$Species = as.character(wildlifestrike$Species)
 
  

#######################################################################
# SUNBURST :  Wildlife Strike by Phase Of Flight, Time Of Day , WildLife Category
#######################################################################


strikephase <- wildlifestrike %>% dplyr::group_by(PhaseOfFlight, TimeOfDay, AnimalCategory) %>% dplyr::summarise(totstrikes = sum(NumberOfStrikes))
#View(strikephase)


# Wildlife Strike by 
strikephase$path = paste(strikephase$PhaseOfFlight, strikephase$TimeOfDay, strikephase$AnimalCategory, sep = "-")
# No need to have separate count 
#strikephase$count = rep(1, nrow(strikephase))
#strikephase$count = totstrikes



# Sunburst showing % of each level
strikephasesunburst <- sunburst(
     data.frame(xtabs(totstrikes~path, strikephase)),
     explanation = htmlwidgets::JS( 
          '
          function(d){
          var explanation = [];
          var parent = d.parent;
          if(d.depth > 1){
          while(parent){
          if(parent.parent){
          explanation.push(
          (100*d.value/parent.value).toPrecision(3) + "%"
          );
          }
          parent = parent.parent;
          };
          }
          explanation.push((100*d.value/this).toPrecision(3) + "%");
          return explanation.join("<br/>");
          }'
          )
     )

# To Call in Shiny menu as
# strikephasesunburst

#######################################################################

# SUNBURST : Cost Strike by WildLife Category, Phase Of Flight, Impact on Flight , Effect..Amount.of.damage..detailed.
########################################################################



strikecost <- wildlifestrike %>% group_by(AnimalCategory, PhaseOfFlight,ImpactToFlight, AmountOfDamage) %>% dplyr::summarise( totdollars = round(sum(CostDollar, na.rm=TRUE)))

#View(strikecost)

# Wildlife Strike by 
strikecost$path <- paste(strikecost$AnimalCategory,strikecost$PhaseOfFlight, strikecost$ImpactToFlight,  strikecost$AmountOfDamage, sep = "-")
#strikecost$count <- strikecost$totdollars
#strikecost$count = rep(1, nrow(strikecost))

# For my learning
# basic sunburst
#sunburst(data.frame(xtabs(count~path, strikecost)))

# citation: https://bl.ocks.org/timelyportfolio/904dae701fee8e73ba621798edf6ee26
# Sunburst showing % of each level

strikecostsunburst <- sunburst(
     data.frame(xtabs(totdollars~path, strikecost)),
     # added a degree of difficulty by providing
     #  not easily sortable names
     explanation = htmlwidgets::JS(
          '
          function(d){
          var explanation = [];
          var parent = d.parent;
          if(d.depth > 1){
          while(parent){
          if(parent.parent){
          explanation.push(
          (100*d.value/parent.value).toPrecision(3) + "%"
          );
          }
          parent = parent.parent;
          };
          }
          explanation.push((100*d.value/this).toPrecision(3) + "%");
          return explanation.join("<br/>");
          }'
          )
     )


###################################################################


######################################################################


# For ChoroPhleth Map Year /Species Wise
wlstrike_year_cause <- wildlifestrike %>% group_by(year, OriginState, Species) %>% summarise(yearcausesum = sum(NumberOfStrikes, na.rm = T))
#View(wlstrike_year_cause)


# For Plots/ Charts statewise /Airport wise

wlstrike_state_airport_strike <- wildlifestrike %>% group_by(year, OriginState, AirportName, AnimalCategory) %>% summarise(speciesstrikesum = sum(NumberOfStrikes, na.rm = T))

wlstrike_state_airport_cost <- wildlifestrike %>% group_by(year, OriginState, AirportName, AnimalCategory) %>% summarise(speciescostsum = sum(CostDollar, na.rm = T))

wlstrike_state_airport <-  inner_join(wlstrike_state_airport_strike, wlstrike_state_airport_cost, by = c("year", "OriginState", "AirportName","AnimalCategory"))


#View(wlstrike_state_airport)

##############################
# Getting Shape File for US state map
tmp <- tempdir()
url <- "http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_state_500k.zip"

file <- basename(url)

download.file(url, file)

unzip(file, exdir = tmp)

cb_2017_us_state_500k <- readOGR(dsn = tmp, layer = "cb_2017_us_state_500k", encoding = "UTF-8")

#head(cb_2017_us_state_500k@data)

#dim(cb_2017_us_state_500k)
#colnames(cb_2017_us_state_500k)
###############################

######################################       Shiny Server        ##################################

server <- function(input, output, session) {
     
     # Plot Of Sunburst Strike Phase
     output$strikephasesunburstPlot <- renderSunburst({
          strikephasesunburst
     })
     
     # Plot Of Sunburst Strike Cost
     output$strikecostsunburstPlot <- renderSunburst({
          strikecostsunburst
     })
     
     # For Tab Panel 1  
     yearstrikecause <- reactive({
          
          # Getting by causal disease and state inputted
          new_yearcause <- dplyr::filter(wlstrike_year_cause, Species == input$inputcause, year == input$inputyear) #%>% mutate(stRank = dense_rank(desc(new_yearcause$yearcausesum)) )# %>% arrange(stateRank) %>% mutate(hovertxt = paste0(OriginStateCode, '<br>', "Strikes: ", yearcausesum))
          #new_yearcause <- dplyr::filter(wlstrike_year_cause, Species == "Gulls", year == "2010")# %>% mutate(stRank = dense_rank(desc(new_yearcause$yearcausesum)) )
          new_yearcause$stRank <- dense_rank(desc(new_yearcause$yearcausesum)) 
          #new_yearcause$hovertxt = paste0(OriginState, '<br>', "Strikes: ", yearcausesum)
          #View(new_yearcause)
          new_yearcause
       
     })
     
     
     output$strikeCausePlot <- renderLeaflet({
          
          new_yearcause1 <-  yearstrikecause()
          colnames(new_yearcause1) <- c("year", "NAME", "Species", "yearcausesum" , "stRank")
          # change the state column name to that matching in spatial data , this seems case sensitive
          # required to join later
          new_yearcause1$NAME = factor(new_yearcause1$NAME)
          
          
     
          
          #class(cb_2017_us_state_500k)
          # join the spatial data and non spatial attributes
          usstate_join <- dplyr::full_join(cb_2017_us_state_500k@data, new_yearcause1)
          # merging Important step
          new_usstate_join <- sp::merge(x=cb_2017_us_state_500k, y=usstate_join)
          
          # an option for pop ups on clicking of state , add popup = state_popupclick
          # state_popupclick <- paste0("<strong>State: </strong>", 
          #                       new_usstate_join$NAME, 
          #                       "<br><strong> </strong>", 
          #                       new_usstate_join$yearcausesum)
          
          # could use provider tiles
          # leaflet(data = new_usstate_join) %>%
          #      addProviderTiles("CartoDB.Positron") %>%
          #      addPolygons(fillColor = ~pal(yearcausesum), 
          #                  fillOpacity = 0.8, 
          #                  color = "#BDBDC3", 
          #                  weight = 1, 
          #                  popup = state_popup)
          
          
          
          # an option for pop ups on hoveirng of state , add label = state_popup
          state_popup <- sprintf("<strong>%s</strong><br/> Total Strikes by %s: %g <br/> State Rank For Year %g:  %g", 
                                                         new_usstate_join$NAME, 
                                                         new_usstate_join$Species,
                                                         new_usstate_join$yearcausesum, 
                                                         new_usstate_join$year,
                                                         new_usstate_join$stRank)  %>% lapply(htmltools::HTML)
          
          bins <- c(0,2,5,10,15,20,50,75,100,150)
          pal <- colorBin("Reds", domain = new_usstate_join$yearcausesum, bins = bins)
          
          
          m <- leaflet(new_usstate_join) %>% setView(-96, 37.8, 4) %>% addTiles() %>%  addPolygons()
          
          m <- m %>% 
               addPolygons(fillColor = ~pal(new_usstate_join$yearcausesum), 
                           weight = 1, opacity = 4, color = "grey",dashArray = "3", 
                           fillOpacity = 0.7, highlight = highlightOptions( 
                                weight = 2,
                                color = "red",
                                dashArray = "",
                                fillOpacity = 0.7,
                                bringToFront = TRUE), label = state_popup) %>% 
               addLegend("bottomright", 
                         pal = pal, 
                         values = ~new_usstate_join$yearcausesum,
                         title = "Number Of Strikes",
                         opacity = 1)
          
                  
     })
     
     
      
  
     
     # For Tab Panel 2
     #selectin state wise airport
     output$choose_airport <- renderUI({
          # If missing input, return to avoid error later in function
          if(is.null(input$inputstate))
               return()
          
          # Getting by airport name as per state inputted
          onlystateairport <- filter(wlstrike_state_airport,  OriginState == input$inputstate)
          
          
          # Create airport select dropdown
          selectInput("inputairport",
                         tags$b(h4("Airport")),
                         choices = c(unique(onlystateairport$AirportName)),selected = "RALEIGH-DURHAM INTL"
                       )
          
     })
     
     
     stateairportstrike <- reactive({
          
          # Getting by airport name and state inputted
          new_stateairport <- filter(wlstrike_state_airport, AirportName == input$inputairport, OriginState == input$inputstate)
          #View(new_statecause)
          
     })
     
     # Plot Of State Mortality Vs Naitonal Avg Vs Region Avg
     output$statewisePlot <- renderPlot({
          stateairportstrike() %>% ggplot(aes(year)) + geom_line(aes(y = speciesstrikesum ,color = "Species Strikes")) + geom_line(aes(y=speciescostsum, color = "Srike Cost"))  + ylab("Strikes / Cost ") +
               ggtitle(paste0("Strikes Vs Cost Comparison For ",input$inputstate ," For Species Of ", input$inputairport)) +
               theme(plot.title = element_text(lineheight = 40,face = "bold"))
     })
    
     # For front dashboard 
    output$putimg <- renderUI({
         
         tags$img(src="aeroplane-bird.jpg",align="center", width= 1000, height=800)
    })
     
}






###############################         Shiny UI       ##################################
#Dashboard header 
headerDash <- dashboardHeader(title = "WildLife Strikes")  



#Sidebar for dashboard

sidebarDash <- dashboardSidebar( 
     sidebarMenu(
          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Sunburst Plots", tabName = "sunTab", icon = icon("sun")),
          menuItem("Maps", tabName = "mapTab", icon = icon("glyphicon")),
          menuItem("Charts", tabName = "chartTab", icon = icon("glyphicon"))
          
     )
)

bodyDash <- dashboardBody(
     tabItems(
          tabItem("dashboard",
                  fluidRow(
                       box(
                            uiOutput("putimg", height = "600", width = "100%")
                       )
                  )
                  
          ),
          tabItem("sunTab",
                  fluidRow(
                       box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Phase Of Flight-Time Of Day-Animal Category",
                            sunburstOutput("strikephasesunburstPlot", height = "600", width = "100%")
                       ),
                       box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Animal Category-Phase Of Flight-Flight Impact-Damage Level",
                            sunburstOutput("strikecostsunburstPlot", height = "600", width = "100%")
                       )
                       
                  )
                  
          ),
          tabItem("mapTab",
                  fluidRow(
                       box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Strikes By Species",
                            leafletOutput("strikeCausePlot", height = "600", width = "100%")
                       )
                   ),
                  fluidRow(
                       column(4,
                              sliderInput("inputyear",
                                          label = tags$b(h4("Year")),
                                          value = 2010, min = 2000, max = 2015
                              ) 
                       ),
                       br(),
                       
                       column(6, offsetState = 2,
                              selectInput("inputcause",
                                          tags$b(h4("WildLife Species")),
                                          choices = c(unique(wildlifestrike$Species)),selected = "Red-tailed hawk"
                              )
                       )
                       
                  )# End of fluid Row
          ),
          tabItem("chartTab",
                  
                  fluidRow(
                       box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Strikes By State",
                            plotOutput("statewisePlot", height = "600", width = "100%")
                       )
                  ),
                  fluidRow(
                       
                            column(4,
                                   selectInput("inputstate",
                                               tags$b(h4("State")),
                                               choices = c(unique(wildlifestrike$OriginState)),selected = "North Carolina"
                                   ),
                                   br()
                                   
                            ),
                            column(6, offsetState = 2,
                                   uiOutput("choose_airport")
                                   
                            )
                          
                  )
          
          )
          
     )
     
     

)


# ui  dashboardPage
ui <- dashboardPage(title = 'WildLife Strikes', headerDash, sidebarDash, bodyDash, skin='blue')

shinyApp(ui = ui, server = server)
     
    
     
     
     
