
# "DATA 608 Final Project: WildLife Strikes Shiny App"
# "Kumudini Bhave"
# "May 20, 2018"


# Imports Of Libraries

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


library(purrr)
library(highcharter)
library(DT)

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



# Extract day of month of the wildlife strike
wildlifestrike$daynum <- format(as.Date(wildlifestrike$CollisonDateTime,format = "%m/%d/%Y"),"%d")
wildlifestrike$daynum = as.numeric(wildlifestrike$daynum)

# New Column Season formed
wildlifestrike$season[wildlifestrike$month %in% c("Mar","Apr","May")] = "Spring"
wildlifestrike$season[wildlifestrike$month %in% c("Jun","Jul","Aug")] = "Summer"
wildlifestrike$season[wildlifestrike$month %in% c("Sep","Oct","Nov")] = "Fall"
wildlifestrike$season[wildlifestrike$month %in% c("Dec","Jan","Feb")] = "Winter"




# change the cost dollar to remove commas, change to numeric
wildlifestrike$CostDollar <- as.numeric(str_replace_all(wildlifestrike$CostDollar,',',''))

# Other numeric conversion which are initial factors
wildlifestrike$CostAircraftOutTime <- as.numeric(wildlifestrike$CostAircraftOutTime)
wildlifestrike$FeetAboveGround <- as.numeric(wildlifestrike$FeetAboveGround)

wildlifestrike$NumberOfStrikes <- as.numeric(wildlifestrike$NumberOfStrikes)


#change Take-off Run to TakeOff Run as hyphen in Take-ff run disrupts the sunburst level

levels(wildlifestrike$PhaseOfFlight)[levels(wildlifestrike$PhaseOfFlight) == "Take-off run"] <- "Takeoff run"
levels(wildlifestrike$ImpactToFlight)[levels(wildlifestrike$ImpactToFlight) == "Aborted Take-off"] <- "Aborted Takeoff"


# accomodating the NAs doe some of the columns 
# as the strike still was reported. Marking them as Unknown

 levels(wildlifestrike$PhaseOfFlight)<-c(levels(wildlifestrike$PhaseOfFlight),"Unknown")  #Add the extra level to the factor
wildlifestrike$PhaseOfFlight[is.na(wildlifestrike$PhaseOfFlight)] <- "Unknown"
 
 levels(wildlifestrike$TimeOfDay)<-c(levels(wildlifestrike$TimeOfDay),"Unknown")  #Add the extra level to the factor
 wildlifestrike$TimeOfDay[is.na(wildlifestrike$TimeOfDay)] <- "Unknown"           #Change NA to "None"
 
 levels(wildlifestrike$ImpactToFlight)<-c(levels(wildlifestrike$ImpactToFlight),"Unknown")  #Add the extra level to the factor
 wildlifestrike$ImpactToFlight[is.na(wildlifestrike$ImpactToFlight)] <- "Unknown"           #Change NA to "None"

 
 ########################################################################
 
 # type conversion to appropriate types
 
 wildlifestrike$OriginState = as.character(wildlifestrike$OriginState)
 wildlifestrike$OriginStateCode = as.character(wildlifestrike$OriginStateCode)
 wildlifestrike$AirportName = as.character(wildlifestrike$AirportName)
 wildlifestrike$AirportCode = as.character(wildlifestrike$AirportCode)
 
 wildlifestrike$AnimalCategory= as.character(wildlifestrike$AnimalCategory)
 wildlifestrike$SpeciesGroup = as.character(wildlifestrike$SpeciesGroup)
 wildlifestrike$Species = as.character(wildlifestrike$Species)
 
  

##################################################################################
# SUNBURST :  Wildlife Strike by Phase Of Flight, Time Of Day , WildLife Category
##################################################################################


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

#####################################################################################################################

# SUNBURST : Cost Strike by WildLife Category, Phase Of Flight, Impact on Flight , Effect..Amount.of.damage..detailed.
#####################################################################################################################



strikecost <- wildlifestrike %>% group_by(AnimalCategory, PhaseOfFlight,ImpactToFlight, AmountOfDamage) %>% dplyr::summarise( totdollars = round(sum(CostDollar, na.rm=TRUE)))

#View(strikecost)

# Wildlife Strike by 
strikecost$path <- paste(strikecost$AnimalCategory,strikecost$PhaseOfFlight, strikecost$ImpactToFlight,  strikecost$AmountOfDamage, sep = "-")




# For my learning
#strikecost$count <- strikecost$totdollars
#strikecost$count = rep(1, nrow(strikecost))

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
               ggtitle(paste0("Strikes Vs Cost Comparison For ",input$inputstate ," For Airport ", input$inputairport)) +
               theme(plot.title = element_text(lineheight = 40,face = "bold"))
     })

     
     # For Tab Panel 3

     # Puting month in chronological order Jan..Dec. for mapping
     wildlifestrike$month = factor(wildlifestrike$month, levels = month.abb)
     monthswise <- wildlifestrike %>% group_by(OriginState, month, daynum ,season) %>% dplyr::summarise( totformonthday = round(sum(NumberOfStrikes, na.rm=TRUE)))

     monthswisespecies <- wildlifestrike %>% group_by(OriginState, month, season,Species) %>% dplyr::summarise( totforspecies = round(sum(NumberOfStrikes, na.rm=TRUE)))
     monthswisetime <- wildlifestrike %>% group_by(OriginState, month ,season,TimeOfDay) %>% dplyr::summarise( totfortime = round(sum(NumberOfStrikes, na.rm=TRUE)))
     
     
     
     stateentered <- reactive({
          # Filter for month selected 
          newmonthswise <- monthswise %>%  filter( OriginState == input$inputstatemonth ) 
         # monthswise <- monthswise %>%  filter( OriginState == "North Carolina" ) 
          
          #View(monthswise)
          #View(monthswisespecies)
     })
     
      statetoptwomonths <- reactive({
          # Filter for month selected 
           statetwomonths <- wildlifestrike %>% group_by(OriginState, month) %>% dplyr::summarise( topformonthday = round(sum(NumberOfStrikes, na.rm=TRUE)))
          # View(statetwomonths)
           statetwomonths <- statetwomonths %>%  filter( OriginState == input$inputstatemonth) 
         # statetwomonths <- statetwomonths %>%  filter( OriginState == "North Carolina")
           statetwomonths <- statetwomonths %>% dplyr::arrange(desc(topformonthday))
           statetwomonths <- head(statetwomonths ,2)
           statetwomonths
      })
      
     
      output$vbox <- renderValueBox({
           statetwomonths <- statetoptwomonths()
           valueBox(
                paste0(statetwomonths$month[1]," & " , statetwomonths$month[2])
                ,paste('Top Strikes Months For This State Per Data')
                ,icon = icon("info-sign",lib='glyphicon')
                ,color = "purple", width = 4)
           
           
      })
      
      
     stateenteredmonth <- reactive({
          # Filter for state and month selected 
          
          newmonthswisetime <- monthswisetime %>%  filter( OriginState == input$inputstatemonth ) %>%  filter( month == input$inputmonth ) 
          #View(monthswise)
          #View(monthswisespecies)
     })
     
     statemonthspecies <- reactive({
     monthswisespeciestime <- wildlifestrike %>% group_by(OriginState, month, season,TimeOfDay,Species) %>% dplyr::summarise( TotalStrikes = round(sum(NumberOfStrikes, na.rm=TRUE))) %>%  filter( OriginState == input$inputstatemonth ) %>%  filter( month == input$inputmonth ) 
     
     newmonthswisespeciestime <- data.frame(monthswisespeciestime$TimeOfDay,monthswisespeciestime$Species,monthswisespeciestime$TotalStrikes)
     colnames(newmonthswisespeciestime) <- c("TimeOfDay","Species","TotalStrikes")
     newmonthswisespeciestime 
     })
     
     # Plot Of State Wise Month Plot
     output$statemonthwisePlot <- renderPlot({
          stateentered() %>% ggplot( aes(x = daynum,y = totformonthday, color = season)) + geom_bar(stat = "identity") + facet_wrap(~month) 
     })
     
     # time of day donut
     output$choosenmonthdonut <- renderPlot({
          
          dat<- data.frame(stateenteredmonth())
          
          dat$fraction = dat$totfortime / sum(dat$totfortime)
          dat = dat[order(dat$fraction), ]
          dat$ymax = cumsum(dat$fraction)
          dat$ymin = c(0, head(dat$ymax, n=-1))
          
          # Make the plot
          p1 = ggplot(dat, aes(fill=TimeOfDay, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
               geom_rect() +
               coord_polar(theta="y") + xlab("") + ylab("") +
               xlim(c(0, 4)) +
               theme(panel.grid=element_blank()) +
               theme(axis.text=element_blank()) +
               theme(axis.ticks=element_blank()) +
               annotate("text", x = 0, y = 0, label = "Strike Time") +
               labs(title="")
          p1
          
          
     })
     
     
     
     
     
     output$table <- DT::renderDataTable(
         # newstatemonthspecies <- statemonthspecies() 
          DT::datatable(statemonthspecies(), options = list( list(3, 'desc'),lengthChange = FALSE, pageLength = 5, selection = 'single'), rownames= FALSE)
          )
     
     
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
          menuItem("Sunburst Plots", tabName = "sunTab", icon = icon("certificate",lib = "glyphicon")),
          menuItem("Maps", tabName = "mapTab", icon = icon("map-marker",lib = "glyphicon")),
          menuItem("Charts", tabName = "chartTab", icon = icon("stats" ,lib = "glyphicon")),
          menuItem("Bar Graphs", tabName = "barTab", icon = icon("bar-chart-o")),
          menuItem("Data", icon = icon("file-code-o"), 
                   href = "https://github.com/DoDataDriven/DATA608/tree/master/FinalProject")
          
          
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
                       # A static infoBox
                       infoBox("Number Of Strikes Reported", icon = icon("info-sign" ,lib = "glyphicon"))
                  ),
                  fluidRow(
                       box(
                            width = 8, status = "info", solidHeader = TRUE,
                            title = "Animal Category-Phase Of Flight-Flight Impact-Damage Level",
                            sunburstOutput("strikecostsunburstPlot", height = "600", width = "100%")
                       ),
                       infoBox("Cost % For Strikes ",  icon = icon("info-sign" ,lib = "glyphicon"))
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
                       
                       column(8, offsetState = 2,
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
                    ),                  # end of chartTab
                  tabItem("barTab",
                          
                          fluidRow(
                               box(
                                    width = 6, status = "info", solidHeader = TRUE,
                                    title = "Strikes By Month (Across 2000 - 2015)",
                                    # highchartOutput("statemonthwisePlot", height = "800", width = "100%")
                                    plotOutput("statemonthwisePlot", height = "800", width = "100%")
                               ),
                               box(
                                    width = 6, status = "info", solidHeader = TRUE,
                                    title = "Time Of Day",
                                    # highchartOutput("statemonthwisePlot", height = "800", width = "100%")
                                    plotOutput("choosenmonthdonut", height = "400", width = "100%")
                               ),
                               box(
                                    width = 6, status = "info", solidHeader = TRUE,
                                    title = "Species Stats (2000 - 2015)", dataTableOutput("table")
                               )
                               
                          ),
                          fluidRow(
                               
                               column(4,
                                      selectInput("inputstatemonth",
                                                  tags$b(h4("State")),
                                                  choices = c(unique(wildlifestrike$OriginState)),selected = "North Carolina"
                                      ),
                                      br()
                                      
                                ),
                               valueBoxOutput("vbox"),
                               column(width = 4,offsetState = 40,
                                      selectInput("inputmonth",
                                                  tags$b(h4("Select Month For Details")),
                                                  choices = c(unique(wildlifestrike$month)),selected = "Jan"
                                      ),
                                      br()
                               )
                               
                          )
                          
                  ) # end of barTab

         
          
     )# end of tabitems
     
     

)# end dashboard


# ui  dashboardPage
ui <- dashboardPage(title = 'WildLife Strikes', headerDash, sidebarDash, bodyDash, skin='blue')

shinyApp(ui = ui, server = server)
     
    
     
     
     
