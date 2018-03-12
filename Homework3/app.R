
# "DATA 608 HomeWork 3 : Shiny App"
# "Kumudini Bhave"
# "March 11, 2018"


library(shiny)
library(dplyr)
library(DT)
library(datasets)



if(!require("MASS", character.only = TRUE, quietly = TRUE)) {
     install.packages("MASS")
     library("MASS", character.only = TRUE)
}
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




mortality <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv",header=TRUE,sep=",",na.strings = c("","NA"))

mortality$ICD.Chapter = as.character(mortality$ICD.Chapter)

# Get The state abbreviations, state names , region
statesdf <- data.frame(State = state.abb,state.name,state.region,stringsAsFactors = FALSE)
statesdf$State <- as.character(statesdf$State)
statesdf$state.name <- as.character(statesdf$state.name)
statesdf$state.region <- as.character(statesdf$state.region)
colnames(statesdf) <- c("State","StateName","Region")

# Crude Rates:
#Crude Rates are expressed as the number of deaths reported each calendar year per the factor you select. The default factor is per 100,000 population, reporting the death rate per 100,000 persons.
#Crude Rate = Count / Population * 100,000 

# National Average :
# Total Mortalities Across All States For The Year / Total Population For The Year



# Getting the National Average Per Cause Per Year
# Hence we get the National Avg by Crude.Rate mean for the year and the cause.
mortality_year_cause <- mortality %>% group_by(Year, ICD.Chapter) %>% summarise(yearcauseavg = mean(Crude.Rate))


# joining mortality and states
state_mortal_df <- inner_join(mortality, statesdf,by="State")


# Region Wise Average Mortality For The Cause
mortality_year_cause_region <- state_mortal_df %>% group_by(Year, ICD.Chapter, Region) %>% summarise(yearcauseregionavg = mean(Crude.Rate))
#View(mortality_year_cause_region)

# Joining mortality states (state_mortal_df) and the calculated mortality national avgs (mortality_year_cause)
state_mortal_cause_df <- inner_join(state_mortal_df, mortality_year_cause,by=c("ICD.Chapter","Year"))


#Join further with mortality_year_cause_region to have the region wise average.
state_mortal_cause_df <- inner_join(state_mortal_cause_df, mortality_year_cause_region,by=c("ICD.Chapter","Year",'Region'))
#View(state_mortal_cause_df)

######################################       Shiny Server        ##################################

server <- function(input, output, session) {
     
     # For Question 1   
     yearmortalcause <- reactive({
          
          # Getting by causal disease and state inputted
          new_yearcause <- filter(state_mortal_cause_df, ICD.Chapter == input$inputcause, Year == input$inputyear)  %>% mutate(stateRank = dense_rank(desc(Crude.Rate))) %>% arrange(stateRank) %>% mutate(hovertxt = paste0(State, '<br>', "Mortality: ", Crude.Rate))
          #View(new_yearcause)
          
     })
     
     # Plot Of Year wise Mortality Across All States For Particular Cause
     output$mortalityCausePlot <- renderPlotly({
          # specify some map projection/options
          g <- list(
               scope = 'usa',
               projection = list(type = 'albers usa'),
               lakecolor = toRGB('white')
          )
          plot_ly(yearmortalcause(), z = ~Crude.Rate, text = ~StateName, locations = ~State,
                  type = 'choropleth', colors='YlOrRd',locationmode = 'USA-states') %>%  colorbar(title = "State Mortality") %>% layout(title = paste0(input$inputyear,' Mortality Rate Across US States Due To ', input$inputcause), geo = g)
          
     })
     
     
     
     # Summary Of Mortalities For Cause, For Year Across States
     output$mortalityCauseTable <- DT::renderDataTable({
          yearmortalcause() %>% select(StateName, Crude.Rate, stateRank) %>% DT::datatable(colnames = c('State','State Mortality','Rank')) %>% formatStyle("StateName", color = "black") %>% formatStyle("Crude.Rate", color = "black")   %>% formatStyle("stateRank", color = "black")  
     })
     
     
     
     
     # For Question 2   
     statemortalcause <- reactive({

          # Getting by causal disease and state inputted
          new_statecause <- filter(state_mortal_cause_df, ICD.Chapter == input$inputcause, StateName == input$inputstate)  
          #View(new_statecause)
     
     })
     

     calcRegion <- reactive({
          selRegion <- unique(state_mortal_cause_df[state_mortal_cause_df$StateName == input$inputstate,8])
     })
     

     # Plot Of State Mortality Vs Naitonal Avg Vs Region Avg
     output$statevsnationalPlot <- renderPlot({
          statemortalcause() %>% ggplot(aes(Year)) + geom_line(aes(y=Crude.Rate ,color = "State Mortality")) + geom_line(aes(y=yearcauseavg,color = "National Avg")) + geom_line(aes(y=yearcauseregionavg,color = "Region Avg"))  + ylab("Mortality") +
               ggtitle(paste0("Mortality For ",input$inputstate ," State Vs National Average & ", calcRegion() ," Region Average For ", input$inputcause)) +
               theme(plot.title = element_text(lineheight = 40,face="bold"))
     })
     
     # Summary Of Mortalities For State, Cause, Per Year
     output$statevsnationalTable <- DT::renderDataTable({
          statemortalcause() %>% select(Year, ICD.Chapter, Crude.Rate, yearcauseavg, yearcauseregionavg) %>% DT::datatable(colnames = c('Year','Cause','State Mortality','National Avg','Region Avg ')) %>% formatStyle("Year", color = "black") %>% formatStyle("ICD.Chapter", color = "black")   %>% formatStyle("Crude.Rate", color = "blue") %>% formatStyle("yearcauseavg", color = "red")  %>% formatStyle("yearcauseregionavg", color = "green")    
     })
   
}



###############################         Shiny UI       ##################################



ui <- navbarPage(strong("US Mortality Data"),
                 fluid = TRUE,
                   tabPanel(tags$b("Year-Wise Across States"),
                            hr(),
                            tabsetPanel(
                                 tabPanel("Plot", plotlyOutput("mortalityCausePlot")),
                                 #tags$head(tags$style(HTML("#table tr.selected {background-color:yellow}"))),
                                 tabPanel("Summary", dataTableOutput('mortalityCauseTable'))
                                 
                            ),
                            
                            hr(),
                            
                            fluidRow(
                                 column(4,
                                        sliderInput("inputyear",
                                                    label = tags$b(h4("Year")),
                                                    value = 2010, min = 1999, max = 2010
                                                    ) 
                                        ),
                                        br(),
                                        
                                 column(6, offsetState = 2,
                                        selectInput("inputcause",
                                                    tags$b(h4("Mortality Reason")),
                                                    choices = c(unique(mortality$ICD.Chapter)),selected = "Neoplasms"
                                        )
                                 )
                                 
                            )# End of fluid Row
                            
                         ),# End Of Year Wise
                   tabPanel(tags$b("State-Wise Across Years"),
                            hr(),
                            tabsetPanel(
                                 tabPanel("Plot", plotOutput("statevsnationalPlot")),
                                 #tabPanel("Plot", plotOutput("statevsnationalPlot"),plotOutput("dotPlot1")),
                                 #tags$head(tags$style(HTML("#table tr.selected {background-color:yellow}"))),
                                 tabPanel("Summary", dataTableOutput('statevsnationalTable'))
                                 
                            ),
                            
                            hr(),
                            
                            fluidRow(
                                 column(4,
                                        selectInput("inputstate",
                                                    tags$b(h4("State")),
                                                    choices = c(statesdf$StateName),selected = "North Carolina"
                                        ),
                                        br()
                                        
                                 ),
                                 column(6, offsetState = 2,
                                        selectInput("inputcause",
                                                    tags$b(h4("Mortality Reason")),
                                                    choices = c(unique(mortality$ICD.Chapter)),selected = "Neoplasms"
                                        )
                                 )
                                 
                            )# End of fluid Row
                    
                            
                         )# StateWise
) # navbarpage


shinyApp(ui = ui, server = server)
