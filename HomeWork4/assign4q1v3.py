# -*- coding: utf-8 -*-
"""
Created on Thu Mar 22 13:58:22 2018
DATA 608 Assignment 4 : Question 1, Safe Swim Site Recommender

@author: Kumudini Bhave
"""
import dash
import dash_core_components as dcc
import dash_html_components as html
import plotly.graph_objs as go
import dash_table_experiments as dte

import pandas as pd
from datetime import datetime as dt
from scipy.stats.mstats import mode, gmean, hmean

import plotly.plotly as py
import plotly.graph_objs as go
import plotly.figure_factory as ff
import numpy as np



# Assignment 4 Data
# https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module4/Data/riverkeeper_data_2013.csv

date_month = None

riverdf = pd.read_csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module4/Data/riverkeeper_data_2013.csv')
riverdf.dtypes

# Data Preparation And Cleansing
# Converting to date format and clearning the enterocount of the special characters.
riverdf['Date'] = pd.to_datetime(riverdf['Date'], format='%m/%d/%Y')
riverdf['EnteroCount'] = pd.to_numeric(pd.Series(riverdf['EnteroCount'].str.replace('>|<',"")))

# Sorting b site and Date
riverdf = riverdf.sort_values(['Site', 'Date'], ascending=[True, False])

riverdf.head(40)

riverdf.describe()


available_sites = riverdf['Site'].unique()
riverdf['Month'] = riverdf['Date'].dt.month # Month In Number

river_safe = riverdf

############################################## To Convert into month name ###################################
#river_safe['month'] = river_safe['Date'].dt.to_period('M')
#river_safe['month'] = river_safe['Date'].apply(lambda x:x.strftime('%B')) # Month In Words


############################################### Gives Arithmatic mean and count ##################################
#river_safe_gr1 = river_safe.groupby(['Site','Month']).agg(['mean','count'])
#river_safe_gr = river_safe.groupby(['Site','Month'], as_index=False).mean()[['Site', 'EnteroCount','SampleCount', 'Month']].round(2)


####################### Gives count of cases considered in geometric mean for that month #########################
river_safe_grmoncount = river_safe.groupby(['Site','Month']).size()

########################## Calculates Geometric mean per Site and Month Of Year..considering the month enterocount for all years #################3
river_safe_grgmean = river_safe.groupby(['Site','Month'], as_index=False).EnteroCount.apply(gmean).round(2)


river_safe_df =  pd.concat([river_safe_grgmean, river_safe_grmoncount], axis=1, join='inner')
river_safe_df = river_safe_df.reset_index()

river_safe_df.columns = ['Site','Month','EnteroGMean','Samples'] 



# For Question 1, we consider that the Enterocount provided are the geometric means of the 35 samples taken on that particular day.
# Hence a User is allowed to check which Sites are safe, by entering date and list of safe sites would be recommended to the user.
def generate_table(dataframe, max_rows=10):
    '''Given dataframe, return template generated using Dash components
    '''
    return html.Table(
        # Header
        [html.Tr([html.Th(col) for col in dataframe.columns])] +

        # Body
        [html.Tr([
            html.Td(dataframe.iloc[i][col]) for col in dataframe.columns
        ]) for i in range(min(len(dataframe), max_rows))]
    )

    

markdown_text1 = '''
###   Observations And Assumptions:

From the River Data , it is observed that the EnteroCount Samples 
have been taken across years from 2006 - 2013 primarily in the Summer months.
i.e. Most of the data available is for the samples taken from April To October.

Since any aspiring swimmer would like to check the water quality and is likely to swim in these months, 
the water quality test version application takes into account the date entered by the user.
It captures the 'Month' and finds the geometric mean of the EnteroCount levels taken during that month 
across all years for all sites.

### The Recommender

The recommender follows the Federal guidelines of safety standards. The Following are considered safe.
1.  Single Sample with less than 110 Enterococcus/100 mL
2.  Five or more samples with a geometric mean (a weighted average) less than 30 Enterococcus/100 mL.

It then displays the Sites that show the Least Geometric Mean of EnteroCount For The Mode Of Samples (most common sample number ) taken during that month . 
It also displays the Site that is most tested ( with most samples) and which is within the safety standards with least Enterocount.



'''    
    

app = dash.Dash()
app.layout = html.Div([
        
    # Page Header
    html.Div([
        html.H1('Swim Site Recommender')
    ]),
    
    html.Div([
             dcc.Markdown(children=markdown_text1)
    ]),
    
     # Select Date Picker
     html.Div([
    
        html.Div([
            html.H4('Select Swim Date')
        ]),
     
        dcc.DatePickerSingle(
            id='my-date-picker-single',
            min_date_allowed=dt(2006, 1, 1),
            max_date_allowed=dt(2013, 12, 31),
            show_outside_days=False,
            initial_visible_month=dt(2013, 8, 5),
            date=dt(2013, 8, 5)
        ),
    ]),
    

    html.Div([
         
        html.Div(id='output-container-date-picker-single'),

        # Match Results Table
                
        html.Div(
            dte.DataTable(
            rows=[{}],
            filterable=True,
            sortable=True,
            id='table'
            )
        )
    ]),
            
     html.Div([
        # Para Header
                 html.H3('Site EnteroCount HeatMap'),

        # Graph Double Y Axis Entero And Rain
                dcc.Graph(id='indicator-graphic')
            
        ]),
])







@app.callback(
    dash.dependencies.Output('output-container-date-picker-single', 'children'),
    [dash.dependencies.Input('my-date-picker-single', 'date')])
def update_output(date):
    string_prefix1 = 'We recommend the following sites for  the month of '
    string_prefix2 = ' for your safe swim !!'
    if date is not None:
        date = dt.strptime(date, '%Y-%m-%d')
        date_string = date.strftime('%B') # Returning name of the Month , from date selected
        date_month = date.strftime('%m') # Returning the Month of Selection Date
        return string_prefix1 + date_string + string_prefix2 # Returning the Month numerical for comparison for those months readings across all years for that site.





def getRecommendation(swimsitedf):

    
    conditions  = [ (swimsitedf['Samples'] >= 5) & (swimsitedf['EnteroGMean'] <= 30), (swimsitedf['Samples'] == 1) & (swimsitedf['EnteroGMean'] <= 110) ,(swimsitedf['Samples'] < 5) & (swimsitedf['EnteroGMean'] <= 30), (swimsitedf['Samples'] != 1) & (swimsitedf['EnteroGMean'] > 30) ]

    choices     = [ 1, 1, 2, 5 ]

    swimsitedf["Rating"] = np.select(conditions, choices, default=np.nan)

    swimsiteBySampledf = swimsitedf.sort_values(['Rating','Samples', 'EnteroGMean'], ascending=[True,False,True])
    swimsiteBySampledf.Rating.replace([1, 2, 5], ['Good', 'Less data', 'Bad'],  inplace=True)

    swimsiteByGMeandf = swimsitedf.sort_values(['Rating','EnteroGMean','Samples'], ascending=[True,True, False])
    swimsiteByGMeandf.Rating.replace([1, 2, 5], ['Good', 'Less data','Bad'],  inplace=True)


 ############## Finding Site From the Common / Most occurred Sample Number for the Month ###################
    # Finding the Mode (Number Of Samples) For The Month Selected

    monthsamplemode = (swimsiteBySampledf['Samples'].mode()[0])

    # Finding the best sites for the common number of samples for that mode
    modesitedf = swimsiteBySampledf[(swimsiteBySampledf['Samples'] == monthsamplemode) & (swimsiteBySampledf['Rating'] == 'Good')]
    # Top 3 sites with least Enterocount for the commonly number of samples.
    modesitedfrec1 = (modesitedf.sort_values(['EnteroGMean'], ascending=[True])).head(3)

    ########################
    ################ Finding the Site Withing Safety Standards With the most samples for the month ###############

    maxsamplegoodsiterec2 = swimsiteBySampledf.head(1)

    ################ Finding Site with the least EnterCount But Least tested.(less samples) Dropped as considered Bad ################

    #minsamplegoodsite = swimsiteBySampledf[swimsiteBySampledf['Samples'] == monthsamplemode & swimsiteBySampledf['Rating'] == 'Less data']

    return modesitedfrec1.append(maxsamplegoodsiterec2) # Combined Recommendations


 
    


    
@app.callback(
        dash.dependencies.Output('table', 'rows'), 
        [dash.dependencies.Input('my-date-picker-single', 'date')])
def update_newtable(date):
    """
    For user selections, return the relevant table
    """
    date_month = None
    string_prefix = 'You have selected: '
    if date is not None:
        date = dt.strptime(date, '%Y-%m-%d')
        date_string = date.strftime('%B %d, %Y')
        date_month = date.strftime('%m')
        newriversafe = river_safe_df[river_safe_df['Month'] == int(date_month)]
        
        newriversafe = getRecommendation(newriversafe)
        return newriversafe.to_dict('records')   





@app.callback(
        dash.dependencies.Output('indicator-graphic', 'figure'), 
        [dash.dependencies.Input('my-date-picker-single', 'date')])
def update_graph(date):
    """
    For user selections, return the relevant table
    """
    date_month = None
    string_prefix = 'You have selected: '
    if date is not None:
        date = dt.strptime(date, '%Y-%m-%d')
        date_string = date.strftime('%B %d, %Y')
        date_month = date.strftime('%m')
        newriversafe = river_safe_df[river_safe_df['Month'] == int(date_month)]
        
        newriversafe = getRecommendation(newriversafe)
    

        trace = go.Heatmap(z=newriversafe['EnteroGMean'],
                   y=newriversafe['Site'],
                   x=newriversafe['Samples'],
                   colorscale='Viridis',
                   colorbar = dict(title = 'EnteroCount (Weighted Mean)', titleside = 'top'),
                   hoverinfo='text',
                   text=newriversafe['EnteroGMean'])
        
        return {
                'data': [trace],
                'layout': go.Layout(
                    xaxis={
                        'title': 'Samples'
                    },
                    yaxis={
                        'title': 'Swim Site'
                    },
                    hovermode='closest'
                )
                }
    
    
                   
                 
      
if __name__ == '__main__':
    app.run_server(debug=True)