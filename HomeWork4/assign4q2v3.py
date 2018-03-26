# -*- coding: utf-8 -*-
"""
Created on Thu Mar 22 13:58:22 2018
DATA 608 Assignment 4 : Question 2, Water Quality And Precipitation

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


# Data Preparation / Cleansing
# Converting to date format and clearning the enterocount of the special characters.
riverdf['Date'] = pd.to_datetime(riverdf['Date'], format='%m/%d/%Y')
riverdf['EnteroCount'] = pd.to_numeric(pd.Series(riverdf['EnteroCount'].str.replace('>|<',"")))

riverdf = riverdf.sort_values(['Site', 'Date'], ascending=[True, False])


riverdf.head(40)

riverdf.describe()

# Getting All the Site Names from data
available_sites = riverdf['Site'].unique()
riverdf['Month'] = riverdf['Date'].dt.month # Month In Number

river_safe = riverdf



# For Question 2, we compare the precipitation across all the months and period provided with the EnteroCount 
# for each of the Sites
# 

markdown_text1 = '''
### 4 Day Rain Stats

count    3397.000000

mean:        0.568001

std:         1.000387

min:         0.000000

25%:         0.000000

50%:         0.200000

75%:         0.700000

max:         8.500000


### Inference

It is observed that as the rainfall increases , the Enterococcus has higher existence. 
This is apparant from the plots which depict that with minimal 0 - 3 range the EnterCount are in lesser 
and safety level range , most of the times.

With Higher rain, the EnteroCount goes high, most of the times.


'''    

app = dash.Dash()
app.layout = html.Div([
        
    # Page Header
    html.Div([
        html.H1('Swim Site Quality And Precipitation Compare')
    ]),
    
     # Select Site Picker
     
     html.Div([
     
         html.Div([
            html.H3('Select Swim Site')
        ]),
   
    
         html.Div([
                dcc.Dropdown(
                    id='xaxis-column',
                    options=[{'label': i, 'value': i} for i in available_sites],
                    value='125th St. Pier'
                ),
        ],
        style={'width': '20%', 'display': 'inline-block'}),

     ]),
    html.Div([
        html.Div([
             # Para Header
                html.H3('Precipitation For Site'),
                
            # Match Results Table        
            
                dte.DataTable(
                rows=[{}],
                filterable=True,
                sortable=True,
                id='table'
                )

            ], style={'width': '60%', 'display': 'inline-block'})
        ]),
    
   html.Div([ 
        html.Div([
        # Para Header
                 html.H3('EnteroCount Vs Precipitation For Site'),

        # Graph Double Y Axis Entero And Rain
                dcc.Graph(id='indicator-graphic')
            
        ], style={'width': '60%', 'display': 'inline-block'}),

        html.Div([
             dcc.Markdown(children=markdown_text1)
        ])
    
    ])
])



  
    
@app.callback(
        dash.dependencies.Output('table', 'rows'), 
        [dash.dependencies.Input('xaxis-column', 'value')])
def update_newtable(sitename):
    """
    For user selections, return the relevant table
    """
    riverrain = None
    string_prefix = 'You have selected: '
    if sitename is not None:

        riverrain = riverdf[riverdf['Site'] == sitename]
        #        
#        newriversafe = getRecommendation(newriversafe)
        return riverrain.to_dict('records')   
        


@app.callback(
        dash.dependencies.Output('indicator-graphic', 'figure'), 
        [dash.dependencies.Input('xaxis-column', 'value')])
def update_graph(sitename):
    """
    For user selections, return the relevant table
    """
    riverrain = None
    string_prefix = 'You have selected: '
    if sitename is not None:
          riverrain = riverdf[riverdf['Site'] == sitename]
          newriverraintable = pd.DataFrame(riverrain)
          
          trace1 = go.Scatter(
                    x=newriverraintable['Date'],
                    y=newriverraintable['EnteroCount'],
                    name='EnteroCount'
              ) 
          trace2 = go.Scatter(
                    x=newriverraintable['Date'],
                    y=newriverraintable['FourDayRainTotal'],
                    name='Precipitation',
                    yaxis='y2'
              )
          return {
                  'data': [trace1, trace2],
                'layout': go.Layout(
                    xaxis={
                        'title': 'TimeLine'
                    },
                    yaxis={
                        'title': 'EnteroCount'
                    },
                    yaxis2=dict(
                            title='Precipitation (inches)',
                            titlefont=dict(
                                color='rgb(148, 103, 189)'
                            ),
                            tickfont=dict(
                                color='rgb(148, 103, 189)'
                            ),
                            overlaying='y',
                            side='right'
                      ),
                    hovermode='closest'
                 )
                  
                   
                 }
                  
   
  
if __name__ == '__main__':
    app.run_server(debug=True)