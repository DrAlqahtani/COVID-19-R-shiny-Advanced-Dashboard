# Functions for creating fluid page layouts in shiny Application.
# A fluid page layout consists of rows which in turn include columns
################################################################################

ui <- material_page( include_nav_bar = TRUE,include_fonts=TRUE,nav_bar_color ="indigo lighten-2",background_color="grey lighten-4",
  
tags$style(" 
body {font-family: Cairo !important;} 
          .card {border: 2px solid #B1B1B1;
                     padding: 10px;
                     border-radius: 15px;
                 margin: .2rem 0 .2rem 0;
                 }

             .card:hover {
             box-shadow: 0 6px 10px 0 rgba(0,0,0,0.14), 0 1px 18px 0 rgba(0,0,0,0.12), 0 3px 5px -1px rgba(0,0,0,0.3);}
            .card .card-content .card-title {
           text-align: center;
           color: #bcbcbc;
              }
          
          h5 {
              text-align: center;
              font-size: 2rem;
              color: #8282bf;
          } 
h6 {
              text-align: center;
           font-size: 2rem;
           color: red;
}

h3 {
              text-align: center;
font-size: 2rem;
color: #bcbcbc;
}

h2 {
              text-align: center;
font-size: 1rem;
color: #bcbcbc;
}
          .author {
          position: absolute;
          left: 7%;
          right: 7%;
          bottom: 7%;
          color: #888888;
           border: 2px solid #b1b1b19c;
           border-radius: 15px;
           padding: 12px;
          width: 86%;
          text-align: center;
}

          .author:hover {
           color: #8282bf;
           border: 2px solid #b1b1b19c;
           border-radius: 15px;
           background-color: #f3f3f3;
           padding: 12px;
           box-shadow: 0 6px 10px 0 rgba(158, 158, 158, 0.14), 0 1px 18px 0 rgba(169, 169, 169, 0.12), 0 3px 5px -1px rgba(216, 216, 216, 0.14);}

           }
           }
           
           "
            
              ),
                      material_side_nav(fixed=TRUE,
                        image_source = "cor2.png",
                        material_row(
                        #   material_column(
                        #     offset = 1,
                        #     width = 10,
                        #     material_text_box(
                        #       input_id = "example_text_box",
                        #       label = "twitter Hashtag",
                        #       color = "#8283BB"
                        #     )
                        #   )
                        # ,
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # material_column(
                        #   offset = 1,
                        #   width = 10,
                        #   material_slider(
                        #     input_id = "example_slider",
                        #     label = "Number of tweets",
                        #     min_value = 200,
                        #     max_value = 3000,
                        #     initial_value = 500,
                        #     color = "#8283BB"
                        #   )
                        # )
                        # ,
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                        # tags$br(),
                          material_column(
                            width = 10,
                            offset = 1,
                            HTML("<a class='author' href='https://twitter.com/Alqahtani_khald' target='_blank'> Author <i class='material-icons'>open_in_new</i></a>")
                          )
                        )
              
                    
                     
                 
  #fluidRow(
  #column(
  #width = 0,
  # selectInput("selnodes", "Nodes selection :",
  # matchdata$user.names),
  #selectInput("Color", "Color :",
  #  1:5)
  #),
  # column(
  #  width = 8,
  # material_parallax(
  #   image_source = "sna.png"
  # )
  # ,
  
     
    
     
     
     
    # ,
     
     # material_column(
     #   width = 4,
     #   material_dropdown(
     #    input_id = "rtrep_dropdown",
     #   label = "Social Network based on",
     #    choices = 
     #    c("Retweet","Reply")
     #   ,selected = "Retweet" ,color = "#8283BB"
     #   )
     #)
     #,
   
   #material_column(
     #width = 4,
     #material_dropdown(
     #  input_id = "example_dropdown",
      # label = "User",
      # choices = 
       #  matchdata$user.names
       #, color = "#8283BB"
     #))
   
  ,
material_side_nav_tabs(
  side_nav_tabs  = c(
    "Home Page" = "home",
    "Descriptive Analysis" = "disAnal",
    "Growth rate & Curve-flattening" = "GRP",
    #"Twitter Analysis" = "twit",
    "Daily cases forecasting"="DC",
    "Cumulative forecasting"="forcast",
    "Newspaper Analysis" = "news",
    "TV Analysis" = "TV"
  ),icons=c("home","adjust","trending_up","chat","trending_up","desktop_mac","tv"),color="indigo lighten-2"
)),
##############################################################
material_side_nav_tab_content(
  side_nav_tab_id = "home",
  material_row( material_column(width = 12, h3("COVID-19 Dashboard"))),
  material_row( material_column(width = 12, p(a('COVID 19'), " is the ", em("Corona Virus"), ", an Rshiny application offering powerful Analysis tools which contain 6 platforms as follows: Descriptive Analysis (credit to DrFabach), Growth rate and Curve flatting, Cumulative forecast (credit to Ben Phillips),Daily Cases forcasting, Newspaper Analysis, and TV Analysis. This page is an interface to the data collected from Johns Hopkins CSSE,and some GDELT APIs. The data is 1 day behind current data (credit to Andre Calero). The author is ", a('Dr Khaled M Alqahtani.', href = 'https://twitter.com/Alqahtani_khald', target="_blank"),a('Credit to Andre Calero,', href = 'https://github.com/Sumidu/covid19shiny', target="_blank"),a('DrFabach, and ', href = 'https://github.com/DrFabach/Corona/blob/master/shiny.r', target="_blank"),
                                              a('Ben Phillips at the University of Melbourne', href = 'https://github.com/benflips/nCovForecast', target="_blank") )))
  )
,
#############################################################
material_side_nav_tab_content(
  side_nav_tab_id="twit",
  # material_row(
  #   tags$br(),
  #   tags$br(),
  #   material_column(offset = 2,
  #     width = 8,
  #     material_card( depth = 1,
  #     material_row(
  #       # material_column(width=6,
  #       #                material_text_box(
  #       #                   input_id = "example_text_box",
  #       #                   label = "twitter Hashtag",
  #       #                   color = "#8283BB"
  #       #                 )),
  #      material_column(width=6,
  #                      material_slider(
  #                        input_id = "example_slider",
  #                        label = "Number of tweets",
  #                        min_value = 200,
  #                        max_value = 1000,
  #                        initial_value = 500,
  #                        color = "#8283BB"
  #                                       )
  #      )
  #     )
  # 
  # 
  #   )
  #   )
  # )
  #  ,
########### Introduction

material_row(
  material_column( offset = 2,
                   width = 8,
                   material_card(
                     depth = 1,title = "Analysis of: ",
                     #tags$div("SNA for Hashtag") ,
                     h5(textOutput('title'))
                     #print("SNA for Hashtag: "),h1(textOutput('title') ,style="display:inline")
                   )




  )


)
,

########### 1
material_row(
  material_column( offset = 2,
                   width = 8,
                   material_card(
                     depth = 1,title = "Avarage number of tweets per user ",
                     plotOutput("dis")

                   )

  ))
,
# tags$br(),
# tags$br(),
# tags$br(),
# tags$br(),

######## 2
 material_row(
  material_column( offset = 2,
                   width = 8,
                   material_card(
                    depth = 1,title="Counts of tweets, retweets, and messages",
                    plotOutput("dis2")

                    )

                     ))
,

######## 3
# material_row(
#   material_column( offset = 2,
#                    width = 8,
#                    material_card(
#                      depth = 1,title="Ratio of retweets to tweets",
#                      plotOutput("dis3")
#
#                    )
#
#   ))
# ,

######## 3
material_row(
  material_column( offset = 2,
                   width = 8,
                   material_card(
                     depth = 1,title="Number of messages containing the URL",
                     DT::dataTableOutput("dis4")

                   )

  ))
,

######## 4
#material_row(
 # material_column( offset = 2,
   #                width = 8,
    #               material_card(
    #                 depth = 1,title="Word Clouds",
     #                plotOutput("dis5")

       #            )

 # ))
#,
##########
material_row(
  material_column( offset = 2,
                   width = 8,
                   material_card(
                     depth = 1,title="Social Network Analysis (SNA)",
                     visNetworkOutput("network_proxy_nodes")

                   )

  ))
,

##########
material_row(
  material_column( offset = 2,
                   width = 8,
                   material_card(
                     depth = 1,title="Sentiment Analysis of Tweets",
                     plotOutput("dis6")

                   )

  ))
,
# material_row(
#   material_column( offset = 2,
#                    width = 8,
#                    material_card(
#                      depth = 1,title = "Avarage number of tweets per user for Hashtag: "
#                      #,
#                      #tags$div("SNA for Hashtag") ,
#                      #h5(textOutput('title'))
#                      #print("SNA for Hashtag: "),h1(textOutput('title') ,style="display:inline")
#                    )
#
#
#
#
#   )
#
#
# )
# ,
#
# #
# material_row(
#   material_column( offset = 2,
#                    width = 8,
#                    material_card(
#                      depth = 1,
#                      plotOutput("dis")
#
#                    )
#
#   ))
# ,

########
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
)
 ,
#,
##############################################################
material_side_nav_tab_content(
  side_nav_tab_id = "news",
  material_row(
    tags$br(),
    tags$br(),
    material_column(offset = 2,
                    width = 8,
                    material_card( depth = 1,
                                   material_row( 
                                     # material_column(width=4,
                                     #                material_text_box(
                                     #                   input_id = "search1",
                                     #                  label = "Search",
                                     #                   color = "#8283BB"
                                     #                 )),
                                     material_column(width=4,
                                                     material_dropdown(
                                                       input_id = "showtype",
                                                       label = "Result Show",
                                                       choices = c('GEO24','CONTENT','TIMELINE'),
                                                       color = "#8283BB"
                                                     )              
                                     ),
                                     
                                     
                                     # material_column(width=4,
                                     #                 material_text_box(
                                     #                   input_id = "domain",
                                     #                   label = "Domain",
                                     #                   color = "#8283BB"
                                     #                 )),
                                     # material_column(width=4,
                                     #                 material_date_picker(
                                     #                   input_id = "sdate",
                                     #                   label = "Start Date",
                                     #                   color = "#8283BB"
                                     #                 )),
                                     # material_column(width=4,
                                     #                 material_date_picker(
                                     #                   input_id = "edate",
                                     #                   label = "End Date",
                                     #                   color = "#8283BB"
                                     #                 )),
                                     material_column(width=4,
                                                     material_dropdown(
                                                       input_id = "sort1",
                                                       label = "Sort by",
                                                       choices =sort_options,
                                                       color = "#8283BB"
                                                     )              
                                     )
                                     #,
                                     # material_column(width=3,
                                     #                 material_dropdown(
                                     #                   input_id = "Sourcelanguage",
                                     #                   label = "Source language(s)",
                                     #                    choices = lang_codes,
                                     #                   multiple = TRUE,
                                     #                   color = "#8283BB"
                                     #                 ))
                                     
                                   )
                                   
                                   
                    )
    )
  )
  ,
  material_row(
    material_column( offset = 2,
                     width = 8,
                     material_card(
                       depth = 1,title = "NEWS Analysis",
                       #tags$div("SNA for Hashtag") ,
                       uiOutput('frame')
                       #print("SNA for Hashtag: "),h1(textOutput('title') ,style="display:inline") 
                     )
                     
                     
                     
                     
    )
    
    
  )
)
,
############################################################# tv 
material_side_nav_tab_content(
  side_nav_tab_id = "TV",
  material_row(
    tags$br(),
    tags$br(),
    material_column(offset = 2,
                    width = 8,
                    material_card( depth = 1,
                                   material_row( 
                                     # material_column(width=5,
                                     #                 material_text_box(
                                     #                   input_id = "search2",
                                     #                   label = "Search",
                                     #                   color = "#8283BB"
                                     #                 )),
                                     material_column(width=5,
                                                     material_dropdown(
                                                       input_id = "tvmodes",
                                                       label = "Result Show",
                                                       choices = tv_modes,
                                                       color = "#8283BB"
                                                     )              
                                     ),
                                     
                    
                                     # material_column(width=5,
                                     #                 material_date_picker(
                                     #                   input_id = "sdate2",
                                     #                   label = "Start Date",
                                     #                   color = "#8283BB"
                                     #                 )),
                                     # material_column(width=5,
                                     #                 material_date_picker(
                                     #                   input_id = "edate2",
                                     #                   label = "End Date",
                                     #                   color = "#8283BB"
                                     #                 )),
                                     material_column(width=8,
                                                     material_dropdown(
                                                       input_id = "sort2",
                                                       label = "Sort by",
                                                       choices =sort_options_tv,
                                                       color = "#8283BB"
                                                     )   ,
                                                     material_column(width=8,
                                                                     material_slider(
                                                                       input_id = "smooth2",
                                                                       label = "Smooth of the step",
                                                                       min_value = 1,
                                                                       max_value = 30,
                                                                       initial_value = "",
                                                                       color = "#8283BB"
                                                                     )              
                                                     )
                                     )
                                     #,
                                     # material_column(width=3,
                                     #                 material_dropdown(
                                     #                   input_id = "Sourcelanguage",
                                     #                   label = "Source language(s)",
                                     #                    choices = lang_codes,
                                     #                   multiple = TRUE,
                                     #                   color = "#8283BB"
                                     #                 ))
                                     
                                   )
                                   
                                   
                    )
    )
  )
  ,
  material_row(
    material_column( offset = 2,
                     width = 8,
                     material_card(
                       depth = 1,title = "TV Analysis",
                       #tags$div("SNA for Hashtag") ,
                       uiOutput('frame2')
                       #print("SNA for Hashtag: "),h1(textOutput('title') ,style="display:inline") 
                     )
                     
                     
                     
                     
    )
    
    
  )
),
#############################################################
#disanal
##############################################################
material_side_nav_tab_content(
  side_nav_tab_id = "disAnal",
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br(),
  material_row(
    material_column( offset = 2,
                     width = 2,
                     material_card(
                       depth =1,title = "Total Number of Cases:  ",
                       #tags$div("SNA for Hashtag") ,
                       h5(scales::number(Ccase),h6("."))
                       
                     )),
                     material_column( offset = 1,
                                      width = 2,
                                      material_card(
                                        depth =1,title = "Total Number of Deaths: (%) ",
                                        #tags$div("SNA for Hashtag") ,
                                        h5(scales::number(Ddeaths),h6(Dp,"%"))
                                        
                                      )),
                                      material_column( offset = 1,
                                                       width = 2,
                                                       material_card(
                                                         depth =1,title = "Total Number of Recovered: (%)",
                                                         #tags$div("SNA for Hashtag") ,
                                                         h5(scales::number(Rrecovered),h6(Rp,"%"))
                                                         
                                                       ))
                     
                     
    )      
    
  
  ,
  material_row(
    tags$br(),
    tags$br(),
    
    material_column(offset = 2,
                    width = 8,
                    material_card( depth = 1,
                                   material_row( 
                                     material_column(width=4,
                                                     material_dropdown(
                                                       input_id = "choices",
                                                       label = "Cases or Deaths or Recovered ?",
                                                       choices = c("Cases","Deaths","Recovered"),
                                                       selected="Cases",
                                                       color = "#8283BB"
                                                     )              
                                     ),
                                     material_column(width=6,
                                                     uiOutput("Slider")
                                     ),
                                     
                                     
                                     
                                     material_column(width=2,
                                                     material_checkbox("legend", "Show legend", initial_value=TRUE)   
                                                                   
                                     )
                                     
                                   )
                                   
                                   
                    )
    )
  )
  ,
  material_row(material_column( offset = 2,
                                width = 8,
  material_card( div(style = "height:10px"),
                 depth =1,
  material_row(material_column( offset = 2,
                                width = 8,
                                
  h3(material_column(width=8,align="center" ,offset = 2,
                  uiOutput("selection")
                  )
                                ))))))
  ,
  material_row(
    material_column( offset = 2,
                     width = 8,
                     material_card(
                       depth =1,title = "Outbreak Map",
                       #tags$div("SNA for Hashtag") ,
                       leafletOutput("map",height=700)
                       # , 
                       # absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
                       #               #selectInput("choices", "Cases or Deaths ?", choices = c("Cases","Deaths"),selected = "Cases"),
                       #               #uiOutput("Slider"),
                       #               #helpText("The detail of each country can be obtained by clicking on it."), 
                       #               #uiOutput("selection"),
                       #               checkboxInput("legend", "Show legend", TRUE)
                       #               
                       # )
                       #print("SNA for Hashtag: "),h1(textOutput('title') ,style="display:inline") 
                     )
                     
                     
                     
                     
    )
    
    
  )
  ,
  material_row(
    material_column( offset = 2,
                     width = 8,
                     material_card(
                       depth =1,title = "Raw Data",
                       #tags$div("SNA for Hashtag") ,
                       DT::dataTableOutput("disraw")
                       # , 
                       # absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
                       #               #selectInput("choices", "Cases or Deaths ?", choices = c("Cases","Deaths"),selected = "Cases"),
                       #               #uiOutput("Slider"),
                       #               #helpText("The detail of each country can be obtained by clicking on it."), 
                       #               #uiOutput("selection"),
                       #               checkboxInput("legend", "Show legend", TRUE)
                       #               
                       # )
                       #print("SNA for Hashtag: "),h1(textOutput('title') ,style="display:inline") 
                     )
                     
                     
                     
                     
    )
    
    
  ),
  material_row(
    material_column( offset = 2,
                     width = 8,
                     material_card(
                       depth =1,title = "Top 10 countries based on Cases",
                       #tags$div("SNA for Hashtag") ,
                       plotOutput("figg")
                       # , 
                       # absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
                       #               #selectInput("choices", "Cases or Deaths ?", choices = c("Cases","Deaths"),selected = "Cases"),
                       #               #uiOutput("Slider"),
                       #               #helpText("The detail of each country can be obtained by clicking on it."), 
                       #               #uiOutput("selection"),
                       #               checkboxInput("legend", "Show legend", TRUE)
                       #               
                       # )
                       #print("SNA for Hashtag: "),h1(textOutput('title') ,style="display:inline") 
                     )
                     
    )      
    
  ) 
  
)
##################
#GRP
#######
,
material_side_nav_tab_content(
  side_nav_tab_id = "GRP",
  material_row(
    tags$br(),
    tags$br(),

    material_column(offset = 2,
                    width = 8,
                    material_card( depth = 1,
                                   material_row(
                      
                                     material_column(width=4,
                                                     material_dropdown(
                                                       input_id = "country_selector",
                                                       label = "Select Countries",
                                                       choices =countries1,
                                                       multiple = TRUE,
                                                       color = "#8283BB"
                                                     )),
                                                     #pickerInput("country_selector", "Select Countries", countries1, multiple = TRUE,
                                                                 #options = list(`actions-box` = TRUE),
                                                                 #selectize = TRUE,
                                                                # selected = c("US", "Germany", "Italy", "France", "Iran", "Spain", "Korea, South"))),
material_column(width=4,
               sliderInput("cases_limit", "Pick #cases for alignment", min = 1, max = 500, value = 100)),
material_column(width=4,
              sliderInput("start_date", "Limit Duration", min = 0, max = 100, value=c(0,100))),
 material_column(width=4,
               material_checkbox("scalesfree", "Free Y-Scale", initial_value = TRUE)),
material_column(width=4,
                material_checkbox("logscale", "Logarithmic Y-Scale", initial_value = TRUE)

                      ),
material_column(width=4,
                material_checkbox("labelshow", "Show case counts", initial_value = FALSE)

 )

                                   )


                   )
    )
  )
#   ,
# material_row(
#   material_column( offset = 2,
#                    width = 8,
#                    material_card(
#                      depth =1,title = h4("Total cases:",scales::number(all_cases)),
#                      #tags$div("SNA for Hashtag") ,
#                      
#                      
#                              br()
# 
#                      )
#                      # #                      
#                      # #                      
#                      # #                      
#                      # #                      
#                    )
#                    # #     
#                    # #     
#   
# )

,
  material_row(
    material_column( offset = 2,
                     width = 8,
                     material_card(
                       depth =1,title = "Comparison of case trajectories",
                       #tags$div("SNA for Hashtag") ,
                       
                       plotOutput("distPlot", width = "100%", height = "640"),br(),br()
                       # shiny::wellPanel(
                       #   h4("Log-Linear model fit"),
                       #   withMathJax(
                       #     div("This table shows the exponential of the log-linear model fit as a percentage value and the corresponding p-value. ",
                       #         "A growth rate of 30% indicates that that the following exponential function best approximates the curve:",
                       #         "$$\\text{cases} = cases_{0} \\times 1.3^{days} + c$$",
                       #         "The constant cases0 refers the the alignment specified above. The constant c adjusts for small differences and is not reported here.",
                       #         br(),
                       #         "A growth rate of 30% would mean approx. 30% increase of cases per day.")),br()
                       #   ,
                       #   DT::DTOutput("modeltable")
#                        # ,
# #                        # absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
# #                        #               #selectInput("choices", "Cases or Deaths ?", choices = c("Cases","Deaths"),selected = "Cases"),
# #                        #               #uiOutput("Slider"),
# #                        #               #helpText("The detail of each country can be obtained by clicking on it."), 
# #                        #               #uiOutput("selection"),
# #                        #               checkboxInput("legend", "Show legend", TRUE)
# #                        #               
# #                        # )
# #                        #print("SNA for Hashtag: "),h1(textOutput('title') ,style="display:inline") 
                     #)
# #                      
# #                      
# #                      
# #                      
    )
# #     
# #     
   )
 ),
# # 
material_row(
  material_column( offset = 2,
                   width = 8,
                   material_card(
                     depth =1,title = h5("Growth rate and curve-flattening"),
                     #tags$div("SNA for Hashtag") ,
                     
                     h5("Growth rate"),
                     p("This is the growth rate of the number of active cases for the last 10 days.  It can be thought of as the interest rate, compounded daily."),
                     p("Positive is bad, negative is good. Progress in control would be indicated by steady decline in growth rate over time, and holding in negative territory."),
                     p("Note, days with low or zero growth followed by large spikes are reporting issues: countries miss a day (or several) of reporting and then aggregate cases into the following day."),
                     plotOutput("growthRate"),
                     hr(),
                     h5("Curve flattening index"),
                     p("This is a measure of how well a country is flattening the epidemic curve at any point in time.  Positive values mean growth rates are declining at that point in time."),
                     p("Note, this last plot covers the entire time period of the pandemic, not just the last ten days."),
                     plotOutput("cfi")
                     
                                       
                   )
                      
  )
)
# # 
  )



#################### 10 day forcast 
##################

#######
,
material_side_nav_tab_content(
  side_nav_tab_id = "forcast",
  material_row(
    tags$br(),
    tags$br(),
    
    material_column(offset = 2,
                    width = 8,
                    
                    material_card( depth = 1,
                                   material_row(
                                     
                                     material_column(offset=2,width=4,
                                                     selectInput(inputId = "countryFinder",
                                                                 label = "Select Country/Region:",
                                                                 choices = ddReg, 
                                                                 selected = "Saudi Arabia")
                                                     ),
                                     hr(),
                                     hr(),
                                     material_column(offset=2,width=4,
                                                     sliderInput(inputId = "fitWinSlider", min = 3, max = 10, value = 7, label = "Fit window:", post = "days")
                                                     
                                     )
                                     ))))
  ,
  material_row(
    tags$br(),
    tags$br(),
    
    material_column(offset = 2,
                    width = 8,

                    material_card( depth = 1,
                                     #pickerInput("country_selector", "Select Countries", countries1, multiple = TRUE,
                                     #options = list(`actions-box` = TRUE),
                                     #selectize = TRUE,
                                    
                                   # selected = c("US", "Germany", "Italy", "France", "Iran", "Spain", "Korea, South"))),
                                   material_row(
                                    material_column(offset=2,width=4,h5("Raw case numbers:"),
                                                     tableOutput(outputId = "rawStats")
                                                     ),
                                    
                                     material_column(offset=2,width=4,h5("Doubling time (days):"),
                                                     textOutput(outputId = "doubTime")
                                                     )
                                    
                                     
                                     
                                   )
                                   
                    ) 
                    )
    )
 
  #   ,
  # material_row(
  #   material_column( offset = 2,
  #                    width = 8,
  #                    material_card(
  #                      depth =1,title = h4("Total cases:",scales::number(all_cases)),
  #                      #tags$div("SNA for Hashtag") ,
  #                      
  #                      
  #                              br()
  # 
  #                      )
  #                      # #                      
  #                      # #                      
  #                      # #                      
  #                      # #                      
  #                    )
  #                    # #     
  #                    # #     
  #   
  # )
  
   ,
  material_row(
    material_column( offset = 2,
                     width = 8,
                     material_card(
                       depth =1,title = h5("plot of the generated distribution (top is raw, bottom is log)"),
                       #tags$div("SNA for Hashtag") ,

                       plotlyOutput("rawPlot"),br(),br(),
                       plotOutput("logPlot")
                       
                         #                        # ,
                         # #                        # absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
                         # #                        #               #selectInput("choices", "Cases or Deaths ?", choices = c("Cases","Deaths"),selected = "Cases"),
                         # #                        #               #uiOutput("Slider"),
                         # #                        #               #helpText("The detail of each country can be obtained by clicking on it."),
                         # #                        #               #uiOutput("selection"),
                         # #                        #               checkboxInput("legend", "Show legend", TRUE)
                         # #                        #
                         # #                        # )
                         # #                        #print("SNA for Hashtag: "),h1(textOutput('title') ,style="display:inline")
                       )
                       # #
                       # #
                       # #
                       # #
                     )
                     # #
                     # #
    ),
  material_row(
    material_column( offset = 2,
                     width = 8,
                     material_card(
                       depth =1,title = h5("Prediction of Confirmed Active Case (Min, Max)"),
                       #tags$div("SNA for Hashtag") ,
                       
                       
                       DT::dataTableOutput(outputId = "tablePredConf")
                      
                      
                       
                       #                        # ,
                       # #                        # absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
                       # #                        #               #selectInput("choices", "Cases or Deaths ?", choices = c("Cases","Deaths"),selected = "Cases"),
                       # #                        #               #uiOutput("Slider"),
                       # #                        #               #helpText("The detail of each country can be obtained by clicking on it."),
                       # #                        #               #uiOutput("selection"),
                       # #                        #               checkboxInput("legend", "Show legend", TRUE)
                       # #                        #
                       # #                        # )
                       # #                        #print("SNA for Hashtag: "),h1(textOutput('title') ,style="display:inline")
                     )
                     # #
                     # #
                     # #
                     # #
    )
    # #
    # #
  )
 # )
  # # 
)

#######
#Forcasting Daily stocastic
#Kalman Filter
,
material_side_nav_tab_content(
  side_nav_tab_id = "DC",
  # material_row(
  #   tags$br(),
  #   tags$br(),
  #   
  #   material_column(offset = 2,
  #                   width = 8,
  #                   
  #                   material_card( depth = 1,
  #                                  material_row(
  #                                    
  #                                    material_column(offset=2,width=4,
  #                                                    selectInput(inputId = "countryFinder",
  #                                                                label = "Select Country/Region:",
  #                                                                choices = ddReg, 
  #                                                                selected = "Saudi Arabia")
  #                                    )
  #                                  ))))
  # ,
  # material_row(
  #   tags$br(),
  #   tags$br(),
  #   
  #   material_column(offset = 2,
  #                   width = 8,
  #                   
  #                   material_card( depth = 1,
  #                                  #pickerInput("country_selector", "Select Countries", countries1, multiple = TRUE,
  #                                  #options = list(`actions-box` = TRUE),
  #                                  #selectize = TRUE,
  #                                  
  #                                  # selected = c("US", "Germany", "Italy", "France", "Iran", "Spain", "Korea, South"))),
  #                                  material_row(
  #                                    material_column(offset=2,width=4,h5("Raw case numbers:"),
  #                                                    tableOutput(outputId = "rawStats")
  #                                    ),
  #                                    
  #                                    material_column(offset=2,width=4,h5("Doubling time (days):"),
  #                                                    textOutput(outputId = "doubTime")
  #                                    )
  #                                    
  #                                    
  #                                    
  #                                  )
  #                                  
  #                   ) 
  #   )
  # )
  # 
  #   ,
  # material_row(
  #   material_column( offset = 2,
  #                    width = 8,
  #                    material_card(
  #                      depth =1,title = h4("Total cases:",scales::number(all_cases)),
  #                      #tags$div("SNA for Hashtag") ,
  #                      
  #                      
  #                              br()
  # 
  #                      )
  #                      # #                      
  #                      # #                      
  #                      # #                      
  #                      # #                      
  #                    )
  #                    # #     
  #                    # #     
  #   
  # )
  
  # ,
  # material_row(
  #   material_column( offset = 2,
  #                    width = 8,
  #                    material_card(
  #                      depth =1,title = h5("plot of the generated distribution (top is raw, bottom is log)"),
  #                      #tags$div("SNA for Hashtag") ,
  #                      
  #                      plotlyOutput("rawPlot"),br(),br(),
  #                      plotOutput("logPlot")
  #                      
  #                      #                        # ,
  #                      # #                        # absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
  #                      # #                        #               #selectInput("choices", "Cases or Deaths ?", choices = c("Cases","Deaths"),selected = "Cases"),
  #                      # #                        #               #uiOutput("Slider"),
  #                      # #                        #               #helpText("The detail of each country can be obtained by clicking on it."),
  #                      # #                        #               #uiOutput("selection"),
  #                      # #                        #               checkboxInput("legend", "Show legend", TRUE)
  #                      # #                        #
  #                      # #                        # )
  #                      # #                        #print("SNA for Hashtag: "),h1(textOutput('title') ,style="display:inline")
  #                    )
  #                    # #
  #                    # #
  #                    # #
  #                    # #
  #   )
  #   # #
  #   # #
  # )
  #,
  material_row(
    material_column( offset = 2,
                     width = 8,
                     material_card(
                       depth =1,title = h5("Prediction of daily Active Cases ")))),
                       #tags$div("SNA for Hashtag") ,
                       
  material_row(
    material_column( offset = 2,
                     width = 8,
                     material_card( selectInput(inputId = "countryFinder2",
                                                label = "Select Country/Region:",
                                                choices = ddReg, 
                                                selected = "Saudi Arabia")
                     )))  ,     
                       
    material_row(
                       material_column( offset = 2,
                                        width = 8,
                                        material_card(DT::dataTableOutput(outputId = "tablePredstocastc"))))
    
        
                                         
                       
                       #                        # ,
                       # #                        # absolutePanel(id = "input_date_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F,
                       # #                        #               #selectInput("choices", "Cases or Deaths ?", choices = c("Cases","Deaths"),selected = "Cases"),
                       # #                        #               #uiOutput("Slider"),
                       # #                        #               #helpText("The detail of each country can be obtained by clicking on it."),
                       # #                        #               #uiOutput("selection"),
                       # #                        #               checkboxInput("legend", "Show legend", TRUE)
                       # #                        #
                       # #                        # )
                       # #                        #print("SNA for Hashtag: "),h1(textOutput('title') ,style="display:inline")
                     )
                     # #
                     # #
                     # #
                     # #
    )
    # #
    # #
  #)
  # )
  # # 
#)

#)
 