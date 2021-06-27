library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)

mergedData <- read.csv("world_map.csv")
world_map_stress_levels <-
  plot_ly(
    mergedData,
    type = 'choropleth',
    locations = mergedData$CODE,
    z = mergedData$PSS10_avg,
    text = mergedData$COUNTRY,
    colorscale = "Portland"
  ) %>%
  layout(paper_bgcolor = 'transparent')


ui <-
  fluidPage(
    theme = shinytheme("flatly"),
    fixedRow(
      column(1, br(),
             tags$div(
               tags$a(href = "https://www.inf.ovgu.de/",
                      tags$div(
                        id = "logo-img",
                        tags$img(src = "logo.png", width = "170px")
                      )),
             )),
      br(),
      column(7, offset = 1, br(),
             tags$div(
               # tags$img(src = "Heading.PNG", width = "900px")
               tags$h1("Psychological & Behavioural Distress of COVID-19 & Infodemics")
             )),
      column(1, offset = 1,
             tags$div(
               tags$a(href = "https://brain.cs.uni-magdeburg.de/kmd/DataSciR/",
                      tags$div(
                        id = "dsrlogo-img",
                        tags$img(src = "DScR.png", width = "145px")
                      )),
             ))
      
    ),
    br(),
    
    tabsetPanel(
      tabPanel("Overview",
               fluidRow(
                 column(
                   7,
                   br(),
                   tags$h4("Background And Motivation"),
                   
                   "The coronavirus COVID-19 pandemic is an unprecedented health crisis that has impacted the world to a large extent.
                       According to WHO, mental disorders are one of the leading causes of disability worldwide, so considering that this pandemic
                       has caused further complications to mental ailment. The stress, anxiety, depression stemming from fear, isolation, and stigma
                       around COVID-19 affected all of us in one way or another. We could see that many people are losing their jobs and the elderly
                       are isolated from their usual support network. The issue is that the effects of the pandemic and mental health, maybe for
                       longer-lasting than the disease itself.",
                   br(),
                   br(),
                   "In this limelight, although the measures are taken to slow the spread of the virus, it has affected our physical activity levels,
                       our eating behaviors, our sleep patterns, and our relationship with addictive substances, including social media. Into this last point,
                       both our increased use of social media while stuck at home, as well as the increased exposure to disaster news past year, have amplified
                       the negative effects of social media on our mental health. This motivates us to perform some diagnostic analysis of this pattern and
                       portray some meaningful insights on global survey data."
                   
                 ),
                 
                 column(
                   5,
                   br(),
                   br(),
                   
                   tags$iframe(
                     src = "https://www.youtube.com/embed/bJGFZ_1T4Us",
                     frameborder = "0",
                     allow = "accelerometer;
                                autoplay; encrypted-media; gyroscope; picture-in-picture",
                     allowfullscreen = NA,
                     width = "540px",
                     height = "263px"
                   )
                 ),
                 
                 fluidRow(
                   column(
                     9,
                     offset = 1,
                     br(),
                     
                     tags$h4("Objectives"),
                     
                     "1. Here we aim to analyze and visualize the survey dataset to come up with certain descriptive and diagnostic statistics
                   including the number of countries that participated in the survey. We look forward to analyzing the various stress levels
                   from each country, with a focus on visualizing the distress scale with the source of distress and the level of distress.
                   Further, we plan to visualize the coping scale which consists of the source of coping and detailing what was the personal
                   conscious effort, to solve personal and interpersonal problems, to master, minimize or tolerate stress and conflict. We also
                   target to depict the visualization of the level of trust such as Country's Civil service, Country's Police, Country's Healthcare
                   system, WHO, Government's measures against COVID which also play a crucial role. We extend to diagnose on areas such as compliance
                   and the level of agreement, the concern level, and the key factor of media from where the respondents sought to agree to take the
                   information from. The different relationship between perceived stress, social support, loneliness, and extroversion according to
                   different age groups will also be set as our basis for analysis.",
                     br(),
                     br(),
                     "2. We propose to know the impact of COVID-19 tackling infodemics and misinformation on Twitter. This is done by extracting recent
                   popular tweets from a specific location across different countries. It will help us describe the false information that is spread
                   with the sole purpose of causing confusion and harm. We target to extract hashtags like #covid19, #misinformation, #fakenews, #disinformation,
                   #, etc., to get the related posts about it and analyze how the information processing and decision-making behaviors are compromised.
                   We perform sentimental analysis on the tweets to understand the sentiments of people which is crucial during the time of this pandemic.",
                     br(),
                     br(),
                     "3. Our final key interest is to perform a comparative analysis on `Infodemics`. That is to outline the interaction patterns of fake news
                   information spreading across media. The news which is rolled out in different environments having different interaction settings and
                   audiences are verified. We report the rumor amplification parameters for each of the social media platform due to COVID-19 to show how
                   the misinformation is spread on different mainstream online platforms."
                     
                     
                   )
                 )
               )),
      tabPanel(
        "Stress Analysis",
        navlistPanel(
          tabPanel(
            "World Stress Levels",
            plotlyOutput("world_stress_map"),
            "This map indicates the average stress value of each country which can be observed upon hovering your mouse pointer
                          over the desired country.",
            br(),
            "The countries with no colour are the countries which did not participate in the survery."
          ),
          tabPanel(
            "Sources of Distress",
            fluidRow(column(
              1,
              offset = 3,
              br(),
              
              tags$img(src = "Sources_of_Distress.png", width = "550px")
            )),
            br(),
            "The above bar plot shows the level of stress for different sources. People are more stressed due to the fear of economy
                          collapse and catching corona virus but having no religious activities have causes least stress to the people."
          ),
          tabPanel(
            "Coping with Stress",
            fluidRow(column(
              1,
              offset = 3,
              br(),
              
              tags$img(src = "coping_corona.png", width = "550px")
            )),
            br(),
            "The above boxplot represents which methods people are using to cope with the Corona stress. According to this survey, people
            prefer the long range interaction with family and friends and spend time doing some hobby to cope with the Corona stress."
          ),
          tabPanel(
            "Bivariate Relationship",
            fluidRow(column(
              1,
              offset = 2,
              br(),
              
              tags$img(src = "bivariate_plot.png", width = "680px")
            )),
            fluidRow(
              br(),
              "The above boxplot represents which methods people are using to cope with the Corona stress. According to this survey, people
            prefer the long range interaction with family and friends and spend time doing some hobby to cope with the Corona stress."
              ,
              br(),
              br()
            )
          )
        )
      ),
      
      tabPanel("Infodemics",
               fluidRow(
                 class = "data-panel",
                 navlistPanel(
                   tabPanel(
                     "Trust Sources",
                     fluidRow(column(
                       1,
                       offset = 0,
                       br(),
                       
                       tags$img(
                         src = "Trust in Media sources.png",
                         width = "900px",
                         height = "320px"
                       )
                     )),
                     br(),
                     "We interestingly find that amount of perceived knowledge regarding the COVID-19 information in
                                   the popular country list comes from Newspaper",
                     br(),
                     "websites which are estimated to be in the range of
                                   [0.1, 0.16]. Whereas this range is more for non-popular country list is in the range of [0.17, 0.27]
                                   ",
                     br(),
                     "and the trust score comes from the national government.",
                     br(),
                     "People believe in conspiracy theories
                                   for a variety of reasons to explain random events, to feel special or unique, or for a sense of social
                                   belonging,",
                     br(),
                     " to name a few. It also seems interesting that in our analysis the measure of Conspiracy
                                   belief in non-popular countries comes from Facebook and",
                     br(),
                     " it is estimated to be [0.14, 0.22]. And this
                                   range is lower [0.8, 0.15] from the same source for popular countries.",
                     br(),
                     "From the data, the conspiracy
                                   belief levels from W.H.O. do have a negative estimate for both popular and non-popular countries.",
                     br(),
                     br()
                   ),
                   tabPanel(
                     "IRI Score",
                     fluidRow(column(
                       1,
                       offset = 0,
                       br(),
                       
                       tags$img(
                         src = "IRI risk volume across countries.png",
                         width = "900px",
                         height = "450px"
                       )
                     )),
                     br(),
                     "This plot shows IRI vs Confirmed COVID-19 cases for Infodemics and Epidemics data aggregation by Country and at a border level
                      categorized into ",
                     br(),
                     "the continent.",
                     br(),
                     "It can be inferred that the IRI volume is very high in `USA` with approximately 383,210 cases. Also, we have focused to show
                     this critical impact on ",
                     br(),
                     "the top 5 continents around the globe. Moreover, it has to be strikingly noted that the IRI score is
                     very high in Peru which is almost around 0.98 ",
                     br(),
                     "although the total infected cases are 11.",
                     br(),
                     br()
                     
                   ),
                   tabPanel("Comparisons",
                            
                            tabsetPanel(
                              tabPanel(
                                "Governments",
                                fluidRow(column(
                                  1,
                                  offset = 1,
                                  br(),
                                  
                                  tags$img(
                                    src = "neg-rel-gov-o3.png",
                                    width = "800px",
                                    height = "400px"
                                  ),
                                  br(),
                                  br(),
                                )),
                                "It is seemingly evident that in early 2020 New Zealand was declared as a COVID-19 free country so we estimate the
                             trust in its government is",
                                br(),
                                " remarkably higher in comparison to other countries of the world. On the contrary,
                             it modeled a low trust score in global governments",
                                br(),
                                "At the same time trust score in Brazil's local government is extremely lower but it shows a high trust in
                            global governments.",
                                br(),
                                "This shows a strong and negative correlation between trust in a particular country's
                            government and in global government. The confidence band is an ",
                                br(),
                                "indicator that it is 95% confident that the true
                            regression line lies in that gray zone.",
                                br(),
                                br()
                              ),
                              tabPanel(
                                "Scientist",
                                fluidRow(column(
                                  1,
                                  offset = 1,
                                  br(),
                                  
                                  tags$img(
                                    src = "Pos_rel-scientist-o3.png",
                                    width = "800px",
                                    height = "400px"
                                  ),
                                  br(),
                                  br(),
                                ))
                                ,
                                
                                "For the same set of 12 different countries, we do a comparative analysis by plotting the trust in the scientists
                                    of their country vs. the trust in ",
                                br(),
                                "global scientists. From our data, it is evident that for `Italy` the
                                    trust in
                                    local government is as low as 5.78 whereas globally it is just 5.31. For `Brazil`",
                                br(),
                                " the local trust score
                                    is nearly
                                    8.4 whereas the global trust score around 8.6.",
                                br(),
                                "This shows a strong and positive correlation between trust in a particular country's scientists and in global
                                    scientists. The confidence band shows",
                                br(),
                                " that it is 95% confident that the true regression line lies i
                                n that gray zone.",
                                br(),
                                br()
                                
                              )
                            ))
                 )
               ))
      ,
      
      tabPanel(
        "Twitter Analysis",
        fluidRow(class = "data-panel",
                 sidebarLayout(
                   sidebarPanel(h1(
                     class = "radioSelect",
                     radioButtons(
                       "twittercharts",
                       "Please select the chart below.",
                       c(
                         "Sentiment with Most Contributed Words" = "tweet_plot1",
                         "Tweet Counts with Emotions" = "tweet_plot2",
                         "English Word Cloud with Emotions" = "tweet_plot3",
                         "Sentiment Change with Time(Hour)" = "tweet_plot4",
                         "German Tweets Word Cloud" = "tweet_plot5"
                       )
                     ),
                     
                     br(),
                   )),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     tags$h4(class = "tweet-header",
                             uiOutput("tweet_header"),),
                     tags$div(class = "plot-wraper twitter-plot",
                              uiOutput("tweet_plot")),
                     tags$div(class = "tweet-summary",
                              uiOutput("tweet_summary")),
                     
                   )
                   
                 ))
      ),
      tabPanel("Final Analysis",
               fluidRow(
                 class = "data-panel",
                 navlistPanel(
                   tabPanel("Stress Analysis",
                            fluidRow()),
                   tabPanel("Infodemics",
                            fluidRow()),
                   tabPanel("Twitter Analysis",
                            fluidRow())
                 )
               )),
      tabPanel("Resources",
               fluidRow(
                 class = "data-panel",
                 navlistPanel(
                   tabPanel("R Markdown process notebook",
                            fluidRow(column(
                              1,
                              offset = 4,
                              br(),
                              br(),
                              br(),
                              
                              tags$a(
                                href = "",
                                target = "_blank",
                                tags$img(id = "r-logo", src =
                                           "R-logo.png")
                              )
                              
                            ))),
                   tabPanel("Source Code",
                            fluidRow(column(
                              1,
                              offset = 4,
                              br(),
                              br(),
                              br(),
                              tags$a(
                                href = "https://github.com/ranjiGT/Data-Science-with-R-2021",
                                target = "_blank",
                                tags$img(id = "git-logo", src =
                                           "git-logo.png", width = "300px")
                              )
                            ))),
                   tabPanel(
                     "Datasets",
                     fluidRow(column(
                       5, br(),
                       class = "dataset-content",
                       tags$div(
                         class = "data-link",
                         tags$a(
                           href = "https://osf.io/hejng/",
                           target = "_blank",
                           "Covid Stress Global Survey"
                         )
                       )
                     )),
                     fluidRow(column(
                       5, br(),
                       tags$div(
                         class = "data-link",
                         tags$a(href =
                                  "https://osf.io/n6upx/", target =
                                  "_blank",
                                "COVID19 Infodemics")
                       )
                     )),
                     fluidRow(column(
                       5, br(),
                       tags$div(
                         class = "data-link",
                         tags$a(href =
                                  "https://zenodo.org/record/3831406#.YNhxFej7Q6b", target =
                                  "_blank",
                                "COVID19 2020 Twitter Data")
                       )
                     ))
                     
                   ),
                   tabPanel("References",
                            fluidRow())
                 )
               ))
    )
  )

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$world_stress_map <- renderPlotly({
    world_map_stress_levels
  })
  
}

shinyApp(ui, server)