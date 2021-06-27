library(shiny)
library(shinythemes)
library(tidyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(readr)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(RCurl)
library(plotly)
library(lubridate)
library(threejs)
library(leaflet)
library(timevis)
library(wordcloud2)

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
    theme = shinytheme("cyborg"),
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
               tags$img(src = "Heading.PNG", width = "900px")
               # tags$h2("Psychological & Behavioural Distress of COVID-19 & Infodemics")
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
                   box(
                     title = "Background And Motivation",
                     width = 12,
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
                   )
                 ),
                 
                 column(5, br(), br(),
                        box(
                          tags$iframe(
                            src = "https://www.youtube.com/embed/bJGFZ_1T4Us",
                            frameborder = "0",
                            allow = "accelerometer;
                                autoplay; encrypted-media; gyroscope; picture-in-picture",
                            allowfullscreen = NA,
                            width = "540px",
                            height = "263px"
                          )
                        )),
                 
                 fluidRow(column(
                   12,
                   br(),
                   box(
                     title = "Objectives",
                     width = 12,
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
                 ))
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
        ),
        fluidRow()
      ),
      
      tabPanel("Infodemics",
               fluidRow(
                 class = "data-panel",
                 navlistPanel(
                   tabPanel(
                     "Patient locations",
                     
                     tags$div(class = "plot-wraper patient-plot",
                              plotlyOutput("patient_loc_map")),
                     tags$div(
                       class = "patient-caption",
                       tags$div(
                         "This shows us the locations of patients of our across the globe plotted on the world map.The outbreak spread from the Chinese city of Wuhan to more than 180 countries and territories affecting every continent except Antartica. Efforts to stamp out the pneumonia-like illness have driven all the countries to enforce lockdowns, widespread halts of international travel, mass layoffs and shattered financial markets."
                       )
                     )
                   ),
                   tabPanel("Symptoms",
                            tabsetPanel(
                              tabPanel(
                                "Outline",
                                tags$div(class = "plot-wraper patient-plot",
                                         wordcloud2Output("symptom_wordcloud")),
                                tags$div(
                                  class = "patient-caption",
                                  tags$div(
                                    "From our wordcloud we can say that cough and fever are most common symptoms among the patients. People generally develop signs and symptoms, including mild respiratory symptoms and fever, on an average of 5-6 days after infection (mean incubation period 5-6 days, range 1-14 days)."
                                  )
                                )
                              ),
                              tabPanel(
                                "Distribution",
                                tags$div(class = "plot-wraper patient-plot",
                                         plotlyOutput("symptom_piechart")),
                                tags$div(
                                  class = "patient-caption",
                                  tags$div(
                                    "We plotted a pie chart to display the common symptoms seen initially in Wuhan, China which is the origin.It describes the highest ranked symptoms seen together in the origin of COVID-19 that is Wuhan,China.Covid-19 symptoms are  non-specific and the disease presentation can range from no symptoms(asymptomatic) to severe pneumonia and death."
                                  )
                                )
                              )
                            )),
                   tabPanel(
                     "Age Group Analysis",
                     tabsetPanel(
                       tabPanel(
                         "Distribution",
                         tags$div(class = "plot-wraper patient-plot",
                                  plotlyOutput("age_dist_plot")),
                         tags$div(
                           class = "patient-caption",
                           tags$div(
                             "We compared the age distribution among the patients ( Child, Adult and Senior Adult) . The age group between 40 to 60 have been affected the most. From our analysis ans well as confirmed by International research the percentage of children among the confirmed COVID-19 patients is low, ranging from 1% in young children to 6% in older children.  Children with COVID-19 do have the same symptoms as adults. The most common symptoms in children are coughing, fever and sore throat. Worldwide, very few children with COVID-19 have died.In clusters of patients, adults are almost always the source patient. On basis of this, decisions such as re-opening of schools and childcare facilities were made [@Children]."
                           )
                         )
                       ),
                       tabPanel(
                         "Likely To Recover",
                         tags$div(class = "plot-wraper patient-plot",
                                  plotlyOutput("likely_recover_boxplot")),
                         tags$div(
                           class = "patient-caption",
                           tags$div(
                             "We create a boxplot to  analyse the age of people who have died and of those who did not. The rhombus in the boxplot shows the mean(y) value of those who survived as 43.9 , while the mean(x) for those who have died as 65.5. So, this indeed shows that older people have lower chances of survival."
                           )
                         )
                       ),
                       tabPanel(
                         "Unlikely To Recover",
                         tags$div(class = "plot-wraper patient-plot",
                                  plotlyOutput("unlikely_recover_boxplot")),
                         tags$div(
                           class = "patient-caption",
                           tags$div(
                             "We create a boxplot to  analyse the age of people who have died and of those who did not. The rhombus in the boxplot shows the mean(y) value of those who survived as 43.9 , while the mean(x) for those who have died as 65.5. So, this indeed shows that older people have lower chances of survival."
                           )
                         )
                       )
                     )
                     
                   ),
                   tabPanel(
                     "Post Onset Symptoms Analysis",
                     tabsetPanel(
                       tabPanel(
                         "Days Till Recovery",
                         tags$div(class = "plot-wraper patient-plot",
                                  plotlyOutput("days_to_recovery_plot")),
                         tags$div(
                           class = "patient-caption",
                           tags$div(
                             "Covid-19 is a confusing illness , wrapped with uncertainty. There has not been sufficient scientific studies to tell precisely how long it takes for a person to recover. While some possible ranges have been identified. These seems to vary from person to person. According to WHO, recovery times tend to be about 2 weeks for those with mild disease and about 3-2 weeks for those with severe/critical disease."
                           )
                         )
                       ),
                       tabPanel(
                         "Days Till Death",
                         tags$div(class = "plot-wraper patient-plot",
                                  plotlyOutput("days_to_death_plot")),
                         tags$div(
                           class = "patient-caption",
                           tags$div(
                             "Covid-19 is a confusing illness , wrapped with uncertainty. There has not been sufficient scientific studies to tell precisely how long it takes for a person to recover. While some possible ranges have been identified. These seems to vary from person to person. According to WHO, recovery times tend to be about 2 weeks for those with mild disease and about 3-2 weeks for those with severe/critical disease."
                           )
                         )
                       )
                     )
                   ),
                   tabPanel("Chronic Disease Analysis",
                            tabsetPanel(
                              tabPanel(
                                "Distribution",
                                tags$div(class = "plot-wraper patient-plot",
                                         plotlyOutput("chroic_disease_gender_plot")),
                                tags$div(
                                  class = "patient-caption",
                                  tags$div(
                                    "Everyone exposed to virus are at risk. However, some people are more vulnerable than others to become severely ill and more in need for medical facilities. According to \"Centers for Disease Control and Prevention\" , people of any age with certain chronic diseases are at higher risk for severe illness from covid-19."
                                  )
                                )
                              ),
                              tabPanel(
                                "Chronic Disease Frequency",
                                tags$div(class = "plot-wraper patient-plot",
                                         plotlyOutput("chronic_disease_freq_plot")),
                                tags$div(
                                  class = "patient-caption",
                                  tags$div(
                                    "Most of the people infected with the virus have mild to moderate disease, which includes non-pneumonia and pneumonia cases. Asymptomatic infection reported are majority of the relatively rare cases which are asymptomatic on the date of identification/report went on to develop disease[@who]."
                                  )
                                )
                              )
                            ))
                 )
               )),
      
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
                   tabPanel(
                     "Stress Analysis",
                     fluidRow(
                       class = "final-analysis-content",
                       tags$p(
                         "Our overall goal was to exploit the sample patient dataset. Each observation of the dataset is related to the details of the confirmed COVID-19 patient such as age, DatOfOnsetSymptoms , DateOfDischarge etc.
Among 15466 observations, 6006 (38.83%)are Females and 9460 (61.17%) are Males. 766(4.95%) are 18 and below, 12059(77.97%) are between 20 and 60 inclusive, 2641(17.07%) above 60. The low count of children suggests that there is a relatively low attack rate in this age group. The median age is 45  years (range 1 year-100 years old; IQR 38-53 years old) with the majority of cases aged between 16-75 years."
                       ),
                       tags$p(
                         "Individuals with higher risk for severe disease and death include those with some underlying medical conditions such as hypertension, diabetes, cardiovascular disease, chronic respiratory disease and cancer. Our sample consists of 720(4.6%) patients with chronic diseases out of which 302(41.94%) are Female and 418(58.06%) are Male. Additionally, extracting each chronic disease from 149 observations(excluding missing values) and plotting its frequency showed us that people with hypertension(34.7%)and diabetes(24.3%) are more vulnerable to COVID-19."
                       ),
                       tags$p(
                         "Since people with chronic disease are likely to face an increased risk of developing severe symptoms and eventually die, we try to find the chances of their recovery and also compare it with those who don't. The recovery:death ratio for patients with chronic disease is 17:49 and for the others is 53:8. This indeed proves that people having chronic disease, when infected by COVID19 have very low chances of recovery."
                       ),
                       tags$p(
                         "
Based on 1644 confirmed cases (excluding observations with missing values for Symptoms)  collected until March 2020, typical signs and symptoms include: Fever (32.65%), Dry Cough(18.38%), sore throat,(3.76%) pneumonia (3.5%), fatigue(2.5%), malaise(2.5%), rhinorrhea(2.3%), headache(2.23%), myalgias(2.22%), shortness of breath(1.9%), sputum(1.5%) etc. Focusing on Wuhan City, we plot a pie chart to see the initially seen symptoms. "
                       ),
                       tags$p(
                         "Fever was seen in the majority of the cases (44.4%) on its own as well as with other symptoms like cough(28.1%), weakness(2.96%), sore throat(2.96%) and fatigue(2.96%).The outbreak soon spread from China to other parts of the world. We use the geographical locations of the patients  provided in our dataset to find the places that were affected or not affected from COVID-19. The map reveals that the virus was spread from Chinese city of Wuhan to more than 180 countries and territories affecting every continent except Antarctica. In addition to chronic disease, age also influences  the level of risk for disease and death.People aged more than 60 are at a higher risk than those below 60 can be concluded with the help of the statistical hypothesis testing such as t-test. Creating a separate data frame of those who recovered (372 observations) , we create a boxplot that shows the median age as 45 (IQR 30-53) and also the average age to be 43.29.  We then create another data frame of those who died and created a boxplot. The median in this plot is 67 (IQR 55-79)."
                       ),
                       tags$p(
                         "According to WHO, the recovery time tends to be about two weeks for those with mild symptoms and about 3-6 weeks for those with severe or critical disease. However, these seem to be only rough guidelines as studies have already shown a number of exceptions. We plot the timeline for recovery for some patients and see variations in the number of days taken for recovery. With this, we can conclude that a window of 2-4 weeks can be considered as recovery time. Similarly, we plot the timeline for death for some patients. In this case, most of the patients died within 3 weeks whereas the majority of the patients older than 70 took less than 2 weeks.These are analysis that we have made on the COVID-19 patient medical data."
                       )
                     )
                   ),
                   tabPanel(
                     "Infodemics",
                     fluidRow(
                       class = "final-analysis-content",
                       tags$p(
                         "We need to know that no prediction is certain as long as once in a while the past repeats. There are different factors that come into play while doing the prediction such as psychological which emphasizes more on how people distinguish and react in a dangerous situation, availability of data and the variable used. Assuming that the information used is reliable which in future will follow the past trends of the disease, our forecasts say that there will be an increment within the confirmed COVID-19 cases ( deaths and recovered ) with a slight instability."
                       ),
                       tags$p(
                         "We can see that in Germany the restrictions have taken important steps towards the containment of the virus. This has led to fewer deaths and confirmed cases, as for example in the US.
It is interesting to note that the strict Spanish restrictions on the virus have made only a small difference to the less stringent restrictions in Germany (confirmed cases)."
                       )
                     )
                   ),
                   tabPanel(
                     "Twitter Analysis",
                     fluidRow(
                       class = "final-analysis-content",
                       tags$p(
                         "The objective of this Twitter sentiment analysis was to identify the emotions and sentiment direction of the public during the corona virus outbreak. Based on the plots, it is revealed that people had more positive sentiment towards the situation rather than the negative feelings. Furthermore , it is identified that anticipation and trust are the most
expressed emotions during the pandemic. When analyzed the frequent words used to express sentiments, it is found that \"ugh\", \"die\", \"miss\" , \"sue\", \"worse\" are the words used frequently to express negative sentiment while, \"good\", \"trump\", \"love\", \"hug\", \"wow\" are the words used for positive sentiment."
                       ),
                       tags$p(
                         "When analyzed the word cloud for German tweets, we could identify some of the words like \"schon\", \"deutschland\", \"pandamie\", \"lockdown\", \"youtube\" etc have been used frequently in the tweets."
                       )
                     )
                     
                   )
                 )
               )),
      tabPanel("Resources",
               fluidRow(
                 class = "data-panel",
                 navlistPanel(
                   tabPanel(
                     "R Markdown process notebook",
                     fluidRow(
                       class = "resources-content",
                       tags$a(
                         href = "https://shivanihegde.github.io/Data-Science-with-R-2020/Process%20notebook/notebook_final.html",
                         target = "_blank",
                         tags$img(id = "r-logo", src =
                                    "R-logo.png")
                       )
                     )
                   ),
                   tabPanel("Source Code",
                            fluidRow(
                              class = "resources-content",
                              tags$a(
                                href = "https://github.com/ShivaniHegde/Data-Science-with-R-2020",
                                target = "_blank",
                                tags$img(id = "git-logo", src =
                                           "git-logo.png")
                              )
                            )),
                   tabPanel(
                     "Datasets",
                     fluidRow(
                       class = "dataset-content",
                       tags$div(
                         class = "data-link",
                         tags$a(
                           href = "https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases",
                           target = "_blank",
                           "John Hopkins timeseries datasets"
                         )
                       ),
                       tags$div(
                         class = "data-link",
                         tags$a(href =
                                  "https://datarepository.wolframcloud.com/resources/Patient-Medical-Data-for-Novel-Coronavirus-COVID-19", target =
                                  "_blank",
                                "Patient Medical Dataset")
                       )
                     )
                     
                   ),
                   tabPanel(
                     "References",
                     fluidRow(class = "resources-content references-content",
                              tags$div(
                                tags$ol(
                                  tags$br(),
                                  tags$li(
                                    tags$span(
                                      "Dey, Samrat K., et al. \"Analyzing the epidemiological outbreak of COVID-19: A visual exploratory data analysis approach.\" Journal of medical virology 92.6 (2020): 632-638"
                                    )
                                  ),
                                  tags$br(),
                                  tags$li(
                                    tags$span(
                                      "Gupta, Rajan, and Saibal Kumar Pal. \"Trend Analysis and Forecasting of COVID-19 outbreak in India.\" medRxiv (2020)."
                                    )
                                  ),
                                  tags$br(),
                                  tags$li(
                                    tags$span(
                                      " Singh, Sarbjit, et al. \"Development of new hybrid model of discrete wavelet decomposition and autoregressive integrated moving average (ARIMA) models in application to one month forecast the casualties cases of COVID-19.\" Chaos, Solitons & Fractals (2020): 109866"
                                    )
                                  ),
                                  tags$br(),
                                  tags$li(tags$span(
                                    "https://ncase.me/covid-19/ - 21.06.2020"
                                  )),
                                  tags$br(),
                                  tags$li(
                                    tags$span(
                                      "https://www.zdf.de/nachrichten/heute/coronavirus-ausbreitung-infografiken-102.html - 21.06.2020"
                                    )
                                  ),
                                  tags$br(),
                                  tags$li(
                                    tags$span(
                                      "https://www.bundesgesundheitsministerium.de/coronavirus/chronik-coronavirus.html - 21.06.2020"
                                    )
                                  ),
                                  tags$br(),
                                  tags$li(
                                    tags$span(
                                      "https://www.mdr.de/nachrichten/politik/corona-chronik-chronologie-coronavirus-100.html - 21.06.2020"
                                    )
                                  ),
                                  tags$br(),
                                  tags$li(
                                    tags$span(
                                      "https://towardsdatascience.com/r-tutorial-analyzing-covid-19-data-12670cd664d6 "
                                    )
                                  ),
                                  tags$br(),
                                  tags$li(
                                    tags$span(
                                      "https://www.rivm.nl/en/novel-coronavirus-covid-19/children-and-covid-19"
                                    )
                                  ),
                                  tags$br(),
                                  tags$li(
                                    tags$span(
                                      "https://www.who.int/docs/default-source/coronaviruse/who-china-joint-mission-on-covid-19-final-report.pdf?sfvrsn=fce87f4e_2"
                                    )
                                  ),
                                  tags$br(),
                                  tags$li(
                                    tags$span(
                                      "https://edition.cnn.com/2020/03/24/health/coronavirus-gender-mortality-intl/index.html"
                                    )
                                  ),
                                  tags$br(),
                                  tags$li(
                                    tags$span(
                                      "Manguri, Kamaran & Ramadhan, Rebaz & Mohammed Amin, Pshko. (2020). Twitter Sentiment Analysis on Worldwide COVID-19 Outbreaks. Kurdistan Journal of Applied Research. 54-65. 10.24017/covid.8."
                                    )
                                  ),
                                  tags$br(),
                                  tags$li(
                                    tags$span(
                                      "Dubey, Akash Dutt, Twitter Sentiment Analysis during COVID-19 Outbreak (April 9, 2020). Available at SSRN: https://ssrn.com/abstract=3572023 or http://dx.doi.org/10.2139/ssrn.3572023"
                                    )
                                  )
                                )
                              ))
                   )
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