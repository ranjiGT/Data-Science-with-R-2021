library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinythemes)
library(ggplot2)
library(plotly)
library(GGally)
library(dplyr)
library(bib2df)
library(DT)
library(vov)


# Read Data Files
mergedData <- read.csv("data/world_map.csv")
data_scatterplot_gov <- read.csv("data/data_scatter_gov.csv")
data_scatterplot_scient <- read.csv("data/data_scatter_scient.csv")
sentiment_2021 <- read.csv("data/sentiment_scores_2021.csv")
sentiment_2020 <- read.csv("data/sentiment_scores_2020.csv")
distress_source <- read.csv("data/distress_source.csv")
coping_source <- read.csv("data/coping_source.csv")
word_sentiment_2021 <- read.csv("data/word_sentiment_2021.csv")
word_sentiment_2020 <- read.csv("data/word_sentiment_2020.csv")


ui <-
  fluidPage(
    theme = shinytheme("flatly"),
    useShinyjs(),
    use_vov(),
    fluidRow(
      style = "background-color: #00081e",
      column(1, br(),
             tags$div(
               tags$a(href = "https://github.com/ranjiGT/Data-Science-with-R-2021",
                      tags$div(
                        id = "logo-img",
                        tags$img(src = "logo.png", width = "170px")
                      ))
             )),
      br(),
      column(6, offset = 1, br(),
             tags$div(
               # tags$img(src = "Heading.PNG", width = "900px")
               tags$h1(
                 style = "color: #FFFFFF",
                 "Psychological & Behavioural Distress of",
                 br(),
                 "COVID-19 & Infodemics",
                 
               )
             )),
      column(1,
             tags$img(src = "corona.gif", width = "250px")),
      column(2, offset = 1,
             tags$div(
               tags$a(href = "https://brain.cs.uni-magdeburg.de/kmd/DataSciR/",
                      tags$div(
                        id = "dsrlogo-img",
                        tags$img(src = "DScR.png", width = "145px")
                      ))
             ))
      
    ),
    
    tabsetPanel(
      tabPanel("Overview",
               fluidRow(
                 column(
                   7,
                   br(),
                   br(),
                   bsCollapse(
                     id = "collapseExample",
                     open = "Background And Motivation",
                     bsCollapsePanel(
                       "Background And Motivation",
                       "The coronavirus COVID-19 pandemic is an unprecedented health crisis that has impacted the world to a large extent.",
                       br(),
                       br(),
                       "According to WHO, mental disorders are one of the leading causes of disability worldwide, so considering that this pandemic
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
                       portray some meaningful insights on global survey data.",
                       style = "primary"
                     ),
                     bsCollapsePanel(
                       "Objectives",
                       panel(
                         style = "overflow-y:scroll; max-height: 280px; position:relative; align: centre",
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
                       ),
                       style = "info"
                     )
                   )
                 ),
                 
                 column(
                   5,
                   br(),
                   br(),
                   
                   tags$iframe(
                     src = "https://www.youtube.com/embed/b2b1hFEGxa8",
                     frameborder = "0",
                     allow = "accelerometer;
                                autoplay; encrypted-media; gyroscope; picture-in-picture",
                     allowfullscreen = NA,
                     width = "575px",
                     height = "320px"
                   ),
                   
                   br(),
                   br(),
                   column(1,
                          offset = 2,
                          br(),
                          tags$div(
                            tags$a(href = "https://www.inf.ovgu.de/",
                                   tags$div(
                                     id = "fin",
                                     tags$img(src = "fin_transp.png", width = "350px")
                                   ))
                          ))
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
              12,
              br(),
              plotlyOutput("distress_source", height = "535px"),
            )),
            br(),
            "The above bar plot shows the level of stress for different sources. People are more stressed due to the fear of economy
                          collapse and catching corona virus but having no religious activities have causes least stress to the people.",
            br(),
            br()
          ),
          tabPanel(
            "Coping with Stress",
            fluidRow(column(
              12,
              br(),
              plotlyOutput("coping_source", height = "535px"),
            )),
            br(),
            "The above boxplot represents which methods people are using to cope with the Corona stress. According to this survey, people
            prefer the long range interaction with family and friends and spend time doing some hobby to cope with the Corona stress.",
            br(),
            br()
          ),
          tabPanel(
            "Bivariate Relationship",
            fluidRow(column(
              12,
              # offset = 1,
              br(),
              panel(style = "overflow-y:scroll; max-height: 400px; position:relative; align: centre",
                    fade_in(
                      duration = "slow",
                      tags$img(
                        src = "bivariate_plot.png",
                        width = "850px",
                        height = "750px"
                      )
                    ))
            )),
            fluidRow(
              br(),
              "It is evident that in general female respondents are on a higher number as compared to males in participation.
              It is very evident that Loneliness is the root cause of stress which contributes to a major level. The medians
              of the boxplot for `Perceived Support` heavily overlap and they do not differ hence, there is a positive correlation (0.251)
              among the two groups.",
              tags$div(tags$ul(
                tags$li(
                  tags$b("Loneliness"),
                  "tends to increase `Perceived Stress` in females and it is about 0.559 which is seen lesser in male respondents."
                ),
                tags$li(
                  "During",
                  tags$b("Extraversion"),
                  " the `Perceived Support` in males are higher (0.263) in comparison to females (0.2331)."
                ),
                tags$li(
                  tags$b("Extraversion"),
                  " reduces the perceived stress significantly in both cases and it is highly effective for females than male respondents."
                )
              )),
              
              br(),
              br()
            )
          )
        )
      ),
      
      tabPanel(
        "Twitter Analysis",
        fluidRow(
          class = "data-panel",
          navlistPanel(
            tabPanel("Word Cloud",
                     
                     tabsetPanel(
                       tabPanel("2020",
                                fluidRow(column(
                                  1,
                                  offset = 1,
                                  
                                  fade_in(
                                    duration = "slow",
                                    tags$img(
                                      src = "wordCloud2020.png",
                                      width = "800px",
                                      height = "400px"
                                    )
                                  )
                                )),
                                
                                fluidRow(
                                  column(
                                    12,
                                    "It is pretty evident that",
                                    tags$b("coronavirus"),
                                    " outweigh other tags in the year 2020 which
                                           was trending when the arrival of the pandemic was sensed which remarks the global sentiment to a large extent."
                                    ,
                                    br(),
                                    br()
                                  )
                                )),
                       tabPanel("2021",
                                fluidRow(column(
                                  1,
                                  offset = 1,
                                  br(),
                                  fade_in(
                                    duration = "slow",
                                    tags$img(
                                      src = "wordcloud_2021.png",
                                      width = "800px",
                                      height = "400px"
                                    )
                                  )
                                  
                                )),
                                
                                fluidRow(
                                  column(
                                    12,
                                    "It can be seen that in 2021 the greater prominence is given to tweets with hashtags ",
                                    tags$b("capacity"),
                                    " that appear more frequently in the recent time period. From our research it was found that CAPACITY is a
                           registry of patients with COVID-19 and has been established to answer questions on the role of cardiovascular
                           disease in this pandemic. outweigh other tags in the year 2020 which was trending when the arrival of the pandemic
                           was sensed which remarks the global sentiment to a large extent."
                                    ,
                                    br(),
                                    br()
                                  )
                                ))
                     )),
            tabPanel(
              "Emotion Analysis",
              
              tabsetPanel(
                tabPanel("2020",
                         fluidRow(
                           column(
                             1,
                             offset = 1,
                             br(),
                             
                             plotlyOutput("sentiment_2020", width = "800px", height = "400px"),
                             
                             
                             br(),
                             br(),
                           )
                         )),
                tabPanel("2021",
                         fluidRow(
                           column(
                             1,
                             offset = 1,
                             br(),
                             
                             plotlyOutput("sentiment_2021", width = "800px", height = "400px"),
                             
                             br(),
                             br(),
                           )
                         )),
                
                fluidRow(
                  column(
                    12,
                    "Based on a comparative analysis, we infer that in the year 2021 the most dominant sentiment across people was positive while
                             it was negative in the year 2020."
                    ,
                    br(),
                    br()
                  )
                )
              )
            ),
            tabPanel(
              "Sentiment With Words",
              
              tabsetPanel(
                tabPanel("2020",
                         fluidRow(
                           column(
                             1,
                             offset = 1,
                             br(),
                             
                             plotlyOutput("word_sentiment_2020", width = "800px", height = "400px"),
                             
                             br(),
                             br(),
                           )
                         )),
                tabPanel("2021",
                         fluidRow(
                           column(
                             1,
                             offset = 1,
                             br(),
                             
                             plotlyOutput("word_sentiment_2021", width = "800px", height = "400px"),
                             
                             br(),
                             br(),
                           )
                         )),
                
                fluidRow(
                  column(
                    12,
                    "This gives us an important insight after a year while the negativity towards the ",
                    tags$b("virus"),
                    "and the other ongoing after effects of the pandemic such as ",
                    tags$b("death"),
                    ", ",
                    tags$b("risk"),
                    " symptoms has increased. On the other hand after a year, there can be seen a huge rise in the positive sentiment as people are ",
                    tags$b("recovering"),
                    ",",
                    tags$b("helping"),
                    " and ",
                    tags$b("supporting"),
                    " each other, are",
                    tags$b("safe"),
                    "  and getting ",
                    tags$b("free"),
                    " vaccination."
                    ,
                    br(),
                    br()
                  )
                )
              )
            ),
            tabPanel(
              "Network Analysis",
              tabsetPanel(tabPanel("2020",
                                   fluidRow(
                                     column(
                                       1,
                                       offset = 1,
                                       br(),
                                       
                                       fade_in(
                                         duration = "slow",
                                         tags$img(
                                           src = "network2020.png",
                                           width = "800px",
                                           height = "400px"
                                         )
                                       ),
                                       br(),
                                       br(),
                                     )
                                   )),
                          
                          tabPanel("2021",
                                   fluidRow(
                                     column(
                                       1,
                                       offset = 1,
                                       br(),
                                       
                                       fade_in(
                                         duration = "slow",
                                         tags$img(
                                           src = "network2021.png",
                                           width = "800px",
                                           height = "400px"
                                         )
                                       ),
                                       br(),
                                       br(),
                                     )
                                   ))),
              
              
              fluidRow(
                column(
                  12,
                  "In 2020 we observe that there is not much awareness with the pandemic. In 2021 we can observe that words like ",
                  tags$b("doses"),
                  ", ",
                  tags$b("age"),
                  " occur together, and the ",
                  tags$b("coronavirus"),
                  " cluster include
                           words like ",
                  tags$b("lockdown"),
                  ", ",
                  tags$b("restrictions"),
                  ", ",
                  tags$b("vaccine"),
                  ", ",
                  tags$b("testing"),
                  " which have become a part of daily life."
                  ,
                  br(),
                  br(),
                  "",
                  tags$b("Dose"),
                  " and ",
                  tags$b("Capacity"),
                  " have been the most commonly occurring words which tells us how important the number of doses and capacity
                           in hospitals has been an issue throughout the world.",
                  br(),
                  br()
                )
              )
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
                     "This plot shows IRI vs Confirmed COVID-19 cases for Infodemics and Epidemics data aggregation by Country and at a border level",
                     br(),
                     "categorized into the continent.",
                     br(),
                     "It can be inferred that the IRI volume is very high in `USA` with approximately 383,210 cases. Also, we have focused to show
                     this",
                     br(),
                     " critical impact on the top 5 continents around the globe. Moreover, it has to be strikingly noted that the IRI score is
                     very high in Peru" ,
                     br(),
                     "which is almost around 0.98 although the total infected cases are 11.",
                     br(),
                     br()
                     
                   ),
                   tabPanel(
                     "Cumulative IRI",
                     fluidRow(column(
                       10,
                       offset = 0,
                       br(),
                       
                       plotlyOutput("iri_red_plot", height = "500px")
                       
                     )),
                     br(),
                     "The sample sizes of the reported cases for the groups in the range of 3-7, 8-15 are reasonably symmetric indicative of less
                     variability in the analysis but, for groups 1-2, 16-50, 51-9999, 10k+ are left-skewed signifies some level of variability.",
                     br(),
                     br()
                     
                   ),
                   tabPanel("Comparisons",
                            
                            tabsetPanel(
                              tabPanel(
                                "Governments",
                                fluidRow(column(
                                  11,
                                  # offset = 1,
                                  br(),
                                  
                                  plotlyOutput("trust_gov_plot"),
                                  
                                  br(),
                                  br(),
                                )),
                                
                                "This shows a strong and negative correlation between trust in a particular country's
                            government and in global government. The confidence band is an indicator that it is 95% confident that the true
                            regression line lies in that gray zone.",
                                br(),
                                br()
                              ),
                              tabPanel(
                                "Scientist",
                                fluidRow(column(
                                  11,
                                  # offset = 1,
                                  br(),
                                  
                                  plotlyOutput("trust_sci_plot"),
                                  
                                  
                                  br(),
                                  br(),
                                ))
                                ,
                                
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
               )),
      
      tabPanel("Final Analysis",
               fluidRow(
                 class = "data-panel",
                 navlistPanel(
                   tabPanel("Stress Analysis",
                            fluidRow(column(
                              width = 12,
                              br(),
                              panel(
                                style = "overflow-y:scroll; max-height: 400px; position:relative; align: centre",
                                
                                
                                "The countries included in the analyses, and the respective sample size, are:",
                                tags$b("Austria"),
                                "(279)",
                                tags$b("Belgium"),
                                " (557)",
                                tags$b("Bulgaria"),
                                " (4,538)",
                                tags$b("Croatia"),
                                " (2,909)",
                                tags$b("Cyprus"),
                                " (34)",
                                tags$b("Czech Republic"),
                                "(1,344)",
                                tags$b("Denmark"),
                                " (10,327)",
                                tags$b("Estonia"),
                                " (34)",
                                tags$b("Finland"),
                                " (20,810)",
                                tags$b("France") ,
                                "(12,446)",
                                tags$b("Germany"),
                                " (1,271)",
                                tags$b("Greece"),
                                " (628)",
                                tags$b("Hungary"),
                                " (1,427)",
                                tags$b("Ireland"),
                                " (209)",
                                tags$b("Italy"),
                                " (1,370)",
                                tags$b("Latvia"),
                                " (22)",
                                tags$b("Lithuania"),
                                " (8,056)",
                                tags$b("Luxembourg"),
                                " (59)",
                                tags$b("Malta"),
                                " (21)",
                                tags$b("Netherlands"),
                                " (1256)",
                                tags$b("Poland"),
                                " (3,052)",
                                tags$b("Portugal"),
                                " (827)",
                                tags$b("Romania"),
                                " (189)",
                                tags$b("Slovakia"),
                                " (597)",
                                tags$b("Slovenia"),
                                " (21)",
                                tags$b("Spain"),
                                " (554)",
                                tags$b("Sweden"),
                                " (2,733).",
                                br(),
                                br(),
                                "Respondents were",
                                tags$b("74.18%"),
                                " female, ",
                                tags$b("24.63%"),
                                " male. The remaining respondents answered 'other' or did not provide an answer.",
                                br(),
                                "The majority of respondents",
                                tags$b("(67.16%)"),
                                " were in full-time, part-time work or self-employed,",
                                tags$b("16.06%"),
                                " were either unemployed or
                                retired,",
                                tags$b("16.79%"),
                                " were students.",
                                br(),
                                br(),
                                "The age of the respondents ranged from ",
                                tags$b("18"),
                                " to ",
                                tags$b("110"),
                                ", with a median age of ",
                                tags$b("38"),
                                ".",
                                br(),
                                br(),
                                "Individual's general stress levels were measured using an established ten-item scale developed by psychologists [@jstor1983]. This scale measures
                                participants' stress during the last week by using indicators of stress responses, for instance, perceived lack of control over events,
                                pressure from mounting difficulties and feeling upset about unexpected changes. Scores are considered ",
                                tags$b("moderate"),
                                " above ",
                                tags$b("2.4"),
                                ",
                                and ",
                                tags$b("high"),
                                " above ",
                                tags$b("3.7"),
                                ". Levels of stress were moderate or lower in many countries. Poland and Portugal reported the highest
                                levels of stress in Europe, and Denmark and the Netherlands the lowest.",
                                br(),
                                br(),
                                "Levels of stress remained fairly stable over the middle of April, with a negligible decrease between April 4th and
                                April 13th. Overall levels of stress remained higher in women compared to men throughout the period under consideration.",
                                br(),
                                br(),
                                "Participants were asked to indicate the extent to which a range of different
                                factors represented a source of distress during the COVID-19 health crisis. Specifically, participants indicated their d
                                isagreement or agreement with how much each factor from a list represented a source of distress",
                                tags$i("( _1 = Strongly Disagree,
                                6 = Strongly Agree_ )"),
                                ". Results indicated that people were on average concerned with the ",
                                tags$b("state of the national economy"),
                                ".
                                Economic considerations were followed closely by
                                ",
                                tags$b("health-related risks"),
                                " , such as the risks of ",
                                tags$b("being hospitalized"),
                                " and of",
                                tags$b("contracting the new disease"),
                                " .",
                                br(),
                                br(),
                                "Participants were asked how much they trusted six key institutions , in relation to the COVID-19 emergency
                                (on a scale from 1 = not at all to 10 = completely). Specifically, participants were asked about their trust
                                towards the ",
                                tags$b("health care systems"),
                                " , the ",
                                tags$b("World Health Organization (W.H.O.)"),
                                ", the ",
                                tags$b("national governments' efforts to tackle the COVID-19"),
                                " , the ",
                                tags$b("Police"),
                                " , the ",
                                tags$b("civil service"),
                                " and the national ",
                                tags$b("government"),
                                ".",
                                br(),
                                br(),
                                "Overall, it was reported only medium levels of trust, with the highest levels of trust for their countrie's
                                ",
                                tags$b("healthcare system"),
                                " and the ",
                                tags$b("WHO"),
                                ". Trust towards the national government was relatively lower,
                                compared to the other institutions examined."
                                
                              )
                            ))),
                   
                   tabPanel("Twitter Analysis",
                            fluidRow(column(
                              12,
                              br(),
                              panel(
                                style = "overflow-y:scroll; max-height: 400px; position:relative; align: centre",
                                
                                "We make use of 35,725 tweets dated from June 17, 2021 to June 19, 2021 and about 15000 tweets in English
                                              from last year June for our analysis. From our analysis from unigram and bigram tag clouds of 2020,
                                              we find that most of the tweets consisted of hashtags with",
                                tags$b("#coronavirus"),
                                ",
                                              ",
                                tags$b("#el coronavirus"),
                                "  followed by ",
                                tags$b("#china"),
                                ", ",
                                tags$b("#wuhan"),
                                ", ",
                                tags$b("#de"),
                                ".
                                              Whereas in 2021 the Twitter community seems to post their opinions
                                              and views on ",
                                tags$b("#capacity"),
                                ", ",
                                tags$b("#vaccines"),
                                ", ",
                                tags$b("#coivd19"),
                                ", ",
                                tags$b("#cowin coivid19"),
                                " , ",
                                tags$b("#dose2 capacity"),
                                "
                                              ,",
                                tags$b("#dose1 capacity"),
                                " , ",
                                tags$b("#delta variant"),
                                "
                                              which shows their inclination towards awareness about the cure rather than panic to an extent.",
                                br(),
                                br(),
                                "

                                              From the sentimental analysis perspective to capture the tone of the emotions, we targeted the top 15 words
                                              where the community expressed their choice of words
                                              whenever they posted something on the Twitter page. We report that instead of mentioning the "
                                ,
                                tags$b("coronavirus"),
                                "
                                              community rather resorted to the acronym ",
                                tags$b("covid"),
                                " to
                                              signify the ease of posting and associating related tweets.",
                                
                                br(),
                                br(),
                                
                                "From network analysis we see that ",
                                tags$b("Dose"),
                                "  and ",
                                tags$b("Capacity"),
                                "  have been the most commonly occurring words which tells
                                  us how important the number of doses and capacity in hospitals has been an issue throughout the world especially in India.
                                  We can see a pattern that ",
                                tags$b("lockdown"),
                                "  , ",
                                tags$b("restrictions"),
                                "  ,",
                                tags$b("vaccine"),
                                " , ",
                                tags$b("testing"),
                                "  are the most commonly occurring words
                                  together which indicates that they have become a part of daily life.",
                                
                                br(),
                                br(),
                                "Also from our NRC and based on a comparative analysis, we could see that there is a significant rise in the
                                              ",
                                tags$b("positive"),
                                " sentiment of the people. The positive
                                              sentiment registered by the people in the year 2021 is ",
                                tags$b("24.4%"),
                                " while compared
                                              with the positive sentiment of",
                                tags$b("15%"),
                                "
                                              in the year 2020. Similarly, negative sentiment declined by ",
                                tags$b("6%"),
                                " in the year 2021
                                              compared to the year 2020."
                              )
                            ))),
                   tabPanel("Infodemics",
                            fluidRow(column(
                              width = 12,
                              br(),
                              
                              panel(
                                style = "overflow-y:scroll; max-height: 400px; position:relative; align: centre",
                                
                                "Just as we need to flatten the Covid-19 curve we must also tackle the infodemic curve. Just as with Covid-19 we must attack the curve on two fronts (suppress the contagion and increase our capacity to deal with the surge of information that is coming our way).",
                                br(),
                                "A comparative correlation for the trust in media with factors concerning the following were taken into account during our regression analysis:",
                                br(),
                                tags$div(tags$ul(
                                  tags$li(
                                    "Whether COVID-19 is a naturally occurring virus or an artificially made (e.g. lab created)? (`virus_natart`)"
                                  )
                                )),
                                "From our analsyis for `virus_natart` and `media_underover` the true correlation is around",
                                tags$b("13%"),
                                " which means media has a very high role for overhyping over this question.",
                                br(),
                                tags$div(tags$ul(
                                  tags$li(
                                    "How is the media in general is reporting on the COVID-19 situation? Underplaying or Over-hyping or Just right. (`media_underover`)"
                                  )
                                )),
                                "We compare our scores at two places one with `virus_natart` where they are overhyped over this news and for `feelinginformed_avg` they likely to be less informed since they share a negative correlation.",
                                br(),
                                tags$div(tags$ul(
                                  tags$li(
                                    "- How often a contradictory news is found and turned out to be fake news? (`fakenews`)"
                                  )
                                )),
                                "Also, for `virus_natart` and `fakenews` with only just",
                                tags$b("36%"),
                                " it is difficult to verify the trueness of this question which is very uncertain.",
                                br(),
                                tags$div(tags$ul(
                                  tags$li(
                                    "How informed (`feelinginformed_avg`) are the citizens feeling about:"
                                  ),
                                  tags$ul(
                                    tags$li("The risk of contracting COVID-19"),
                                    tags$li("Symptoms of COVID-19"),
                                    tags$li("How COVID-19 virus spreads?"),
                                    tags$li("How to prevent COVID-19 from spreading?"),
                                    tags$li("Treatment of COVID-19")
                                  )
                                )),
                                
                                "We have analyzed both of these with respect to `fakenews` and `media_underover` for the former a very low correlation is observed meaning atleast by slightly the average number of
                                people are convinced by fake news over social media Whereas, for the latter a negative correlation to this observed meaning an average number of people are likely to be less informed
                                by the media tacit.To our readers we therefore request to abide to the following in the near future to make sure perform the following:",
                                tags$div(tags$ul(
                                  tags$li("Fact check to alert yourself what is currently going around"),
                                  tags$li("Stick to trusted sources"),
                                  tags$li("Do not forward without checking the authenticity of messages"),
                                  tags$li(
                                    "Increase supply of data by engaging regularly and meaningfully on the platforms that people are already using"
                                  )
                                )),
                              )
                            )))
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
                                href = "https://rpubs.com/ranjiraj9/covidistress",
                                target = "_blank",
                                tags$img(id = "r-logo", src =
                                           "R-studio.svg", width = "300px")
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
                                           "github.svg", width = "300px")
                              )
                            ))),
                   tabPanel("Screencast",
                            fluidRow(column(
                              1,
                              offset = 4,
                              br(),
                              br(),
                              br(),
                              
                              
                              tags$a(
                                href = "https://www.youtube.com/watch?v=b2b1hFEGxa8",
                                target = "_blank",
                                tags$img(id = "yt-logo", src =
                                           "YT.svg", width = "300px")
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
                                  "https://zenodo.org/record/3831406#.YNhxFej7Q6b",
                                target =
                                  "_blank",
                                "COVID19 2020 Twitter Data")
                       )
                     ))
                     
                   ),
                   tabPanel("References",
                            fluidRow(br(),
                                     column(
                                       11,
                                       DT::dataTableOutput("bibTable")
                                     )))
                 )
               ))
    )
  )

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  # world_map_stress_levels
  output$world_stress_map <- renderPlotly({
    world_map_stress_levels <- function(continent) {
      map <- plot_ly(
        mergedData,
        type = 'choropleth',
        locations = mergedData$CODE,
        z = mergedData$PSS10_avg,
        text = mergedData$COUNTRY,
        colorscale = "Portland"
        
      ) %>%
        layout(paper_bgcolor = 'transparent')
      
      return(map)
    }
    
    world_map_stress_levels(input$continent)
  })
  
  # Government Plot
  output$trust_gov_plot <- renderPlotly({
    ggplotly(
      ggplot(data_scatterplot_gov, aes(x = govglob, y = govloc)) +
        geom_point(col = "blue", alpha = 0.9) +
        geom_text(label = data_scatterplot_gov$country) +
        xlab("Trust in global governments") +
        ylab("Trust in country's government") +
        ggtitle(
          "Plot for trust among citizens in Country's government for 12 different countries"
        ) +
        geom_smooth(method = "lm")
    )
    
  })
  
  
  # Scientist Plot
  output$trust_sci_plot <- renderPlotly({
    ggplotly(
      ggplot(data_scatterplot_scient,
             aes(x = scientglob, y = scientloc)) +
        geom_point(col = "blue", alpha = 0.5) +
        geom_text(label = data_scatterplot_scient$country) +
        xlab("Trust in scientists globally") +
        ylab("Trust in country's scientists") +
        ggtitle(
          "Plot for trust among citizens in Country's scientists for 12 different countries"
        ) +
        geom_smooth(method = "lm")
    )
    
  })
  
  # emotion plot 2020
  output$sentiment_2020 <- renderPlotly({
    library(ggplot2)
    ggplot(data = sentiment_2020, aes(x = sentiment, y = Score)) + geom_bar(aes(fill =
                                                                                  sentiment), stat = "identity") +
      theme(legend.position = "none") +
      xlab("Sentiments") + ylab("scores") + ggtitle("Emotions of people behind the tweets on COVID19 in 2020 May")
  })
  
  # emotion plot 2021
  output$sentiment_2021 <- renderPlotly({
    library(ggplot2)
    ggplot(data = sentiment_2021, aes(x = sentiment, y = Score)) + geom_bar(aes(fill =
                                                                                  sentiment), stat = "identity") +
      theme(legend.position = "none") +
      xlab("Sentiments") + ylab("scores") + ggtitle("Emotions of people behind the tweets on COVID19 in 2021 June")
  })
  
  # sentiment with words plot 2020
  output$word_sentiment_2020 <- renderPlotly({
    word_sentiment_2020 %>%
      filter(word != "trump") %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 2000)) +
      facet_wrap( ~ sentiment, scales = "free_y") +
      labs(title = "Sentiment with popular words during 2020 June",
           y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
  })
  
  # sentiment with words plot 2021
  output$word_sentiment_2021 <- renderPlotly({
    word_sentiment_2021 %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 2000)) +
      facet_wrap( ~ sentiment, scales = "free_y") +
      labs(title = "Sentiment with popular words during 2021 June",
           y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
  })
  
  # distress source plot
  output$distress_source <- renderPlotly({
    fig_pie <-
      plot_ly(
        distress_source,
        labels = distress_source$Source,
        values = distress_source$Value,
        type = 'pie'
      )
    fig_pie %>% layout(
      title = 'Sources of Distress during Corona',
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      )
    )
  })
  
  # coping source plot
  output$coping_source <- renderPlotly({
    fig_pie <-
      plot_ly(
        distress_source,
        labels = coping_source$Source,
        values = coping_source$Value,
        type = 'pie'
      )
    fig_pie %>% layout(
      title = 'How people usually cope with \nCorona Stress',
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      )
    )
  })
  
  
  # IRI Reduction Box Plot
  output$iri_red_plot <- renderPlotly({
    INFODEMIC_REDUCED_FILE_PATH <-
      file.path("data/infodemics_reduced.csv")
    WORLD_RISK_INDEX_FILE_PATH <-
      file.path("data/world_risk_index.csv")
    dat.red <-
      read.table(INFODEMIC_REDUCED_FILE_PATH,
                 header = T,
                 sep = ";")
    dat.red$date <- as.Date(dat.red$date)
    dat.iri.world <-
      read.table(WORLD_RISK_INDEX_FILE_PATH,
                 header = T,
                 sep = ";")
    
    dat.corr2 <- data.frame()
    dat.corr <-
      dat.red[, c("date", "iso3", "EPI_CONFIRMED", "IRI_ALL")]
    
    for (cc in unique(dat.corr$iso3)) {
      tmp <- dat.corr[which(dat.corr$iso3 == cc), ]
      tmp <- tmp[order(tmp$date), ]
      tmp$EPI_CONFIRMED_DAILY <- c(0, diff(tmp$EPI_CONFIRMED))
      tmp$IRI_ALL_CUMMEAN <- dplyr::cummean(tmp$IRI_ALL)
      dat.corr2 <- rbind(dat.corr2, tmp)
    }
    
    dat.corr2 <- dat.corr2[!is.na(dat.corr2$IRI_ALL), ]
    dat.corr2 <-
      dat.corr2[-which(dat.corr2$EPI_CONFIRMED == 0), ]
    
    bin <- rep(0, nrow(dat.corr2))
    bin[which(dat.corr2$EPI_CONFIRMED <= 2)] <- 0
    bin[which(3 <= dat.corr2$EPI_CONFIRMED &
                dat.corr2$EPI_CONFIRMED < 8)] <- 1
    bin[which(8 <= dat.corr2$EPI_CONFIRMED &
                dat.corr2$EPI_CONFIRMED < 16)] <- 2
    bin[which(16 <= dat.corr2$EPI_CONFIRMED &
                dat.corr2$EPI_CONFIRMED < 51)] <- 3
    bin[which(51 <= dat.corr2$EPI_CONFIRMED &
                dat.corr2$EPI_CONFIRMED < 10001)] <- 4
    bin[which(10001 <= dat.corr2$EPI_CONFIRMED &
                dat.corr2$EPI_CONFIRMED < 81000)] <- 5
    
    dat.corr2$bin <- bin
    
    labels.min <-
      dat.corr2 %>% group_by(bin) %>% summarise_at(vars(EPI_CONFIRMED), min)
    labels.max <-
      dat.corr2 %>% group_by(bin) %>% summarise_at(vars(EPI_CONFIRMED), max)
    
    lab <-
      paste0(labels.min$EPI_CONFIRMED,
             '-',
             labels.max$EPI_CONFIRMED)
    lab[5:6] <- c('51-9999', '10000+')
    
    
    ggplotly(
      ggplot(dat.corr2, aes(as.factor(bin), IRI_ALL_CUMMEAN)) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              legend.position = "none") +
        geom_boxplot(
          aes(fill = as.numeric(bin)),
          size = 0.15,
          outlier.color = "grey70",
          color = "grey70",
          notch = TRUE
        ) +
        xlab("Cumulative Number of Reported Cases") +
        ylab("IRI Cumulative Mean") +
        scale_fill_viridis_c() +
        scale_x_discrete(labels = lab) +
        ggtitle("Cumulative IRI vs. Epidemic per index confirmed")
    )
  })
  
  # references
  get_bib <- reactive({
    df <-
      bib2df("data/references.bib") %>% dplyr::select(TITLE, AUTHOR, YEAR, DOI, URL)
    df$AUTHOR <-
      unlist(lapply(df$AUTHOR, paste, collapse = " "))
    return(df)
  })
  output$bibTable <- DT::renderDataTable({
    bib <- get_bib()
    bib
  })
  
}

shinyApp(ui, server)
