library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(GGally)
library(dplyr)
library(bib2df)
library(DT)


# Read Data Files
mergedData <- read.csv("world_map.csv")
bivariateData <- read.csv("bivariate.csv")
data_scatterplot_gov <- read.csv("data_scatter_gov.csv")
data_scatterplot_scient <- read.csv("data_scatter_scient.csv")
sentiment_2021 <- read.csv("sentiment_scores_2021.csv")
sentiment_2020 <- read.csv("sentiment_scores_2020.csv")



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
            # ,
            # br(),
            # selectizeInput(
            #   "continent",
            #   choices = list(
            #     "World",
            #     "Asia",
            #     "Europe",
            #     "Africa",
            #     "North America",
            #     "South America",
            #     "Oceania"
            #   ),
            #   label = "Select Continents"
            # )
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
              11,
              # offset = 2,
              br(),
              
              plotOutput("bivariate_graph", height = "600px"),
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
      
      tabPanel(
        "Twitter Analysis",
        fluidRow(
          class = "data-panel",
          navlistPanel(
            tabPanel("Word Cloud",
                     
                     tabsetPanel(
                       tabPanel("2020",
                                fluidRow(
                                  column(
                                    1,
                                    offset = 1,
                                    br(),
                                    
                                    tags$img(
                                      src = "wordcloud_2020.png",
                                      width = "800px",
                                      height = "400px"
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
                                    
                                    tags$img(
                                      src = "wordcloud_2021.png",
                                      width = "800px",
                                      height = "400px"
                                    ),
                                    br(),
                                    br(),
                                  )
                                ))
                     )),
            tabPanel("Emotion Analysis",
                     
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
                                ))
                     )),
            tabPanel("Sentiment With Words",
                     
                     tabsetPanel(
                       tabPanel("2020",
                                fluidRow(
                                  column(
                                    1,
                                    offset = 1,
                                    br(),
                                    
                                    tags$img(
                                      src = "sentiment_2020.png",
                                      width = "800px",
                                      height = "400px"
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
                                    
                                    tags$img(
                                      src = "sentiment_2021.png",
                                      width = "800px",
                                      height = "400px"
                                    ),
                                    br(),
                                    br(),
                                  )
                                ))
                     )),
            tabPanel("Network Analysis",
                     
                     tabsetPanel(
                       tabPanel("2020",
                                fluidRow(
                                  column(
                                    1,
                                    offset = 1,
                                    br(),
                                    
                                    tags$img(
                                      src = "word_network_2020.png",
                                      width = "800px",
                                      height = "400px"
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
                                    
                                    tags$img(
                                      src = "word_network_2021.png",
                                      width = "800px",
                                      height = "400px"
                                    ),
                                    br(),
                                    br(),
                                  )
                                ))
                     ))
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
                                href = "https://github.com/ranjiGT/Data-Science-with-R-2021",
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
                                  "https://zenodo.org/record/3831406#.YNhxFej7Q6b", target =
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
  
  # Bivariate Plot
  output$bivariate_graph <- renderPlot({
    bivariateData$X <- NULL
    levels(bivariateData$Dem_gender)[levels(bivariateData$Dem_gender) == "Other/would rather not say"] = "Undisclosed"
    
    ggpairs(
      bivariateData,
      columnLabels = c(
        "Perceived Stress",
        "Loneliness",
        "Perceived Support",
        "Extraversion",
        "Gender"
      ),
      mapping = ggplot2::aes(col = Dem_gender, alpha = .2),
      upper = list(continuous = wrap("cor", size = 3)),
      title = "Bivariate relationship of Perceived Stress, Social Support, Loneliness, and Extraversion"
    ) +
      theme(title = element_text(face = "bold")) + theme(plot.title = element_text(hjust = 0.5)) +
      theme(text = element_text(size = 14))
    
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
      ggplot(data_scatterplot_scient, aes(x = scientglob, y = scientloc)) +
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
  
  output$sentiment_2020 <- renderPlotly({
    library(ggplot2)
    ggplot(data = sentiment_2020, aes(x = sentiment, y = Score)) + geom_bar(aes(fill =
                                                                                  sentiment), stat = "identity") +
      theme(legend.position = "none") +
      xlab("Sentiments") + ylab("scores") + ggtitle("Emotions of people behind the tweets on COVID19 in 2020 May")
  })
  
  output$sentiment_2021 <- renderPlotly({
    library(ggplot2)
    ggplot(data = sentiment_2021, aes(x = sentiment, y = Score)) + geom_bar(aes(fill =
                                                                                  sentiment), stat = "identity") +
      theme(legend.position = "none") +
      xlab("Sentiments") + ylab("scores") + ggtitle("Emotions of people behind the tweets on COVID19 in 2021 June")
  })
  
  
  # IRI Reduction Box Plot
  output$iri_red_plot <- renderPlotly({
    INFODEMIC_REDUCED_FILE_PATH <-
      file.path("infodemics_reduced.csv")
    WORLD_RISK_INDEX_FILE_PATH <-
      file.path("world_risk_index.csv")
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
    dat.corr2 <- dat.corr2[-which(dat.corr2$EPI_CONFIRMED == 0), ]
    
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
      paste0(labels.min$EPI_CONFIRMED, '-', labels.max$EPI_CONFIRMED)
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
  
  get_bib <- reactive({
    df <-
      bib2df("references.bib") %>% dplyr::select(TITLE, AUTHOR, YEAR, DOI, URL)
    df$AUTHOR <- unlist(lapply(df$AUTHOR, paste, collapse = " "))
    return(df)
  })
  output$bibTable <- DT::renderDataTable({
    bib <- get_bib()
    bib
  })
  
}

shinyApp(ui, server)