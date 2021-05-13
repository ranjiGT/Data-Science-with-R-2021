# `Data Science with R 2021` 
<p align="center"> ![logo](https://github.com/ranjiGT/Data-Science-with-R-2021/blob/main/logo.svg) </p>

## `Psychological & Behavioural distress of COVID-19 & Infodemics`

## `PREFACE` :scroll:

The coronavirus COVID-19 pandemic is an **unprecedented** health crisis. One of the biggest impacts of this situation has been the facts on our collective, mental health studies on the global burden of disease were already shown by WHO that mental disorders, make a 5 out of 10 leading causes of disability worldwide and this is before the pandemic started. The stress, anxiety, depression stemming from fear, isolation, and stigma around COVID-19 affected, all of us in one way or another. Many people are losing their jobs and the elderly are isolated from their usual support network. Of course, the past year has been hard on healthcare workers having to work long hours, personal risk of exposure to the virus, and witnessing firsthand the loss of people's colleagues and loved ones. The issue is that the effects of the pandemic and mental health, maybe for longer-lasting than the disease itself.    

Indirectly, the measures are taken to slow, the spread of the virus has affected our physical activity levels, our eating behaviors, our sleep patterns, and our relationship with addictive substances, including social media. Into this last point, both our increased use of social media while stuck at home, as well as the increased exposure to disaster news past year, have amplified the negative effects of social media on our mental health. This motivates us to perform some diagnostic analysis of this pattern and portray some meaningful insights on global survey data.


## `PROJECT OBJECTIVES` (TBD) :hourglass:
:bulb:**`Objective 1`:** Here we aim to analyze and visualize the survey dataset to come up with certain descriptive and diagnostic statistics including the number of countries that participated in the survey. We look forward to analyzing the various stress levels from each country, with a focus on visualizing the distress scale with the source of distress and the level of distress. Further, we plan to visualize the coping scale which consists of the source of coping and detailing what was the personal conscious effort, to solve personal and interpersonal problems, to master, minimize or tolerate stress and conflict. We also target to depict the visualization of the level of trust such as Country's Civil service, Country's Police, Country's Healthcare system, WHO, Government's measures against COVID which also play a crucial role. We extend to diagnose on areas such as compliance and the level of agreement, the concern level, and the key factor of media from where the respondents sought to agree to take the information from. The different relationship between perceived stress, social support, loneliness, and extroversion according to different age groups will also be set as our basis for analysis.  :heavy_check_mark:

:bulb:**`Objective 2`:** 
We propose to know the impact of COVID-19 in Twitter/Linkedin. This is done by retrieving the top tweets or popular posts from a specific location after the coronavirus outbreak across the globe. It will help us analyze the various responses taken by the administration as well as people's response as the virus unfolded. We target to extract hashtags like *#covidjobs, #workfromhome, #wfh, #remotejobs* etc., in order to get the related posts pertaining to it and analyze the engagement of people in this subject and maximize reach based on similar interests. Retrieving the re-tweeted tweets or posts will help us understand the effects of COVID-19 among people. We perform sentimental analysis on the tweets in order to understand the sentiments of people is crucial during this time of pandemic. 

:bulb:**`Objective 3`:** Our final major objective will be for [`Infodemics`](https://www.nature.com/articles/s41598-020-73510-5), that is to perform a comparative analysis and also to outline the interaction patterns of information spreading dynamics around the same argument in different environments having different interaction settings and audiences and reporting the rumor amplification parameters for each the social media due to COVID-19 to show how inaccurate and the misinformation is spread on different mainstream online platforms and its ill-effects raging the fear among the common public in day-to-day life.

## `DATASETS` :globe_with_meridians:

**1. COVIDISTRESS all global survey data**  
(The COVIDiSTRESS global survey is an open science collaboration,
created by researchers in over 40 countries to rapidly and organically
collect data on human experiences of the Coronavirus epidemic 2020.)
Dataset can be downloaded here:
[COVIDiSTRESS global survey network (2020) _COVIDiSTRESS global survey_ . DOI 10.17605/OSF.IO/Z39US, Retrieved from osf.io/z39us]
*https://osf.io/z39us/files/*

These datasets mainly focuses on stress levels, sources of stress and trust in institutions across the EU. Furthermore, also include factors such as loneliness, media use, personality, social provisions and perceived sources of psychological relief.


**2. Twitter/Linkedin Data**  
We aim work on the most recent dataset aggregated from Twitter and Linkedin using twitteR, Rlinkedin and rtweet libraries within a particular time and location. 

Here [`twitteR`](https://www.rdocumentation.org/packages/twitteR/versions/1.1.9) and [`Rlinkedin`](https://cran.r-project.org/web/packages/Rlinkedin/Rlinkedin.pdf) which provides an interface and access to Twitter and Linkedin web API respectively, [`rtweet`](https://cran.r-project.org/web/packages/rtweet/rtweet.pdf) which acts as the client for Twitter's REST and stream APIs will be used to retrieve data .


**3. Social Media & COVID-19 raw survey data**   
(Social Media & COVID-19: A Global Study of Digital Crisis Interaction among Gen Z and Millennials)
Dataset can be downloaded here: [A Global Study of Digital Crisis Interaction among Gen Z and Millennials] 

*https://www.covid19-infodemic.com/#resources*

This dataset comprises of raw survey data collected from mobile devices in form of questions and answers format along with combined data in CSV format.

### Other useful links:

- https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7536964/
- http://thinktostart.com/analyze-linkedin-with-r/
