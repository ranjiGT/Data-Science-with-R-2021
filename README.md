# Psychological & Behavioural distress of COVID-19 & Infodemics <img src="https://github.com/ranjiGT/Data-Science-with-R-2021/blob/main/files/logo.svg" align="right" alt="" width="300" />



## `Preamble` :scroll:

The coronavirus COVID-19 pandemic is an unprecedented health crisis that has impacted the world to a large extent. According to WHO, mental disorders are one of the leading causes of disability worldwide, so considering that this pandemic has caused further complications to mental ailment. The stress, anxiety, depression stemming from fear, isolation, and stigma around COVID-19 affected all of us in one way or another. We could see that many people are losing their jobs and the elderly are isolated from their usual support network. The issue is that the effects of the pandemic and mental health, maybe for longer-lasting than the disease itself.    

In this limelight, although the measures are taken to slow the spread of the virus, it has affected our physical activity levels, our eating behaviors, our sleep patterns, and our relationship with addictive substances, including social media. Into this last point, both our increased use of social media while stuck at home, as well as the increased exposure to disaster news past year, have amplified the negative effects of social media on our mental health. This motivates us to perform some diagnostic analysis of this pattern and portray some meaningful insights on global survey data.


## `Datasets` :globe_with_meridians:	

**1. COVIDISTRESS all global survey data** 

(The COVIDiSTRESS global survey is an open science collaboration,
created by researchers in over 40 countries to rapidly and organically
collect data on human experiences of the Coronavirus epidemic 2020.)
Dataset can be downloaded here:
[COVIDiSTRESS global survey network  (2020, March 30). COVIDiSTRESS global survey. DOI 10.17605/OSF.IO/Z39US, Retrieved from osf.io/z39us]
*https://osf.io/z39us/*

These datasets mainly focus on the stress levels, sources of stress, and trust in institutions across the EU. Furthermore, it also includes factors such as loneliness, media use, personality, social provisions, and perceived sources of psychological relief.


**2. Twitter Data**  
We aim to work on the most recent dataset aggregated from Twitter using twitteR and rtweet libraries within a particular time and location.

Here [`twitteR`](https://www.rdocumentation.org/packages/twitteR/versions/1.1.9) which provides an interface and access to Twitter web API respectively, [`rtweet`](https://www.rdocumentation.org/packages/rtweet/versions/0.7.0) which acts as the client for Twitter's REST and stream APIs will be used to retrieve data.


**3. COVID19 Infodemics Observatory**   
(The Role of Trust and Information during the COVID-19 Pandemic and Infodemic)
Dataset can be downloaded here: [R. Gallotti, N. Castaldo, F. Valle, P. Sacco and M. De Domenico, COVID19 Infodemics Observatory (2020). DOI: 10.17605/OSF.IO/N6UPX]
[Van Mulukom, V. (2021, May 15). The Role of Trust and Information during the COVID-19 Pandemic and Infodemic. https://doi.org/10.17605/OSF.IO/GFWBQ]

- *https://osf.io/n6upx/*, 
- *https://osf.io/67zhg/*, 
- *https://osf.io/rtacb/*, 
- *https://osf.io/dh879/*,
- *https://osf.io/c37wq/*

These datasets comprises of summary of infodemics data collected from across countries, the world risk index, population emotional state, and news reliability.

( _Very large files to this repository has been added via git-lfs._ )

Folder Structure :open_file_folder:
============================

> Folder structure and naming conventions for this project

### A top-level directory layout

    .
    ├── data                   # Parent files (.csv, .sav)
    ├── files                    # logo and misc. files
    ├── process-nbk
            ├── data                    #Compiled files (.csv)
            ├── fig                         #Compiled images (.png, .jp(e)g)
            └── scripts                        #Compiled codes(.R, .Rmd)
    ├── process_notebook_final                 # Final process notebook                                
    ├── project_proposal                    # Low-level design (LLD) documentation files                  
    ├── .gitignore
    ├── LICENSE
    └── README.md

## `Process Notebook` :open_book:

https://rpubs.com/ranjiraj9/covidistress

( _Click the button_ `Hide Toolbars` _at bottom right corner of Rpubs for better view_. )

_Click_ [here](https://github.com/ranjiGT/Data-Science-with-R-2021/blob/main/process_notebook_final/ultimate-process-notebook.Rmd) _to access the source code of the notebook_.

## `Screencast` :arrow_forward:	

[![screencast](https://img.youtube.com/vi/b2b1hFEGxa8/maxresdefault.jpg)](https://youtu.be/b2b1hFEGxa8)

## `Project website` :trackball:

https://covid-distress-infodemics.shinyapps.io/shinyapp/


To launch the app from Rstudio :point_up_2: 

```
library(shiny)
runApp("covid-distress-infodemics")
```
