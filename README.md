# Psychological & Behavioural Distress of COVID-19 & Infodemics

<img src="https://github.com/ranjiGT/Data-Science-with-R-2021/blob/main/files/logo.svg" align="right" alt="Project Logo" width="280" />

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![R](https://img.shields.io/badge/Built%20with-R-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-App-blueviolet.svg)](https://covid-distress-infodemics.shinyapps.io/shinyapp/)

A data science project analysing the psychological and behavioural impact of the COVID-19 pandemic, including stress, anxiety, trust in institutions, and the role of social media infodemics — built with R.

---

## Table of Contents

- [Overview](#overview)
- [Datasets](#datasets)
- [Project Structure](#project-structure)
- [Process Notebook](#process-notebook)
- [Shiny Web App](#shiny-web-app)
- [Screencast](#screencast)
- [Getting Started](#getting-started)
- [Contributors](#contributors)
- [License](#license)

---

## Overview

The COVID-19 pandemic is an unprecedented health crisis that has impacted the world to a large extent. According to the WHO, mental disorders are among the leading causes of disability worldwide, and the pandemic has further compounded mental health challenges. Stress, anxiety, and depression — stemming from fear, isolation, and stigma — have affected all of us in one way or another, with widespread job losses and elderly populations isolated from their support networks.

Measures taken to slow the spread of the virus have also affected physical activity, eating behaviours, sleep patterns, and our relationship with addictive substances — including social media. Increased social media usage during lockdowns, combined with constant exposure to disaster news, has amplified negative effects on mental well-being.

This project performs diagnostic analysis of these patterns and derives meaningful insights from global survey data, Twitter sentiment analysis, and infodemics research.

---

## Datasets

### 1. COVIDiSTRESS Global Survey

An open science collaboration created by researchers in over 40 countries to collect data on human experiences during the 2020 coronavirus epidemic.

> COVIDiSTRESS global survey network (2020, March 30). COVIDiSTRESS global survey. DOI [10.17605/OSF.IO/Z39US](https://osf.io/z39us/)

Covers stress levels, sources of stress, trust in institutions across the EU, loneliness, media use, personality traits, social provisions, and perceived sources of psychological relief.

### 2. Twitter Data

COVID-19 related tweets collected using [`twitteR`](https://www.rdocumentation.org/packages/twitteR/versions/1.1.9) and [`rtweet`](https://www.rdocumentation.org/packages/rtweet/versions/0.7.0) R packages for sentiment analysis.

- **2020 data**: Academic dataset of tweet IDs from [Zenodo](https://zenodo.org/record/3831406)
- **2021 data**: Collected directly via the Twitter API

### 3. COVID-19 Infodemics Observatory

Examines the role of trust and information during the pandemic and infodemic.

> R. Gallotti, N. Castaldo, F. Valle, P. Sacco and M. De Domenico, COVID19 Infodemics Observatory (2020). DOI: [10.17605/OSF.IO/N6UPX](https://osf.io/n6upx/)
>
> Van Mulukom, V. (2021, May 15). The Role of Trust and Information during the COVID-19 Pandemic and Infodemic. DOI: [10.17605/OSF.IO/GFWBQ](https://doi.org/10.17605/OSF.IO/GFWBQ)

Sources: [osf.io/n6upx](https://osf.io/n6upx/) · [osf.io/67zhg](https://osf.io/67zhg/) · [osf.io/rtacb](https://osf.io/rtacb/) · [osf.io/dh879](https://osf.io/dh879/) · [osf.io/c37wq](https://osf.io/c37wq/)

Includes infodemics summary data, the World Risk Index, population emotional state, and news reliability indicators.

> **Note:** Very large files in this repository are tracked via [Git LFS](https://git-lfs.github.com/).

---

## Project Structure

```
.
├── data/                        # Raw datasets (.csv, .sav)
├── files/                       # Logo and miscellaneous assets
├── process-nbk/
│   ├── data/                    # Processed data files
│   ├── fig/                     # Generated figures (.png, .jpg)
│   └── scripts/                 # Analysis scripts (.R, .Rmd)
├── process_notebook_final/      # Final process notebook (.Rmd)
├── project_proposal/            # Project proposal documentation
├── shinyapp/                    # Shiny web application source code
├── .gitignore
├── LICENSE
└── README.md
```

---

## Process Notebook

The full process notebook is published on RPubs:

**[View Process Notebook on RPubs](https://rpubs.com/ranjiraj/covidistress)**

> *Tip: Click "Hide Toolbars" at the bottom-right corner of RPubs for a cleaner reading experience.*

The source code for the notebook is available [here](https://github.com/ranjiGT/Data-Science-with-R-2021/blob/main/process_notebook_final/ultimate-process-notebook.Rmd).

---

## Shiny Web App

An interactive dashboard built with R Shiny:

**[covid-distress-infodemics.shinyapps.io](https://covid-distress-infodemics.shinyapps.io/shinyapp/)**

To run the app locally from RStudio:

```r
library(shiny)
runApp("shinyapp")
```

---

## Screencast

[![Screencast](https://img.youtube.com/vi/b2b1hFEGxa8/maxresdefault.jpg)](https://youtu.be/b2b1hFEGxa8)

---

## Getting Started

### Prerequisites

- [R](https://cran.r-project.org/) (>= 4.0)
- [RStudio](https://posit.co/download/rstudio-desktop/) (recommended)

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/ranjiGT/Data-Science-with-R-2021.git
   cd Data-Science-with-R-2021
   ```

2. Open the `.Rproj` file in RStudio.

3. Install required R packages (listed in individual scripts).

---

## Contributors

This project was developed as a collaborative effort. Thanks to all contributors:

| Name | GitHub |
|------|--------|
| **Ranji Raj** | [@ranjiGT](https://github.com/ranjiGT) |
| **Madhuri Sajith** | [@madhurisajith](https://github.com/madhurisajith) |
| **Vishnu Jayanand** | [@VishnuJayanand](https://github.com/VishnuJayanand) |
| **Usama Ashfaq** | [@aaashfaq](https://github.com/aaashfaq) |
| **Sujith NS** | [@sujithnsudhakar](https://github.com/sujithnsudhakar) |

---

## License

This project is licensed under the [MIT License](LICENSE).
