# library loaders
# may add to the list if required

packages <- c(
  "dplyr",
  "foreign",
  "qgraph",
  "ggplot2",
  "haven",  
  "RColorBrewer",
  "stringr",
  "performance",
  "ggpubr",
  "stringr",
  "patchwork",
  "plotly",
  "ggthemes",
  "wordcloud2",
  "lavaan",
  "reshape2",
  "tidyr",
  "lubridate",
  "readr",
  "lmerTest",
  "lme4",
  "twitteR",
  "tidytext",
  "wordcloud",
  "RColorBrewer",
  "SentimentAnalysis",
  "wordcloud2",
  "scales",
  "sm"
)

verify.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = 
                       TRUE)
  sapply(pkg, library, character.only = TRUE)
}

verify.packages(packages)
