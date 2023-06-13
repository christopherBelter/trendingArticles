# trendingArticles
R code for generating trending article lists using the altmetric.com API

## Overview
This repo contains code and instructions for generating lists of articles on specific topics that have been trending in social media and news outlets over a specified period of time. The code uses the  "Citations" enpoint of the [Altmetric Details Page API](https://api.altmetric.com/index.html) from [altmetric.com](https://www.altmetric.com/) to obtain a specified number of articles with the highest altmetric attention scores over a specified timeframe and then filters the resulting list to retain articles on a specific topic. 

The repo contains two .r files: `getAltmetric_dev2.r` and `trending_articles.r`. The `getAltmetric_dev2.r` file provides custom functions for working with the altmetric details API and the `trending_articles.r` file contains the code for using the functions in an R session to obtain the relevant articles. To use them, download the `getAltmetric_dev2.r` file to your local computer and then run the code in the `trending_articles.r` file to retrieve the articles. You can also modify the `trending_articles.r` code to change parameters like the timeframe, number of articles to retrieve, and  `topic_search` regular expression.

Please note before using this code that altmetric attention scores can be influenced by many factors besides the "quality" of an article and should therefore be used with caution. 

More details about how the code works are below.

## Annotated code
In this vignette, I will walk through the code to identify articles published in the past week on topics of interest to NICHD that are in the top 500 for altmetric attention score over the past 7 days.

First, we load the `getAltmetric_dev2.r` file with `source()`. If you saved the file to a directory other than your current working directory, change the file path accordingly.
```r
source("getaltmetric_dev2.r")
```
Next, use the `getAltByTime()` function to retrieve the top 500 articles by attention score over the past week. We can change the number of articles to retrieve by changing the `max_results` attribute and we can change the timeframe over which the scores are generated by changing the `timeframe` attribute. The function will then return a data frame of the requested articles.
```r
all_articles <- getAltByTime("1w", num_results = 100, max_results = 500)
```
Next, we clean up the article records a bit by changing the `added_on` date column to an R-friendly Date format and add the names of poular preprint servers to the `journal` column based on patterns in the DOI. 
```r
all_articles$added_on <- as.Date(as.POSIXct(all_articles$added_on, origin = "1970-01-01"))
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.21203\\/rs\\.", all_articles$doi)] <- "Research Square"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.20944\\/preprints", all_articles$doi)] <- "preprints.org"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("clinicaltrials\\.gov", all_articles$url)] <- "clinicalTrials.gov"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.1101.+\\.\\d{8}", all_articles$doi)] <- "medRxiv"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.5281\\/zenodo", all_articles$doi)] <- "Zenodo"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.13140\\/rg", all_articles$doi)] <- "ResearchGate"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.22541\\/au\\.", all_articles$doi)] <- "Authorea"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.3386\\/w", all_articles$doi)] <- "NBER Working Paper Series"
```
Next, we save the regular expression that we will use to filter the list of all articles to articles on topics of interest. This sting identifies articles on the topics of reproductive health, pregnancy, pediatrics, and intellectual, physical, and developmental disabilities, but the string can be changed to identify articles on other topics.
```r
topic_search <- "pregnan|gestat|maternal|child|kid[^n]|neo[ -]*nat|pre[ -]*nat|ante[ -]*nat|preterm|pediat|infant|toddler|adolescen|young adult|young person|young people|school|famil|birth|menstru|\\b\\d[ -]year[ -]old|\\b1\\d[ -]year[ -]old|aged* [01]|household|fetus|fetal|embryo|lactat|breast[ -]*fe|\\bmother|kawasaki|mis-c|reproduct|pelvi[sc]|contracept|abortion|\\bdobbs\\b|obstetric|gynecolog|disabled|disabil|rehabilita|endometr|sperm|fertil|oocyt|pubert|pubesc|\\b(pre[ -])*teens*\\b|uterus|uterine|down syndrom|intellectual(ly)* disab|fragile x|developmental(ly)* disab"
```
Next, we run the `topic_search` regular expression on the titles, journals, and abstracts (if available) and return articles with a match in at least one of those places. We also remove any extra whitespace in the journal titles for consistent formatting later on.
```r
relevant_articles <- all_articles[grepl(relevant_articles_search, all_articles$title, ignore.case = TRUE) | grepl(relevant_articles_search, all_articles$journal, ignore.case = TRUE) | grepl(relevant_articles_search, all_articles$abstract, ignore.case = TRUE),]
relevant_articles$title <- tm::stripWhitespace(relevant_articles$title)
```
Next, we filter the `relevant_articles` data frame to only retain articles added to the altmetric.com database in the past 7 days. Articles can spike in attention score well after their initial publication date, so this step removes such articles from the list. But we could skip it if we wanted to retain everything. 
```r
relevant_articles <- relevant_articles[relevant_articles$added_on >= (Sys.Date() - 7),]
```
Finally, we reformat the data frame to create and save a text file that can be copy/pasted into an email or Word document. We also remove items published in The Conversation and trial records from clinicaltrials.gov that also frequently appear in these lists.
```r
relevant_articles_out <- paste(relevant_articles$title, paste0(relevant_articles$journal, ". Added to altmetric.com on ", relevant_articles$added_on, "."), paste0(relevant_articles$url, "\n"), sep = "\n")
relevant_articles_out <- relevant_articles_out[!grepl("The Conversation|clinicalTrials\\.gov", relevant_articles_out)]
relevant_articles_out <- c(paste0("Trending Articles on NICHD topics: ", as.character(Sys.Date()), "\n"), "This list includes articles and preprints on topics of interest to NICHD that were published in the past 7 days and that were in the top 500 for almetric.com attention score over the past 7 days. These are not necessarily the most important articles on these topics published in the past 7 days, and their content is not necessarily accurate, so their inclusion in this list does not constitute endorsement.\n", relevant_articles_out)
relevant_articles_out <- unique(relevant_articles_out)
relevant_articles_out
writeLines(relevant_articles_out, con = paste0("trending_relevant_articles_", gsub("-", "_", as.character(Sys.Date())), ".txt"))
```
