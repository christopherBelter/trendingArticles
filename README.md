# trendingArticles
R code for generating trending article lists using the altmetric.com API

## Overview
This repo contains code and instructions for generating lists of articles on specific topics that have been trending in social media and news outlets over a specified period of time. The code uses the  "Citations" enpoint of the [Altmetric Details Page API](https://api.altmetric.com/index.html) from [altmetric.com](https://www.altmetric.com/) to obtain a specified number of articles with the highest altmetric attention scores over a specified timeframe and then filters the resulting list to retain articles on a specific topic. 
The repo contains two .r files: getAltmetric_dev2.r and trending_articles.r. The getAltmetric_dev2.r file provides custom functions for working with the Details API and the trending_articles.r file contains the code for using the functions in an R session to obtain the relevant articles.
...
