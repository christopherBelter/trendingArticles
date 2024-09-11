setwd("./COVID 19")
source("../R/functions/getAltmetric_dev.r")
today <- getAltByTime("2d", num_results = 50)
today$added_on <- as.Date(as.POSIXct(today$added_on, origin = "1970-01-01"))
plyr::count(today$added_on)
today$journal[is.na(today$journal) == TRUE & grepl("10\\.21203\\/rs\\.", today$doi)] <- "Research Square"
today$journal[is.na(today$journal) == TRUE & grepl("clinicaltrials\\.gov", today$url)] <- "clinicalTrials.gov"
today$journal[is.na(today$journal) == TRUE & grepl("10\\.1101.+\\.\\d{8}", today$doi)] <- "medRxiv"
today$journal[is.na(today$journal) == TRUE & grepl("10\\.5281\\/zenodo", today$doi)] <- "Zenodo"
today$journal[is.na(today$journal) == TRUE & grepl("10\\.13140\\/rg", today$doi)] <- "ResearchGate"
today$journal[is.na(today$journal) == TRUE & grepl("10\\.22541\\/au\\.", today$doi)] <- "Authorea"
today$journal[is.na(today$journal) == TRUE & grepl("10\\.3386\\/w", today$doi)] <- "NBER Working Paper Series"
today$journal[is.na(today$journal) == TRUE & grepl("10\\.20944\\/preprints", today$doi)] <- "preprints.org"
altFiles <- list.files(path = "Daily updates/", pattern = "covid.+\\.txt", full.names = TRUE)
alt <- lapply(altFiles, scan, what = "varchar", sep = "\n")
today$isNew <- today$url %in% unlist(alt) == FALSE
today$isNew[today$isNew == TRUE] <- "Yes"
today$isNew[today$isNew == "FALSE"] <- "No"
covid_alt <- today[grepl("covid|ncov|mers|sars|coronavir|BNT162b2|ChAdOx1|mRNA-1273|Ad26.COV2.S|omicron", today$title, ignore.case = TRUE),]
covid_alt <- covid_alt[!grepl("The Conversation", covid_alt$journal),]
plyr::count(covid_alt$added_on)
alt_covid <- paste(covid_alt$title, paste0(covid_alt$journal, ". Added to altmetric.com on ", covid_alt$added_on, ". New to these lists: ", covid_alt$isNew), paste0(covid_alt$url, "\n"), sep = "\n")
alt_covid <- c(paste0("Trending COVID-19 Articles: ", as.character(Sys.Date()), "\n"), paste0("This list includes COVID-19 journal articles and preprints in the top 50 for altmetric attention score over the past two days, according to altmetric.com. These aren't necessarily the most important articles on COVID-19, nor is their content necessarily accurate, so inclusion in this list does not constitute endorsement. These are simply the most discussed articles, mostly on X (formerly Twitter) and in the news, listed here in decreasing order by attention score. -Chris Belter, NICHD Division of Extramural Research.", "\n"), alt_covid)
alt_covid <- gsub("-", "-", alt_covid)
alt_covid[1:5]
readr::write_lines(alt_covid, paste0("covid_highAltmetric_", gsub("-", "_", as.character(Sys.Date())), "_2d.txt"))
today_out <- paste(today$title, paste0(today$journal, ". Added to altmetric.com on ", today$added_on, ". New to these lists: ", today$isNew), paste0(today$url, "\n"), sep = "\n")
readr::write_lines(today_out, paste0("today_highAltmetric_", gsub("-", "_", as.character(Sys.Date())), "_2d.txt"))
tCols <- which(sapply(1:ncol(today), function(x) class(today[,x])) == "list")
today[,tCols] <- sapply(tCols, function(x) sapply(today[,x], paste, collapse = ";"))
write.csv(today, file = paste0("today_highAltmetric_", gsub("-", "_", as.character(Sys.Date())), "_2d.csv"), row.names = FALSE)
plyr::count(covid_alt$isNew)
