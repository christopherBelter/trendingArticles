source("getaltmetric_dev2.r")

all_articles <- getAltByTime("1w", num_results = 100, max_results = 500)

all_articles$added_on <- as.Date(as.POSIXct(all_articles$added_on, origin = "1970-01-01"))
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.21203\\/rs\\.", all_articles$doi)] <- "Research Square"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.20944\\/preprints", all_articles$doi)] <- "preprints.org"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("clinicaltrials\\.gov", all_articles$url)] <- "clinicalTrials.gov"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.1101.+\\.\\d{8}", all_articles$doi)] <- "medRxiv"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.5281\\/zenodo", all_articles$doi)] <- "Zenodo"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.13140\\/rg", all_articles$doi)] <- "ResearchGate"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.22541\\/au\\.", all_articles$doi)] <- "Authorea"
all_articles$journal[is.na(all_articles$journal) == TRUE & grepl("10\\.3386\\/w", all_articles$doi)] <- "NBER Working Paper Series"

topic_search <- "pregnan|gestat|maternal|child|kid[^n]|neo[ -]*nat|pre[ -]*nat|ante[ -]*nat|preterm|pediat|infant|toddler|adolescen|young adult|young person|young people|school|famil|birth|menstru|\\b\\d[ -]year[ -]old|\\b1\\d[ -]year[ -]old|aged* [01]|household|fetus|fetal|embryo|lactat|breast[ -]*fe|\\bmother|kawasaki|mis-c|reproduct|pelvi[sc]|contracept|abortion|\\bdobbs\\b|obstetric|gynecolog|disabled|disabil|rehabilita|endometr|sperm|fertil|oocyt|pubert|pubesc|\\b(pre[ -])*teens*\\b|uterus|uterine|down syndrom|intellectual(ly)* disab|fragile x|developmental(ly)* disab"

relevant_articles <- all_articles[grepl(relevant_articles_search, all_articles$title, ignore.case = TRUE) | grepl(relevant_articles_search, all_articles$journal, ignore.case = TRUE) | grepl(relevant_articles_search, all_articles$abstract, ignore.case = TRUE),]
relevant_articles$title <- tm::stripWhitespace(relevant_articles$title)

relevant_articles <- relevant_articles[relevant_articles$added_on >= (Sys.Date() - 7),]

relevant_articles_out <- paste(relevant_articles$title, paste0(relevant_articles$journal, ". Added to altmetric.com on ", relevant_articles$added_on, "."), paste0(relevant_articles$url, "\n"), sep = "\n")
relevant_articles_out <- relevant_articles_out[!grepl("The Conversation|clinicalTrials\\.gov", relevant_articles_out)]
relevant_articles_out <- c(paste0("Trending Articles on NICHD topics: ", as.character(Sys.Date()), "\n"), "This list includes articles and preprints on topics of interest to NICHD that were published in the past 7 days and that were in the top 500 for almetric.com attention score over the past 7 days. These are not necessarily the most important articles on these topics published in the past 7 days, and their content is not necessarily accurate, so their inclusion in this list does not constitute endorsement.\n", relevant_articles_out)
relevant_articles_out <- unique(relevant_articles_out)
relevant_articles_out
writeLines(relevant_articles_out, con = paste0("covid 19/nichd updates/trending_relevant_articles_", gsub("-", "_", as.character(Sys.Date())), ".txt"))
