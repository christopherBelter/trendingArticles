## requires packages httr and jsonlite and plyr

getAltByTime <- function(timeframe, startPage = 1, num_results = 25, max_results = 100, cited_in = NULL, doi_prefix = NULL, order_by = "score") {
	if (!timeframe %in% c("at", "1d", "2d", "3d", "4d", "5d", "6d", "1w", "1m", "3m", "1y")) {
		stop("Invalid timeframe. Valid timeframes are: at, 1d, 2d, 3d, 4d, 5d, 6d, 1w, 1m, 3m, and 1y.")
	}
	if (num_results > 100) {
		stop("Invalid number of results. You can only request up to 100 articles at a time.")
	}
	theData <- list()
	message("Retrieving page ", startPage)
	theURL <- httr::GET(paste0("https://api.altmetric.com/v1/citations/", timeframe), query = list(page = startPage, num_results = num_results, cited_in = cited_in, doi_prefix = doi_prefix, order_by = order_by))
	theData[[1]] <- httr::content(theURL, as = "text")
	if (httr::http_error(theURL) == TRUE) {
		message("HTTP error.")
		print(httr::http_status(theURL))
	}
	theData[[1]] <- jsonlite::fromJSON(theData[[1]])
	theData[[1]] <- theData[[1]]$results
	heads <- httr::headers(theURL)
	hourlyratelimit_total <- heads$`x-hourlyratelimit-limit`
	hourlyratelimit_remaining <- heads$`x-hourlyratelimit-remaining`
	dailyratelimit_total <- heads$`x-dailyratelimit-limit`
	dailyratelimit_remaining <- heads$`x-dailyratelimit-remaining`
	#thePages <- list(theData$results)
	#resultCount <- as.numeric(theData$num_results)
	pagesNeeded <- ceiling(max_results / num_results) # defaults to only retrieve up to max_results. Add logic to get all 
	#message("Total pages needed: ", pagesNeeded)
	Sys.sleep(1)
	# loop for additional pages
	for (i in 2:pagesNeeded) {
		startPage <- i
		message("Retrieving page ", startPage)
		theURL <- httr::GET(paste0("https://api.altmetric.com/v1/citations/", timeframe), query = list(page = startPage, num_results = num_results, cited_in = cited_in, doi_prefix = doi_prefix, order_by = order_by))
		myDat <- httr::content(theURL, as = "text")
		myDat <- jsonlite::fromJSON(myDat)
		theData[[i]] <- myDat$results
		Sys.sleep(1)
	}
	theData <- lapply(theData, jsonlite::flatten)
	#theData <- rbind_pages(thePages)
	theData <- do.call(plyr::rbind.fill, theData)
	message("Finished retrieving documents.")
	message(paste("Total hourly limit:", hourlyratelimit_total, ". Hourly limit remaining:", hourlyratelimit_remaining, "."))
	message(paste("Total daily limit:", dailyratelimit_total, ". Daily limit remaining:", dailyratelimit_remaining, "."))
	return(theData)
}

getAltByID <- function(theIDs, idtype) {
	if (idtype == "aid") {
		baseURL <- "https://api.altmetric.com/v1/id/"
	}
	else if (idtype == "doi") {
		baseURL <- "https://api.altmetric.com/v1/doi/"
	}
	else if (idtype == "pmid") {
		baseURL <- "https://api.altmetric.com/v1/pmid/"
	}
	else if (idtype == "arxiv") {
		baseURL <- "https://api.altmetric.com/v1/arxiv/"
	}
	else if (idtype == "ads") {
		baseURL <- "https://api.altmetric.com/v1/ads/"
	}
	else if (idtype == "uri") {
		baseURL <- "https://api.altmetric.com/v1/uri/"
	}
	else if (idtype == "isbn") {
		baseURL <- "https://api.altmetric.com/v1/isbn/"
	}
	else {
		stop("Invalid idtype value. Valid idtype values are 'aid', 'doi', 'pmid', 'arxiv', 'ads', 'uri', and 'isbn'.")
	}
	theData <- ""
	for (i in 1:length(theIDs)) {
		message(paste("Getting document", i))
		theURL <- httr::GET(paste0(baseURL, theIDs[[i]]))
		theData <- paste(theData, httr::content(theURL, as = "text"), sep = "\n")
		if (theURL$status_code == "404") {
			message("Document not available.")
		}
		#if (httr::http_error(theURL) == TRUE) { 
		#	if (theURL$status_code == 404) {
		#		theData <- NA
		#		return(theData)
		#	} ## note: status code 404 for this API indicates that the requested ID isn't in the system
		#	else {
		#		print("Encountered an HTTP error. Details follow.") 
		#		print(httr::http_status(theURL)) 
		#	}
		#}
		Sys.sleep(1)
	}
	heads <- httr::headers(theURL)
	hourlyratelimit_total <- heads$`x-hourlyratelimit-limit`
	hourlyratelimit_remaining <- heads$`x-hourlyratelimit-remaining`
	dailyratelimit_total <- heads$`x-dailyratelimit-limit`
	dailyratelimit_remaining <- heads$`x-dailyratelimit-remaining`
	message("Finished retrieving documents.")
	message(paste("Total hourly limit:", hourlyratelimit_total, ". Hourly limit remaining:", hourlyratelimit_remaining, "."))
	message(paste("Total daily limit:", dailyratelimit_total, ". Daily limit remaining:", dailyratelimit_remaining, "."))
	return(theData)
}

getAltByUri <- function(theURL) {
	theURL <- httr::GET(paste0("https://api.altmetric.com/v1/uri/", theURL))
	theData <- httr::content(theURL, as = "text")
	if (httr::http_error(theURL) == TRUE) {
		print("Encountered an HTTP error. Details follow.")
		print(httr::http_status(theURL))
	}
	Sys.sleep(1)
	heads <- httr::headers(theURL)
	hourlyratelimit_total <- heads$`x-hourlyratelimit-limit`
	hourlyratelimit_remaining <- heads$`x-hourlyratelimit-remaining`
	dailyratelimit_total <- heads$`x-dailyratelimit-limit`
	dailyratelimit_remaining <- heads$`x-dailyratelimit-remaining`
	message(paste("Total hourly limit:", hourlyratelimit_total, ". Hourly limit remaining:", hourlyratelimit_remaining, "."))
	message(paste("Total daily limit:", dailyratelimit_total, ". Daily limit remaining:", dailyratelimit_remaining, "."))
	return(theData)
}

extractAltmetric <- function(theResults) {
	theData <- unlist(strsplit(theResults, "\n"))
	theData <- theData[!theData == "Not Found"]
	theData <- theData[!theData == ""]
	theData <- gsub("null", "0", theData)
	theData <- lapply(theData, jsonlite::fromJSON)
	dois <- sapply(1:length(theData), function(x) theData[[x]]$doi)
	dois[sapply(dois, is.null) == TRUE] <- NA
	dois <- unlist(dois)
	pmids <- sapply(1:length(theData), function(x) theData[[x]]$pmid)
	pmids[sapply(pmids, is.null) == TRUE] <- NA
	pmids <- unlist(pmids)
	titles <- sapply(1:length(theData), function(x) theData[[x]]$title)
	titles[sapply(titles, is.null) == TRUE] <- NA
	titles <- unlist(titles)
	arxiv_id <- sapply(1:length(theData), function(x) theData[[x]]$arxiv_id)
	arxiv_id[sapply(arxiv_id, is.null) == TRUE] <- NA
	arxiv_id <- unlist(arxiv_id)
	ads_id <- sapply(1:length(theData), function(x) theData[[x]]$ads_id)
	ads_id[sapply(ads_id, is.null) == TRUE] <- NA
	ads_id <- unlist(ads_id)
	#nlmid
	#handles
	#tq
	#isbns
	#altmetric_jid
	#issns
	journals <- sapply(1:length(theData), function(x) theData[[x]]$journal)
	journals[sapply(journals, is.null) == TRUE] <- NA
	journals <- unlist(journals)
	cohorts_public <- sapply(1:length(theData), function(x) theData[[x]]$cohorts$pub)
	cohorts_public[sapply(cohorts_public, is.null) == TRUE] <- NA
	cohorts_public <- unlist(cohorts_public)
	cohorts_practitioners <- sapply(1:length(theData), function(x) theData[[x]]$cohorts$doc)
	cohorts_practitioners[sapply(cohorts_practitioners, is.null) == TRUE] <- NA
	cohorts_practitioners <- unlist(cohorts_practitioners)
	cohorts_scientists <- sapply(1:length(theData), function(x) theData[[x]]$cohorts$sci)
	cohorts_scientists[sapply(cohorts_scientists, is.null) == TRUE] <- NA
	cohorts_scientists <- unlist(cohorts_scientists)
	cohorts_communicators <- sapply(1:length(theData), function(x) theData[[x]]$cohorts$com)
	cohorts_communicators[sapply(cohorts_communicators, is.null) == TRUE] <- NA
	cohorts_communicators <- unlist(cohorts_communicators)
	#cohorts
	abstract <- sapply(1:length(theData), function(x) theData[[x]]$abstract)
	abstract[sapply(abstract, is.null) == TRUE] <- NA
	abstract <- unlist(abstract)
	#context
	context_allPct <- sapply(1:length(theData), function(x) theData[[x]]$context$all$pct)
	context_allPct[sapply(context_allPct, is.null) == TRUE] <- NA
	context_allPct <- unlist(context_allPct)
	context_agePct <- sapply(1:length(theData), function(x) theData[[x]]$context$similar_age_3m$pct)
	context_agePct[sapply(context_agePct, is.null) == TRUE] <- NA
	context_agePct <- unlist(context_agePct)
	context_journalPct <- sapply(1:length(theData), function(x) theData[[x]]$context$journal$pct)
	context_journalPct[sapply(context_journalPct, is.null) == TRUE] <- NA
	context_journalPct <- unlist(context_journalPct)
	context_journalAgePct <- sapply(1:length(theData), function(x) theData[[x]]$context$similar_age_journal_3m$pct)
	context_journalAgePct[sapply(context_journalAgePct, is.null) == TRUE] <- NA
	context_journalAgePct <- unlist(context_journalAgePct)
	authors <- lapply(1:length(theData), function(x) theData[[x]]$authors)
	authors[sapply(authors, is.null) == TRUE] <- NA
	authors <- sapply(authors, paste, collapse = "|")	
	type <- sapply(1:length(theData), function(x) theData[[x]]$type)
	type[sapply(type, is.null) == TRUE] <- NA
	type <- unlist(type)	
	#handles
	#altmetric_id
	#schema
	isOA <- sapply(1:length(theData), function(x) theData[[x]]$is_oa)
	isOA[sapply(isOA, is.null) == TRUE] <- NA
	isOA <- unlist(isOA)
	#publisher_subjects NOTE: publisher subjects return data frames of subjects with the publisher schema used
	cited_by_fbwalls_count <- sapply(1:length(theData), function(x) theData[[x]]$cited_by_fbwalls_count)
	cited_by_fbwalls_count[sapply(cited_by_fbwalls_count, is.null) == TRUE] <- NA
	cited_by_fbwalls_count <- unlist(cited_by_fbwalls_count)
	cited_by_feeds_count <- sapply(1:length(theData), function(x) theData[[x]]$cited_by_feeds_count)
	cited_by_feeds_count[sapply(cited_by_feeds_count, is.null) == TRUE] <- NA
	cited_by_feeds_count <- unlist(cited_by_feeds_count)
	cited_by_gplus_count <- sapply(1:length(theData), function(x) theData[[x]]$cited_by_gplus_count)
	cited_by_gplus_count[sapply(cited_by_gplus_count, is.null) == TRUE] <- NA
	cited_by_gplus_count <- unlist(cited_by_gplus_count)
	cited_by_msm_count <- sapply(1:length(theData), function(x) theData[[x]]$cited_by_msm_count)
	cited_by_msm_count[sapply(cited_by_msm_count, is.null) == TRUE] <- NA
	cited_by_msm_count <- unlist(cited_by_msm_count)
	cited_by_peer_review_sites_count <- sapply(1:length(theData), function(x) theData[[x]]$cited_by_peer_review_sites_count)
	cited_by_peer_review_sites_count[sapply(cited_by_peer_review_sites_count, is.null) == TRUE] <- NA
	cited_by_peer_review_sites_count <- unlist(cited_by_peer_review_sites_count)
	cited_by_policies_count <- sapply(1:length(theData), function(x) theData[[x]]$cited_by_policies_count)
	cited_by_policies_count[sapply(cited_by_policies_count, is.null) == TRUE] <- NA
	cited_by_policies_count <- unlist(cited_by_policies_count)
	cited_by_patents_count <- sapply(1:length(theData), function(x) theData[[x]]$cited_by_patents_count)
	cited_by_patents_count[sapply(cited_by_patents_count, is.null) == TRUE] <- NA
	cited_by_patents_count <- unlist(cited_by_patents_count)
	cited_by_posts_count <- sapply(1:length(theData), function(x) theData[[x]]$cited_by_posts_count)
	cited_by_posts_count[sapply(cited_by_posts_count, is.null) == TRUE] <- NA
	cited_by_posts_count <- unlist(cited_by_posts_count)
	cited_by_rh_count <- sapply(1:length(theData), function(x) theData[[x]]$cited_by_rh_count)
	cited_by_rh_count[sapply(cited_by_rh_count, is.null) == TRUE] <- NA
	cited_by_rh_count <- unlist(cited_by_rh_count)
	cited_by_tweeters_count <- sapply(1:length(theData), function(x) theData[[x]]$cited_by_tweeters_count)
	cited_by_tweeters_count[sapply(cited_by_tweeters_count, is.null) == TRUE] <- NA
	cited_by_tweeters_count <- unlist(cited_by_tweeters_count)
	cited_by_accounts_count <- sapply(1:length(theData), function(x) theData[[x]]$cited_by_accounts_count)
	cited_by_accounts_count[sapply(cited_by_accounts_count, is.null) == TRUE] <- NA
	cited_by_accounts_count <- unlist(cited_by_accounts_count)
	cited_by_wikipedia_count <- sapply(1:length(theData), function(x) theData[[x]]$cited_by_wikipedia_count)
	cited_by_wikipedia_count[sapply(cited_by_wikipedia_count, is.null) == TRUE] <- NA
	cited_by_wikipedia_count <- unlist(cited_by_wikipedia_count)
	last_updated <- sapply(1:length(theData), function(x) theData[[x]]$last_updated)
	last_updated[sapply(last_updated, is.null) == TRUE] <- NA
	last_updated <- unlist(last_updated)
	#last_updated <- as.Date(as.POSIXct(last_updated, origin = "1970-01-01"))	
	score <- sapply(1:length(theData), function(x) theData[[x]]$score)
	score[sapply(score, is.null) == TRUE] <- NA
	score <- unlist(score)	
	#history
	added_on <- sapply(1:length(theData), function(x) theData[[x]]$added_on)
	added_on[sapply(added_on, is.null) == TRUE] <- NA
	added_on <- unlist(added_on)
	#added_on <- as.Date(as.POSIXct(added_on, origin = "1970-01-01"))
	published_on <- sapply(1:length(theData), function(x) theData[[x]]$published_on)
	published_on[sapply(published_on, is.null) == TRUE] <- NA
	#published_on <- as.Date(as.POSIXct(published_on, origin = "1970-01-01"))
	published_on <- unlist(published_on)
	subjects <- sapply(1:length(theData), function(x) theData[[x]]$subjects)
	subjects[sapply(subjects, is.null) == TRUE] <- NA
	subjects <- sapply(subjects, paste, collapse = "|")
	scopus_subjects <- lapply(1:length(theData), function(x) theData[[x]]$scopus_subjects)
	scopus_subjects[sapply(scopus_subjects, is.null) == TRUE] <- NA
	scopus_subjects <- sapply(scopus_subjects, paste, collapse = "|")
	citeulike_readers <- sapply(1:length(theData), function(x) theData[[x]]$readers$citeulike)
	citeulike_readers[sapply(citeulike_readers, is.null) == TRUE] <- NA
	citeulike_readers <- unlist(citeulike_readers)
	mendeley_readers <- sapply(1:length(theData), function(x) theData[[x]]$readers$mendeley)
	mendeley_readers[sapply(mendeley_readers, is.null) == TRUE] <- NA
	mendeley_readers <- unlist(mendeley_readers)
	connotea_readers <- sapply(1:length(theData), function(x) theData[[x]]$readers$connotea)
	connotea_readers[sapply(connotea_readers, is.null) == TRUE] <- NA
	connotea_readers <- unlist(connotea_readers)
	readers_count <- sapply(1:length(theData), function(x) theData[[x]]$readers_count)
	readers_count[sapply(readers_count, is.null) == TRUE] <- NA
	readers_count <- unlist(readers_count)
	#images
	details_url <- sapply(1:length(theData), function(x) theData[[x]]$details_url)
	details_url[sapply(details_url, is.null) == TRUE] <- NA
	details_url <- unlist(details_url)
	theDF <- data.frame(dois, pmids, authors, titles, journals, abstract, type, isOA, cohorts_public, cohorts_practitioners, cohorts_scientists, cohorts_communicators, cited_by_fbwalls_count, cited_by_feeds_count, cited_by_gplus_count, cited_by_msm_count, cited_by_peer_review_sites_count, cited_by_policies_count, cited_by_patents_count, cited_by_posts_count, cited_by_rh_count, cited_by_tweeters_count, cited_by_accounts_count, cited_by_wikipedia_count, last_updated, score, context_allPct, context_agePct, context_journalPct, context_journalAgePct, added_on, published_on, subjects, scopus_subjects, citeulike_readers, mendeley_readers, connotea_readers, readers_count, details_url)
	return(theDF)
}

# usage
## timeData <- getAltByTime("1d")
## timeData ends up being a pretty usable data frame as is, so no need to extract
## idData <- getAltByID(theIDs, idtype = "doi")
## if you want: writeLines(idData, con = "paperData.txt")
## theDF <- extractAltmetric(idData)
## summary(theDF)