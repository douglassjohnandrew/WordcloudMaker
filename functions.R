PickMethod <- function(process, mycol, marker, importdel, lettdel, stemlem, collsize) {
    return(
        switch(process,
            "As categorical data (factors)"= Categorical(mycol, marker, importdel),
            "As concatenated text using NLP"= NLP(paste(as.vector(mycol), collapse=" "),
                marker, importdel, lettdel, stemlem, collsize)
        )
    )
}

# Function that treats data as a column when processing
Categorical <- function(mycol, marker, importdel) {
    
    # Create a frequency table. mycol represents tbl$temp from server.R
    tbl <- setNames(as.data.frame(table(mycol),
        stringsAsFactors=FALSE),c('term','freq'))
    tbl <- subset(tbl, !(tolower(term) %in%
        unlist(strsplit(tolower(importdel), split=" "))
    ))
    
    # Create a wordcount column
    tbl$wordcount <- str_count(tbl$term, "\\s+")+1
    
    # Use quotes or underscores to indicate each ngram. Skip if "Spaces" is chosen
    if (marker=="Quotes") {
        tbl$term <- paste('"',tbl$term,'"',sep = "")
        tbl$term <- ifelse(tbl$wordcount == 1, gsub('"','',tbl$term), tbl$term)
    } else if (marker=="Underscores") tbl$term <- gsub(' ','_',tbl$term)
    
    # Change column types so that DT package is happy
    tbl$freq <- as.integer(as.character(tbl$freq))
    tbl$wordcount <- as.integer(as.character(tbl$wordcount))
    
    # Show a modal indicating that the import was succcessful
    shinyalert(type="success", title="Success", closeOnClickOutside = TRUE,
        text = "Data successfully imported as a column")
    
    # Sort descending
    return(tbl[order(tbl$freq, decreasing = TRUE),])
}

# Function that transforms text using NLP
NLP <- function(text, marker, importdel, lettdel, stemlem, collsize) {
    
    # Sets all text to lowercase and removes stopwords
    text <- gsub('[^[:lower:]]',' ',tolower(text))
    text <- removeWords(text, tm::stopwords("en"))
    
    # Stems or lemmatizes words depending on what the user specifies
    if (stemlem != "No") {
        strvec <- unlist(strsplit(text, split=" "))
        text <- switch(stemlem,
            "Stem words"= paste(stem_words(strvec), collapse=" "),
            "Lemmatize words"= paste(lemmatize_words(strvec), collapse=" ")
        )
    }
    
    # Continue cleaning the text
    text <- removeWords(text, unlist(strsplit(tolower(importdel), split=" ")))
    text <- gsub(sprintf(" *\\b[[:alpha:]]{1,%s}\\b *",lettdel),' ', text)
    text <- gsub('[[:blank:]]',' ', text)
    
    # Create 2 corpora, one for the DTM and one for ngram Collocations
    cdoc <- corpus(text)
    ddoc <- Corpus(VectorSource(text))
    
    # Creates a dataframe of all unigrams (individual words)
    mtx <- as.matrix(TermDocumentMatrix(ddoc))
    uni <- setNames(subset(as.data.frame(as.table(mtx)), select=c(Terms, Freq)),c("term", "freq"))
    uni$wordcount <- as.integer(as.character(rep(1,nrow(uni))))
    
    # Create a collocation of all ngrams
    if (collsize > 1) {
        colls <- setNames(subset(textstat_collocations(cdoc, method="lambda",
            size=2:collsize), select=c(collocation,count)), c("term", "freq"))
        colls$wordcount <- str_count(colls$term, "\\s+")+1
    } else colls <- data.frame()
    
    # Use quotes or underscores to indicate each ngram where n > 1. Skip if no ngrams are > 1
    if (nrow(colls) > 0) {
        colls$term <- switch(marker,
            "Quotes" = paste('"',colls$term,'"',sep = ""),
            "Spaces" = colls$term,
            "Underscores" = gsub(' ','_',colls$term)
        )
    }
    
    # Combine colls and uni into a new dataframe. Then add a wordcount column
    combine <- rbind(colls,uni)
    
    # Change column types so that DT package is happy
    combine$freq <- as.integer(as.character(combine$freq))
    combine$wordcount <- as.integer(as.character(combine$wordcount))
    
    # Show a modal indicating that the import was succcessful
    shinyalert(type="success", title="Success", closeOnClickOutside = TRUE,
        text = "Data successfully imported using NLP")
    
    # Sort descending
    return(combine[order(combine$freq, decreasing = TRUE),])
}