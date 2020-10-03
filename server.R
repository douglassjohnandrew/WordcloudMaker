function(input, output, session) {
    
    # Generates modals showing the valid filetypes available palettes, and colors
    observeEvent(input$showFile, {
        showModal(modalDialog(easyClose = TRUE, size="s", title = "Supported File Types", filetypes)) })
    observeEvent(input$showPals, { showModal(modalDialog(easyClose = TRUE, paldisplay)) })
    
    # Reactive value for maxword, uses { if 1 <= input$max <= nrow(dff$activedf) }
    maxword <- reactive({ ifelse(between(input$max, 1, nrow(dff$activedf)), input$max, nrow(dff$activedf)) })
    # Grabs the words from the wchide textbox reactively
    wchide <- reactive({ input$wchide })
    # Reactive value to grab the active dataset (initialized with init/demoFreq in global.R)
    dff <- reactiveValues(activedf=init)
    # Reactive value to store the extension
    ext <- reactive({ ifelse(is.null(file_ext(input$file$name)),"NA",file_ext(input$file$name)) })
    # Outputs the summary tables of frequencies and freqgroups
    output$summary <- renderTable( reactSummary(), digits=0 )
    output$freqgroup <- renderTable( reactGroupFreq() )
    # Dynamically renders the wordcloud
    observeEvent(c(paldf, input$adjust, input$refreshWC), { output$wordcloud2 <- renderWordcloud2({ reactWC() }) })
    
    # Reactive value to filter the dataset by the words in input$wchide
    dfh <- reactive({
        req(input$nrange)
        if (input$nrange=="All") {nval <- unique(dff$activedf$wordcount)}
            else nval <- as.integer(input$nrange)
        
        # Do the filtering :)
        subset(dff$activedf, (!(tolower(dff$activedf$term) %in%
            unlist(strsplit(tolower(wchide()), split=" "))) &
            dff$activedf$wordcount %in% nval))
    })
    
    # Reactive value to generate a summary table of dff$activedf
    reactSummary <- reactive({
        rough <- summary(dff$activedf$freq) %>% unclass() %>% as.data.frame()
        rough <- cbind(rownames(rough), data.frame(rough, row.names=NULL)) %>%
            setNames(c("Statistic","FreqValue"))
        rough
    })
    
    # Reactive value that creates a table, grouping by freq
    reactGroupFreq <- reactive({ table(dff$activedf$wordcount) %>%
        as.data.frame() %>% setNames(c("WordCount","TotalTerms")) })

    # Outputs a datatable of the active dataframe
    output$mytable <- DT::renderDT(
        dff$activedf, filter="top", rownames = FALSE, options = list(
            columnDefs = list(list(className = 'dt-center', targets="_all")),
            lengthChange = FALSE, pageLength = 7, pagingType="full",
            sDom  = '<"top">lrt<"bottom">ip',
            language = list(
                info="Showing _START_ to _END_ of _TOTAL_",
                infoEmpty="Showing 0 to 0 of 0",
                infoFiltered="(filtered from _MAX_ total)",
                paginate = list(previous="Prev")
            )
        )
    )
    
    # Creates the wordcloud. First assigns color
    reactWC <- reactive({
        dfhide <- dfh()
        mycolor <- switch(input$scheme,
            'Foreground'= input$fore,
            'Palette'= rep(brewer.pal(input$numpal, input$pals),
                nrow(dff$activedf)/input$numpal+1),
            'Random-Dark'= 'random-dark',
            'Random-Light'= 'random-light')
        wordcloud2(data= head(dfhide,maxword()),
            size=input$size, shape=input$shape, gridSize = input$grid,
            color = mycolor, backgroundColor=input$back, ellipticity=input$ellip,
            fontWeight = input$bold, rotateRatio = input$rotate*0.01)
    })
    
    # Stores csv, sas7dbat, and tsv files in an eventReactive variable.
    # Used in uploadui, NOT to create dff$activedf
    temptbl <- eventReactive(input$file, {
        filepath <- input$file$datapath
        ext <- ext()
        if (is.null(filepath)||is.null(ext)) { return(NULL) }
        
        if (ext %in% c("csv","sas7bdat","tsv")) { switch(ext,
            "csv"= read_csv(filepath),
            "sas7bdat"= read_sas(filepath),
            "tsv"= read_tsv(filepath)
        )}
    })
    
    # Output the slider to show the number of colors available per palette
    output$colorcount <- renderUI({
        sliderInput('numpal', 'Total palette colors',
            min=3, max=as.integer(paldf$numcolor[paldf$palette==input$pals]),
            value=3, step=1, ticks = FALSE)
    })
    
    # Output the slider to show certain ngrams in the word cloud
    output$wcngrams <- renderUI({
        wc <- reactSummary()
        vec <- c("All",sort(unique(dff$activedf$wordcount)))
        sliderTextInput("nrange","Specify which n-grams should appear in the cloud",
            choices=vec, selected="All")
    })
    
    # Renders different UIs if the file is unstructured text, image, sturctured, Excel, or not supported.
    output$uploadui <- renderUI({
        filepath <- input$file$datapath
        ext <- ext()
        if (is.null(filepath)||is.null(ext)) { return(NULL) }
        
        # Gets a list of Excel sheets and columns
        if (ext %in% c("xls","xlsx")) {
            xlsheets <- list()
            for (i in excel_sheets(filepath)) {
                xlsheets[[i]] <- colnames(read_excel(filepath, sheet=i, range=cell_rows(1)))
            }
        } # Or gets temptbl() if need be
        if (ext %in% c("csv","sas7bdat","tsv")) { tbl <- temptbl() }
        
        switch(ext,
            "doc"=,"docx"=,"pdf"=,"rtf"=,"txt"= tagList(
                p("Retrieves the text in the document and processes using NLP."),
                actionButton("processDoc","Process Data")
            ),
            "csv"=,"sas7bdat"=,"tsv"= tagList(
                selectInput("tblcol", label="Select a column", selectize=FALSE,
                    choices=colnames(tbl)),
                actionButton("processTbl","Process Data")
            ),
            "xls"=,"xlsx"= tagList(
                selectInput('sheet', label='Select a sheet', selectize=FALSE,
                    choices = names(xlsheets)),
                selectInput('xlscol', label="Select a column", choices=xlsheets),
                actionButton("processXls","Process Data")
            ),
            p("File type not supported :(")
        )
    })
    
    # Functionality for the "Use Sample Data" button
    observeEvent(input$revertInit, {
        dff$activedf <- init
        shinyalert(title="Note", text = "Default dataset is now active",
            closeOnClickOutside = TRUE)
    })

    # Download a table of frequencies and ngrams in CSV, RDS, TSV, or XLSX
    output$savedf <- downloadHandler(
        filename <- function() {
            paste("frequencies.",input$dftype, sep = "") },
        content <- function(file) {
            switch(input$dftype,
                'csv' = write_csv(dff$activedf, file),
                'rds' = write_rds(dff$activedf, file),
                'tsv' = write_tsv(dff$activedf, file),
                'xlsx' = write_xlsx(dff$activedf, file)
            )
        }
    )
    
    # Process when clicking processDoc button
    observeEvent(input$processDoc, {
        tryCatch ({
            filepath <- input$file$datapath
            ext <- ext()
            if (is.null(filepath)||is.null(ext)) { return(NULL) }
            
            text <- read_document(filepath, remove.empty=TRUE, combine=TRUE, format=FALSE)
            dff$activedf <- NLP(text, input$marker, input$importdel,
                input$lettdel, input$stemlem, input$collsize)
                
        },  error=function(e) {
            shinyalert(type="error", title="Error", closeOnClickOutside = TRUE,
                text = paste(toupper(ext)," data import failed")
            )
        })
    })
    
    # Process when clicking processTbl button
    observeEvent(input$processTbl, {
        tryCatch({
            filepath <- input$file$datapath
            ext <- ext()
            if (is.null(filepath)||is.null(ext)) { return(NULL) }
            
            tbl <- temptbl()
            tbl <- tbl %>% subset(select = input$tblcol) %>% setNames('temp')
            
            dff$activedf <- PickMethod(input$colprocess, tbl$temp, input$marker,
                input$importdel, input$lettdel, input$stemlem, input$collsize)
        },  error=function(e) {
            shinyalert(type="error", title="Error", closeOnClickOutside = TRUE,
                text = paste(toupper(ext)," data import failed")
            )
        })
    })

    # Process when clicking processXls button
    observeEvent(input$processXls, {
        tryCatch({
            filepath <- input$file$datapath
            ext <- ext()
            if (is.null(filepath)||is.null(ext)) { return(NULL) }
            
            tbl <- read_excel(filepath, sheet=input$sheet) %>%
                subset(select = input$xlscol) %>% setNames('temp')
            
            dff$activedf <- PickMethod(input$colprocess, tbl$temp, input$marker,
                input$importdel, input$lettdel, input$stemlem, input$collsize)
        },  error=function(e) {
            shinyalert(type="error", title="Error", closeOnClickOutside = TRUE,
                text = "Excel data import failed"
            )
        })
    })
    
    # Process when clicking processWeb button
    observeEvent(input$processWeb, {
        tryCatch({
            webpage <- xml2::read_html(input$url)
            col <- switch(input$selectype,
                'CSS'= webpage %>% html_nodes(css = input$selector) %>% html_text(),
                'XPath'= webpage %>% html_nodes(xpath = input$selector) %>% html_text()
            )
            dff$activedf <- PickMethod(input$colprocess, col, input$marker,
                input$importdel, input$lettdel, input$stemlem, input$collsize)
        },  error=function(e) {
            shinyalert(type="error", title="Error", closeOnClickOutside = TRUE,
                text = "Either the URL is incorrect, the selector is incorrect,
                    or the website is down"
            )
        })
    })
    
    # Process when clicking the Reset WC settings button
    observeEvent(input$resetWC, {
        updateRadioButtons(session,'scheme',selected='Foreground')
        colourpicker::updateColourInput(session,'fore',value='black')
        colourpicker::updateColourInput(session,'back',value='lightgray')
        updateSelectInput(session,'pals',selected='BrBG')
        updateSliderInput(session,'numpal',value=3)
        updateSelectInput(session,'shape',selected='circle')
        updateSliderInput(session,'ellip',value=1)
        updateSliderTextInput(session,'nrange',selected="All")
        updateTextInput(session,'wchide',value='')
        updateSliderInput(session,'max',value=55)
        updateSliderInput(session,'size',value=1)
        updateSliderInput(session,'grid',value=15)
        updateSliderInput(session,'bold',value=500)
        updateSliderInput(session,'rotate',value=50)
        shinyalert(title="Note", text = "Default wordcloud settings have been restored",
            closeOnClickOutside = TRUE)
    })
    
    # Process when clicking optreset button
    observeEvent(input$optreset, {
        updateTextInput(session,'importdel',value='')
        updateRadioButtons(session,'marker',selected='Underscores')
        updateRadioButtons(session,'colprocess',
            selected='As categorical data (factors)')
        updateRadioButtons(session,'collsize',selected=2)
        updateRadioButtons(session,'lettdel',selected=3)
        updateRadioButtons(session,'stemlem',selected='No')
        shinyalert(title="Note", text = "General and NLP defaults have been restored",
            closeOnClickOutside = TRUE)
    })
    
    # observeEvents for reverting color scheme radio based on other inputs
    observeEvent(input$fore, { updateRadioButtons(session,'scheme',selected="Foreground") }, ignoreInit=TRUE)
    observeEvent(c(input$numpal, input$pals), { updateRadioButtons(session,'scheme',selected="Palette") }, ignoreInit=TRUE)
}