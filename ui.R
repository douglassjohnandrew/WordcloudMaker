fluidPage(style="margin-top: 1em" , title = "Wordcloud Maker",
    
    # Turn off the hover wordcount behavior and track window resizes
    tags$head(
        tags$style(HTML('
            div#wcLabel {display: none;}
        ')),
        tags$script(HTML('
            $(document).ready(function() {
                $("#refreshWC").click(function(){
                    $("#canvas").attr("width",$("#wordcloud2").width()).attr("height",$("#wordcloud2").height());
                    Shiny.setInputValue("adjust", 420,{priority: "event"});
                }); 
            });
        '))
    ),
    
    # Allows the UI to use shinyalerts and shinyjs
    useShinyalert(),
    sidebarLayout(
        
        sidebarPanel(
            
            # The tabset that organizes the web app
            tabsetPanel(type="tabs", selected = "Edit Wordcloud",
                
                # The tab that lets the user change the wordcloud's appearance
                tabPanel("Edit Wordcloud", br(),
                    
                    tabsetPanel(type="pills", selected = "Page 1",
                    
                        # Tab panel for Page 1 of WC options
                        tabPanel('Page 1', br(),
                        
                            # Choose a color scheme
                            radioButtons('scheme', span('Color scheme ', a(strong("(see colors)"),
                                target="_blank", href="r_colors.pdf")), inline=TRUE,
                                choices=c('Foreground', 'Palette', 'Random-Dark','Random-Light')),
                            
                            # Choose foreground and background color
                            fluidRow(style="margin-top: 25px",
                                column(width=6,
                                    colourpicker::colourInput('fore','Foreground color','black',
                                        returnName = TRUE, allowTransparent = TRUE)),
                                column(width=6,
                                    colourpicker::colourInput('back','Background color','lightgray',
                                        returnName = TRUE, allowTransparent = TRUE))
                            ),
                            
                            # Choose palette specifications
                            fluidRow(
                                column(width=6,
                                    selectInput('pals',
                                        span("Palette to use ", actionLink("showPals", "(see palettes)")),
                                        choices = list(
                                            Diverging = paldf$palette[paldf$type=="Diverging"],
                                            Qualitative = paldf$palette[paldf$type=="Qualitative"],
                                            Sequential = paldf$palette[paldf$type=="Sequential"]
                                        )
                                    )
                                ),
                                column(width=6, uiOutput('colorcount'))
                            ),
                            
                            # Choose things pertaining to the shape
                            fluidRow(
                                column(width=6,
                                    selectInput('shape', 'Shape to use', selectize = FALSE,
                                        multiple = FALSE, choices = c('circle', 'diamond',
                                        'pentagon', 'star', 'triangle', 'triangle-forward'))),
                                column(width=6,
                                    sliderInput('ellip', 'Shape flatness',
                                        min=0.5, max=1.5, value=1, step=0.1, ticks = FALSE))
                            ),
                            
                            # Change which terms and wordcounts appear
                            fluidRow(
                                column(width=6, textInput("wchide",
                                    "Terms to hide from the cloud, separated by spaces")),
                                column(width=6, uiOutput('wcngrams'))
                            )
                        ),
                        
                        # Tab panel for Page 2 of WC options
                        tabPanel('Page 2', br(),
                            # Slider inputs to control size, space between words, max words, font-weight, and rotations
                            sliderInput('max', 'Maximum number of words', min=10, max=100, value=55, step=1, ticks = FALSE),
                            sliderInput('size', 'Font size to use', min=0.5, max=1.5, value=1, step=0.1, ticks = FALSE),
                            sliderInput('grid', 'Space between words', min=0, max=30, value=15, step=5, ticks = FALSE),
                            sliderInput('bold', 'Font weight (400 = normal, 700 = bold)',
                                min=100, max=900, value=500, step=100, ticks = FALSE),
                            sliderInput('rotate', 'Percentage of words to rotate',
                                min=0, max=100, value=50, step=5, ticks = FALSE)
                        )
                    ),
                    p("Refresh & resize wordcloud", actionButton("refreshWC","Refresh", style="margin-left: 14px")),
                    span("Reset wordcloud settings", actionButton("resetWC", "Reset", style="margin-left: 28px"))
                ),
                
                # The tab that lets the user import data
                tabPanel('Import Data', br(),
                    
                    strong("Where is your data coming from?"),
                    br(), br(),
                    
                    # The sub-tabset that determines where the data comes from
                    tabsetPanel(type="pills", selected = "File",
                    
                        # Tab panel for file upload
                        tabPanel('File', br(),
                            fileInput("file", multiple = FALSE, label=p("Upload a file ",
                                actionLink("showFile", "(see supported filetypes)"))),
                            uiOutput("uploadui")
                        ),
                        
                        # Tab panel for web scraping
                        tabPanel('Web Scraping', br(),
                            textInput("url", label = "Enter a URL to scrape (absolute)"),
                            textInput("selector", label = "CSS or XPath selector to scrape
                                (relative or absolute)"),
                            radioButtons("selectype", label="What kind of selector are you using?",
                                choices=c("CSS","XPath"), selected="XPath", inline=TRUE),
                            actionButton("processWeb","Process Data")
                        ),
                        
                        # Tab panel for the sample data
                        tabPanel('Options', br(),
                            
                            tabsetPanel(type="tabs", selected = "General",
                            
                                # Tab panel for general options
                                tabPanel('General', br(),
                                    textInput("importdel",label="Words to delete when importing data,
                                        separated by spaces"),
                                    radioButtons('marker', 'What marker do you want to use for n-grams > 1?',
                                        selected = "Underscores", choices = c("Quotes","Spaces","Underscores")),
                                    radioButtons('colprocess', 'How should columns be processed?',
                                        choices=c("As categorical data (factors)",
                                            "As concatenated text using NLP"))
                                ),

                                # Tab panel for NLP-specific options
                                tabPanel('NLP', br(),
                                    radioButtons('collsize', 'Generate n-grams up to N =', selected = 2,
                                        choices = c(1:4), inline=TRUE),
                                    radioButtons('lettdel', choices = c(1:4), selected=3, inline=TRUE,
                                        label='Delete words that have this many letters or fewer'),
                                    radioButtons('stemlem', 'Do you want to remove word endings?',
                                        choices = c('No','Stem words','Lemmatize words'), selected="No")
                                ),
                                
                                # Tab panel to reset the wordcloud's setting to their defaults
                                tabPanel('Defaults', br(),
                                    p("Reset General and NLP options to their defaults",
                                        actionButton("optreset", "Reset", style="margin-left: 14px")),
                                    span("Use default data from", a("wordfrequency.info",href="https://www.wordfrequency.info/"),
                                        actionButton("revertInit", "Sample Data", style="margin-left: 14px")
                                    )
                                )
                            )
                        )
                    )
                ),
                
                # The tab that lets the viewer see current datasets / ngrams and download them
                tabPanel("View Data", br(),
                    
                    tabsetPanel(type="pills", selected = "Terms",
                        
                        # Displays a Javascript DT of frequencies and ngrams
                        tabPanel('Terms', br(),
                            
                            # Outputs the table :)
                            fluidRow(column(width=12, DT::DTOutput("mytable"))),
                            fluidRow(
                                column(width=4, p(strong("Save terms as"), style="margin-top: 27px")),
                                column(width=4, selectInput("dftype", '', selectize = FALSE,
                                    multiple = FALSE, choices = c("csv","rds","tsv","xlsx"))),
                                column(width=4, downloadButton("savedf", "Download",
                                    style="margin-top: 20px"))
                            )
                        ),
                        
                        # Displays a summary renderTable of the active dataset
                        tabPanel('Summary', br(),
                            
                            # Outputs the renderTables :)
                            p(strong("Summary tables based on the Terms table")), br(),
                            fluidRow(
                                column(width=6, p('5-number summary of "freq" column')),
                                column(width=6, p('Grouping by the "wordcount" column'))
                            ),
                            fluidRow(
                                column(width=6, tableOutput('summary')),
                                column(width=6, tableOutput('freqgroup'))
                            )
                        )
                    )
                ),
                
                # Shows the About tab with help stuff
                tabPanel("About", br(),
                    h4("Created by ", a("Andrew Douglass", target="_blank",
                        href="https://www.linkedin.com/in/andrew-douglass/")), br(), accord
                )
            )
        ),
        
        # Displays the wordcloud2
        mainPanel(wordcloud2Output("wordcloud2", height="96vh"))
    )
)