# Load functions into global environment
source("functions.R")

# All necessary packages
require("bsplus")
require("colourpicker")
require("corpus")
require("dplyr")
require("DT")
require("haven")
require("quanteda")
require("RColorBrewer")
require("readr")
require("readxl")
require("rvest")
require("shiny")
require("shinyalert")
require("shinyjs")
require("shinyWidgets")
require("stringr")
require("textreadr")
require("textstem")
require("tm")
require("tools")
require("wordcloud2")
require("writexl")

# Initialize the dataframe as demoFreq from wordcloud2. Changing column
# types is necessary to get the DT filters to work correctly
init <- read_excel("data/frequency.xlsx", sheet="frequency")
init$freq <- init$freq %>% as.character() %>% as.integer()
init <- cbind(init, rep(1,nrow(init))) %>% setNames(c("term","freq","wordcount"))
init$wordcount <- init$wordcount %>% as.character() %>% as.integer()
paldf <- read_excel("data/palettes.xlsx", sheet="palettes")

titles <- c(
    "How can I save and copy the word clouds I create?",
    "How was this app created?",
    "How does this app use NLP?",
    "What R packages are used in this app?"
)

contents <- list(
    span("All you need to do is right click on the word cloud. From there, you can choose either ",
        strong("Save image as"),' or ',strong("Copy image")),
    span("This app was developed using Shiny and containerized using Docker.
        The container is currently hosted on Azure."),
    tagList(
        p("This app cleans the text by lowercasing, removing stopwords, and removing punctuation,
            in that order."),
        span("The app also lets users choose the n-grams they want, and if words should be
             stemmed or lemmatized.")
    ),
    span(strong("In alphabetical order: "), "bsplus, colourpicker, corpus, dplyr, DT, haven,
         quanteda, RColorBrewer, readr, readxl, rvest, shiny, shinyalert, shinyjs,
         shinyWidgets, stringr, textreadr, textstem, tm, tools, wordcloud2, writexl")
)

accord <- bs_accordion(id="help")
for (i in 1:length(titles)) {
    accord <- accord %>% bs_set_opts(use_heading_link = TRUE) %>%
        bs_append(title = titles[i], content = contents[i])
}

struct <- c("csv","sas7bdat","tsv","xls","xlsx")
unstructdoc <- c("doc","docx","pdf","rtf","txt")
filetypes <- tagList(
    p(strong("Structured: "),paste(struct,collapse=", ")),
    p(strong("Plain text: "),paste(unstructdoc,collapse=", "))
)

paldisplay <- tagList(
    tabsetPanel(type="tabs", selected="About Palettes",
        tabPanel('About Palettes', br(),
            p("Palette images are from ", a("datanovia.com", target="_blank",
                href="https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/")),
            p("Also see RColorBrewer's ", a("documentation", target="_blank",
                href="https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf"))
        ),
        tabPanel('All Palettes',
            tags$img(alt="All available palettes", src = "all_palettes.png")
        ),
        tabPanel('Colorblind Friendly',
            tags$img(alt="Colorblind friendly palettes", src = "colorblind_palettes.png")
        )
    )
)