
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(reshape2)
library(lubridate)
library(knitr)
library(plotly)
library(ggpubr)
library(grid)
library(gridExtra)
library(topicmodels)

### Load predefines model(lda) from file

        load("tfcc_lda_40.RData")  ### results in abstract_lda
        abstracts <- read.csv("abstracts.csv")
        first_author <- read.csv("first_author.csv")
### tidy it
        tidy_lda <- tidy(abstr_lda)
## top_terms for each topic
   
        top_terms <- tidy_lda %>% 
                group_by(topic) %>%
                filter(!is.na(term)) %>% 
                top_n(10,beta) %>% 
                ungroup() %>% 
                arrange(topic,-beta)
        
## calculate gamma and related papers
        #Extract gamma , arrange by topic and gamma and extract top 10 gamma for each topic 
        
       
        lda_gamma_topic <- tidy(abstr_lda,matrix="gamma") %>% arrange(topic,desc(gamma)) %>% group_by(topic) %>% top_n(50,gamma)
        lda_gamma_topic$document <- as.integer(lda_gamma_topic$document)  # change for joining
      
        ## join with pm_data to reveal title etc.
        
        topic_joined <- left_join(lda_gamma_topic,abstracts, by=c("document"="DOI")) %>% select(-abstract)
        topic_joined <- left_join(topic_joined,first_author, by=c("document"="L1"))
        topic_joined <- topic_joined %>% unite("Name",LastName,value,sep=" ") %>% select(document,topic,gamma, title, journal, year,Name)
        topic_joined$year <- as.integer(topic_joined$year)
        

# Define UI for app that draws a histogram ----
ui <- dashboardPage(
        # App title ----
        dashboardHeader(title= "Dupuytren NLP"),
        dashboardSidebar(
                sidebarMenu(
                        menuItem("Rationale", tabName = "ratio", icon = icon("comment")),
                        menuItem("Topics", tabName="topic", icon=icon("chart-bar"))
                )
        ),
        
        dashboardBody(
                tabItems(
                        tabItem(tabName="ratio",
                fluidRow(
                        box(title= "Why and how",
                            "Locating of relevant literature can be time consuming. Most searches are based on Pubmed. How can we refine a Pubmed search ?",
                            "One solution may be the use of natural language processing (NLP) and combine it with topic modelling.",br(),
                            "NLP enables computers to handle and analyse large amounts of natural language. A topic model is a special statistical representation of topics within a collection of documents.",
                            "Every document is a mixture of topics. We imagine that each document may contain words from several topics in
                            particular proportions. For example, in a two-topic model we could say “Document 1 is 90% topic A and 10% topic B, while Document 2 is 30% topic A and 70% topic B.”
                            Every topic is a mixture of words. For example, we could imagine a two-topic model of American news, with one topic for “politics” and one for “entertainment.”
                            The most common words in the politics topic might be “President”, “Congress”, and “government”, while the entertainment topic may be made up of words such as “movies”, “television”, and “actor”.
                            Words can be shared between topics; a word like “budget” might appear in both.
                            LDA is a mathematical method for estimating both at the same time: finding the mixture of words that is associated with each topic, while also determining the mixture of topics that describes each document.
                            [https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation]", br(),br(),
                            "A pubmed query: (Triangular Fibrocartilage[MeSH Terms]) OR TFCC 638 publications, 611 of them have an abstract.",br(), 
                            "The abstracts are tokenized (separated into words), then stop words, which doesn't contain specific information as (and, I, we, find etc.) are filtered. Then within each documents the occurrence of words is counted.
                            I filter words occurring frequently in each document too,  e.g. TFCC, wrist, Background, study, clinical, result etc.", br(), 
                        "Then topic models for all publications from the above mentioned query are calculated. The topics and the top 10 words
                            associated with each topic are displayed in Tab 2 (topics), giving the probability for a word belonging to that topic, beta-value. Be patient calculating and displaying the topics take some time. " , br(),
                            "On the right side, you can choose the topic you are interested in. The top 50 publications associated with that topic are displayed. They are sorted by gamma-value from 1.0 to 0.-- 1.0 shows maximum association between
                            topic and publications.",width=12
                        )
                )),
                
                tabItem(tabName="topic",
                        fluidRow(
                                column(width=8,
                                        box(plotOutput("topics"),width=NULL, height = 1000)
                ),
                                column(width=4,
                                       box(title="Topic",width=NULL,
                                           numericInput("top",
                                                        label = "Select topic number",
                                                        min=1,max=48,value=1)),
                                       box(title="relevant Papers",tableOutput("rel_pap"),width=NULL)
                                       )
                )
                ) ## close body
                ))) ## close page
                
        

# Define server logic  ----
server <- function(input, output) {
        
        selectedtopic <- reactive({
                input$top 
        })
        
        rel_tab <- reactive({
                x <- topic_joined %>% filter(topic==selectedtopic())
        })
        
        ### output of top items for each topic
        output$topics <- renderPlot({
                top_terms %>%
                        mutate(term = reorder(term, beta)) %>%
                        group_by(topic, term) %>%    
                        arrange(desc(beta)) %>%  
                        ungroup() %>%
                        mutate(term = factor(paste(term, topic, sep = "__"), 
                                             levels = rev(paste(term, topic, sep = "__")))) %>%
                        ggplot(aes(term, beta, fill = as.factor(topic))) +
                        geom_col(show.legend = FALSE) +
                        coord_flip() +
                        scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
                        labs(title = "Top 10 terms in each STM topic",
                             x = NULL, y = expression(beta)) +
                        facet_wrap(~ topic, ncol = 6, scales = "free")
                
        },height = 1000)
        
        ### output of the table with relevant papers for selected topic
        output$rel_pap <- renderTable({
                rel_tab()
        })
        
        
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)