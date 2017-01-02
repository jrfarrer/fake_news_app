library(shiny)
library(miniUI)
library(tidyverse)
library(ggplot2)
library(DT)
library(RMySQL)
library(shinyBS)
library(stringr)
library(jsonlite)


articles <- readr::read_delim("articles.txt", "|")
source("credentials.R")

options(mysql = list(
  "host" = host,
  "port" = 3306,
  "user" = user,
  "password" = password
))
databaseName <- "fake_news"

randos <- replicate(3600*24, sample(1:11))

ui <- miniPage(
  tags$head(
    tags$style(HTML('.toggle {float: left;} 
                     body {font-size: 95%;}
                     #emailBox {text-align:center}
                    '))
  ),
  gadgetTitleBar("Is the headline REAL or FAKE?", left = NULL, right = NULL),
  miniTabstripPanel(
    miniTabPanel("Headline", icon = icon("sliders"),
                 miniContentPanel(
                    uiOutput("textme")
                    , uiOutput('emailBox')
                    , textOutput("errormessage")
                    , uiOutput('submitBtn')
                    , br()
                 )
    ),
    miniTabPanel("Results", icon = icon("area-chart"),
                 miniContentPanel(
                   DT::dataTableOutput('results')
                 )
    ),
    miniTabPanel("Leaderboard", icon = icon("table"),
                 miniContentPanel(
                   DT::dataTableOutput("leaderboard")
                 )
    )
  )
)

server <- function(input, output, session) {
  
  isolate({
    time_int <- round(as.numeric(Sys.time()) - as.numeric(as.POSIXct(Sys.Date())),0)
  })
  
  output$textme <- renderUI({
    a <- paste0('<div class="checkbox">
                        <label class="clearfix">
                          <input id=', articles$id[randos[, time_int]],' checked type="checkbox" data-toggle="toggle" data-on="Real" data-off="Fake" data-size="small">
                          ', articles$article[randos[, time_int]],' 
                        </label>
                    </div>')
    
  HTML(paste0('<head>
                <link rel="stylesheet" type="text/css" href="https://gitcdn.github.io/bootstrap-toggle/2.2.2/css/bootstrap-toggle.min.css">
                <script type="text/javascript" src="https://gitcdn.github.io/bootstrap-toggle/2.2.2/js/bootstrap-toggle.min.js"></script>
            </head>',
           a)) 
  })
  
  output$emailBox <- renderUI({
      textInput("email", "Email:", placeholder = "jrf@wharton.upenn.edu") 
  })
  
  output$submitBtn <- renderUI({
    #div(
      bsButton('submit', "Submit", icon = icon('check'), style = "default", 
                 size = "default", type = "action", block = FALSE, disabled = FALSE, value = FALSE)
        #, style = "text-align:center;")
  })
  
  observeEvent(input$submit, {
    
    email <- str_trim(input$email, side = c("both"))
    
    # Check if email already exists
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    current_emails <- dbGetQuery(db, "SELECT distinct email FROM responses") %>% as_tibble()
    dbDisconnect(db)
    if (email %in% current_emails[[1]]) {
      output$errormessage <- renderText({
        paste0("Hey you've already taken a crack at this")
      })
    } else {
      request <- paste0("http://apilayer.net/api/check?access_key=",mailbox_layer,"&email=",email,"&smtp=1&format=1")
      req <- fromJSON(request)
      
      if(req$score < .65) {
        
        # Might have been a typo
        if (req$did_you_mean !="") {
          output$errormessage <- renderText({
            paste0("Did you mean ", req$did_you_mean,"?")
          })
          
        } else if (req$disposable) {
          output$errormessage <- renderText({
            paste0("That looks like a disposable email")
          })
        } else {
          output$errormessage <- renderText({
          paste0("This doesn't look like an email")
          })
        }
        
      } else if (str_extract(req$domain,".{3}$") != "edu") {
          output$errormessage <- renderText({
            paste0("Please enter a school email")
          })
      } else {
        output$errormessage <- renderText({
          ""
        })
        domain <- req$domain
        q_id <- 1:11
        selected = c(input$`1`, input$`2`, input$`3`, input$`4`, input$`5`, input$`6`, input$`7`, input$`8`, input$`9`, input$`10`, input$`11`)
        
        query <- paste0("INSERT INTO responses (email, domain, q_id, response) VALUES", paste0("('",email,"','",domain,"',",q_id,",",selected,")", collapse = ", "),";")
        
        
        db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                        port = options()$mysql$port, user = options()$mysql$user, 
                        password = options()$mysql$password)
        dbGetQuery(db, query)
        dbDisconnect(db)
        
        updateButton(session, "submit", label = "Submitted", style = "sucess", icon = icon('check-circle'), disabled = TRUE)
      }
    }
  })
  
  output$results <- DT::renderDataTable({
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    current <- dbGetQuery(db, "SELECT q_id, response FROM responses") %>% as_tibble()
    dbDisconnect(db)
    
    df <- 
      articles %>% 
      mutate(response_actual = if_else(type == "Real", 1, 0)) %>%
      inner_join(current, by = c('id' = 'q_id')) %>%
      group_by(id, article, url, type) %>%
      summarise(`Others Chose Correctly` = sum(response_actual == response) / n()) %>%
      ungroup() %>%
      inner_join(
        data_frame(
          id = 1:11
          #, selected = selected
          , selected = c(input$`1`, input$`2`, input$`3`, input$`4`, input$`5`, input$`6`, input$`7`, input$`8`, input$`9`, input$`10`, input$`11`)
        )
        , by = c('id')
      ) %>%
      mutate(selected = if_else(selected == TRUE, "Real","Fake")) %>%
      rename(`Actual` = type, `You Selected` = selected) %>%
      mutate(`Article` = paste0('<a href="',url,'" target="_blank">',article,'</a>')) %>%
      select(`Article`, `Actual`, `You Selected`, `Others Chose Correctly`)
    
    df2 <- df[randos[, time_int], ]
    
    cols <- ifelse(df2$`Actual` == df2$`You Selected`, NA,"#FCBBA1")
    
    DT::datatable(df2
                  , rownames = FALSE
                  , escape =  FALSE
                  , options = list(
                    dom = 't'
                    , pageLength = 11
                    , ordering = FALSE
                    , columnDefs = list(list(className = 'dt-center', targets = 1:3))
                  )
    ) %>% DT::formatStyle(columns = 1:4, valueColumns = 1, backgroundColor = styleEqual(df2$Article, cols)) %>%
      DT::formatPercentage(4)
    
  })
  
  output$leaderboard <-  DT::renderDataTable({
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    leader_temp <- dbGetQuery(db, "SELECT domain, q_id, SUM(response) AS `Real`, SUM(!response) AS Fake  FROM responses GROUP BY 1, 2;") %>%
                          as_tibble()
    dbDisconnect(db)
    
    leaders <- 
      leader_temp %>%
        gather(response, cnt, -domain, -q_id) %>%
        inner_join(articles, by = c('q_id' = 'id')) %>%
        mutate(num_correct = if_else(type == response, cnt, 0)) %>%
        group_by(domain) %>%
        summarise(
          `Respondents` = sum(cnt) / 11
          , `Percent Correct` = sum(num_correct) / sum(cnt)
          ) %>%
        arrange(desc(`Percent Correct`)) %>%
        rename(Domain = domain)
      
    
    DT::datatable(leaders
                  , rownames = FALSE
                  , escape =  FALSE
                  , options = list(
                    dom = 't'
                    , pageLength = 11
                    , ordering = FALSE
                    , columnDefs = list(list(className = 'dt-center', targets = 1:2))
                  )
    ) %>% DT::formatPercentage(3, digits = 1)
      
    
  })
  
}

shinyApp(ui, server)
#runGadget(shinyApp(ui, server), viewer = paneViewer())