library(httr)
library(jsonlite)
library(knitr)
library(dplyr)
library(jpeg)
library(ggplot2)
library(plotly)
library(shiny)

base <- ("https://congress.api.sunlightfoundation.com/")

ui <- fluidPage(
  
  titlePanel("Congress App"),
  

  
  img(src="flag.jpg", height=245), 
  img(src="capitolbuilding.jpg", height=245), 
  img(src="congress.jpg", height=245), hr(),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Parameters"),
      textInput('zip', "Zipcode", value="90210"),
      h5(em("Enter a zipcode to view legislators from the district(s) within that zipcode.")), 
      hr(), 
      selectInput('topic', "Topic of Legislation", choices=c("education", "health", "guns", "veterans", "budget", 
                                                             "law", "welfare", "taxes", "diplomacy", "defense")),
      hr(), 
      textInput('roll.id', "Roll.id of Vote", value="h65-2017"), 
      ("The roll.id of a roll call vote is made up of the first letter of the chamber (h or s), the vote number,
        and the year the vote took place. Any vote that took place during or after 2009 can be used.
        The default roll.id is h65-2017, the No Taxpayer Funding for Abortion and 
        Abortion Insurance Full Disclosure Act.  To find a bill's roll.id, you can use"), 
        tags$a(href="https://www.govtrack.us/congress/votes", "GovTrack", target="_blank"),
        ("and compile the roll.id based on the vote number and year. Or, you can explore one of the
         roll.ids listed below:"), 
      br(),
      ("h2-2017: the 2017 election of the Speaker of the House"), 
      br(),
      ("s59-2017: Senate confirmation of Jeff Sessions for attorney general"),
      br(),
      ("s334-2015: Every Child Achieves Act of 2015"), br(),
      ("h768-2009: the Patient Protection and Affordable Care Act")
      ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Local Reps",
                           h3("Local Representatives"),
                           ("Below are the members of Congress that represent the zipcode"),
                           textOutput('zipcode', inline=TRUE),
                           tableOutput('reps'),
                           uiOutput('photos'),
                           em("The images are shown (left to right) based on the order in which the representatives are listed in the table.")),
                  
                  tabPanel("Recent Bills",
                           h3("Current Bills being Discussed by Congress"),
                           ("Below is a table of all the bills regarding"), strong(textOutput('topic', inline=TRUE)), ("that are being discussed by the 
                            current Congress."), 
                           dataTableOutput('bills')),
                  
                  tabPanel("Voting on an Issue",
                           h3(("How Congress Voted on"), textOutput('roll.id', inline= TRUE), textOutput('question', inline=TRUE)),
                           ("Note that for roll call votes with more than three possible votes
                          (e.g. Speaker elections), only the top three votes are shown in the outcome
                          table and pie chart. This vote required"),
                           textOutput('required', inline=TRUE), ("of the votes, and the result was:"),
                           textOutput('result', inline=TRUE),
                           splitLayout(plotOutput('piechart'),
                           tableOutput('pie')),
                           br(),
                           ("The table below shows how each member of"), 
                           tableOutput('vote')),
                  
                  tabPanel("Vote Breakdown",
                           h3("Vote Breakdown"),
                           ("Breaking down various aspects of"), textOutput('roll.id3', inline=TRUE),
                           h4("Party Votes"), 
                           ("This table shows how each party voted on"),
                           textOutput('roll.id2', inline=TRUE),
                           ("what the majority vote was, how many party members voted with the majority, 
                            and the number of party members in the chamber of Congress. Note that this table does not work for Speaker of the House elections,
                            or other roll call votes with votes other than 'yea', 'nay.'"),
                           tableOutput('party.voting'),
                           h4("Breakdown of Votes by Gender"),
                           ("The table and pie chart below show the breakdown of the vote by gender.
                            Note that for roll call votes with more than three possible votes (e.g. Speaker elections),
                            only the four categories with the most votes are shown."),
                           splitLayout(cellWidths= c("50%", "50%"), 
                                       plotOutput('genderpie', height = 300),
                                       tableOutput('genderr'))
                           ),
                  
                 tabPanel("Demographics",
                          h3("Demographic Makeup of the Current Congress"),
                          h4("House of Representatives"),
                          ("The House is made up of"),
                          textOutput('housesize', inline=TRUE),
                          ("members."),
                          splitLayout(
                            plotOutput('house', height = 300),
                            plotOutput('house.party', height = 300), align="center"),
                          splitLayout(
                            tableOutput('htable'),
                            tableOutput('housetable')
                          ),
                          hr(), h4("Senate"),
                          ("The Senate is made up of 100 members: two representatives from each state."),
                          splitLayout(
                            plotOutput('senate', height = 300),
                            plotOutput('senate.party', height = 300)),
                          splitLayout(
                            tableOutput('stable'),
                            tableOutput('senatetable')
                          ))
                 ))
    ),
  hr(),
  ("Image credits for header photos (L to R):"), 
  tags$a(href="http://feelgrafix.com/group/american-flag.html", "feelgrafix", target = "_blank"), 
  ("|"),
  tags$a(href="https://en.wikipedia.org/wiki/United_States_Congress", "Wikipedia", target = "_blank"), 
  ("|"),
  tags$a(href="https://www.brookings.edu/multi-chapter-report/vital-statistics-on-congress/", "Brookings", target = "_blank"), br(),
  ("This application explores data from the Sunglight Foundation's"),
  tags$a(href="https://sunlightlabs.github.io/congress/index.html", "Congress API", target="_blank"), hr()

  
)