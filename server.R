library(httr)
library(jsonlite)
library(dplyr)
library(jpeg)
library(ggplot2)
library(plotly)
library(shiny)

base <- ("https://congress.api.sunlightfoundation.com/")

server <- function(input, output) {
  
  legislators <- reactive({
    resource <- ("legislators/locate")
    query <- paste0("?zip=", input$zip)
    response <- GET(paste0(base, resource, query))
    body <- fromJSON(content(response, "text"))
    legislators <- flatten(body$results) %>% mutate(name = paste(first_name, last_name)) %>% select(name, chamber, party, state, phone, website)
    return(legislators)
  })
  
  output$reps <- renderTable({
    return(legislators())
  })
  
  output$zipcode <- reactive({
    return(paste0(input$zip, ":"))
  })
  
  output$zipcode2 <- reactive({
    return(paste0(input$zip, ":"))
  })
  
  output$photos <- renderUI({
    resource <- ("legislators/locate")
    query <- paste0("?zip=", input$zip)
    response <- GET(paste0(base, resource, query))
    body <- fromJSON(content(response, "text"))
    bio.ids <- flatten(body$results) %>% select(bioguide_id)
    picture.base <- ("https://theunitedstates.io/images/congress/225x275/")
    picture.query <- (".jpg")
    num.reps <- nrow(bio.ids)
    if (num.reps ==1 ) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      tags$img(src=picture1)
    } else if (num.reps == 2) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
      tagList(tags$img(src=picture1), 
              tags$img(src=picture2))
    } else if (num.reps == 3) {
    picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
    picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
    picture3 <- paste0(picture.base, bio.ids[3,1], picture.query)
    tagList(tags$img(src=picture1), 
    tags$img(src=picture2), 
    tags$img(src=picture3)) }
    else if (num.reps == 4) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
      picture3 <- paste0(picture.base, bio.ids[3,1], picture.query)
      picture4 <- paste0(picture.base, bio.ids[4,1], picture.query)
      tagList(tags$img(src=picture1, width = 187.5),
      tags$img(src=picture2, width = 187.5),
      tags$img(src=picture3, width = 187.5), 
      tags$img(src=picture4, width = 187.5)) 
    } else if (num.reps == 5) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
      picture3 <- paste0(picture.base, bio.ids[3,1], picture.query)
      picture4 <- paste0(picture.base, bio.ids[4,1], picture.query)
      picture5 <- paste0(picture.base, bio.ids[5,1], picture.query)
      tagList(tags$img(src=picture1, width=150), 
      tags$img(src=picture2, width=150), 
      tags$img(src=picture3, width=150), 
      tags$img(src=picture4, width=150), 
      tags$img(src=picture5, width=150)) 
    } else if (num.reps == 6) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
      picture3 <- paste0(picture.base, bio.ids[3,1], picture.query)
      picture4 <- paste0(picture.base, bio.ids[4,1], picture.query)
      picture5 <- paste0(picture.base, bio.ids[5,1], picture.query)
      picture6 <- paste0(picture.base, bio.ids[6,1], picture.query)
      tagList(tags$img(src=picture1, width=125), 
      tags$img(src=picture2, width=125),
      tags$img(src=picture3, width=125), 
      tags$img(src=picture4, width=125), 
      tags$img(src=picture5, width=125), 
      tags$img(src=picture6, width=125))
    } else if (num.reps == 7) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
      picture3 <- paste0(picture.base, bio.ids[3,1], picture.query)
      picture4 <- paste0(picture.base, bio.ids[4,1], picture.query)
      picture5 <- paste0(picture.base, bio.ids[5,1], picture.query)
      picture6 <- paste0(picture.base, bio.ids[6,1], picture.query)
      picture7 <- paste0(picture.base, bio.ids[7,1], picture.query)
      tagList(tags$img(src=picture1, width=107),
      tags$img(src=picture2, width=107),
      tags$img(src=picture3, width=107),
      tags$img(src=picture4, width=107),
      tags$img(src=picture5, width=107), 
      tags$img(src=picture6, width=107), 
      tags$img(src=picture7, width=107))
    }
    
  })


  
  
  
  bills <- reactive({
    bills.resource <- ("bills/search?query=")
    filters <- ("&congress=115&order=last_action_at")
    bills.response <- GET(paste0(base, bills.resource, input$topic, filters))
    bills.body <- fromJSON(content(bills.response, "text"))
    bills <- flatten(bills.body$results)
    bills <- select(bills, bill_id, introduced_on, official_title)
    colnames(bills)[colnames(bills) == "bill_id"] <- "bill"
    colnames(bills)[colnames(bills) == "introduced_on"] <- "date introduced"
    colnames(bills)[colnames(bills) == "official_title"] <- "full title of bill"
    return(bills)
  })
  
  output$bills <- renderDataTable({
    return(bills())
  })
  
  output$topic <- reactive({
    return(input$topic)
  })
  
  output$roll.id <- reactive({
    roll <- paste0(input$roll.id, ":")
    return(roll)
  })
  
  output$roll.id2 <- reactive({
    roll <- paste0(input$roll.id, ":")
    return(roll)
  })
  
  output$roll.id3 <- reactive({
    return(input$roll.id)
  })

  output$result <- reactive({
    votes.resource <- ("votes?roll_id=")
    vote.filter <- ("&fields=result")
    vote.response <- GET(paste0(base, votes.resource, input$roll.id, vote.filter))
    vote.body <- content(vote.response)
    vote.list <- vote.body$results[[1]]$result
    vote.result <- toJSON(vote.list)
    the.result <- fromJSON(vote.result)
    result <- paste0(the.result, ".")
    return(result)
  })
    
  output$required <- reactive({
    votes.resource <- ("votes?roll_id=")
    vote.filter <- ("&fields=required")
    vote.response <- GET(paste0(base, votes.resource, input$roll.id, vote.filter))
    vote.body <- content(vote.response)
    vote.list <- vote.body$results[[1]]$required
    vote.required <- toJSON(vote.list)
    required <- fromJSON(vote.required)
    return(required)
  })
  
  output$question <- reactive({
    votes.resource <- ("votes?roll_id=")
    vote.filter <- ("&fields=question")
    vote.response <- GET(paste0(base, votes.resource, input$roll.id, vote.filter))
    vote.body <- content(vote.response)
    vote.list <- vote.body$results[[1]]$question
    vote.question <- toJSON(vote.list)
    question <- fromJSON(vote.question)
    return(question)
  })
  
  vote <- reactive({
    votes.resource <- ("votes?roll_id=")
    votes.filters <- ("&fields=voters")
    votes.response <- GET(paste0(base, votes.resource, input$roll.id, votes.filters))
    request.body.as.list <- content(votes.response)
    voters.list <- request.body.as.list$results[[1]]$voters
    names(voters.list) <- NULL
    voters.json <- toJSON(voters.list)
    voters.as.data.frame <- flatten(fromJSON(voters.json, flatten=TRUE))
    voters <- select(voters.as.data.frame, voter.first_name, voter.last_name, voter.party, voter.state, vote)
    colnames(voters)[colnames(voters) == "voter.party"] <- "party"
    colnames(voters)[colnames(voters) == "voter.first_name"] <- "first name"
    colnames(voters)[colnames(voters) == "voter.last_name"] <- "last name"
    colnames(voters)[colnames(voters) == "voter.state"] <- "state"
    return(voters)
  })
  
  output$vote <- renderTable({
    return(vote())
  })
  
  party.voting.data.plot <- reactive({  
    votes.resource <- ("votes?roll_id=")
    votes.filters <- ("&fields=voters")
    votes.response <- GET(paste0(base, votes.resource, input$roll.id, votes.filters))
    request.body.as.list <- content(votes.response)
    voters.list <- request.body.as.list$results[[1]]$voters
    names(voters.list) <- NULL
    voters.json <- toJSON(voters.list)
    voters.as.data.frame <- flatten(fromJSON(voters.json, flatten=TRUE))
    voters.plot <- select(voters.as.data.frame, voter.party, vote, voter.gender)
    voters <- select(voters.as.data.frame, voter.first_name, voter.last_name, voter.party, vote)
    colnames(voters)[colnames(voters) == "voter.party"] <- "party"
    colnames(voters)[colnames(voters) == "voter.first_name"] <- "first name"
    colnames(voters)[colnames(voters) == "voter.last_name"] <- "last name"
    unique.votes <- unique(voters[c("vote")])    
    num.responses <- nrow(unique.votes)
    PartyVotes <- function(party.choice) {
      party.votes <- filter(voters, voters$party == party.choice)
      yea.votes <- party.votes %>% filter(party.votes$vote == "Yea") %>% nrow()
      nay.votes <- party.votes %>% filter(party.votes$vote == "Nay") %>% nrow()  
      in.chamber <- nrow(party.votes)
      if (yea.votes > nay.votes) {
        majority.vote <- "Yea"
        sum.majority <- yea.votes
      } else if (nay.votes > yea.votes) {
        majority.vote <- "Nay"
        sum.majority <- nay.votes
      } else if (yea.votes == nay.votes & yea.votes > 0) {
        majority.vote <- "Tie"
        sum.majority <- yea.votes
      } else {
        majority.vote <- "N/A"
        sum.majority <- yea.votes
      }
      party.voting <- (data.frame(party.choice, majority.vote, sum.majority, in.chamber, stringsAsFactors = FALSE))
      return(party.voting)
    }
    party.voting <- PartyVotes("D")
    party.voting[(nrow(party.voting)+1),] <- PartyVotes("R")
    party.voting[(nrow(party.voting)+1),] <- PartyVotes("I")
    colnames(party.voting)[colnames(party.voting) == "party.choice"] <- "party"
    colnames(party.voting)[colnames(party.voting) == "sum.majority"] <- "number of majority votes"
    colnames(party.voting)[colnames(party.voting) == "in.chamber"] <- "number of party members in chamber"
    colnames(party.voting)[colnames(party.voting) == "majority.vote"] <- "majority vote"
    return(party.voting)
  })
  
  
  output$party.voting <- renderTable({
    return(party.voting.data.plot())
  })

  
  vote.pie.table <- reactive({
    votes.resource <- ("votes?roll_id=")
    votes.filters <- ("&fields=voters")
    votes.response <- GET(paste0(base, votes.resource, input$roll.id, votes.filters))
    request.body.as.list <- content(votes.response)
    voters.list <- request.body.as.list$results[[1]]$voters
    names(voters.list) <- NULL
    voters.json <- toJSON(voters.list)
    voters.as.data.frame <- flatten(fromJSON(voters.json, flatten=TRUE))
    VoteResults <- function(a.vote) {
      vote.filter <- filter(voters.as.data.frame, voters.as.data.frame$vote == a.vote)
      number.votes <- nrow(vote.filter)
      return(data.frame("vote" = a.vote, "votes" = number.votes, stringsAsFactors = FALSE))
    }    
    NotVoting <- function(vote) {
      not.voting <- filter(voters.as.data.frame, voters.as.data.frame$vote == "Not Voting") %>% nrow()
      return(data.frame("vote" = "Not Voting", "votes" = not.voting, stringsAsFactors = FALSE))
    }
    unique.votes <- unique(voters.as.data.frame[c("vote")])
    num.responses <- nrow(unique.votes)
    if (num.responses == 1) {
      vote.pie <- VoteResults(toString(unique.votes[1,1]))
    } else if (num.responses == 2) {
      first.vote <- VoteResults(toString(unique.votes[1,1]))
      second.vote <- VoteResults(toString(unique.votes[2,1]))
      vote.pie <- rbind(first.vote, second.vote)
    } else if (num.responses == 3) {
      first.vote <- VoteResults(toString(unique.votes[1,1]))
      second.vote <- VoteResults(toString(unique.votes[2,1]))
      third.vote <- VoteResults(toString(unique.votes[3,1]))
      vote.pie <- rbind(first.vote, second.vote, third.vote)
    } else if (num.responses == 4) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4)
    } else if (num.responses == 5) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5)
    } else if (num.responses == 6) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6)
    } else if (num.responses == 7) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7)
    } else if (num.responses == 8) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8)
    } else if (num.responses == 9) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      gender9 <- VoteResults(toString(unique.votes[9,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9)      
    } else if (num.responses == 10)  {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      gender9 <- VoteResults(toString(unique.votes[9,1]))
      gender10 <- VoteResults(toString(unique.votes[10,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10)      
    } else if (num.responses == 11)  {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      gender9 <- VoteResults(toString(unique.votes[9,1]))
      gender10 <- VoteResults(toString(unique.votes[10,1]))
      gender11 <- VoteResults(toString(unique.votes[11,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11)        
    } else if (num.responses == 12) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      gender9 <- VoteResults(toString(unique.votes[9,1]))
      gender10 <- VoteResults(toString(unique.votes[10,1]))
      gender11 <- VoteResults(toString(unique.votes[11,1]))
      gender12 <- VoteResults(toString(unique.votes[12,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12)        
    } else if (num.responses == 13) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      gender9 <- VoteResults(toString(unique.votes[9,1]))
      gender10 <- VoteResults(toString(unique.votes[10,1]))
      gender11 <- VoteResults(toString(unique.votes[11,1]))
      gender12 <- VoteResults(toString(unique.votes[12,1]))
      gender13 <- VoteResults(toString(unique.votes[13,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13) 
    } else if (num.responses == 14) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      gender9 <- VoteResults(toString(unique.votes[9,1]))
      gender10 <- VoteResults(toString(unique.votes[10,1]))
      gender11 <- VoteResults(toString(unique.votes[11,1]))
      gender12 <- VoteResults(toString(unique.votes[12,1]))
      gender13 <- VoteResults(toString(unique.votes[13,1]))
      gender14 <- VoteResults(toString(unique.votes[14,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14) 
    } else if (num.responses == 15) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      gender9 <- VoteResults(toString(unique.votes[9,1]))
      gender10 <- VoteResults(toString(unique.votes[10,1]))
      gender11 <- VoteResults(toString(unique.votes[11,1]))
      gender12 <- VoteResults(toString(unique.votes[12,1]))
      gender13 <- VoteResults(toString(unique.votes[13,1]))
      gender14 <- VoteResults(toString(unique.votes[14,1]))
      gender15 <- VoteResults(toString(unique.votes[15,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14,
                          gender15) 
    } else if (num.responses == 16) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      gender9 <- VoteResults(toString(unique.votes[9,1]))
      gender10 <- VoteResults(toString(unique.votes[10,1]))
      gender11 <- VoteResults(toString(unique.votes[11,1]))
      gender12 <- VoteResults(toString(unique.votes[12,1]))
      gender13 <- VoteResults(toString(unique.votes[13,1]))
      gender14 <- VoteResults(toString(unique.votes[14,1]))
      gender15 <- VoteResults(toString(unique.votes[15,1]))
      gender16 <- VoteResults(toString(unique.votes[16,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14,
                          gender15, gender16) 
    } else if (num.responses == 17) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      gender9 <- VoteResults(toString(unique.votes[9,1]))
      gender10 <- VoteResults(toString(unique.votes[10,1]))
      gender11 <- VoteResults(toString(unique.votes[11,1]))
      gender12 <- VoteResults(toString(unique.votes[12,1]))
      gender13 <- VoteResults(toString(unique.votes[13,1]))
      gender14 <- VoteResults(toString(unique.votes[14,1]))
      gender15 <- VoteResults(toString(unique.votes[15,1]))
      gender16 <- VoteResults(toString(unique.votes[16,1]))
      gender17 <- VoteResults(toString(unique.votes[17,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14,
                          gender15, gender16, gender17) 
    } else if (num.responses == 18) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      gender9 <- VoteResults(toString(unique.votes[9,1]))
      gender10 <- VoteResults(toString(unique.votes[10,1]))
      gender11 <- VoteResults(toString(unique.votes[11,1]))
      gender12 <- VoteResults(toString(unique.votes[12,1]))
      gender13 <- VoteResults(toString(unique.votes[13,1]))
      gender14 <- VoteResults(toString(unique.votes[14,1]))
      gender15 <- VoteResults(toString(unique.votes[15,1]))
      gender16 <- VoteResults(toString(unique.votes[16,1]))
      gender17 <- VoteResults(toString(unique.votes[17,1]))
      gender18 <- VoteResults(toString(unique.votes[18,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14,
                          gender15, gender16, gender17, gender18) 
    } else if (num.responses == 19) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      gender9 <- VoteResults(toString(unique.votes[9,1]))
      gender10 <- VoteResults(toString(unique.votes[10,1]))
      gender11 <- VoteResults(toString(unique.votes[11,1]))
      gender12 <- VoteResults(toString(unique.votes[12,1]))
      gender13 <- VoteResults(toString(unique.votes[13,1]))
      gender14 <- VoteResults(toString(unique.votes[14,1]))
      gender15 <- VoteResults(toString(unique.votes[15,1]))
      gender16 <- VoteResults(toString(unique.votes[16,1]))
      gender17 <- VoteResults(toString(unique.votes[17,1]))
      gender18 <- VoteResults(toString(unique.votes[18,1]))
      gender19 <- VoteResults(toString(unique.votes[19,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14,
                          gender15, gender16, gender17, gender18, gender19) 
    } else if (num.responses == 20) {
      gender1 <- VoteResults(toString(unique.votes[1,1]))
      gender2 <- VoteResults(toString(unique.votes[2,1]))
      gender3 <- VoteResults(toString(unique.votes[3,1]))
      gender4 <- VoteResults(toString(unique.votes[4,1]))
      gender5 <- VoteResults(toString(unique.votes[5,1]))
      gender6 <- VoteResults(toString(unique.votes[6,1]))
      gender7 <- VoteResults(toString(unique.votes[7,1]))
      gender8 <- VoteResults(toString(unique.votes[8,1]))
      gender9 <- VoteResults(toString(unique.votes[9,1]))
      gender10 <- VoteResults(toString(unique.votes[10,1]))
      gender11 <- VoteResults(toString(unique.votes[11,1]))
      gender12 <- VoteResults(toString(unique.votes[12,1]))
      gender13 <- VoteResults(toString(unique.votes[13,1]))
      gender14 <- VoteResults(toString(unique.votes[14,1]))
      gender15 <- VoteResults(toString(unique.votes[15,1]))
      gender16 <- VoteResults(toString(unique.votes[16,1]))
      gender17 <- VoteResults(toString(unique.votes[17,1]))
      gender18 <- VoteResults(toString(unique.votes[18,1]))
      gender19 <- VoteResults(toString(unique.votes[19,1]))
      gender20 <- VoteResults(toString(unique.votes[20,1]))
      vote.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14,
                          gender15, gender16, gender17, gender18, gender19) 
    }
    vote.pie <- arrange(vote.pie, desc(votes))
    return(vote.pie)
  })
    

  output$pie <- renderTable({
    return(vote.pie.table())
  })
  
  
  output$local <- renderTable({
    my.legislators <- left_join(legislators, voters, by = c("name"))
    my.legislators.vote <- left_join(my.legislators, party.voting, by = c("party.x" = "party.choice"))
    my.legislators.vote$with.party <- my.legislators.vote$majority.vote == my.legislators.vote$vote
    my.legislators.vote <- my.legislators.vote %>% select(name, vote, with.party) %>% filter(vote %in% c("Yea", "Nay", "Not Voting"))
    colnames(my.legislators.vote)[colnames(my.legislators.vote) == "with.party"] <- "voted with party"
  })


  
  output$piechart <- renderPlot({
    p <- ggplot(vote.pie.table(), aes(x= "", y = votes, fill=vote))+
      geom_bar(width=1, stat="identity")+
      coord_polar("y", start=0)+
      labs(x=" ", y = " ") +
      theme(legend.title = element_text(size=15))+
      theme(legend.text = element_text(size=12))+
      theme(axis.text.y = element_blank())+
      theme(axis.ticks = element_blank())
    return(p)
  })
  
  

  
gender.voting <- reactive({
    votes.resource <- ("votes?roll_id=")
    votes.filters <- ("&fields=voters")
    votes.response <- GET(paste0(base, votes.resource, input$roll.id, votes.filters))
    request.body.as.list <- content(votes.response)
    voters.list <- request.body.as.list$results[[1]]$voters
    names(voters.list) <- NULL
    voters.json <- toJSON(voters.list)
    voters.as.data.frame <- flatten(fromJSON(voters.json, flatten=TRUE))
    voters.as.data.frame <- mutate(voters.as.data.frame, name = paste(voter.first_name, voter.last_name))
    voters.plot <- select(voters.as.data.frame, name, voter.party, vote, voter.gender)
    GenderVotes <- function(g, v) {
      gender.votes <- filter(voters.plot, voters.plot$voter.gender == g)
      votes <- nrow(filter(gender.votes, gender.votes$vote == v))
      if (v == "Not Voting") {
        vo <- "Abstain"
      } else {
        vo <- v }
      if (g == "F") {
        label <- paste("Female", vo, sep = " ")
      } else{
        label <- paste("Male", vo, sep = " ")
      }
      return(data.frame("demographic" = label, "votes" = votes, stringsAsFactors = FALSE))
    }
    NotVoting <- function(g) {
      notvoting <- filter(voters.plot, voters.plot$vote == "Not Voting")
      votes <- nrow(notvoting)
      if (g== "F") {
      label <- ("Female Abstain")
      } else {
        label <- ("Male Abstain")
      }
      return(data.frame("demographic" = label, "votes" = votes, stringsAsFactors = FALSE))
    }
    unique.votes <- unique(voters.plot[c("voter.gender", "vote")])
    num.responses <- nrow(unique.votes)
    if (num.responses == 1) {
      gender.pie <- GenderVotes(toString(unique.votes[1,1], toString(unique.votes[1,2])))
    } else if (num.responses == 2) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender.pie <- rbind(gender1, gender2)
    } else if (num.responses == 3) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender.pie <- rbind(gender1, gender2, gender3)
    } else if (num.responses == 4) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4)
    } else if (num.responses == 5) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5)
    } else if (num.responses == 6) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6)
    } else if (num.responses == 7) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7)
    } else if (num.responses == 8) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8)
    } else if (num.responses == 9) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender9 <- GenderVotes(toString(unique.votes[9,1]), toString(unique.votes[9,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9)      
    } else if (num.responses == 10)  {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender9 <- GenderVotes(toString(unique.votes[9,1]), toString(unique.votes[9,2]))
      gender10 <- GenderVotes(toString(unique.votes[10,1]), toString(unique.votes[10,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10)      
    } else if (num.responses == 11)  {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender9 <- GenderVotes(toString(unique.votes[9,1]), toString(unique.votes[9,2]))
      gender10 <- GenderVotes(toString(unique.votes[10,1]), toString(unique.votes[10,2]))
      gender11 <- GenderVotes(toString(unique.votes[11,1]), toString(unique.votes[11,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11)        
    } else if (num.responses == 12) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender9 <- GenderVotes(toString(unique.votes[9,1]), toString(unique.votes[9,2]))
      gender10 <- GenderVotes(toString(unique.votes[10,1]), toString(unique.votes[10,2]))
      gender11 <- GenderVotes(toString(unique.votes[11,1]), toString(unique.votes[11,2]))
      gender12 <- GenderVotes(toString(unique.votes[12,1]), toString(unique.votes[12,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12)        
    } else if (num.responses == 13) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender9 <- GenderVotes(toString(unique.votes[9,1]), toString(unique.votes[9,2]))
      gender10 <- GenderVotes(toString(unique.votes[10,1]), toString(unique.votes[10,2]))
      gender11 <- GenderVotes(toString(unique.votes[11,1]), toString(unique.votes[11,2]))
      gender12 <- GenderVotes(toString(unique.votes[12,1]), toString(unique.votes[12,2]))
      gender13 <- GenderVotes(toString(unique.votes[13,1]), toString(unique.votes[13,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13) 
    } else if (num.responses == 14) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender9 <- GenderVotes(toString(unique.votes[9,1]), toString(unique.votes[9,2]))
      gender10 <- GenderVotes(toString(unique.votes[10,1]), toString(unique.votes[10,2]))
      gender11 <- GenderVotes(toString(unique.votes[11,1]), toString(unique.votes[11,2]))
      gender12 <- GenderVotes(toString(unique.votes[12,1]), toString(unique.votes[12,2]))
      gender13 <- GenderVotes(toString(unique.votes[13,1]), toString(unique.votes[13,2]))
      gender14 <- GenderVotes(toString(unique.votes[14,1]), toString(unique.votes[14,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14) 
    } else if (num.responses == 15) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender9 <- GenderVotes(toString(unique.votes[9,1]), toString(unique.votes[9,2]))
      gender10 <- GenderVotes(toString(unique.votes[10,1]), toString(unique.votes[10,2]))
      gender11 <- GenderVotes(toString(unique.votes[11,1]), toString(unique.votes[11,2]))
      gender12 <- GenderVotes(toString(unique.votes[12,1]), toString(unique.votes[12,2]))
      gender13 <- GenderVotes(toString(unique.votes[13,1]), toString(unique.votes[13,2]))
      gender14 <- GenderVotes(toString(unique.votes[14,1]), toString(unique.votes[14,2]))
      gender15 <- GenderVotes(toString(unique.votes[15,1]), toString(unique.votes[15,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14,
                          gender15) 
    } else if (num.responses == 16) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender9 <- GenderVotes(toString(unique.votes[9,1]), toString(unique.votes[9,2]))
      gender10 <- GenderVotes(toString(unique.votes[10,1]), toString(unique.votes[10,2]))
      gender11 <- GenderVotes(toString(unique.votes[11,1]), toString(unique.votes[11,2]))
      gender12 <- GenderVotes(toString(unique.votes[12,1]), toString(unique.votes[12,2]))
      gender13 <- GenderVotes(toString(unique.votes[13,1]), toString(unique.votes[13,2]))
      gender14 <- GenderVotes(toString(unique.votes[14,1]), toString(unique.votes[14,2]))
      gender15 <- GenderVotes(toString(unique.votes[15,1]), toString(unique.votes[15,2]))
      gender16 <- GenderVotes(toString(unique.votes[16,1]), toString(unique.votes[16,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14,
                          gender15, gender16) 
    } else if (num.responses == 17) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender9 <- GenderVotes(toString(unique.votes[9,1]), toString(unique.votes[9,2]))
      gender10 <- GenderVotes(toString(unique.votes[10,1]), toString(unique.votes[10,2]))
      gender11 <- GenderVotes(toString(unique.votes[11,1]), toString(unique.votes[11,2]))
      gender12 <- GenderVotes(toString(unique.votes[12,1]), toString(unique.votes[12,2]))
      gender13 <- GenderVotes(toString(unique.votes[13,1]), toString(unique.votes[13,2]))
      gender14 <- GenderVotes(toString(unique.votes[14,1]), toString(unique.votes[14,2]))
      gender15 <- GenderVotes(toString(unique.votes[15,1]), toString(unique.votes[15,2]))
      gender16 <- GenderVotes(toString(unique.votes[16,1]), toString(unique.votes[16,2]))
      gender17 <- GenderVotes(toString(unique.votes[17,1]), toString(unique.votes[17,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14,
                          gender15, gender16, gender17) 
    } else if (num.responses == 18) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender9 <- GenderVotes(toString(unique.votes[9,1]), toString(unique.votes[9,2]))
      gender10 <- GenderVotes(toString(unique.votes[10,1]), toString(unique.votes[10,2]))
      gender11 <- GenderVotes(toString(unique.votes[11,1]), toString(unique.votes[11,2]))
      gender12 <- GenderVotes(toString(unique.votes[12,1]), toString(unique.votes[12,2]))
      gender13 <- GenderVotes(toString(unique.votes[13,1]), toString(unique.votes[13,2]))
      gender14 <- GenderVotes(toString(unique.votes[14,1]), toString(unique.votes[14,2]))
      gender15 <- GenderVotes(toString(unique.votes[15,1]), toString(unique.votes[15,2]))
      gender16 <- GenderVotes(toString(unique.votes[16,1]), toString(unique.votes[16,2]))
      gender17 <- GenderVotes(toString(unique.votes[17,1]), toString(unique.votes[17,2]))
      gender18 <- GenderVotes(toString(unique.votes[18,1]), toString(unique.votes[18,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14,
                          gender15, gender16, gender17, gender18) 
    } else if (num.responses == 19) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender9 <- GenderVotes(toString(unique.votes[9,1]), toString(unique.votes[9,2]))
      gender10 <- GenderVotes(toString(unique.votes[10,1]), toString(unique.votes[10,2]))
      gender11 <- GenderVotes(toString(unique.votes[11,1]), toString(unique.votes[11,2]))
      gender12 <- GenderVotes(toString(unique.votes[12,1]), toString(unique.votes[12,2]))
      gender13 <- GenderVotes(toString(unique.votes[13,1]), toString(unique.votes[13,2]))
      gender14 <- GenderVotes(toString(unique.votes[14,1]), toString(unique.votes[14,2]))
      gender15 <- GenderVotes(toString(unique.votes[15,1]), toString(unique.votes[15,2]))
      gender16 <- GenderVotes(toString(unique.votes[16,1]), toString(unique.votes[16,2]))
      gender17 <- GenderVotes(toString(unique.votes[17,1]), toString(unique.votes[17,2]))
      gender18 <- GenderVotes(toString(unique.votes[18,1]), toString(unique.votes[18,2]))
      gender19 <- GenderVotes(toString(unique.votes[19,1]), toString(unique.votes[19,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14,
                          gender15, gender16, gender17, gender18, gender19) 
    } else if (num.responses == 20) {
      gender1 <- GenderVotes(toString(unique.votes[1,1]), toString(unique.votes[1,2]))
      gender2 <- GenderVotes(toString(unique.votes[2,1]), toString(unique.votes[2,2]))
      gender3 <- GenderVotes(toString(unique.votes[3,1]), toString(unique.votes[3,2]))
      gender4 <- GenderVotes(toString(unique.votes[4,1]), toString(unique.votes[4,2]))
      gender5 <- GenderVotes(toString(unique.votes[5,1]), toString(unique.votes[5,2]))
      gender6 <- GenderVotes(toString(unique.votes[6,1]), toString(unique.votes[6,2]))
      gender7 <- GenderVotes(toString(unique.votes[7,1]), toString(unique.votes[7,2]))
      gender8 <- GenderVotes(toString(unique.votes[8,1]), toString(unique.votes[8,2]))
      gender9 <- GenderVotes(toString(unique.votes[9,1]), toString(unique.votes[9,2]))
      gender10 <- GenderVotes(toString(unique.votes[10,1]), toString(unique.votes[10,2]))
      gender11 <- GenderVotes(toString(unique.votes[11,1]), toString(unique.votes[11,2]))
      gender12 <- GenderVotes(toString(unique.votes[12,1]), toString(unique.votes[12,2]))
      gender13 <- GenderVotes(toString(unique.votes[13,1]), toString(unique.votes[13,2]))
      gender14 <- GenderVotes(toString(unique.votes[14,1]), toString(unique.votes[14,2]))
      gender15 <- GenderVotes(toString(unique.votes[15,1]), toString(unique.votes[15,2]))
      gender16 <- GenderVotes(toString(unique.votes[16,1]), toString(unique.votes[16,2]))
      gender17 <- GenderVotes(toString(unique.votes[17,1]), toString(unique.votes[17,2]))
      gender18 <- GenderVotes(toString(unique.votes[18,1]), toString(unique.votes[18,2]))
      gender19 <- GenderVotes(toString(unique.votes[19,1]), toString(unique.votes[19,2]))
      gender20 <- GenderVotes(toString(unique.votes[20,1]), toString(unique.votes[20,2]))
      gender.pie <- rbind(gender1, gender2, gender3, gender4, gender5, gender6, gender7, gender8, gender9, gender10, gender11, gender12, gender13, gender14,
                          gender15, gender16, gender17, gender18, gender19) 
    }
    gender.pie <- arrange(gender.pie, desc(votes))
    if (num.responses > 6) {
        gender.pie <- gender.pie[c(1:4),]
    }
    return(gender.pie)
    })

  
  output$genderr <- renderTable({
    return(gender.voting())
  })

  output$genderpie <- renderPlot({
    p <- ggplot(gender.voting(), aes(x=" ", y = votes, fill = demographic)) +
      geom_col(width=1)+
      coord_polar("y", start=0) +
      theme(legend.title = element_text(size=12))+
      theme(legend.text = element_text(size=12))+
      labs(x = " ", y = " ")+
      theme(axis.text.y = element_blank())+
      theme(axis.ticks = element_blank())
    return(p)
  })
   
house <- reactive({
    legislators.resource.h <- ("legislators?chamber=house&per_page=all")
    house.body <- GET(paste0(base, legislators.resource.h))
    house.content <- fromJSON(content(house.body, "text"))
    house.df <- flatten(house.content$results)
    house <- house.df %>% select(gender)
    GenderMakeup <- function(df, sex) {
    gender <- filter(df, df$gender == sex)
    vote <- nrow(gender)
    return(data.frame("gender" = sex, "members" = vote, stringsAsFactors = FALSE))
    }
    house.plot <- GenderMakeup(house, "M")
    house.plot[(nrow(house.plot)+1),] <- GenderMakeup(house, "F")
    return(house.plot)
})

output$htable <- renderTable({
  return(house())
})

senate <- reactive({
    legislators.resource.s <- ("legislators?chamber=senate&per_page=all")
    senate.body <- GET(paste0(base, legislators.resource.s))
    senate.content <- fromJSON(content(senate.body, "text"))
    senate <- flatten(senate.content$results) %>% select(gender)
    GenderMakeup <- function(df, sex) {
      gender <- filter(df, df$gender == sex)
      vote <- nrow(gender)
      return(data.frame("gender" = sex, "members" = vote, stringsAsFactors = FALSE))
    }
    senate.plot <- GenderMakeup(senate, "M")
    senate.plot[(nrow(senate.plot)+1),] <- GenderMakeup(senate, "F")
    return(senate.plot)
})

output$stable <- renderTable({
  return(senate())
})


output$senate <- renderPlot({
ggplot(senate(), aes(x=" ", y=members, fill=gender))+
    geom_bar(width=1, stat="identity")+
    coord_polar(theta="y")+
    scale_fill_manual(values = c("#F39CF3", "#09C4F6"), labels = c("Female", "Male"))+
    labs(x = " ", y = " ")+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    ggtitle("Gender Makeup of the Senate")

})
  

output$house <- renderPlot({
ggplot(house(), aes(x=1, y=members, fill=gender))+
    geom_bar(width=1, stat="identity")+
    coord_polar(theta="y") +
    scale_fill_manual(values = c("#F39CF3", "#09C4F6"), labels = c("Female", "Male"))+
    labs(x = " ", y = " ")+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    ggtitle("Gender Makeup of the House")

})

houseparty <- reactive({
  legislators.resource.h <- ("legislators?chamber=house&per_page=all")
  house.body <- GET(paste0(base, legislators.resource.h))
  house.content <- fromJSON(content(house.body, "text"))
  house.df <- flatten(house.content$results)
  house <- house.df %>% select(party)
  PartyMakeup <- function(df, dr) {
    party <- filter(df, df$party == dr)
    vote <- nrow(party)
    return(data.frame("party" = dr, "members" = vote, stringsAsFactors = FALSE))
  }
  house.plot <- PartyMakeup(house, "D")
  house.plot[(nrow(house.plot)+1),] <- PartyMakeup(house, "R")
  house.plot[(nrow(house.plot)+1),] <- PartyMakeup(house, "I")
  return(house.plot)
})

output$housetable <- renderTable({
  return(houseparty())
})

output$housesize <- reactive({
  legislators.resource.h <- ("legislators?chamber=house&per_page=all")
  house.body <- GET(paste0(base, legislators.resource.h))
  house.content <- fromJSON(content(house.body, "text"))
  house.df <- flatten(house.content$results)
  house <- house.df %>% select(party)
  return(nrow(house))
})

senateparty <- reactive({
  legislators.resource.s <- ("legislators?chamber=senate&per_page=all")
  senate.body <- GET(paste0(base, legislators.resource.s))
  senate.content <- fromJSON(content(senate.body, "text"))
  senate <- flatten(senate.content$results) %>% select(party)
  PartyMakeup <- function(df, dr) {
    party <- filter(df, df$party == dr)
    vote <- nrow(party)
    return(data.frame("party" = dr, "members" = vote, stringsAsFactors = FALSE))
  }
  senate.plot <- PartyMakeup(senate, "D")
  senate.plot[(nrow(senate.plot)+1),] <- PartyMakeup(senate, "R")
  senate.plot[(nrow(senate.plot)+1),] <- PartyMakeup(senate, "I")
  return(senate.plot)
})

output$senatetable <- renderTable({
  return(senateparty())
})

output$senate.party <- renderPlot({
  ggplot(senateparty(), aes(x=" ", y=members, fill=party))+
    geom_bar(width=1, stat="identity")+
    coord_polar(theta="y")+
    scale_fill_manual(values=c("#BF0A30", "#6D1FA7", "#002868"), labels = c("Democrat", "Independent", "Republican"))+
    labs(x = " ", y = " ")+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    ggtitle("Party Makeup of the Senate")
})


output$house.party <- renderPlot({
  ggplot(houseparty(), aes(x=" ", y=members, fill=party))+
    geom_bar(width=1, stat="identity")+
    coord_polar(theta="y") +
    scale_fill_manual(values=c("#BF0A30", "#6D1FA7", "#002868"), labels = c("Democrat", "Independent", "Republican"))+
    labs(x = " ", y = " ")+
    theme(axis.text.y = element_blank())+
    theme(axis.ticks = element_blank())+
    ggtitle("Party Makeup of the House")

  })
}
