### This file contains your main code.
### Feel free to rename it, or split it into several files.
###
### Your final product should contain the code along the following lines:
library("jsonlite")
library("dplyr")
library("httr")
library("ggplot2")
library("stringr")
setwd ("~/Desktop/UW/INFO_201/Assignments/Assignment_5/a5-api-parkj45-1336467")

##    ---------- Google Civic Platform ----------
## 1. create the google civic platform request and httr::GET() the result
##    you need to include your api key in the request.  See the documentation
##    https://developers.google.com/civic-information/
##    in particular the reference section.
source('keys.R')

##    Note: you can submit the requests through your browser.  If unsure, or if
##    httr::GET gives you an error, you may always put the address in your browser's
##    address bar.  If correct, it will display the corresponding JSON data.  If
##    incorrect, you get an error message.
base.url <- "https://www.googleapis.com/civicinfo/v2/representatives"
address <- "4535 12th Ave NE, Seattle, WA 98105"

## 2. extract the elected officials' data from the result
##    The data contains many relevant variables, including normalized address,
##    'offices' and 'officials'.  In order to attach the officials (people)
##    with offices (jobs), I recommend to use dplyr joins (what would be the key?)
##    More about joins in
##    https://info201.github.io/dplyr.html#joins
##    http://r4ds.had.co.nz/relational-data.html
query.params <- list(address = address, key=google.key)
response <- GET(base.url, query = query.params)
body <- content(response, "text")
result <- fromJSON(body)


## 3. transform the data into a well formatted table
##    I recommend you transform the data into markdown strings.  For instance,
##    to display a html link as a link in the markdown file, you may want to
##    embed it between "[](" and ")".
##    
##    You may want to consider improved table printing, look for details at the rmarkdown
##    page at
##    http://rmarkdown.rstudio.com/index.html
##    

officials <- result$officials
offices <- result$offices
position <- offices$name
table <- officials %>% select(name, party, emails, phones, photoUrl)
table$photoUrl <- paste0("![](", table$photoUrl, ")")
merged.table <- merge(data.frame(table, row.names = NULL), data.frame(position, row.names = NULL), by = 0, all = TRUE, sort = FALSE)[-1]
colnames(merged.table)[6] <- "position"

merged.table <- sapply(merged.table, as.character)
merged.table[is.na(merged.table)] <- "not available"
final.table <- merged.table[, c(1,6,2,3,4,5)] %>% replace(.=="NULL", "not available") %>% replace(.== "Unknown", "not available") %>% replace (.=="![](NA)", "not available")


 ## -------------------- propublica --------------------
## 4. Get state representatives from propublica congress API
##    you need the respective API key.
##
##    Note1: the api key must be sent as 'X-API-Key'.  No other name, such as 'api-key'
##    will work.
##
##    Note2: Propublica API has several endpoints.  The relevant one here is 'members'.
##    It which allows you to get lists of members, lists of members by state,
##    specific member by id, voting data, and more.
##    
##    Read the documentation:
##    https://projects.propublica.org/api-docs/congress-api/members/

base.url <- "https://api.propublica.org/congress/v1/members/house/WA/current.json"
data <- GET(base.url, add_headers("X-API-Key" = propublica.key))
info <- content(data, "text")
representatives <- fromJSON(info)
representatives <- as.data.frame(representatives$results)
flattened.representatives <- flatten(representatives)

##
## 5. transform it in a form you can use for visualizations.
## 
##    For the first visualization you have to extract the party affiliation of all the members
##    and make a histogram of that data.
representatives <- representatives %>% replace(.== "D", "Democratic") %>% replace(.== "R", "Republican") %>% replace (.== "M", "Male") %>% replace(.== "F", "Female")
ggplot(data = flattened.representatives) +
  geom_bar(mapping = aes(x = representatives$party, fill = party)) +
  ggtitle("Party Affiliation of the Representatives") +
  labs(x = "party", y = "# of Representatives") +
  scale_fill_manual(values = c("blue", "red")) +
  coord_flip()

ggplot(data = flattened.representatives) +
  geom_bar(mapping = aes(x = representatives$gender, fill = gender)) +
  ggtitle("Gender Distribution of the Representatives") +
  labs(x = "gender", y = "# of Representatives") +
  coord_flip()

##    
## 6. pick a representative.
##
##    Note: this representative must correspond to the state the address points to.  Different
##    states have different number of representatives, I recommend to pick one of these at random. 
##

rep.id <- sample(flattened.representatives$id, 1, replace = FALSE,prob = NULL)
chosen.rep <- GET(paste("https://api.propublica.org/congress/v1/members/", rep.id, ".json", sep = ""), add_headers("X-API-KEY" = propublica.key))
rep.info <- content(chosen.rep, "text")
organized.info <- fromJSON(rep.info)
final.info <- as.data.frame(organized.info$results) 


## 7. get this representative's info
##
##    Consult the 'members' endpoint and the examples related to information about a particular member.
##

twitter <- final.info$twitter_account
name <- paste(final.info$first_name, final.info$last_name, sep = " ")
party <- representatives %>% filter(representatives$id == rep.id)
rep.party <- party$party

age.count <- function(birth.date, today) {
  born <- as.POSIXlt(birth.date)
  today <- as.POSIXlt(Sys.Date())
  year <- today$year - born$year
  ifelse((born$month > today$moth) | ((today$month == born$month) & (today$day < born$day)), year-1, year)
  return(year)
}
age <- age.count(final.info$date_of_birth)

## 8. get her recent votes.
##
##    In order to get the percentage of votes with majority, you have:
##    a) get the member's voting data (see the same API documentation)
##    b) pick the most recent votes (the data includes vote date)
##    c) find her position (Yes/No)
##    d) find the total votes (yes/no)
##    Consult the example in the API documentation that includes the relevant JSON result.
##
votes <- GET(paste("https://api.propublica.org/congress/v1/members/", rep.id, "/votes.json", sep = ""), add_headers("X-API-KEY" = propublica.key))
voting <- content(votes, "text")
voting.info <- fromJSON(voting)
final.votes <- as.data.frame(voting.info$results)
final.votings <- as.data.frame(final.votes$votes)
voting.position <- length(final.votings[final.votings$result == "Passed" & final.votings$position == "Yes",1]) + length(final.votings[final.votings$result == "Failed"
                                                                                                                                     & final.votings$position == "No",1])
voting.result <- length(final.votings$result)
votes.percentage <- paste(round((voting.position / voting.result) * 100, 2), "%", sep = "")
