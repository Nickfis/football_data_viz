# cleaning up the workspace
rm(list=ls())
# Loading in necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape)
library(shinyjs)
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)



setwd("/home/Nickfis/Documents/Barcelona GSE/2 Trimester/Data Visualization/Project")
########################################### load datasets ########################################################
games<-read.csv("Games.csv")
games <- games %>%
  arrange(event_id)
# load rosters identification fcb and betis sevilla (from other dataset)
allplayers_fcb <- read.csv("rosterfcb.csv")[,2:3]
names(allplayers_fcb) <- c("player_id", "player_name")
allplayers_bs <- read.csv("rosterbs.csv")[,2:3]
names(allplayers_bs) <- c("player_id", "player_name")


######################################## Reading in the event description ####################################
# encode of event_id values: only first to columns relevant -> eventid / description
encoding_eventid <- read.csv("event_encoding.csv")[ ,1:2]
names(encoding_eventid) <- c("type_id", "event_descr") # column names for later merging

# reading in encoding of qualifier_id's (describe observed events closer)
encoding_pass_qid <- read.csv("Pass_qualifier.csv")[,1:2] 
names(encoding_pass_qid)<- c("qualifier_id", "pass_descr") # give column names

#encoding_shot_qid <- read.csv("Shot_qualifier.csv")


######################################### data preproccessing #####################################################
# get rid of unneeded columns
games$id <- NULL # id not needed
games$X <- NULL # just the row index
games$last_modified <- NULL
date <- as.Date(games$timestamp[1])
games$timestamp<- NULL
games$version <- NULL

################### left join the encoding of the type_id's and qualifier_id's
# event_id
games <- left_join(games, encoding_eventid, by='type_id')

# qualifier_id
games<- left_join(games, encoding_pass_qid, by='qualifier_id') # add pass qualifiers
# games <- left_join(games, encoding_shot_qid, by='qualifier_id') # add shot qualfiiers

## Subset for each team
games_fcb <- games[games$team_id==178,] # fc barcelona
games_bs <- games[games$team_id==185,] # betis sevilla

## Match rosters with player id's from dataset
games_fcb<- left_join(games_fcb, allplayers_fcb, by='player_id')
games_bs <- left_join(games_bs, allplayers_bs, by='player_id')

# reorder
games_fcb<-arrange(games_fcb, event_id)
games_bs <- arrange(games_bs, event_id)

length(unique(games_fcb$player_name)) # all 14 possible slots were used by barca
length(unique(games_bs$player_name)) # na's are also in there for events that were not assigned to a specific player


###  Need the parent events to model the different actions on the field
# If there is no qualifier_id then it has to be a parent event
pe_fcb <- games_fcb[is.na(games_fcb$qualifier_id),] # fcb
pe_fcb$qualifier_id <- NULL # na anyways

# for each team
pe_bs <- games_bs[is.na(games_bs$qualifier_id),] # Betis Sevilla
pe_bs$qualifier_id <- NULL # na anyways

# sort the dataframes by event_id again
pe_fcb<- pe_fcb %>%
  arrange(event_id)
pe_bs <- pe_bs %>%
  arrange(event_id)


################################## match roster for fcb including position ######################################
# starting eleven & bench for fcb
# position of the different players
pos_fcb <- gsub(" ", "", as.character(games_fcb[2,"value"]))
pos_fcb<-unlist(strsplit(x=pos_fcb, ","))
pos_fcb<-revalue(as.factor(pos_fcb), replace=c("1"="Goalkeeper", "2"="Defender", "3"="Midfielder", 
                                               "4"="Forward", "5"="Substitute"))

# Adding the player id's to the positions
roster_fcb<-as.character(games_fcb[3,"value"])
roster_fcb<-gsub(" ", "", roster_fcb)
roster_fcb<-as.data.frame(unlist(strsplit(x=roster_fcb, ",")))
roster_fcb <- as.data.frame(cbind(roster_fcb, pos_fcb))

# adding the numbers of the players
numbers_fcb <- gsub(" ", "", as.character(games_fcb[5,"value"]))
numbers_fcb <- unlist(strsplit(x=numbers_fcb, ","))
roster_fcb <- as.data.frame(cbind(roster_fcb, numbers_fcb))
names(roster_fcb) <- c("player_id", "Position", "Number")

# merging by player_id to get the names of the players
roster_fcb <- arrange(merge(allplayers_fcb,roster_fcb, by="player_id"),Position) #arrange by position! level 1:5
roster_fcb
display_lineup_fcb <- roster_fcb[,2:4]
names(display_lineup_fcb) <- c("Name", "Position", "Number")

# starting eleven and bench for bs
# Getting the positions of all players
pos_bs <- gsub(" ", "", as.character(games_bs[7,"value"]))
pos_bs<-unlist(strsplit(x=pos_bs, ","))
pos_bs<-revalue(as.factor(pos_bs), replace=c("1"="Goalkeeper", "2"="Defender", "3"="Midfielder", 
                                               "4"="Forward", "5"="Substitute"))
# Adding the player id's to the positions
roster_bs<-as.character(games_bs[3,"value"])
roster_bs<-gsub(" ", "", roster_bs)
roster_bs<-as.data.frame(unlist(strsplit(x=roster_bs, ",")))
roster_bs <- as.data.frame(cbind(roster_bs, pos_bs))

# adding the numbers of the players
numbers_bs <- gsub(" ", "", as.character(games_bs[1,"value"]))
numbers_bs <- unlist(strsplit(x=numbers_bs, ","))
roster_bs <- as.data.frame(cbind(roster_bs, numbers_bs))
names(roster_bs) <- c("player_id", "Position", "Number")

# merging by player_id to get the names of the players
roster_bs <- arrange(merge(allplayers_bs,roster_bs, by="player_id"),Position) #arrange by position! level 1:5
roster_bs
display_lineup_bs <- roster_bs[,2:4]
names(display_lineup_bs) <- c("Name", "Position", "Number")



########################## CREATING PASS STATISTICS #############################################
# with all qualifiers
passes_bs <- games_bs[games_bs$event_descr=='Pass',]
passes_fcb <-games_fcb[games_bs$event_descr=='Pass',]

# parent event passes
passes_pe_bs <- pe_bs[pe_bs$event_descr=="Pass",]
passes_pe_fcb <- pe_fcb[pe_fcb$event_descr=='Pass',]

passes_pe_fcb<-arrange(passes_pe_fcb, event_id)

############# Total amount of passes per team /  per player
# teams, first row: FC Barcelona, second row: Betis Sevilla
# Initialize dataframe
team_pass_df <- as.data.frame(c("F.C. Barcelona", "Betis Sevilla"))
names(team_pass_df)[1]<- "Team"
# Extract all passes played in the game
team_pass_df["Passes played"] <- as.data.frame(c(nrow(passes_pe_fcb),nrow(passes_pe_bs)))
# successful passes: Take those with outcome=1
team_pass_df["Successful passes"] <- c(sum(passes_pe_fcb$outcome), sum(passes_pe_bs$outcome))
# Percentage of successful passes
team_pass_df["Rel. successful passes"] <- team_pass_df["Successful passes"]/team_pass_df["Passes played"]
# I want to look at all the passes in the opponents half. x >50
# for FC Barcelona:
opponent_half_fcb<-passes_pe_fcb%>%
  filter(x>50)
# for Betis Sevilla
opponent_half_bs<-passes_pe_bs%>%
  filter(x>50)
# get the same values for this, first overall amounts of passes played
team_pass_df["Abs. passes opp. half"] <- c(nrow(opponent_half_fcb), nrow(opponent_half_bs))
# percentage of passes played in opponents half
team_pass_df["Rel. passes in opp. half"]<-team_pass_df["Abs. passes opp. half"]/team_pass_df["Passes played"]
# successful passes in opponents half
team_pass_df["Successful passes in opp. half"] <- c(sum(opponent_half_fcb$outcome), sum(opponent_half_bs$outcome))
# Percentage of successful passes in opponents half
team_pass_df["Rel. successful passes opp. half"] <- (team_pass_df["Successful passes in opp. half"]/
                                                      team_pass_df["Abs. passes opp. half"])
# Check for the last third of the pitch
# for FC Barcelona:
last_third_fcb<-passes_pe_fcb%>%
  filter(x>66)
# for Betis Sevilla
last_third_bs<-passes_pe_bs%>%
  filter(x>66)
# get the same values for this, first overall amounts of passes played
team_pass_df["Abs. passes last third"] <- c(nrow(last_third_fcb), nrow(last_third_bs))
# percentage of passes played in last third
team_pass_df["Rel. passes last third"]<-team_pass_df["Abs. passes last third"]/team_pass_df["Passes played"]
# successful passes in last third
team_pass_df["Successful passes last third"] <- c(sum(last_third_fcb$outcome), sum(last_third_bs$outcome))
# Percentage of successful passes in last third
team_pass_df["Rel. successful passes last third"] <- (team_pass_df["Successful passes last third"]/
                                                        team_pass_df["Abs. passes last third"])



#### PASS PLOTTING 
############ PLOTTING
# Showing the passes played overall/opponents half/last third and then by team
# look at percentage and absolute value

# Percentage of successful passes in different areas of the pitch
percentage_team <- team_pass_df[,c("Team", "Rel. successful passes", "Rel. successful passes opp. half",
                                   "Rel. successful passes last third")]
percentage_team<- melt(percentage_team)
pct_plot <- ggplot(data=percentage_team, aes(x=variable, y=value)) +   
  geom_bar(aes(fill = Team), position = "dodge", stat="identity")+
  theme(text=element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Successful passes in different areas of the pitch")+
  xlab("")+ylab("Successful passes in %")+
  scale_fill_manual("Team", values = c("F.C. Barcelona" = "blue4", "Betis Sevilla" = "chartreuse2"))
######## Easily observable that no team really shows a significantly higher value in accomplished passes
######## in the different parts of the pitch. But what happens if we look at the absolute passes played
######## in each area?


# Absolute number of passes played in the different areas
abs_team <- team_pass_df[,c("Team","Passes played", "Abs. passes opp. half","Abs. passes last third")]
abs_team<- melt(abs_team)
abs_plot <-ggplot(data=abs_team, aes(x=variable, y=value)) +   
  geom_bar(aes(fill = Team), position = "dodge", stat="identity")+
  theme(text=element_text(size=18), axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Passes played in different areas of the pitch")+
  xlab("")+ylab("Passes played")+ 
  scale_fill_manual("Team", values = c("F.C. Barcelona" = "blue4", "Betis Sevilla" = "chartreuse2"))

######## Betis Sevilla might have (nearly) the same success in passes as Barcelona in every part of the
######## pitch but they don't have the ball as much and can't show the same quantity of passes. Furthermore
######## they seem to not be able to get many passes into the last third. Of all their passes only 13% are
######## played in the last third of the pitch, while Barcelona almost plays 30% of their passes in the 
######## opponents last third. 

# c("Passes Played", "Abs. Passes opp. half","Abs. Passes last third")

##################################### INDIVIDUAL PLAYER STATISTICS ##########################################################
##################################### Check frequency of events ################################################################
# create event frequency for fc barcelona
event_freq_fcb<-count(pe_fcb[,"type_id"])
names(event_freq_fcb)<- c('type_id', 'frequency_fcb')
event_freq_fcb<-left_join(event_freq_fcb, encoding_eventid, by='type_id')
# create event frequency for betis sevilla
event_freq_bs<-count(pe_bs[,"type_id"])
names(event_freq_bs)<- c('type_id', 'frequency_bs')
event_freq_bs<-left_join(event_freq_bs, encoding_eventid, by='type_id')

# merge them on type_id and then add up the frequency
# Barcelona has one row more, tthat's why we will use a left join and FCB on the left side
event_freq_overall<-left_join(event_freq_fcb, event_freq_bs, by="type_id")
rm(event_freq_bs)
rm(event_freq_fcb)
# switch all na's to 0 to calculate the sum later
event_freq_overall[is.na(event_freq_overall)] <- 0
event_freq_overall["Frequency"]<-event_freq_overall["frequency_fcb"]+event_freq_overall["frequency_bs"]
event_freq_overall

event_freq_overall[c("type_id", "event_descr.y")]<- NULL
# Switch order of the columns
event_freq_overall <-event_freq_overall[,c(2,1,3,4)]
# Order by highest frequency
event_freq_overall<-arrange(event_freq_overall, desc(Frequency))
# Give more fitting column names
names(event_freq_overall) <- c("Observed event", "Frequency Barcelona", "Frequency Betis Sevilla", 
                               "Overall Frequency")

main_events_overall <- event_freq_overall[1:15,]

# now this only gives us the most used events, but does not show us how successful the teams were for each event. 
# For passes we already saw the rate of success for both teams in the previous plots, but how 
main_events_overall
# concatenate string columns

count(pe_fcb[,"type_id"])


## do drop down menu for players, ordered by amount of actions
player_freq_fcb<-count(pe_fcb[,"player_name"])
player_freq_fcb <- player_freq_fcb[1:14,] # dismissing the NA's
player_freq_fcb<-arrange(player_freq_fcb, desc(freq)) #

dropdown_freq <- paste0(player_freq_fcb$x,"(",player_freq_fcb$freq,")")

player_played_fcb<- player_freq_fcb$x # gives us all players who were actually in the game for fcb

# create the same for bs
player_freq_bs<-count(pe_bs[,"player_name"])
player_freq_bs <- player_freq_bs[1:14,] # dismissing the NA's
player_freq_bs<-arrange(player_freq_bs, desc(freq)) #

dropdown_freq <- paste0(player_freq_bs$x,"(",player_freq_bs$freq,")")

player_played_bs<- player_freq_bs$x # gives us all players who were actually in the game for bs

# Shot statistics
shot_stats <- event_freq_overall
names(shot_stats) <- c("ObservedEvent", "FreqBarca", "FreqBS", "Frequency")
shot_stats<-shot_stats %>%
  filter(ObservedEvent=="Miss" | ObservedEvent=="Post" | ObservedEvent=="Goal" |
           ObservedEvent=='Attempt Saved')

shot_stats # nothing really interesting to explore here.



#################### Showing areas of the pitch most used by teams & players #############################
##### Creating the football pitch for the heatmap
# Function for drawing the circle in the middle of the pitch
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
# creating the circle
midcircle<-circleFun(c(50,50),diameter=25, npoints=200)
# football pitch creating by usage of different rectangles according to encoding by OPTA 
pitch<-ggplot(data=pe_fcb) +
  geom_rect(xmin=0, xmax=100, ymin=0, ymax=100, fill = "chartreuse3", color="white") +
  geom_rect(xmin=0, xmax=17, ymin=21.1, ymax=78.9, fill = "chartreuse3", color="white") + # box left
  geom_rect(xmin=83, xmax=100, ymin=21.1, ymax=78.9, fill = "chartreuse3", color="white") + # box right
  geom_rect(xmin=0, xmax=5.8, ymin=36.8, ymax=63.2, fill = "chartreuse3", color="white") + #5m left
  geom_rect(xmin=94.2, xmax=100, ymin=36.8, ymax=64.2, fill = "chartreuse3", color="white") + #5m right
  geom_rect(xmin=-3.5, xmax=0, ymin=43.4, ymax=56.6, color="black")+ #goalleft
  geom_rect(xmin=100, xmax=103.5, ymin=43.4, ymax=56.6, color="black")+ #goalright
  geom_vline(xintercept=50, color="white")+
  geom_point(data=midcircle, aes(x=midcircle$x, y=midcircle$y), color="white", size=0.3)+
  xlim(0,105)+ylim(0,105)

# adding the points of barcelona
# pitch + stat_density2d(data=pe_fcb,aes(x=x, y=y, fill = ..level.., alpha = ..level..),
#         size = 0.01, bins = 16, geom = 'polygon')+
#   scale_fill_gradient(low = "yellow", high = "red")+
#   ggtitle("Heatmap of events taken place from Barcelona")+
#   theme(legend.position="none")+xlab(" ")+ylab(" ")











################################## CREATION OF SHINY APP #######################################################
# Define UI for application that plots features of movies 
ui <- fluidPage(
  useShinyjs(), # in order to be able to hide output before it's being created
  h4(date),
  titlePanel("FC Barcelona - Betis Sevilla"),
  p("Also talk about where the data comes from. "),
  h4("General Information of both teams"),
  ### Show Button to click for seeing the lineup + bench of FC Barcelona
  actionButton("fcb_lineup_button", "Show line-up of FC Barcelona"),
  ### Lineup output as table
  dataTableOutput('lineupfcb'),
  ### Show Button to click for seeing the lineup + bench of Betis Sevilla
  actionButton("bs_lineup_button", "Show line-up of Betis Sevilla"),
  ### Lineup output as table
  dataTableOutput('lineupbs'),
  ### Next section of the dashboard: Team statistics
  h2("Team Statistics"),
  ### Start with the passing statistics
  h3("Passing statistics"),
  ### Create sidebar panel and mainpanel to be able to do the analysis on the left side, while seeing the plots on the right
  sidebarLayout(
    sidebarPanel(
      # Text analyisis
      p("Intuitively, knowing the famous ability of F.C. Barcelona and its gifted players to keep the ball and
        create chances by a high level of passing, we might have expected to see them having a way higher percentage of 
        successful passes, especially in the last third of the pitch. 
        This is not supported by the data however, which shows us that the difference between Barcelona and Betis Sevilla is 
        very small (3-4% in every category).
        Furthermore we note a gradually downward shift in accomplished passes, confirming the assumption
        that passing without interception or making a mistake is going to be harder for each team in the last third.")
    ),
    mainPanel(
      actionButton("pass_percentage", "Show/Hide passing percentages of both teams"), # button
      plotOutput('passpct') # plot
    )
  ),
  ### Show Button to click for seeing the percentages of passing by both clubs in different areas of the pitch
  sidebarLayout(
    sidebarPanel(                                                                                                                       
      # Text analyisis
      p("Checking the number of absolute passes though, we are able to paint a different picture. 
        Eventhough Betis Sevilla is almost as likely as Barcelona to play a successful pass (wherever),
        we observe that Barcelona plays way more passes in the last third of the pitch, where posession of
        the ball is obviously more crucial than in the own half. Overall Barcelona played almost 250 passes more during the
        match (694 to 452) and more than 27% of these passes where played in the last third of the pitch, comopared to a 
        mere 13% of Betis Sevillas passes.")
    ),
    mainPanel(
      actionButton("pass_absolute", "Show/Hide absolute number of passes of both teams"), # button
      plotOutput('passabs') # plot
    )
  ),
  # 
  #plotOutput(outputId="heatmap178"),
  h4("Heatmaps"),
  #p("Note that for both graphics the teams from left to right."),
  sidebarLayout(
    sidebarPanel(
      p("The plots on the right-hand side show the area of the pitch where the teams were mostly active. 
        In the plots, both teams play towards the right side and large differences are noticeable. 
        While most actions observed by Betis Sevilla can be found in their own half, most events tracked by
        Barcelona are in the opponent half, with their left wing being the most used part of the pitch. 
        Coincidentally we see, that this is the area where Betis Sevilla also has its most actions. 
        This could be a sign that the events observed for Sevilla might rather be defensive actions and duels, 
        compared to Barcelona, who should have more offensively, intended, on-ball events in this game.
        We will take a closer look in the next section on this question.")
    ),
    mainPanel(
      actionButton("heatmapfcb", "Show/Hide heatmap for F.C. Barcelona"),
      plotOutput(outputId="heatmap178"),
      actionButton("heatmapbs", "Show/Hide heatmap for Betis Sevilla"),
      plotOutput(outputId='heatmap185')
    )
  ),
  # Drop-down menu with every player from FC Barcelona on the roster
  sidebarLayout(
    sidebarPanel(
      p("First we will check closer for the individual heatmaps of the players, because here we already observe multiple, 
        interesting differences, which tell us more about the playing style of both teams. 
        Of course only those players are included that have also played, so you will not find all the names from the line-ups
        in this dropdown menu. 
        Especially taking a closer look at the differences between the defenders of both teams, show why we observed such a 
        huge difference in the approach to the game (Sevilla showing most of their actions on defense and vise-verse for
        Barcelona).
        Furthermore after checking the overall heatmap, my first instinct was to think, that Messi will probably have been
        Barcelona's main guy to give the ball to, but checking for his individual heatmap reveals that he was not the (main)
        reason they were attacking so often over the left wing, but rather the pretty offensive-minded defenders Jordi
        Alba and ...
        Of course this also bears the question why Barcelona mainly attacked over the left wing and whether this was a 
        tactical decision to attack GUY FROM SEVILLA, in case they saw him as the weak link in the opponents defense.")
    ),
    mainPanel(
      selectInput("heatmapselecterfcb", label = h4("Select a player from FC Barcelona to see his radius of action"), 
                  choices = player_played_fcb, 
                  selected = 1),
      plotOutput('heatmap_indiv_fcb'),
      # Drop-down menu with every player from Betis Sevilla on the roster
      selectInput("heatmapselecterbs", label = h4("Select a player from Betis Sevilla to see his radius of action"), 
                  choices = player_played_bs, 
                  selected = 1),
      plotOutput('heatmap_indiv_bs')
    )
  ),
  h2("Actions for individual players in an overview"),
  sidebarLayout(
    sidebarPanel(
      p("Ball touch -> Player unsuccessfully controlled the ball ie lost possession,
    Take on: Attempted dribble past an opponent (excluding when qualifier 211 is present as this is 'overrun'
    and is not always a duel event)
    Clearance: Player under pressure hits ball clear of the defensive zone or/and out of play
    Disposessed: the opponent. Why has Betis more?
    Challenge: When a player fails to win the ball as an opponent successfully dribbles past them
    Aerial: of course theyre the same, but we want to look at the success here, but this table is just
    used to give a first overview. Now we look at the individual players and their
    success in all of the different categories. ")
    ),
    mainPanel(
      # Look at overall frequencies of events
      actionButton("events_in_game", "Show overall number of observations of top 15 events"),
      ### Lineup output as table
      dataTableOutput('events_table_ov')
    )
  ),
  sidebarLayout(
    sidebarPanel(
      p("Look at the actions of each individual player on FC Barcelona")
    ),
    mainPanel(
      # Look at overall frequencies of events of each individual player for FCB
      selectInput("eventselecter_fcb", label = h4("Select a player from FC Barcelona to see actions"), 
                  choices = player_played_fcb, 
                  selected = 1),
      plotOutput("event_player_fcb")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      p("Look at the actions of each individual player on Betis Sevilla")
    ),
    mainPanel(
      # Look at overall frequencies of events of each individual player for bs
      selectInput("eventselecter_bs", label = h4("Select a player from FC Barcelona to see actions"), 
                  choices = player_played_bs, 
                  selected = 1),
      plotOutput("event_player_bs")
    )
  ),
  p("Of course we have not looked at many possible other statistics and aspect of the game, but rather looked at the most
    important differences between the teams and tried to convey them by plotting. Other parts which would have been interesting
    as well but were hard to do within the time limit is the fouls, set pieces, the actual kind of passes (long balls,
    short passes, crosses, etc). And of course, we have not even mentioned the goals scored (the game ended 2-0 for Barcelona
    by the way) or let alone looked at their creation. Attacking mainly over the left wing could be, as mentioned before,
    an indicator for Barcelona either playing to their own strength or rather trying to abuse the weak link in the opponents
    team. This would of course also be very interesting to observe, but for this we might need to include more datasets
    of other games, to judge whether Barcelona usually plays on the left wing or they vary from opponent to opponent.
    Of course more games of Betis Sevilla would also help to see, whether the rightback has usually problems against ...
    The aim here however is to paint as well a pictures as possible of the actual different playing styles of the teams.
    This was done by looking at different aspects of the game (passing, action radius and type of actions taken)...")
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Display line-ups:
  # FC Barcelona
  observeEvent(input$fcb_lineup_button, {
    output$lineupfcb <- renderDataTable(display_lineup_fcb)
  })
  # Betis Sevilla
  observeEvent(input$bs_lineup_button, {
    output$lineupbs <- renderDataTable(display_lineup_bs)
  })
  # Pass percentage bar plots
  # assignment of barplot to the correct output
  output$passpct <- renderPlot(pct_plot)
  # when button is klicked: show/hide the plot
  observeEvent(input$pass_percentage, {
    toggle("passpct")
  })
  # absolut number of passes  
  # assignment of barplot to the correct output   
  output$passabs <- renderPlot(abs_plot)
  # when button is klicked show/hide the plot
  observeEvent(input$pass_absolute, {
    toggle("passabs")
  })
  ########## HEATMAP BARCELONA
  # creation of heatmap for team 178: Barcelona
  output$heatmap178 <- renderPlot({
    pitch+
      stat_density2d(data=pe_fcb,aes(x=x, y=y, fill = ..level.., alpha = ..level..),
                     size = 0.01, bins = 16, geom = 'polygon')+
      scale_fill_gradient(low = "yellow", high = "red")+
      ggtitle("Heatmap of events taken place from Barcelona")+
      theme(legend.position="none")+xlab(" ")+ylab(" ")
  })
  # when button is klicked, toggle heatmap of Barcelona
  observeEvent(input$heatmapfcb, {
    toggle("heatmap178") # hide/show the heatmap on button click
  })
  ########### HEATMAP BETIS SEVILLA
  # creation of heatmap for team 185: Betis Sevilla
  output$heatmap185 <- renderPlot({ #height=800 before the curly braces
    pitch + 
      stat_density2d(data=pe_bs,aes(x=x, y=y, fill = ..level.., alpha = ..level..),
                     size = 0.01, bins = 16, geom = 'polygon')+
      scale_fill_gradient(low = "yellow", high = "red")+
      ggtitle("Heatmap of events taken place from Betis Sevilla")+
      theme(legend.position="none")+xlab(" ")+ylab(" ")
  })
  # when button is klicked, toggle heatmap of Betis Sevilla
  observeEvent(input$heatmapbs, {
    toggle("heatmap185")
  })
  # heatmap for the individual players of F.C. Barcelona
  output$heatmap_indiv_fcb <- renderPlot({
    pitch + stat_density2d(data=pe_fcb[pe_fcb$player_name==input$heatmapselecterfcb ,],
                           aes(x=x, y=y, fill = ..level.., alpha = ..level..),
                           size = 0.01, bins = 16, geom = 'polygon')+
      scale_fill_gradient(low = "yellow", high = "red")+
      ggtitle(paste("Radius of action by",input$heatmapselecterfcb ))+
      theme(plot.title = element_text(hjust = 0.5), legend.position="none")+xlab(" ")+ylab(" ")})
  # Heatmap for the individual players of Betis Sevilla
  output$heatmap_indiv_bs <- renderPlot({
    pitch + stat_density2d(data=pe_bs[pe_bs$player_name==input$heatmapselecterbs,],
                           aes(x=x, y=y, fill = ..level.., alpha = ..level..),
                           size = 0.01, bins = 16, geom = 'polygon')+
      scale_fill_gradient(low = "yellow", high = "red")+
      ggtitle(paste("Radius of action by",input$heatmapselecterbs ))+
      theme(plot.title = element_text(hjust = 0.5), legend.position="none")+xlab(" ")+ylab(" ")})
  # Create overall frequency of all observed events (top 15)
  observeEvent(input$events_in_game, {
    output$events_table_ov <- renderDataTable(main_events_overall)
    #toggle("events_table_ov")
  })
  
  
  
  ###################### INDIVIDUAL EVENT BARPLOTS ########################
  # barplot with success rate for each player of FC Barcelona
  observeEvent(input$eventselecter_fcb, {
    player_fcb <- pe_fcb[!is.na(pe_fcb$player_name),]
    player_fcb <- player_fcb[player_fcb$player_name==input$eventselecter_fcb,]
    
    # player_stats_fcb<-count(pe_fcb[pe_fcb$player_name=="Lionel Messi",]["event_descr"])
    player_stats_table_fcb<-count(pe_fcb[pe_fcb$player_name==input$eventselecter_fcb,]["event_descr"])
    player_stats_table_fcb<-player_stats_table_fcb[!is.na(player_stats_table_fcb$event_descr),]
    player_stats_table_fcb <- player_stats_table_fcb %>%
      arrange(desc(freq))
    player_stats_table_fcb <- player_stats_table_fcb[1:5,] # 5 most frequent events observed by the player
    
    # now we have to add the probabilities
    frequent_events_fcb<-as.character(unique(player_stats_table_fcb$event_descr))
    # loop through them to find the success rate of the player in each event
    
    
    # first create dataframe to work with, just one row, 5 columns consisting of the most often observed events
    successrate_fcb <- list()
    for (i in frequent_events_fcb){
      successrate_fcb[i] <- sum(player_fcb[player_fcb$event_descr==i,]["outcome"])/nrow(player_fcb[player_fcb$event_descr==i,])
    }
    
    successrate_fcb2<-melt(as.data.frame(successrate_fcb))
    successrate_fcb2$variable <- gsub(".", " ", as.character(successrate_fcb2$variable), fixed=TRUE)
    names(successrate_fcb2) <- c("event_descr", "success_rate")
    player_stats_table_fcb
    
    player_stats_table_fcb<-left_join(player_stats_table_fcb, successrate_fcb2, by="event_descr")
    
    # actual plotting
    output$event_player_fcb <- renderPlot({
    ggplot(data = player_stats_table_fcb, aes(x = factor(event_descr), y = freq,fill=success_rate)) + 
      geom_bar(stat = 'identity') + 
      scale_fill_gradient2(low='red', mid='snow3', high='blue4', space='Lab')+
      ggtitle(paste("Most frequent actions of",input$eventselecter_fcb))+
      xlab(paste("Most frequent events by ",input$eventselecter_fcb))+ylab("Absolute frequency of events")+
      theme(text=element_text(size=18))
    })
  })
    ######################################## INDIVIDUAL ACTION PLOTS FOR BETIS SEVILLA #############################
    # barplot with success rate for each player of BETIS SEVILLA
    observeEvent(input$eventselecter_bs, {
      player_bs <- pe_bs[!is.na(pe_bs$player_name),]
      player_bs <- player_bs[player_bs$player_name==input$eventselecter_bs,]

      # player_stats_bs<-count(pe_bs[pe_bs$player_name=="Lionel Messi",]["event_descr"])
      player_stats_table_bs<-count(pe_bs[pe_bs$player_name==input$eventselecter_bs,]["event_descr"])
      player_stats_table_bs<-player_stats_table_bs[!is.na(player_stats_table_bs$event_descr),]
      player_stats_table_bs <- player_stats_table_bs %>%
        arrange(desc(freq))
      player_stats_table_bs <- player_stats_table_bs[1:5,] # 5 most frequent events observed by the player

      # now we have to add the probabilities
      frequent_events_bs<-as.character(unique(player_stats_table_bs$event_descr))

      # loop through them to find the success rate of the player in each event
      # first create dataframe to work with, just one row, 5 columns consisting of the most often observed events
      successrate_bs <- list()
      for (i in frequent_events_bs){
        successrate_bs[i] <- sum(player_bs[player_bs$event_descr==i,]["outcome"])/nrow(player_bs[player_bs$event_descr==i,])
      }

      successrate_bs2<-melt(as.data.frame(successrate_bs))
      successrate_bs2$variable <- gsub(".", " ", as.character(successrate_bs2$variable), fixed=TRUE)
      names(successrate_bs2) <- c("event_descr", "success_rate")
      player_stats_table_bs<-left_join(player_stats_table_bs, successrate_bs2, by="event_descr")

      # actual plotting
      output$event_player_bs <- renderPlot({
        ggplot(data = player_stats_table_bs, aes(x = factor(event_descr), y = freq,fill=success_rate)) +
          geom_bar(stat = 'identity') +scale_fill_gradient2(low='red', mid='snow3', high='darkgreen', space='Lab')+
          ggtitle(paste("Most frequent actions of X",input$eventselecter_bs))+
          xlab(paste("Most frequent events by", input$eventselecter_bs))+ylab("Absolute frequency of events")+
          theme(text=element_text(size=18))
      })
    })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
