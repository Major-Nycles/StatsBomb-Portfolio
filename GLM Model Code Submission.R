```{r}
# Load the required packages. As this code is being written in R and not the 
# default Python as used in the course, explanations of each package used and justification for doing so is included here.

# Package used for handling rjson files.
library(rjson)
# Package used for data pre-processing. Equivalent of Pandas for Python.
library(dplyr)
# Regression and ML package. Equivalent of scikit-learn or PyCaret for Python.
library(caret)
# Graphing/visualisation package. Equivalent of matplotlib for Python.
library(ggplot2)
# Package for data wrangling
library(tidyr)

# Import the Wyscout Data
england <- fromJSON(file="./events/events_England.json")
france <- fromJSON(file="./events/events_France.json")
germany <- fromJSON(file="./events/events_Germany.json")
italy <- fromJSON(file="./events/events_Italy.json")
spain <- fromJSON(file="./events/events_Spain.json")
```

```{r}
# Get events
england.events <- data.frame(do.call(rbind,england),stringsAsFactors = FALSE)
france.events <- data.frame(do.call(rbind,france),stringsAsFactors = FALSE)
germany.events <- data.frame(do.call(rbind,germany),stringsAsFactors = FALSE)
italy.events <- data.frame(do.call(rbind,italy),stringsAsFactors = FALSE)
spain.events <- data.frame(do.call(rbind,spain),stringsAsFactors = FALSE)

all.events <- rbind(england.events,france.events,germany.events,italy.events,
                    spain.events)
```

```{r}
# Get teams
teams <- data.frame(do.call(rbind,fromJSON(file="./teams.json")),stringsAsFactors = FALSE)

teams$name <- as.character(teams$name)
teams$wyId <- as.numeric(teams$wyId)

english_teams <- teams[0,]

for(i in 1:nrow(teams)) {
        if(teams[[5]][[i]][1] == "England") {
                english_teams <- rbind(english_teams, teams[i,])
        }
}
```

```{r}
# Get players
players <- fromJSON(file = "players.json")
```

```{r}
# Get matches

england.matches <- data.frame(do.call(rbind,fromJSON(file="./matches/matches_England.json")),stringsAsFactors = FALSE)
france.matches <- data.frame(do.call(rbind,fromJSON(file="./matches/matches_France.json")),stringsAsFactors = FALSE)
germany.matches <- data.frame(do.call(rbind,fromJSON(file="./matches/matches_Germany.json")),stringsAsFactors = FALSE)
italy.matches <- data.frame(do.call(rbind,fromJSON(file="./matches/matches_Italy.json")),stringsAsFactors = FALSE)
spain.matches <- data.frame(do.call(rbind,fromJSON(file="./matches/matches_Spain.json")),stringsAsFactors = FALSE)

matches <- rbind(england.matches,france.matches,germany.matches,italy.matches,
                 spain.matches)
```

```{r}

# Unnest and collate position data

test <- all.events

test$positions <- as.data.frame(do.call(rbind, test$positions))

test$positions$V1 <- as.data.frame(do.call(rbind, test$positions$V1))
test$positions$V2 <- as.data.frame(do.call(rbind, test$positions$V2))

y1 <- test$positions$V1$y
x1 <- test$positions$V1$x
y2 <- test$positions$V2$y
x2 <- test$positions$V2$x

y1 <- as.data.frame(do.call(rbind, y1))
x1 <- as.data.frame(do.call(rbind, x1))
y2 <- as.data.frame(do.call(rbind, y2))
x2 <- as.data.frame(do.call(rbind, x2))

locations <- as.data.frame(cbind(x1, y1, x2, y2))
colnames(locations) <- c("x1","y1","x2","y2")
final <- cbind(select(test, -positions),locations)

# Select variables of interest according to SPADL format
# SPADL.select is now tidy apart from tags variable

SPADL.select <- final %>% select(matchId, 
                                 matchPeriod,
                                 eventSec,
                                 teamId,
                                 playerId,
                                 x1,y1,x2,y2,
                                 eventName,
                                 subEventName,
                                 tags)

SPADL.select$matchId <- as.numeric(SPADL.select$matchId)
SPADL.select$matchPeriod <- as.character(SPADL.select$matchPeriod)
SPADL.select$eventSec <- as.numeric(SPADL.select$eventSec)
SPADL.select$teamId <- as.numeric(SPADL.select$teamId)
SPADL.select$playerId <- as.numeric(SPADL.select$playerId)
SPADL.select$eventName <- as.character(SPADL.select$eventName)
SPADL.select$subEventName <- as.character(SPADL.select$subEventName)
```

```{r}
# Find events only conducted by liverpool
liverpool <- filter(teams, name == "Liverpool")
liverpool_Id <- liverpool$wyId

liverpool.events <- filter(SPADL.select, teamId %in% liverpool_Id)

# Find events that only occur within liverpool games
liverpool.matches <- filter(england.matches, grepl("Liverpool",label))

liverpool.match.events <- filter(SPADL.select, 
                                 matchId %in% liverpool.matches$wyId)

```

```{r warning=FALSE}
# Build Possession Chains. Possession chain starts when Liverpool gains control of the ball. Possession ends as soon as an events has been recorded by opposition. New chain starts when liverpool regain possession.


# t.liverpool.match.events <- liverpool.match.events[1:(nrow(model.data.frame)-1),]

t.liverpool.match.events <- liverpool.match.events[1:(nrow(liverpool.match.events)-1),]

# Are Liverpool in possession?
t.liverpool.match.events$in.possession <-
        lapply(t.liverpool.match.events$teamId,FUN 
               = function(x) 
                       if(x == 1612) {
                               "YES"}
               else return("NO"))

t.liverpool.match.events$in.possession <- 
        as.character(t.liverpool.match.events$in.possession)

# Is this their first possession?

t.liverpool.match.events$first_possession[1] <- "YES"

for (i in 2:nrow(t.liverpool.match.events)) {
        if(t.liverpool.match.events$in.possession[i] == "YES" &
           t.liverpool.match.events$in.possession[i-1] == "NO") {
                t.liverpool.match.events$first_possession[i] <- "YES"
        } else t.liverpool.match.events$first_possession[i] <- "NO"
}

# Assign possession number
possession_number <- 0

for (i in 1:nrow(t.liverpool.match.events)) {
        if(t.liverpool.match.events$in.possession[i] == "YES" &
           t.liverpool.match.events$first_possession[i] == "YES") {
                possession_number <- possession_number + 1
                t.liverpool.match.events$possession_count[i] <- 
                        possession_number
        } 
        if(t.liverpool.match.events$in.possession[i] == "YES" &
           t.liverpool.match.events$first_possession[i] == "NO") {
                t.liverpool.match.events$possession_count[i] <-
                        possession_number
        }
        if(t.liverpool.match.events$in.possession[i] == "NO") {
                t.liverpool.match.events$possession_count[i] <- "OPPO"
        }
}

# Find end of possession

for (i in 2:(nrow(t.liverpool.match.events)-1)) {
        if(t.liverpool.match.events$possession_count[i] == "OPPO") {
                t.liverpool.match.events$final_action[i] <- "OPPO"
        }
        if(t.liverpool.match.events$possession_count[i] != "OPPO" &
           t.liverpool.match.events$possession_count[i+1] == "OPPO") {
                t.liverpool.match.events$final_action[i] <- 1
        }
        if(t.liverpool.match.events$possession_count[i] != "OPPO" &
           t.liverpool.match.events$possession_count[i+1] != "OPPO") {
                t.liverpool.match.events$final_action[i] <- 0
        }
}

# Does an opposition shot occur within 10 events of losing possession?

for(i in 2:(nrow(t.liverpool.match.events)-10)) {
        if(t.liverpool.match.events$final_action[i] == "OPPO") {
                t.liverpool.match.events$shot[i] <- "OPPO"
        }
        if(t.liverpool.match.events$final_action[i] == 0) {
                t.liverpool.match.events$shot[i] <- 0
        }
        if(t.liverpool.match.events$final_action[i] == 1) {
                next_ten_actions <- t.liverpool.match.events[i+1:i:10,]
                opposition_actions <- filter(next_ten_actions,
                                             teamId != 1612)
                opposition_shots <- filter(opposition_actions,
                                           eventName == "Shot")
                
                if(nrow(opposition_shots) == 0) {
                        t.liverpool.match.events$shot[i] <- 0
                } 
                if(nrow(opposition_shots) > 0) {
                        t.liverpool.match.events$shot[i] <- 1
                }
        }
}


# Filter to just Liverpool events

model.data.frame <- filter(t.liverpool.match.events, teamId == 1612)

# Select required variables

model.data.frame <- model.data.frame %>% select(possession_count,
                                                x1,
                                                y1,
                                                x2,
                                                y2,
                                                shot)

# Allocate shot to rest of possession chain

for (i in 2:(nrow(model.data.frame)-10)) { 
        if(model.data.frame$shot[i] == 1) {
                current_possession <- model.data.frame$possession_count[i]
                for(j in 1:nrow(model.data.frame)) {
                        if(model.data.frame$possession_count[j] ==
                           current_possession) {
                                model.data.frame$shot[j] <- 1
                        }
                }
        }
}

# Omit last 6 actions

model.data.frame <- model.data.frame[1:(nrow(model.data.frame)-6),]
model.data.frame$shot <- as.factor(model.data.frame$shot)
```

```{r}
# Build logistic regression model on Liverpool shot conceded data

set.seed(12345)
tc <- trainControl(method = "repeatedcv", number = 10)
shot.model <- train(data = model.data.frame,
                    shot ~ x1 + y1 + x2 + y2, 
                    method = 'glm',
                    family= binomial)

try.out <- SPADL.select

# Use model on every action in all five leagues to find shot concede probability for each action

prediction <- predict(shot.model, newdata = try.out, type = "prob")

colnames(prediction) <- c("no_shot", "shot")

SPADL.final <- cbind(SPADL.select, prediction)

```

```{r}
# Sum probabilities per player

prob.player <- SPADL.final %>%
        group_by(playerId) %>%
        summarise(Total_Shot = sum(shot),
                  Mean_Shot = mean(shot),
                  events = n(),
                  mean_y_location = mean(y1),
                  Sigma_Shot = sd(shot))


# Eliminate events assigned to no player and sort by ascending

prob.player <- prob.player[-1,]

defenders <- filter(players, role$code2 == "DF")

prob.player.defenders <- filter(prob.player, playerId %in% defenders$wyId)
```

```{r}
# Filter just defenders in PL

england.players <- filter(players, currentTeamId %in% english_teams$wyId)
england.defenders <- filter(england.players, role$code2 == "DF")

prob.player.defenders.england <- filter(prob.player.defenders, playerId 
                                        %in% england.defenders$wyId)

prob.player.defenders.england <- arrange(prob.player.defenders.england, 
                                         Mean_Shot)

```

```{r}
# Assign Correct names to players

for (i in 1:nrow(prob.player.defenders.england)) {
        for (j in 1:nrow(players)) {
                if(prob.player.defenders.england$playerId[i] ==
                   players$wyId[j]) {
                        prob.player.defenders.england$name[i] <-
                                players$shortName[j]
                        prob.player.defenders.england$birthDate[i] <-
                                players$birthDate[j]
                }
        }
}
```
```{r}
# Filter out full-backs based on mean position

centre_backs <- filter(prob.player.defenders.england, mean_y_location > 30 &
                               mean_y_location < 70)

# Filter players with less than 1000 events

centre_backs <- filter(centre_backs, events >1000)

top_ten <- centre_backs[1:10,]
```

```{r}
library(kableExtra) 

top_ten$Total_Shot <- round(top_ten$Total_Shot,3)
top_ten$Mean_Shot <- round(top_ten$Mean_Shot,3)
top_ten$mean_y_location <- round(top_ten$mean_y_location,3)
top_ten$Sigma_Shot <- round(top_ten$Sigma_Shot	,3)

top_ten %>%
        kbl() %>%
        kable_classic(full_width = T, html_font = "Cambria") %>%
        save_kable("test.pdf")
```