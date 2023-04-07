library(tidyverse)
library(readxl)
library(stringr)
library(rvest)
library(writexl)
library(plyr)

# Read in picks ----
picks <- read_excel("Masters_App/Picks/Masters Pool Picks 2023.xlsx")

# Read in leaderboard data ----
leaderboard_data <- read_html("https://www.espn.com/golf/leaderboard")
leaderboard_df <- leaderboard_data %>% html_node("table") %>% html_table(fill = TRUE)
leaderboard_df <- leaderboard_df %>% select(POS, PLAYER, SCORE, TODAY, THRU, R1, R2, R3, R4, TOT)

# Create long formatted picks df
picks_long <- picks %>% select(-c(8,9)) %>% gather(key = "Group", value = "Golfer", -Name)

# Possible players
golfers <- leaderboard_df$PLAYER
golfers_collapsed <- paste(golfers, collapse = " ")

# Find picked golfers
picked_golfers <- golfers[str_detect(golfers, pattern = paste0(picks_long$Golfer, collapse = "|"))]

# Detect golfers and replace with full name
picks_long$Golfer2 <- NA
for(i in 1:nrow(picks_long)){
  picks_long$Golfer2[i] <- ifelse(str_detect(golfers_collapsed, pattern = picks_long$Golfer[i]), 
                                  golfers[str_detect(golfers, pattern = picks_long$Golfer[i])],
                                  picks_long$Golfer[i])
}

# Detect golfers not accounted for
unique(picks_long$Golfer2[!(picks_long$Golfer2 %in% golfers)])

# Fill in golfers that were not accouted for
picks_long <- picks_long %>%
  mutate(
    Golfer3 = ifelse(Golfer2 == "JT", "Justin Thomas",
                     ifelse(Golfer2 == "C Smith", "Cameron Smith",
                            ifelse(Golfer2 == "C Young", "Cameron Young", 
                                   ifelse(Golfer2 == "Connors", "Corey Conners", 
                                          ifelse(Golfer2 == "Theegla", "Sahith Theegala",
                                                 ifelse(Golfer2 == "Hoon Lee", "K.H. Lee",
                                                        ifelse(Golfer2 == "Seamus", "Séamus Power", 
                                                               ifelse(Golfer2 == "A Scott", "Adam Scott",
                                                                      ifelse(Golfer2 == "DJ", "Dustin Johnson",
                                                                             ifelse(Golfer2 == "Oouthuizen", "Louis Oosthuizen", Golfer2))))))))))
  )

# check all golfers accounted for
sum(!(picks_long$Golfer3 %in% golfers))

# convert back to wide format
cleaned_picks <- picks_long %>% select(Name, Group, Golfer3) %>% spread(key = "Group", value = "Golfer3")

# Write excel to read
write_xlsx(cleaned_picks, "Masters_App/Picks/Cleaned Masters Pool Picks 2023.xlsx")

# Get number of golfers picked
cleaned_picks_long <- cleaned_picks %>% gather(key = "Group", value = "Golfer", -Name)
cleaned_picks_long$Name <- factor(cleaned_picks_long$Name)
cleaned_picks_long$Group <- factor(cleaned_picks_long$Group)
cleaned_picks_long$Golfer <- factor(cleaned_picks_long$Golfer)
popularity_df <- ddply(cleaned_picks_long, .(Golfer), summarize,
                       N = length(Golfer)) %>%
  arrange(desc(N))


# Modify picks table to show scores for each golfer
picks_list <- list()
pool_entries <- cleaned_picks$Name
for(name in pool_entries){
  temp_picks_df <- data.frame(
    Entry = c(name, rep(NA, 6)),
    Golfer = c(cleaned_picks %>% filter(Name == name) %>% select(-1) %>% unlist, "Total"),
    Score =  NA,
    `Day 1 Score` = NA,
    `Day 2 Score` = NA,
    `Day 3 Score` = NA,
    `Day 4 Score` = NA,
    `Total Score` = NA,
    check.names = FALSE
  )
  
  for(k in 1:6){
    temp_Score <- leaderboard_df %>% filter(PLAYER %in% temp_picks_df$Golfer[k]) %>% select(SCORE) %>% unlist
    temp_Day1 <- leaderboard_df %>% filter(PLAYER %in% temp_picks_df$Golfer[k]) %>% select(R1) %>% unlist
    temp_Day2 <- leaderboard_df %>% filter(PLAYER %in% temp_picks_df$Golfer[k]) %>% select(R2) %>% unlist
    temp_Day3 <- leaderboard_df %>% filter(PLAYER %in% temp_picks_df$Golfer[k]) %>% select(R3) %>% unlist
    temp_Day4 <- leaderboard_df %>% filter(PLAYER %in% temp_picks_df$Golfer[k]) %>% select(R4) %>% unlist
    temp_Total <- leaderboard_df %>% filter(PLAYER %in% temp_picks_df$Golfer[k]) %>% select(TOT) %>% unlist
    
    temp_picks_df$Score[k] = ifelse(temp_Score == "--", NA, temp_Score)
    temp_picks_df$`Day 1 Score`[k] = ifelse(temp_Day1 == "--", NA, temp_Day1)
    temp_picks_df$`Day 2 Score`[k] = ifelse(temp_Day2 == "--", NA, temp_Day2)
    temp_picks_df$`Day 3 Score`[k] = ifelse(temp_Day3 == "--", NA, temp_Day3)
    temp_picks_df$`Day 4 Score`[k] = ifelse(temp_Day4 == "--", NA, temp_Day4)
    temp_picks_df$`Total Score`[k] = ifelse(temp_Total == "--", NA, temp_Total)
  }
  
  # Account for WD
  WD_players <- which(temp_picks_df$Score %in% "WD")
  temp_picks_df$`Day 1 Score`[WD_players] <- max(leaderboard_df$R1)
  temp_picks_df$`Day 2 Score`[WD_players] <- max(leaderboard_df$R2)
  temp_picks_df$`Day 3 Score`[WD_players] <- max(leaderboard_df$R3)
  temp_picks_df$`Day 4 Score`[WD_players] <- max(leaderboard_df$R4)
  
  # Account for CUT
  CUT_players <- which(temp_picks_df$Score %in% "CUT")
  temp_picks_df$`Day 1 Score`[CUT_players] <- max(leaderboard_df$R1)
  temp_picks_df$`Day 2 Score`[CUT_players] <- max(leaderboard_df$R2)
  temp_picks_df$`Day 3 Score`[CUT_players] <- max(leaderboard_df$R3)
  temp_picks_df$`Day 4 Score`[CUT_players] <- max(leaderboard_df$R4)
  
  # Create corrected score column for data manipulation
  temp_picks_df$Corrected_Score <- c(temp_picks_df$Score[1:6], NA)
  
  # Convert Scores to usable format
  temp_picks_df$Corrected_Score <- str_replace_all(temp_picks_df$Score, pattern = "E", replacement = "0")
  temp_picks_df$Corrected_Score <- str_replace_all(temp_picks_df$Corrected_Score, pattern = "[+]", replacement = "")
  temp_picks_df$Corrected_Score <- as.numeric(temp_picks_df$Corrected_Score)
  
  # Convert scores to numeric
  temp_picks_df$`Day 1 Score` <- as.numeric(temp_picks_df$`Day 1 Score`)
  temp_picks_df$`Day 2 Score` <- as.numeric(temp_picks_df$`Day 2 Score`)
  temp_picks_df$`Day 3 Score` <- as.numeric(temp_picks_df$`Day 3 Score`)
  temp_picks_df$`Day 4 Score` <- as.numeric(temp_picks_df$`Day 4 Score`)
  temp_picks_df$`Total Score` <- as.numeric(temp_picks_df$`Total Score`)
  
  # Add up total score to table
  temp_picks_df$Score[7] <- sum(sort(temp_picks_df$Corrected_Score)[1:5])
  temp_picks_df$`Day 1 Score`[7] <- sum(sort(temp_picks_df$`Day 1 Score`)[1:5])
  temp_picks_df$`Day 2 Score`[7] <- sum(sort(temp_picks_df$`Day 2 Score`)[1:5])
  temp_picks_df$`Day 3 Score`[7] <- sum(sort(temp_picks_df$`Day 3 Score`)[1:5])
  temp_picks_df$`Day 4 Score`[7] <- sum(sort(temp_picks_df$`Day 4 Score`)[1:5])
  temp_picks_df$`Total Score`[7] <- sum(sort(temp_picks_df$`Total Score`)[1:5])
  
  picks_list[[name]] <- temp_picks_df
}

temp_picks_df <- picks_list$`Hunter Moore`
WD_players <- which(temp_picks_df$Golfer %in% "WD")
temp_picks_df$`Day 1 Score`[WD_players] <- max(leaderboard_df$R1)
temp_picks_df$`Day 2 Score`[WD_players] <- max(leaderboard_df$R2)
temp_picks_df$`Day 3 Score`[WD_players] <- max(leaderboard_df$R3)
temp_picks_df$`Day 4 Score`[WD_players] <- max(leaderboard_df$R4)

tyler_df <- picks_list$Tyler
tyler_df

paste0("Tyler", "_df") <- picks_list$Tyler
get(pool_entries[1:2])
rbind(get("Tyler"), get("AJ"))
