# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # Read in data =============================================================================================
    # Read in Masters Pick
    picks_df <- read_excel("Picks/Cleaned Masters Pool Picks 2023.xlsx")
    
    # Read in Masters leaderboard from espn
    leaderboard_data <- read_html("https://www.espn.com/golf/leaderboard")
    leaderboard_df <- leaderboard_data %>% html_node("table") %>% html_table(fill = TRUE)
    leaderboard_df <- leaderboard_df %>% select(POS, PLAYER, SCORE, TODAY, THRU, R1, R2, R3, R4, TOT)
    
    # Picks Tab ================================================================================================
    # Modify picks table to show scores for each golfer
    picks_list <- list()
    pool_entries <- picks_df$Name
    for(name in pool_entries){
        temp_picks_df <- data.frame(
            Entry = c(name, rep(NA, 6)),
            Golfer = c(picks_df %>% filter(Name == name) %>% select(-1) %>% unlist, "Total"),
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
    
    # Turn list into separate data frames
    list2env(picks_list, envir = .GlobalEnv)
    
    # Picks Filter
    output$pool_entry_filter_output <- renderUI({
        pickerInput(
            inputId = "pool_entry_filter", 
            label = "Select Pool Entries to view", 
            choices = levels(factor(pool_entries)), 
            #selected = levels(factor(pool_entries)),
            options = list(pickerOptions(
                actionBox = TRUE,
                selectAllText = "Select All",
                )),
            multiple = TRUE
        )
    })
    
    # Output Picks Popularity Table
    output$pick_popularity_table <- renderRHandsontable({
        picks_long <- picks_df %>% gather(key = "Group", value = "Golfer", -Name)
        popularity_df <- ddply(picks_long, .(Golfer), summarize, N = length(Golfer)) %>% arrange(desc(N))
        rhandsontable(popularity_df,
                      scrollV = "auto",
                      stretchH = "all",
                      overflow = "visible") %>%
            hot_table(readOnly = TRUE)
    })
    
    # Output picks table
    output$picks_table <- renderRHandsontable({
        validate(
            need(length(input$pool_entry_filter) > 0, "Please select pool entry to view")
        )
        combined_picks_df <- data.frame()
        for(entry in 1:length(input$pool_entry_filter)){
            temp_pick_df <- get(input$pool_entry_filter[entry]) %>% select(-Corrected_Score)
            combined_picks_df <- rbind(combined_picks_df, add_row(temp_pick_df))
        }
        rhandsontable(combined_picks_df, 
                      rowHeaders = NULL, 
                      scrollV = "auto",
                      stretchH = "all",
                      overflow = "visible") %>%
            hot_table(readOnly = TRUE)
    })
    
    # Master Leaderboard Tab ===================================================================================
    # Allow for refresh of leaderboard
    display_leaderboard <- reactiveValues(leaderboard = leaderboard_df)
    
    observeEvent(input$masters_leaderboard_refresh, {
        leaderboard_data_new <- read_html("https://www.espn.com/golf/leaderboard")
        leaderboard_df_new <- leaderboard_data_new %>% html_node("table") %>% html_table(fill = TRUE)
        leaderboard_df_new <- leaderboard_df_new %>% select(POS, PLAYER, SCORE, TODAY, THRU, R1, R2, R3, R4, TOT)
        display_leaderboard$leaderboard <- leaderboard_df_new
    })
    
    # Display Masters leaderboard
    output$masters_leaderboard_display <- renderRHandsontable({
        rhandsontable(display_leaderboard$leaderboard,
                      overflow = "visible") %>%
            hot_table(readOnly = TRUE)
    })
    
    # Pool Leaderboard Tab =====================================================================================
    
})
