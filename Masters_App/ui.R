#### Masters Pool App
## Tyler Pollard
## 18 March 2023

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(colourpicker)
library(bs4Dash)
library(tidyverse)
library(plyr)
library(dplyr)
library(readxl)
library(readr)
library(haven)
library(ggplot2)
library(data.table)
library(plotly)
library(shinycssloaders)
library(RColorBrewer)
library(sp)
library(htmltools)
library(fresh)
library(extrafont)
library(stringr)
library(reshape2)
library(png)
library(ggpubr)
library(htmlTable)
library(tibble)
library(EnvStats)
library(xtable)
library(grid)
library(DT)
library(rhandsontable)
library(rvest)
library(stringr)

# Set theme
my_theme <- create_theme(
  theme = "paper",
  bs4dash_sidebar_dark(
    bg = "#2d3b4d"
  ),
  bs4dash_status(
    primary = "#3b4d63", info = "#507B80"
  )
)
tags$style(".buttoncolor.bttn-primary{background-color: #6399b8")

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(dark = NULL, footer = dashboardFooter(left = br()), freshTheme = my_theme,
                # ========= Dahsboard Header ===============
                header = dashboardHeader(
                  title = dashboardBrand(
                    title = div(style = "font-size:14pt",
                                align = "center", 
                                "Master's Pool",
                                dashboardBadge(
                                  color = "warning",
                                  rounded = TRUE,
                                  position = "right",
                                  "v1.0")
                    ),
                    color = "primary"
                  ),
                  compact = FALSE,
                  rightUi = tags$li(
                    class = "dropdown",
                    dropdownMenu(
                      badgeStatus = NULL,
                      type = "notifications",
                      headerText = "Master's Pool",
                      icon = icon("info-circle"),
                      notificationItem(
                        inputId = "info1",
                        text = "Developer: Tyler Pollard",
                        icon = icon("users-cog"),
                        status = "info"
                      ),
                      notificationItem(
                        inputId = "info2",
                        text = "Release Date: 6 April 2023",
                        icon = icon("calendar"),
                        status = "info"
                      ),
                      notificationItem(
                        inputId = "info3",
                        text = "Version: 1.0",
                        icon = icon("code"),
                        status = "info"
                      )
                    )
                  )
                ), # close header
                scrollToTop = TRUE,
                # ============ Dashboard Sidebar =============
                sidebar = dashboardSidebar(
                  skin = "dark",
                  elevation = 5,
                  fixed = FALSE,
                  minified = FALSE,
                  status = "primary",
                  compact = TRUE,
                  # ------------ Sidebar Menu ---------------
                  sidebarMenu(
                    id = "menu_items",
                    menuItem(text = "Home", tabName = "home", icon = icon("home")),
                    menuItem(text = "Masters Leaderboard", tabName = "masters_leaderboard"),
                    #menuSubItem(text = "Workout Options", tabName = "workout_options")
                    menuItem(text = "Pool Leaderboard", tabName = "pool_leaderboard"),
                    menuItem(text = "Picks", tabName = "picks")
                  ) # close sidebar menu
                ), # close dashboard sidebar
                # ============= Dashboard Body ================
                body = dashboardBody(
                  tags$head(tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 30;

        $("#picks_popularity").height(boxHeight - 400);
        $("#pick_popularity_table").height(boxHeight - 400);
        
        $("#picks_output").height(boxHeight - 130);
        $("#picks_table").height(boxHeight - 130);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')),
                  tabItems(
                    # ============ Home Tab ===============
                    tabItem(
                      tabName = "home",
                      jumbotron(
                        width = 12,
                        closable = FALSE,
                        collapsible = FALSE,
                        headerBorder = FALSE,
                        background = "info",
                        title = "Welcome to the Master's Pool",
                        style = "font-size: 36pt",
                        lead = "wefever.",
                        fluidRow(
                          column(6,
                                 p("This dashboard was developed by Tyler Pollard."),
                                 div("Questions, comments, or suggestions can be directed to: ",
                                     em("tpollar@g.clemson.edu")),
                                 br(),
                                 br()
                          ),
                          column(6,
                                 div(img(src = "coverpic.png", height = 400, width = 600))
                          )
                        ), # close fluidRow
                        btnName = "Contact Us",
                        href = "mailto:tpollar@g.clemson.edu"
                      ) # close jumbotron
                    ), # end home tab
                    # ============= Leaderboard Tab ===============
                    tabItem(
                      tabName = "masters_leaderboard",
                      fluidPage(
                        h1("Masters Leaderboard"),
                        hr(),
                        fluidRow(
                          column(4,
                                 actionBttn(inputId = "masters_leaderboard_refresh",
                                            label = "Refresh Leaderboard",
                                            icon = icon("arrows-rotate"))
                          ) # end column
                        ), # end fluid row
                        br(),
                        fluidRow(
                          column(12,
                                 withSpinner(rHandsontableOutput(outputId = "masters_leaderboard_display"), type = 8)
                          ) # end column
                        ) # end fluid row
                      ) # end leaderboard fluid page
                    ), # close leaderboard tab
                    # ============= Pool Leaderboard Tab ===============
                    tabItem(
                      tabName = "pool_leaderboard",
                      fluidPage(
                        h1("Pool Leaderboard"),
                        hr(),
                        tabsetPanel(id = "pool_leaderboard_tabs", 
                                    type = "pills",
                                    tabPanel(title = "Overall", value = "overall_leaderboard"
                                             
                                    ), # end overall tabPanel
                                    tabPanel(title = "Round 1", value = "R1_leaderboard"
                                             
                                    ), # end overall tabPanel
                                    tabPanel(title = "Round 2", value = "R2_leaderboard"
                                             
                                    ), # end overall tabPanel
                                    tabPanel(title = "Round 3", value = "R3_leaderboard"
                                             
                                    ), # end overall tabPanel
                                    tabPanel(title = "Round 4", value = "R4_leaderboard"
                                             
                                    ) # end overall tabPanel
                        ) #end tabsetPanel
                      ) # end leaderboard fluid page
                    ), # close leaderboard tab
                    # ============= Picks Tab ===============
                    tabItem(
                      tabName = "picks",
                      fluidPage(
                        h1("Picks"),
                        hr(),
                        fluidRow(
                          column(3,
                                 fluidRow(
                                   box(title = "Picks Filters", id = "picks_filter", width = 12, status = "primary", solidHeader = TRUE,
                                       uiOutput(outputId = "pool_entry_filter_output")
                                   ) # end box
                                 ), #end fluid Row
                                 fluidRow(
                                   box(title = "Pick Popularity", id = "picks_popularity", width = 12, status = "primary", solidHeader = TRUE,
                                       style = 'overflow-y: scroll;',
                                       withSpinner(rHandsontableOutput(outputId = "pick_popularity_table"), type = 8)
                                   ) # end box
                                 ) #end fluid Row
                          ), # end column
                          column(9,
                                 box(title = "Picks", id = "picks_output", width = 12, status = "primary", solidHeader = TRUE,
                                     style = 'overflow-y: scroll;',
                                     withSpinner(rHandsontableOutput(outputId = "picks_table"), type = 8)
                                     
                                 ) # end box
                          ) # end column
                        ) #end fluid Row
                      ) # end picks fluid page
                    ) # close picks tab
                  ) # end Tab items
                ) # end dashboard body
  ) # end dashboard Page
) # end ShinyUI






