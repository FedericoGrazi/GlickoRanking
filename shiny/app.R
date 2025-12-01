library(tidyverse)
library(ggridges)
library(ggthemes)
library(ggdark)
library(ggtext)
library(plotly)
library(shiny)
library(shinythemes)
library(shinyBS)
library(DT)
library(shinydashboard)
library(emayili)
library(writexl)
library(readxl)

source("ranking_function_shiny.R", local = TRUE)

upd_text <- paste0("Last Update: ", Sys.Date(), ". Added 1.Swiss Liga 2024, 1.Bundesliga Matchday 2, RBWL Lovain-La-Neuve 2025, some Canadian Tournaments. Reparametrized value of c for Squad Competitions with few matches per player.")


ui <- dashboardPage(
  dashboardHeader(title = "EU Ranking Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Nation Rankings", tabName = "dashboard"),
    menuItem("Player Details", tabName = "player_details"),
    menuItem("Tournament Details", tabName = "tournament_details"),
    menuItem("Glicko Calculator", tabName = "calculator")
    ,menuItem("Report Wrong Info", tabName = "report")
  )),
  dashboardBody(tabItems(
    tabItem(
      tabName = "dashboard",
      
      # Pagina 1 ####
      fluidPage(
        titlePanel("Nation Wise Dashboard"),
        fluidRow(column(12, textOutput("curUpdate"))),
        ## Riga 1.1 - Filter ####
        fluidRow(
          column(
            width = 5,
            style = "padding-left: 20px; padding-top: 30px;",
            h3("Top Players"),
            ### Colonna 1.1.1 - Filter Nation ####
            fluidRow(
              column(
                width = 6,
                selectInput(
                  "nation_filter",
                  "Select Nation:",
                  choices = c(
                    "all",
                    "Europe Only",
                    'DE',
                    'IT',
                    'BE',
                    'SUI',
                    'FR',
                    'AU',
                    'CA',
                    'US',
                    'EN',
                    'SWE',
                    'CZE',
                    'SP',
                    'DEN',
                    'NED',
                    'POL',
                    'LAT',
                    'AUS',
                    'ROM',
                    'BRA',
                    'NOR',
                    'LIT',
                    'FIN',
                    'TAI'
                  ),
                  selected = "Europe Only"
                )
              ),
              ### Colonna 1.1.2 - Gender Filter ####
              column(
                width = 6,
                radioButtons(
                  "gender_filter",
                  "Filter by gender:",
                  choices = c("Men" = "M", "Female" = "F"),
                  selected = "M"
                )
              )
            ),
            ## Colonna 1.1 - Tabella #####
            style = "width:100%;",
            dataTableOutput("topPlayers"),
            downloadButton("download_ranking", "Download Ranking", class = "btn-primary")
          )
        ),
        ## Riga 1.2 ####
        fluidRow(
          column(
            width = 6,
            h4("Scatter Plot"),
            fluidRow(column(
              width = 6,
              selectInput(
                "scatter_x",
                "Select X Variable:",
                choices = c(
                  "Rating",
                  "Win",
                  "WinPct",
                  "Loss",
                  "LossPct",
                  "Draw",
                  "DrawPct",
                  "Games",
                  "nTourn",
                  "PeakElo"
                ),
                selected = "Win"
              )
            ), column(
              width = 6,
              selectInput(
                "scatter_y",
                "Select Y Variable:",
                choices = c(
                  "Rating",
                  "Win",
                  "WinPct",
                  "Loss",
                  "LossPct",
                  "Draw",
                  "DrawPct",
                  "Games",
                  "nTourn",
                  "PeakElo"
                ),
                selected = "Rating"
              )
            )),
            
            plotlyOutput("scatter_plot", width = "100%")
          ),
          column(
            width = 5,
            h4("Bar Plot of Gender"),
            plotOutput("gender_plot", width = "100%", height = "500px")
          )
        )
      )
    ),
    tabItem(
      tabName = "player_details",
      # Pagina 2 ####
      fluidPage(
        # Riga 2.1 - Player Select ####
        fluidRow(
          column(
            4,
            tags$head(tags$style(
              HTML(
                "
      .fake-link {
        color: #007BFF;
        text-decoration: underline;
        cursor: pointer;
      }
    "
              )
            )),
            selectizeInput(
              "player_select",
              "Select Player:",
              choices = NULL,
              options = list(placeholder = 'Select a player')
            )
          ),
          column(
            2,
            actionButton("show_summary", "Show Odds Summary", icon = icon("chart-bar"))
          ),
          column(
            2,
            checkboxInput("show_package_dates", "Show packages date", value = FALSE)
          ),
          column(
            2,
            checkboxInput("show_nation", "Show Other Players from Same Country", value = FALSE)
          ),
          column(
            2,
            selectizeInput( "compare_player",label = NULL,  choices = NULL, options = list(placeholder = 'Choose a player to compare'), selected = NULL )
          )
        
        ),
        # Riga 2.2 - Player Info ####
        fluidRow(
          theme = shinytheme("cyborg"),
          # Colonna 2.2.1 - Tabella Info Individuali ####
          column(4, DTOutput("info")),
          
          # Colonna 2.2.2 - Glicko Plot ####
          column(
            8,
            div(
              # style = "position: relative;",
              style = "overflow-x: auto; overflow-y: hidden; height: 600px; width: 100%; border: 1px solid #ccc;",
              plotlyOutput("glicko_plot", height = "600px", width  = "600px")
            )
          )
        ),
        ## Riga 2.3 - Tournament Select ####
        fluidRow(column(
          4,
          selectizeInput(
            "tournament_select",
            "Select Tournament:",
            choices = NULL,
            options = list(placeholder = 'Select a tournament')
          )
        )),
        ### Colonna 2.3.1 - Tournament Data ####
        fluidRow(column(11, DTOutput("player_data"))),
        ## Riga 2.4 ####
        fluidRow(
          style = "width:100%;",
          
          ### Colonna 2.4.1 - Summary Opponent ####
          column(11, h4("Most Played Against"), DTOutput("opponent"))
        )
        
      )
    ),
    # Pagina 3 ####
    tabItem(tabName = "tournament_details", fluidPage(
      fluidRow(column(
        4,
        tags$head(tags$style(
          HTML(
            "
      .fake-link {
        color: #007BFF;
        text-decoration: underline;
        cursor: pointer;
      }
    "
          )
        )),
        selectizeInput(
          inputId = "selected_tournament",
          "Select a Tournament:",
          choices = NULL,
          options = list(placeholder = 'Select a Tournament')
        )
      ), column(6, mainPanel(
        # condition = "input.tournament_tabs == 'history' || input.tournament_tabs == 'impr'",
        selectizeInput(
          inputId = "selected_division",
          "Select a Division:",
          choices = NULL,
          options = list(placeholder = 'Select a Division')
        )
      ))), fluidRow(column(
        12,
        tabBox(
          title = "",
          width = 8,
          id = "tournament_tabs",
          # Make sure you have unique id
          # tabPanel("Tournament History", value = "history",
          #          DTOutput("history_table")),
          tabPanel("Improvement", value = "impr", DTOutput("impr_table")),
          tabPanel("Opponents Ratings", value = "opp", DTOutput("opp_table"))
        )
      ))
    )),
    # Pagina 4 - Calculator Page####
    tabItem(
      tabName = "calculator",
      
      # Radio button for choosing calculation type (Team or Players)
      fluidRow(
        column(width = 2, 
               radioButtons("calc_team_player", "Calculate By:", 
                            choices = c("Team", "Players"), selected = "Team")
        )
      ),
      
      # Conditional Panel for Team Calculations
      conditionalPanel(
        condition = "input.calc_team_player == 'Team'",
        fluidRow(
          column(width = 4, 
                 numericInput("t1_rat", "Team A Rating", value = 1200, min = 700, max = 4000)
          ),
          column(width = 4, 
                 numericInput("t2_rat", "Team B Rating", value = 1200, min = 700, max = 4000)
          )
        ),
        
        fluidRow(
          column(width = 4, 
                 numericInput("t1_dev", "Team A Deviation", value = 100, min = 0, max = 350)
          ),
          column(width = 4, 
                 numericInput("t2_dev", "Team B Deviation", value = 100, min = 0, max = 350)
          )
        )
      ),
      
      # Conditional Panel for Player Calculations
      conditionalPanel(
        condition = "input.calc_team_player == 'Players'",
        fluidRow(
          column(width = 3, 
                 numericInput("p1_rat", "Player A1 Rating", value = 1200, min = 700, max = 4000)
          ),
          column(width = 3, 
                 numericInput("p2_rat", "Player A2 Rating", value = 1200, min = 700, max = 4000)
          ),
          column(width = 3, 
                 numericInput("p3_rat", "Player B1 Rating", value = 1200, min = 700, max = 4000)
          ),
          column(width = 3, 
                 numericInput("p4_rat", "Player B2 Rating", value = 1200, min = 700, max = 4000)
          )
        ),
        
        fluidRow(
          column(width = 3, 
                 numericInput("p1_dev", "Player A1 Deviation", value = 100, min = 0, max = 350)
          ),
          column(width = 3, 
                 numericInput("p2_dev", "Player A2 Deviation", value = 100, min = 0, max = 350)
          ),
          column(width = 3, 
                 numericInput("p3_dev", "Player B1 Deviation", value = 100, min = 0, max = 350)
          ),
          column(width = 3, 
                 numericInput("p4_dev", "Player B2 Deviation", value = 100, min = 0, max = 350)
          )
        ),
        
        # Button for selecting players from the list or manual entry
        fluidRow(
          column(width = 12, 
                 actionButton("player_from_list", "Select Players from List")
          )
        )
        
      ),
      
      # Game Outcome Selection
      fluidRow(
        column(width = 3, 
               selectInput("score_", "Game Outcome", 
                           choices = c("Team A Win (2:0)" = 1, "Team A Win (2:1)" = 0.75, "Draw (1:1)" = 0.5, 
                                       "Team A Loss (1:2)" = 0.25, "Team A Loss (0:2)" = 0))
        )
      ),
      
      
      fluidRow(
        column(width = 2, 
               actionButton("calculate", "Calculate")
        ),
        column(width = 5, 
               textOutput("matchup_text")
        )
      ),
      
      # Table output to display the result
      fluidRow(
        column(width = 6, 
               DTOutput("calculator_outA")
        ),
        
        column(width = 6, 
               DTOutput("calculator_outB")
        )
      )
    ),
    # Pagina 5 - Report####
    tabItem(
      tabName = "report",
      titlePanel("Use this section to report wrong information"),
      fluidRow(column(4, selectizeInput(
        "player_report",
        "Select Player:",
        choices = NULL,
        options = list(placeholder = 'Select a player')
      )),
      column(4, selectInput("what_report", "What do you want to Report?", choices = c(" " = "n", "Wrong Personal Info" = "i", "Wrong Score" = "s","Wrong Opponent" = "p","Other" = "o")))),
      conditionalPanel(
        condition = 'input.what_report == "i"' ,
        fluidRow(
          column(4, selectInput("report_info_gender", "Report Correct Gender:", choices = c("Male" = "M", "Female" = "F"), selected = NULL)),
          column(4, selectInput("report_info_nationality", "Report Correct Nationality:", choices = c( 'DE',
                                                                                                       'IT',
                                                                                                       'BE',
                                                                                                       'SUI',
                                                                                                       'FR',
                                                                                                       'AU',
                                                                                                       'CA',
                                                                                                       'US',
                                                                                                       'EN',
                                                                                                       'SWE',
                                                                                                       'CZE',
                                                                                                       'SP',
                                                                                                       'DEN',
                                                                                                       'NED',
                                                                                                       'POL',
                                                                                                       'LAT',
                                                                                                       'AUS',
                                                                                                       'ROM',
                                                                                                       'BRA',
                                                                                                       'NOR',
                                                                                                       'LIT',
                                                                                                       'FIN',
                                                                                                       'TAI'), selected = NULL))
        )
      ),
      conditionalPanel(
        condition = 'input.what_report == "s"',
        fluidRow(column(4, selectizeInput(
          "tournament_report",
          "Select Tournament:",
          choices = NULL,
          options = list(placeholder = 'Select a Tournament'))
        ),
        column(4, selectizeInput("game_report", "Which Game?",width = "110%" ,choices = NULL,options = list(placeholder = 'Select a Game') )),
        column(2, selectInput("score_correct", "Correct Score?",choices = c(" ", "2-1", "2-0", "1-0", "1-1", "0-1", "0-2", "1-2"))))),
      conditionalPanel(
        condition = 'input.what_report == "p"',
        fluidRow(column(2, selectizeInput(
          "tournament_report2",
          "Select Tournament:",
          choices = NULL,
          options = list(placeholder = 'Select a Tournament'))
        ),
        column(4, selectizeInput("game_report2", "Which Game?",width = "110%" ,choices = NULL,options = list(placeholder = 'Select a Game') )),
        column(4, textInput("opp_correct", "Correct Opponent?")))),
      conditionalPanel(
        condition = 'input.what_report == "o"',
        fluidRow(
          column(12, textInput("other_wrong", "Write the error you found. Always mention in which panel you found the error.")))),
      # conditionalPanel(
      #   condition = 'input.what_report != "n"',
      #   fluidRow(
      #     column(5, actionButton("send_mail", "Send Report!", icon = icon("share")))))
      
      conditionalPanel(
        condition = 'input.what_report != "n"',
        fluidRow(
          column(4, textInput("user_email", "Your Email (optional, for follow-up):"))),
        fluidRow(
          column(4, actionButton("send_mail", "Send Report!", icon = icon("share")))
        ))
      
      
    )
    
  
    
  )))


  
# Server ####

server <- function(input, output, session) {
  # 0 - Reactive ####
  
  lastrat <- reactive({
    NatData <- read_excel("www/NatDatabase.xlsx")[, c("giocatore", "Sesso", "Nazione")] %>% filter(!duplicated(giocatore))
    NatData$giocatore <- stringi::stri_trans_general(str_to_title(NatData$giocatore), "Latin-ASCII")
    rat <- rsList()[[length(rsList())]]$ratings
    
    rat <- 
      left_join(rat, NatData, by = "giocatore") %>%
      mutate(LastPen = as.Date(ifelse(
        LastPen == 0, NA_Date_, date_from_weeks(LastPen)
      )),
      Deviation = round(pmin(sqrt(
        Deviation^2 + 14^2 * (
          weeks_passed(date = as.Date(today())) - weeks_passed(date = as.Date(LastTourn))
        )
      ), 200)))
    
    rat
  })
  
  df <- reactive({
   
    lastT <-  weeks_passed(year(today()), month(today()), day(today())) -
      104
    rat <- subset(lastrat(), nTourn > 2 &
                    Games > 15 & LastTourn > lastT)
    
    
    rat
  })
  
  rsList <- reactive({
    readRDS("www/rsList.RDS")
  })
  
  rat <- reactive({
    rsList()[[length(rsList())]]$ratings
  })
  
  archive <- reactive({
    readRDS("www/Archive.RDS")
  })
  
  percentages <- reactive({
    readRDS("www/Percentages.RDS")
  })
  
  NamesDatabase <- reactive({
    read_excel("www/NatDatabase.xlsx")
  })
  
  
  selected_data <- reactive({
    req(input$selected_tournament)
    rsList()[[input$selected_tournament]]
  })
  
  opp_classifica <- reactive({
    req(input$selected_division, input$selected_tournament)
    tournament_data <- selected_data()
    division <- input$selected_division
    classifica(rs = selected_data(), role = "classifica")[[division]]
  })
  
  dft <- reactive({
    readRDS("www/DataBaseOutput.RDS")
  })
  
  ratinghistr <- reactive({
    readRDS("www/RatingHistory.RDS")
  })
  
  s <- reactive({
    req(input$player_select)
    summary_opponents(input$player_select, perch = percentages(), nmin = 0)
    
  })
  
  
  scatter_df <- reactive({
    readRDS("www/EU_dataset.RDS") %>%
      mutate(
        WinPct = Win / Games,
        LossPct = Loss / Games,
        DrawPct = Draw / Games
      ) %>%
      filter(Sesso == input$gender_filter)
  })
  
  
  
  
  # 0 - Selecticize Input ####

  # Player Select
  observe({
    updateSelectizeInput(
      session,
      "player_select",
      choices = unique(names(archive())),
      selected = "",
      server = TRUE
    )
  })

  observe({
    updateSelectizeInput(
      session,
      "player_report",
      choices = unique(names(archive())),
      selected = "",
      server = TRUE
    )
  })
  
  # Compare Player
  
  
  observe({ 
    req(input$player_select) 
    updateSelectizeInput( session, 
                          "compare_player",
                          choices = setdiff(unique(names(archive())),input$player_select), 
                          selected = " ",
                          server = TRUE ) })
  # Tournament Select
  observeEvent(input$player_select, {
    req(input$player_select)
    player_tournaments <- names(archive()[[input$player_select]])
    
    updateSelectizeInput(
      session,
      "tournament_select",
      choices = player_tournaments,
      selected = "",
      server = TRUE
    )
  })
  observeEvent(input$player_report, {
    req(input$player_report)
    player_tournaments <- names(archive()[[input$player_report]])
    
    updateSelectizeInput(
      session,
      "tournament_report",
      choices = player_tournaments,
      selected = "",
      server = TRUE
    )
  })
  observeEvent(input$tournament_report, {
    req(input$tournament_report)
    tournaments_games <- archive()[[input$player_report]][[input$tournament_report]]
    tournaments_games <- tournaments_games[1:(which(str_detect(tournaments_games$opp1, ":"))-1),]
    tg <- 
      paste0(
        gsub("\\s*\\([^\\)]*\\)", "", tournaments_games$Score), " vs ",
        gsub("\\s*\\([^\\)]*\\)", "", tournaments_games$opp1), " & ",
        gsub("\\s*\\([^\\)]*\\)", "", tournaments_games$opp2)
      )
    
    updateSelectizeInput(
      session,
      "game_report",
      choices = tg,
      selected = "",
      server = TRUE
    )
  })
  observeEvent(input$player_report, {
    req(input$player_report)
    player_tournaments <- names(archive()[[input$player_report]])
    
    updateSelectizeInput(
      session,
      "tournament_report2",
      choices = player_tournaments,
      selected = "",
      server = TRUE
    )
  })
  observeEvent(input$tournament_report2, {
    req(input$tournament_report2)
    tournaments_games <- archive()[[input$player_report]][[input$tournament_report2]]
    tournaments_games <- tournaments_games[1:(which(str_detect(tournaments_games$opp1, ":"))-1),]
    tg <- 
      paste0(
        gsub("\\s*\\([^\\)]*\\)", "", tournaments_games$Score), " vs ",
        gsub("\\s*\\([^\\)]*\\)", "", tournaments_games$opp1), " & ",
        gsub("\\s*\\([^\\)]*\\)", "", tournaments_games$opp2)
      )
    
    updateSelectizeInput(
      session,
      "game_report2",
      choices = tg,
      selected = "",
      server = TRUE
    )
  })
  
  # Detailed Opponent
  observe({
    updateSelectizeInput(
      session,
      "detail_opp",
      choices = s()[, 1],
      selected = "",
      server = TRUE
    )
  })
  
  # Tourn Opponent
  
  
  observe({
    t <- names(rsList())
    updateSelectizeInput(
      session,
      "selected_tournament",
      choices = t,
      selected = "",
      server = TRUE
    )
  })
  
  
  # 1 - Text ####
  output$curUpdate <- renderText(upd_text)
  # 1 - Table ####
  output$topPlayers <- renderDataTable({
    top_nations <- df() %>%
      count(Nazione, sort = TRUE) %>%
      slice_head(n = 10) %>%
      filter(!is.na(Nazione), Nazione != "0") %>%
      pull(Nazione)
    
    all_nation <- df() %>% pull(Nazione) %>% unique
    
    top_df <- df() %>%
      filter(Sesso == input$gender_filter) %>%
      arrange(desc(Rating)) %>%
      mutate(`Win%` = Win / Games) %>%
      relocate(`Win%`, .after = Win) %>%
      select(-Sesso)
    
    
    if (!input$nation_filter %in% c("all", "Europe Only")) {
      top_df <- top_df %>% filter(Nazione == input$nation_filter) %>% select(-Nazione) %>% arrange(-Rating)
    }
    if (input$nation_filter == "Europe Only") {
      top_df <- top_df %>% filter(!Nazione %in% c("US", "CA", "AUS", "TAI", "JAP", "CHI", "BRA", "MEX")) %>% arrange(-Rating)
    }
    if (input$nation_filter == "all") {
      top_df <- top_df %>% arrange(-Rating)
    }
    
    
    
    datatable(
      top_df,
      width   = "100%"      ,
      options = list(
        pageLength = 25,
        escape = FALSE,
        paging = TRUE,
        scrollX = TRUE,
        searching = TRUE
      )
    ) %>%
      formatPercentage(columns = "Win%", digits = 1)
  })
  
  
  # # 1 - Density Plot ####
  # output$nationPlot <- renderPlot({
  #   req(df())
  #
  #   plot_df <- df()
  #
  #   # Top N nations by frequency
  #   top_nations <- plot_df %>%
  #     filter(Nazione != "0") %>%
  #     filter(Sesso == input$gender_filter) %>%
  #     count(Nazione, sort = TRUE) %>%
  #     slice_head(n = as.numeric(input$top_n)) %>%
  #     pull(Nazione)
  #
  #   # Always include selected nation if not already there
  #   if (input$nation_filter != "all" &&
  #       !(input$nation_filter %in% top_nations)) {
  #     top_nations <- c(top_nations, input$nation_filter)
  #   }
  #
  #   # Filter data for only those nations
  #   plot_df <- plot_df %>%
  #     filter(Nazione %in% c(top_nations, input$nation_filter))
  #
  #   plot_df <- plot_df %>%
  #     mutate(
  #       color_flag = case_when(
  #         input$nation_filter != "all" &
  #           Nazione == input$nation_filter ~ "Selected",
  #         input$nation_filter != "all" &
  #           Nazione != input$nation_filter ~ "Other",
  #         TRUE ~ Nazione
  #       ),
  #       alpha_flag = case_when(
  #         input$nation_filter != "all" & Nazione == input$nation_filter ~ .8,
  #         input$nation_filter != "all" &
  #           Nazione != input$nation_filter ~ .4,
  #         TRUE ~ 1
  #       )
  #     )
  #
  #   plot_df <- plot_df %>%
  #     group_by(Nazione) %>%
  #     summarize(sample_size = n()) %>%
  #     arrange(desc(sample_size)) %>%
  #     left_join(plot_df, by = "Nazione") %>%
  #     mutate(
  #       `Win%` = Win / Games,
  #       `Loss%` = Loss / Games,
  #       `Draw%` = Draw / Games
  #     )
  #
  #   # Now create the ggplot
  #   p <- ggplot(plot_df, aes(
  #     x = .data[[input$graph_variable]],
  #     y = reorder(Nazione, sample_size),
  #     fill = color_flag
  #   ))
  #
  #   if (input$plot_type == "Boxplot") {
  #     p <- p +
  #       geom_boxplot(alpha = 0.7, show.legend = FALSE)
  #   } else if (input$plot_type == "Density") {
  #     p <- p +
  #       geom_density_ridges(
  #         scale = 1.2,
  #         show.legend = FALSE,
  #         rel_min_height = 0.001
  #       )
  #
  #   }
  #
  #   if (input$nation_filter != "all") {
  #     p <- p + scale_fill_manual(values = c(
  #       "Selected" = "firebrick1",
  #       "Other" = "grey80"
  #     ))
  #   }
  #
  #
  #   p + theme_ridges(font_size = 20) + theme(legend.position = "none") +
  #     ylab("Nation")
  # }, bg = "transparent")
  #
  #
  # 1 - Gender Bar Plot ####
  output$gender_plot <- renderPlot({
    if (!input$nation_filter %in% c("all", "Europe Only")) {
      plot_gen <- df() %>% filter(Nazione == input$nation_filter)
    } else if (input$nation_filter == "Europe Only") {
      plot_gen <- df() %>% filter(!Nazione %in% c("US", "CA", "AUS", "TAI", "JAP", "CHI", "BRA", "MEX"))
    } else{
      plot_gen <- df()
    }
    
    t <- table(plot_gen$Sesso)
    
    pfull <- df()
    tfull <- table(pfull$Sesso)
    tfull <- tfull / sum(tfull) * sum(t)
    
    barplot(t, ylim = c(0, round(max(t, tfull) * 1.1)))
    
    
    graphics::text(
      paste(round(t[["M"]] / sum(t), 2) * 100, "%", sep = ""),
      x = 1.9,
      y = t[["M"]] * 1.05,
      offset = 0,
      cex = 1.4
    )
    graphics::text(
      paste(round(t[["F"]] / sum(t), 2) * 100, "%", sep = ""),
      x = .7,
      y = t[["F"]] * 1.2,
      offset = 0,
      cex = 1.4
    )
    
    if (input$nation_filter != "all") {
      points(
        pch = 19,
        x = .7,
        y = tfull[["F"]],
        col = "red"
      )
      points(
        pch = 19,
        x = 1.9,
        y = tfull[["M"]],
        col = "red"
      )
      legend(
        "topleft",
        pch = 19,
        col = "red",
        legend = "% Average",
        bty = "n",
        text.col = "red",
        cex = .9
      )
    }
    
  }, bg = "transparent", res = 96)
  
  
  
  # 1 - Scatter Plot ####
  output$scatter_plot <- renderPlotly({
    k <- ""
    if (!input$nation_filter %in% c("all", "Europe Only")) {
      df <- scatter_df()  %>% filter(Nazione == input$nation_filter)
      k <- paste("of Nation", input$nation_filter)
    } else if (input$nation_filter == "Europe Only") {
      df <- scatter_df()  %>% filter(!Nazione %in% c("US", "CA", "AUS", "TAI", "JAP", "CHI", "BRA", "MEX"))
      k <- paste("of", input$nation_filter)
    } else{
      df <- scatter_df()
    }
    
    g <- ifelse(input$gender_filter == "M", "Male", "Female")
    
    
    p <-
      df %>%
      mutate(across(contains("Pct"), ~ round(., 3) * 100)) %>%
      ggplot(aes(
        x = .data[[input$scatter_x]],
        y = .data[[input$scatter_y]],
        text = paste(
          "Player:",
          giocatore,
          paste0("<br>", input$scatter_x, ":"),
          .data[[input$scatter_x]],
          paste0("<br>", input$scatter_y, ":"),
          .data[[input$scatter_y]]
        )
      )) +
      geom_point(size = 0.9, col = "black") +
      ggtitle(paste("Scatter Plor for ", g, " Athletes ", k, sep = "")) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>%
      layout(plot_bgcolor  = "transparent", paper_bgcolor = "transparent")
    
    
  })
  
  
  # 1 - Download Button ####
  output$download_ranking <- downloadHandler(
    filename = function() {
      paste("GlickoRanking-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(df(), path = file)
    }
  )
  
  
  # 2 - Tournamet Arch ####
  output$player_data <- renderDT({
    req(input$player_select)
    
    data <- archive()[[input$player_select]]
    
    if (!is.null(input$tournament_select) &&
        input$tournament_select != "") {
      data <- data[[input$tournament_select]]
      data$Points <- as.numeric(gsub("Tot:", "", data$Points))
      datatable(
        data ,
        options = list(
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          scrollX = TRUE
        ),
        rownames = FALSE,
        caption = "Tournament Infos"
      ) %>%
        formatStyle(
          columns =  'Points',
          # Specify the column to format
          color  = styleInterval(
            c(-Inf, 0, Inf),
            # Define the ranges
            c('grey', 'red', "grey20", "limegreen")  # Corresponding colors
          )
        )
    } else{
      data <- NULL
      datatable(
        data ,
        options = list(
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          scrollX = TRUE
        ),
        rownames = FALSE,
        caption = "Tournament Infos"
      )
    }
    
    
    
    
  })
  
  
  # 2 - Info Table  ####
  output$info <- renderDT({
    req(input$player_select)
    
    
    full <- lastrat() %>%
      filter(giocatore == input$player_select) %>%
      mutate(Sex = NamesDatabase()$Sesso[which(giocatore == NamesDatabase()$giocatore)],
             Nationality = NamesDatabase()$Nazione[which(giocatore == NamesDatabase()$giocatore)]) %>%
      select(-Sesso, -Nazione)
    
    if (any(is.na(full$Sex))) {
      full[, c("Sex", "Nationality")] <- NamesDatabase()[match(input$player_select, NamesDatabase()$giocatore), c("Sesso", "Nazione")]
    }
    
    
    a <- full %>%
      mutate(
        Results = paste(Win, Draw, Loss, sep = "-"),
        Percentile = paste0(Percentile, "%")
      ) %>%
      relocate(Results, .before = Games) %>%
      select(-Win, -Loss, -Draw)
    
    
    b <-  tibble(colnames(a), t(a))
    b[4, 1] <- "Results (W-D-L)"
    b[1, 1] <- "Name"
    
    
    colnames(b) = c("Info", "Value")
    
    
    
    datatable(
      b,
      options = list(
        dom = 't',
        # hide table controls
        paging = FALSE,
        ordering = FALSE,
        searching = FALSE,
        columnDefs = list(list(
          className = 'dt-left', targets = "_all"
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover compact',
      style = "bootstrap"
    ) %>%
      formatStyle('Info', fontWeight = 'bold' ) %>%
      formatStyle('Value', target = 'cell', fontSize = '15px')
    
  })
  
  
  # 2 - Summary Opponent #####
  output$opponent <- renderDT({
    datatable(
      s() %>%
        mutate(
          Giocatore = paste0("<span class='fake-link'>", Giocatore, "</span>")
        ) %>%
        rename(Opponent = Giocatore, `Games Against` = PartiteContro),
      escape = FALSE,
      selection = "single",
      option = list(searching = TRUE, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # 2 - Detailed Matches ####
  observeEvent(input$opponent_rows_selected, {
    row_index <- input$opponent_rows_selected
    if (!is.null(row_index)) {
      clicked_player <- s()[row_index, 1]
      selected_player <- input$player_select
      
      # Find matches
      match_data <- find_matchups(selected_player,
                                  clicked_player,
                                  perch = percentages(),
                                  arch = archive())
      
      
      match_data$Date <- dft()$date[match(match_data$Tournament, dft()$TourName)]
      match_data$`Opponent Rating` <- floor(match_data$`Opponent Rating` /
                                              2)
      match_data$`Team Rating` <- floor(match_data$`Team Rating` / 2)
      
      
      match_data <- match_data %>%
        rename(Points = Punti) %>%
        relocate(Date, .after = "Tournament")
      
      match_data <- match_data[order(match_data$Date, decreasing = T), ]
      # Show modal popup with match data
      showModal(modalDialog(
        title = paste(
          "Matches between",
          selected_player,
          "and",
          clicked_player
        ),
        DTOutput("match_table"),
        size = "l",
        easyClose = TRUE
      ))
      
      # Render the match table inside the modal
      output$match_table <- renderDT({
        datatable(match_data,
                  option = list(scrollX = TRUE),
                  rownames = FALSE) %>%
          formatStyle(
            columns =  'Points',
            # Specify the column to format
            color  = styleInterval(
              c(-Inf, 0, Inf),
              # Define the ranges
              c('grey', 'red', "grey20", "limegreen")  # Corresponding colors
            )
          )
      })
    }
  })
  
  
  
  
  
  # 2 - Glicko Plot #####
  output$glicko_plot <- renderPlotly({
    req(input$player_select)
    
    glicko_graphs(
      input$player_select,
      second.player = input$compare_player,
      arch = archive(),
      ratinghistr = ratinghistr(),
      eug = lastrat(),
      dft = dft(),
      packages = input$show_package_dates,
      nation = input$show_nation
    )
    
    
    
  })
  
  
  # 2 - Odds ####
  observeEvent(input$show_summary, {
    req(input$player_select)
    
    # Call your function with selected player and percentages
    summary_table <- summary_odds(input$player_select, perch = percentages())
    summary_table <- summary_table[, -which(colnames(summary_table) %in% c("Result", "Weight"))]
    colnames(summary_table) <- c(
      "Type",
      "Partner",
      "Opponent Team",
      "Opponent Rating",
      "Pre-Game Odds",
      "Result",
      "Team Rating",
      "Tournament"
    )
    summary_table <- summary_table[, c(
      "Type",
      "Partner",
      "Team Rating",
      "Opponent Team",
      "Opponent Rating",
      "Pre-Game Odds",
      "Result",
      "Tournament"
    )]
    
    summary_table$`Opponent Rating` <- floor(summary_table$`Opponent Rating` /
                                               2)
    summary_table$`Team Rating` <- floor(summary_table$`Team Rating` / 2)
    # Show the modal
    showModal(modalDialog(
      title = paste("Summary Odds for", input$player_select),
      DTOutput("odd_table"),
      size = "l",
      easyClose = TRUE
    ))
    
    # Render the match table inside the modal
    output$odd_table <- renderDT({
      result_colors <- ifelse(
        str_detect(summary_table$Result, regex("win", ignore_case = TRUE)),
        "green",
        ifelse(str_detect(
          summary_table$Result, regex("loss", ignore_case = TRUE)
        ), "red", "grey")
      )
      
      datatable(
        summary_table,
        options = list(
          scrollX = TRUE,
          ordering = FALSE,
          searching = FALSE,
          paging = FALSE
        ),
        rownames = FALSE
      )
    })
  })
  
  # 3 - Select Tourn ####
  
  
  
  
  
  observeEvent(input$selected_tournament, {
    req(input$selected_tournament)
    if (input$tournament_tabs == "history") {
      updateSelectizeInput(session,
                           "selected_division",
                           choices = names(selected_data()$history))
    } else if (input$tournament_tabs == "impr") {
      updateSelectizeInput(session,
                           "selected_division",
                           choices = names(selected_data()$impr))
    }
  })
  
  
  observeEvent(input$selected_tournament, {
    req(input$selected_tournament)
    updateSelectizeInput(session,
                         "selected_division",
                         choices = names(selected_data()$history))
    
  })
  
  # 3 - History Table ####
  output$history_table <- renderDT({
    req(input$selected_tournament, input$selected_division)
    tournament_data <- selected_data()
    division <- input$selected_division
    t <- tournament_data$history[[division]]
    colnames(t)[1] <- "GainedRat"
    t <- t[, -ncol(t)]
    datatable(
      t ,
      selection = "single",
      options = list(
        pageLength = 1000,
        scrollX = TRUE,
        paging = FALSE,
        lengthChange = FALSE
      )
    ) %>%
      formatStyle(
        columns =  'GainedRat',
        # Specify the column to format
        color  = styleInterval(
          c(-Inf, 0, Inf),
          # Define the ranges
          c('grey', 'red', "grey20", "limegreen")  # Corresponding colors
        )
      )
  })
  
  # 3 - Impr Table ####
  output$impr_table <- renderDT({
    req(input$selected_tournament, input$selected_division)
    
    tournament_data <- selected_data()
    division <- input$selected_division
    
    history <- tournament_data$history[[division]][, c("Games", "Wins", "Draws", "CatRating")]
    rhist <- rownames(history)
    
    i <- tournament_data$impr[[division]][, -5]
    rownames(i) <- rimpr <-  i[, 1]
    i <- i[, -1]
    
    i[, (ncol(i) + 1):(ncol(i) + 5)] <- history[match(rimpr, rhist), ]
    i <- i[, -ncol(i)]
    datatable(i,
              options = list(
                pageLength = 1000,
                scrollX = TRUE,
                paging = FALSE,
                lengthChange = FALSE
              )) %>%
      formatStyle(
        columns =  'GainedRat',
        # Specify the column to format
        color  = styleInterval(
          c(-Inf, 0, Inf),
          # Define the ranges
          c('grey', 'red', "grey20", "limegreen")  # Corresponding colors
        )
      )
  })
  
  
  
  
  
  observeEvent(input$history_table_rows_selected, {
    req(input$history_table_rows_selected,
        input$selected_tournament)
    
    row_index <- max(input$history_table_rows_selected)
    
    t <- selected_data()$history[[input$selected_division]]
    
    
    player_name <- rownames(t)[row_index]
    
    match_data <- archive()[[player_name]][[input$selected_tournament]]
    
    
    showModal(modalDialog(
      title = paste(
        "Match history for",
        player_name,
        "in",
        input$selected_tournament
      ),
      DTOutput("archive_modal_table"),
      size = "l",
      easyClose = TRUE
    ))
    
    output$archive_modal_table <- renderDT({
      datatable(
        match_data %>%
          mutate(Points = as.numeric(gsub(
            "Tot:", "", Points
          ))),
        options = list(
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          columns =  'Points',
          # Specify the column to format
          color  = styleInterval(
            c(-Inf, 0, Inf),
            # Define the ranges
            c('grey', 'red', "grey20", "limegreen")  # Corresponding colors
          )
        )
    })
  })
  
  # 3 - Opponents Rating Table ####
  
  output$opp_table <- renderDT({
    req(input$selected_tournament, input$selected_division)
    datatable(
      opp_classifica(),
      # extensions = "Select",
      selection = list(mode = "single", target = "cell"),
      options = list(
        pageLength = 1000,
        scrollX = TRUE,
        paging = FALSE,
        lengthChange = FALSE
      ),
      rownames = FALSE
    )
  })
  
  # 3 - Opponent Rating Modal ####
  
  observeEvent(input$opp_table_cells_selected, {
    req(input$opp_table_cells_selected)
    cell <- matrix(input$opp_table_cells_selected, ncol = 2)
    
    row_index <- cell[1, 1]
    col_index <- cell[1, 2]
    req(col_index > 0)
    
    
    
    player_name <- opp_classifica()[row_index, 1]
    
    result <- classifica(
      selected_data(),
      role = "players",
      cols = col_index,
      sel.pl = player_name
    )
    
    phase <- c("Groups", "Bracket", "Placements")
    
    showModal(modalDialog(
      title = paste("Oppontent during ", phase[col_index], " for ", player_name),
      DT::renderDataTable(
        result,
        options = list(
          scrollX = TRUE,
          paging = FALSE,
          lengthChange = FALSE
        ),
        rownames = FALSE
      ),
      
      size = "l",
      easyClose = TRUE
    ))
  })
  
  

  # 4 - Select players ####
  observeEvent(input$player_from_list, {
    showModal(modalDialog(
      title = "Select Players",
      
      # Initial empty choices, to be updated in server
      selectizeInput("player1", "Select Player A1", choices = NULL),
      selectizeInput("player2", "Select Player A2", choices = NULL),
      selectizeInput("player3", "Select Player B1", choices = NULL),
      selectizeInput("player4", "Select Player B2", choices = NULL),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_selection", "Confirm Selection")
      ),
      easyClose = TRUE,
      size = "m",
      id = "player_modal"
    ))
  })
  
  observeEvent(input$player_from_list, {
    updateSelectizeInput(session, "player1", choices = df()$giocatore, server = TRUE)
    updateSelectizeInput(session, "player2", choices = df()$giocatore, server = TRUE)
    updateSelectizeInput(session, "player3", choices = df()$giocatore, server = TRUE)
    updateSelectizeInput(session, "player4", choices = df()$giocatore, server = TRUE)
  })
  
  
  
  observeEvent(input$confirm_selection, {
    player1 <- input$player1
    player2 <- input$player2
    player3 <- input$player3
    player4 <- input$player4
    
    player1_data <- df() %>% filter(giocatore == player1) %>% select(Rating, Deviation)
    player2_data <- df() %>% filter(giocatore == player2) %>% select(Rating, Deviation)
    player3_data <- df() %>% filter(giocatore == player3) %>% select(Rating, Deviation)
    player4_data <- df() %>% filter(giocatore == player4) %>% select(Rating, Deviation)
    
    
    updateNumericInput(session, "p1_rat", value = player1_data$Rating)
    updateNumericInput(session, "p1_dev", value = player1_data$Deviation)
    
    updateNumericInput(session, "p2_rat", value = player2_data$Rating)
    updateNumericInput(session, "p2_dev", value = player2_data$Deviation)
    
    updateNumericInput(session, "p3_rat", value = player3_data$Rating)
    updateNumericInput(session, "p3_dev", value = player3_data$Deviation)
    
    updateNumericInput(session, "p4_rat", value = player4_data$Rating)
    updateNumericInput(session, "p4_dev", value = player4_data$Deviation)
    
    
    removeModal()
  })
  



    
  # 4 - Calculator #####
  observeEvent(input$calculate, {

    if(input$calc_team_player == "Players"){

      tA.rat <- (input$p1_rat+input$p2_rat)/2
      tB.rat <- (input$p3_rat+input$p4_rat)/2

      tA.dev <- sqrt((input$p1_dev^2+input$p2_dev^2)/2)
      tB.dev <- sqrt((input$p3_dev^2+input$p4_dev^2)/2)

    }else{
      tA.rat <- input$t1_rat
      tB.rat <- input$t2_rat

      tA.dev <- input$t1_dev
      tB.dev <- input$t2_dev
    }

    qv <- (log(10)/400)
    qv2 <- (log(10)/400)^2
    qip3 <- 3 * (qv/pi)^2


    gdevs <- function(RD) 1/sqrt(1 + qip3 * RD^2)
    g <- gdevs(sqrt(tA.dev^2+tB.dev^2))
    
    
    ExpA <- 1/(1+10^(-g*(tA.rat - tB.rat)/400))
    ExpB <- 1-ExpA

    dval2 <- qv2 * (g * ExpA * (1 - ExpA))

    if(input$calc_team_player == "Players"){
      
      if(input$p1_dev<80  & input$p2_dev<80){
        k.funcA1 <- k.funcA2 <- 1/(1/(tA.dev^2) + dval2)
      }else{
        k.funcA1 <- 1/(1/(input$p1_dev^2) + dval2)
        k.funcA2 <- 1/(1/(input$p2_dev^2) + dval2)
      }
      
      if(input$p3_dev<80  & input$p4_dev<80){
        k.funcB1 <- k.funcB2 <- 1/(1/(tB.dev^2) + dval2)
      }else{
        k.funcB1 <- 1/(1/(input$p3_dev^2) + dval2)
        k.funcB2 <- 1/(1/(input$p4_dev^2) + dval2)
      }
      
      PointA1 <-  qv*k.funcA1  * g* (as.numeric(input$score_) - ExpA)
      PointA2 <-  qv*k.funcA2  * g* (as.numeric(input$score_) - ExpA)
      PointB1 <-  qv*k.funcB1  * g* (1-as.numeric(input$score_) - ExpB)
      PointB2 <-  qv*k.funcB2  * g* (1-as.numeric(input$score_) - ExpB)
      
    
      
    RDupdtA1 <- sqrt((1/(input$p1_dev^2)+ dval2)^(-1))
    RDupdtA2 <- sqrt((1/(input$p2_dev^2)+ dval2)^(-1))
    RDupdtB1 <- sqrt((1/(input$p3_dev^2)+dval2)^(-1))
    RDupdtB2 <- sqrt((1/(input$p4_dev^2)+dval2)^(-1))
    
    nameA1 <- if(is.null(input$player1)) "Player A1" else input$player1
    nameA2 <- if(is.null(input$player2)) "Player A2" else input$player2
    nameB1 <- if(is.null(input$player3)) "Player B1" else input$player3
    nameB2 <- if(is.null(input$player4)) "Player B2" else input$player4
    outA <-
      tibble(
        "Statistic" = c("Expected Win Probability", "Points", "New RD"),
        "Player A1" = c(round(ExpA,2), round(PointA1,0), ceiling(RDupdtA1)),
        "Player A2" = c(round(ExpA,2), round(PointA2,0), ceiling(RDupdtA2)))
    
    colnames(outA) <- c("Statistics", nameA1, nameA2)
        
    outB <-
      tibble(
        "Statistic" = c("Expected Win Probability", "Points", "New RD"),
        "Player B1" = c(round(1-ExpA,2), round(PointB1,0), ceiling(RDupdtB1)),
        "Player B2" = c(round(1-ExpA,2), round(PointB2,0), ceiling(RDupdtB2)))
      
    colnames(outB) <- c("Statistics", nameB1, nameB2)
    
    }else{
      
      k.funcA <- 1/(1/(tA.dev^2) + dval2)
      k.funcB <- 1/(1/(tB.dev^2) + dval2)
      
      
      PointA <-  qv*k.funcA  * g* (as.numeric(input$score_) - ExpA)
      PointB <-  qv*k.funcB  * g* (1-as.numeric(input$score_) - ExpB)
      
      
    RDupdt <- sqrt((1/(tA.dev^2)+ dval2)^(-1))
    RDupdtB <- sqrt((1/(tB.dev^2)+dval2)^(-1))
    
    
    outA <-
      data.frame(
        "Statistic" = c("Expected Win Probability", "Points", "New RD"),
        "Values" = c(round(ExpA,2), round(PointA,0), ceiling(RDupdt))
      )
    outB <-
      data.frame(
        "Statistic" = c("Expected Win Probability", "Points", "New RD"),
        "Values" = c(round(1-ExpA,2), round(PointB,0), ceiling(RDupdtB))
      )
    }  

    output$calculator_outA <- renderDT({
      datatable(outA, caption = "Team A" ,options = list(dom = "t",paging = FALSE, ordering = FALSE,searching = FALSE, changeLength = FALSE, pageLength = 3), rownames = FALSE)
    })
    output$calculator_outB <- renderDT({
      datatable(outB, caption = "Team B" ,options = list(dom = "t",paging = FALSE, ordering = FALSE,searching = FALSE, changeLength = FALSE, pageLength = 3), rownames = FALSE)
    })
  })
  
  

  # 5 - Report ####
  
  observeEvent(input$send_mail, {
    req(input$what_report != "n")
    req(input$player_report)
    
    # Fix: Use unique input IDs
    report_details <- switch(
      input$what_report,
      "i" = paste0(
        "Wrong Personal Info Report\n\n",
        "Player: ",
        input$player_report,
        "\n",
        "Correct Gender: ",
        input$report_info_gender,
        "\n",
        "Correct Nationality: ",
        input$report_info_nationality
      ),
      "s" = paste0(
        "Wrong Score Report\n\n",
        "Player: ",
        input$player_report,
        "\n",
        "Tournament: ",
        input$tournament_report,
        "\n",
        "Game: ",
        input$game_report,
        "\n",
        "Correct Score: ",
        input$score_correct
      ),
      "p" = paste0(
        "Wrong Opponent Report\n\n",
        "Player: ",
        input$player_report,
        "\n",
        "Tournament: ",
        input$tournament_report2,
        "\n",
        "Game: ",
        input$game_report2,
        "\n",
        "Correct Opponent: ",
        input$opp_correct
      ),
      "o" = paste0(
        "Other Report\n\n",
        "Player: ",
        input$player_report,
        "\n",
        "Details: ",
        input$other_wrong
      )
    )
    
    user_email <- ifelse(
      !is.null(input$user_email) && input$user_email != "",
      input$user_email,
      "Not provided"
    )
    
    report_details <- paste0(report_details, "\n\nUser Email: ", user_email)
    
    # ---- Configure SMTP connection ----
    smtp <- emayili::server(
      host = "smtp.gmail.com",
      port = 587,
      username = "federicograzi@gmail.com",
      password = "ldjo jlmy yxse aieq",
      # ✅ create in Google Account > Security > App passwords
      reuse = FALSE
    )
    
    # ---- Compose email ----
    email <- emayili::envelope() %>%
      emayili::from("federicograzi@gmail.com") %>%
      emayili::to("federicograzi@gmail.com") %>%
      emayili::subject(paste("Player Report -", input$player_report)) %>%
      emayili::text(report_details)
    
    # ---- Send ----
    
    tryCatch({
      smtp(email)
      
      # ✅ Success popup
      showModal(modalDialog(
        title = "✅ Report Sent Successfully!",
        "Thank you for your report. We’ll review it shortly.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      
    }, error = function(e) {
      
      # ❌ Error popup
      showModal(modalDialog(
        title = "❌ Failed to Send Email",
        paste("An error occurred while sending your report:", e$message),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      
    })
  })
  
}





shinyApp(ui, server)
