library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "NBA Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Team Statistics", tabName = "team_stats", icon = icon("trophy")),
      menuItem("Player Statistics", tabName = "player_stats", icon = icon("user")),
      menuItem("Data Tables", tabName = "data_tables", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Dashboard tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_players", width = 4),
                valueBoxOutput("total_teams", width = 4),
                valueBoxOutput("avg_points", width = 4)
              ),
              fluidRow(
                box(
                  title = "Top 10 Scorers",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("top_scorers_plot", height = 350)
                ),
                box(
                  title = "Top Teams by Win Percentage",
                  status = "success", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("top_teams_plot", height = 350)
                )
              ),
              fluidRow(
                box(
                  title = "Player Position Distribution",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("positions_plot", height = 350)
                ),
                box(
                  title = "Team Performance",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("team_performance_plot", height = 350)
                )
              )
      ),
      
      # Team Statistics tab content
      tabItem(tabName = "team_stats",
              fluidRow(
                box(
                  title = "Team Selector",
                  width = 3,
                  selectInput("team_select", "Select Team:", choices = NULL)
                ),
                valueBoxOutput("team_wins", width = 3),
                valueBoxOutput("team_losses", width = 3),
                valueBoxOutput("team_win_pct", width = 3)
              ),
              fluidRow(
                box(
                  title = "Team Offensive Stats",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("team_offense_plot", height = 350)
                ),
                box(
                  title = "Team Defensive Stats",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("team_defense_plot", height = 350)
                )
              ),
              fluidRow(
                box(
                  title = "Team Players",
                  width = 12,
                  dataTableOutput("team_players_table")
                )
              )
      ),
      
      # Player Statistics tab content
      tabItem(tabName = "player_stats",
              fluidRow(
                box(
                  title = "Player Selector",
                  width = 12,
                  column(4, selectInput("pos_filter", "Filter by Position:", choices = NULL)),
                  column(4, selectInput("team_filter", "Filter by Team:", choices = NULL)),
                  column(4, selectInput("player_select", "Select Player:", choices = NULL))
                )
              ),
              fluidRow(
                valueBoxOutput("player_points", width = 3),
                valueBoxOutput("player_rebounds", width = 3),
                valueBoxOutput("player_assists", width = 3),
                valueBoxOutput("player_minutes", width = 3)
              ),
              fluidRow(
                box(
                  title = "Player Stats Comparison",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("player_stats_plot", height = 350)
                )
              ),
              fluidRow(
                box(
                  title = "Player Shooting Percentages",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("player_shooting_plot", height = 300)
                ),
                box(
                  title = "Player Defense Stats",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("player_defense_plot", height = 300)
                )
              )
      ),
      
      # Data Tables tab content
      tabItem(tabName = "data_tables",
              fluidRow(
                tabBox(
                  width = 12,
                  tabPanel("Players Data", dataTableOutput("players_table")),
                  tabPanel("Teams Data", dataTableOutput("teams_table"))
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Read data
  player_data <- reactive({
    read.csv("player.csv", stringsAsFactors = FALSE)
  })
  
  team_data <- reactive({
    read.csv("team.csv", stringsAsFactors = FALSE)
  })
  
  
  # Update UI elements based on data
  observe({
    # Update team selector choices
    team_choices <- sort(unique(team_data()$team_name))
    updateSelectInput(session, "team_select", choices = team_choices)
    updateSelectInput(session, "team_filter", choices = c("All", team_choices))
    
    # Update position selector choices
    pos_choices <- sort(unique(player_data()$player_position))
    updateSelectInput(session, "pos_filter", choices = c("All", pos_choices))
    
    # Update player selector based on filters
    filtered_players <- player_data()
    
    if (input$pos_filter != "All") {
      filtered_players <- filtered_players %>% 
        filter(player_position == input$pos_filter)
    }
    
    if (input$team_filter != "All") {
      filtered_players <- filtered_players %>% 
        filter(team_name == input$team_filter)
    }
    
    player_choices <- sort(filtered_players$player_name)
    updateSelectInput(session, "player_select", choices = player_choices)
  })
  
  # Dashboard tab outputs
  output$total_players <- renderValueBox({
    valueBox(
      nrow(player_data()),
      "Total Players",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$total_teams <- renderValueBox({
    # Filter out special teams like All-Stars
    regular_teams <- team_data() %>%
      filter(!grepl("All-Stars|Team ", team_name))
    
    valueBox(
      nrow(regular_teams),
      "NBA Teams",
      icon = icon("basketball-ball"),
      color = "green"
    )
  })
  
  output$avg_points <- renderValueBox({
    avg_points <- mean(player_data()$avg_points, na.rm = TRUE)
    
    valueBox(
      round(avg_points, 1),
      "Avg Points Per Player",
      icon = icon("chart-line"),
      color = "orange"
    )
  })
  
  output$top_scorers_plot <- renderPlotly({
    top_players <- player_data() %>%
      filter(games_played > 10) %>%  # Only include players with significant playing time
      arrange(desc(avg_points)) %>%
      head(10)
    
    p <- ggplot(top_players, aes(x = reorder(player_short_name, avg_points), y = avg_points, fill = team_name)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "", y = "Average Points") +
      theme_minimal() +
      theme(legend.title = element_blank(),legend.position = "none") +
      ylim(0, max(top_players$avg_points) * 1.1)
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
  })
  
  output$top_teams_plot <- renderPlotly({
    # Filter out special teams
    regular_teams <- team_data() %>%
      filter(!grepl("All-Stars|Team ", team_name)) %>%
      filter(games_played > 10) %>%  # Only teams with significant games
      arrange(desc(win_percentage)) %>%
      head(10)
    
    p <- ggplot(regular_teams, aes(x = reorder(team_short_display_name, win_percentage), 
                                   y = win_percentage, 
                                   fill = team_short_display_name)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "", y = "Win Percentage") +
      theme_minimal() +
      theme(legend.position = "none") +
      ylim(0, max(regular_teams$win_percentage) * 1.1)
    
    ggplotly(p)
  })
  
  output$positions_plot <- renderPlotly({
    pos_count <- player_data() %>%
      count(player_position) %>%
      filter(!is.na(player_position))
    
    # Create a simple bar chart instead of pie chart for better compatibility
    p <- ggplot(pos_count, aes(x = reorder(player_position, -n), y = n, fill = player_position)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Position", y = "Count", fill = "Position") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
    
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
  })
  
  output$team_performance_plot <- renderPlotly({
    # Filter out special teams and get top performing teams
    regular_teams <- team_data() %>%
      filter(!grepl("All-Stars|Team ", team_name)) %>%
      filter(games_played > 10) %>%
      arrange(desc(avg_points_scored)) %>%
      head(10)
    
    p <- ggplot(regular_teams, aes(x = avg_points_scored, y = win_percentage, 
                                   color = team_short_display_name,
                                   text = team_name)) +
      geom_point(size = 3, alpha = 0.7) +
      labs(x = "Average Points Scored", y = "Win Percentage") +
      theme_minimal() +
      theme(legend.title = element_blank(),legend.position = "none")
    
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  # Team Statistics tab outputs
  output$team_wins <- renderValueBox({
    req(input$team_select)
    
    selected_team <- team_data() %>%
      filter(team_name == input$team_select)
    
    valueBox(
      selected_team$wins,
      "Wins",
      icon = icon("trophy"),
      color = "green"
    )
  })
  
  output$team_losses <- renderValueBox({
    req(input$team_select)
    
    selected_team <- team_data() %>%
      filter(team_name == input$team_select)
    
    valueBox(
      selected_team$losses,
      "Losses",
      icon = icon("thumbs-down"),
      color = "red"
    )
  })
  
  output$team_win_pct <- renderValueBox({
    req(input$team_select)
    
    selected_team <- team_data() %>%
      filter(team_name == input$team_select)
    
    valueBox(
      paste0(round(selected_team$win_percentage, 1), "%"),
      "Win Percentage",
      icon = icon("percentage"),
      color = "blue"
    )
  })
  
  output$team_offense_plot <- renderPlotly({
    req(input$team_select)
    
    selected_team <- team_data() %>%
      filter(team_name == input$team_select)
    
    df <- data.frame(
      Stat = c("Points", "FG%", "3PT%", "FT%"),
      Value = c(selected_team$avg_points_scored, 
                selected_team$avg_fg_pct, 
                selected_team$avg_3pt_pct, 
                selected_team$avg_ft_pct)
    )
    
    p <- ggplot(df, aes(x = Stat, y = Value, fill = Stat)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Value, 1)), vjust = -0.5) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "", y = "") +
      ylim(0, max(df$Value) * 1.1)
    
    ggplotly(p)
  })
  
  output$team_defense_plot <- renderPlotly({
    req(input$team_select)
    
    selected_team <- team_data() %>%
      filter(team_name == input$team_select)
    
    df <- data.frame(
      Stat = c("Rebounds", "Steals", "Blocks", "Turnovers"),
      Value = c(selected_team$avg_rebounds, 
                selected_team$avg_steals, 
                selected_team$avg_blocks, 
                selected_team$avg_turnovers)
    )
    
    p <- ggplot(df, aes(x = Stat, y = Value, fill = Stat)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Value, 1)), vjust = -0.5) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "", y = "") +
      ylim(0, max(df$Value) * 1.1)
    
    ggplotly(p)
  })
  
  output$team_players_table <- renderDataTable({
    req(input$team_select)
    
    team_players <- player_data() %>%
      filter(team_name == input$team_select) %>%
      select(player_name, player_position, games_played, avg_points, avg_rebounds, 
             avg_assists, avg_steals, avg_blocks, avg_minutes) %>%
      arrange(desc(avg_points))
    
    datatable(team_players, options = list(pageLength = 10))
  })
  
  # Player Statistics tab outputs
  selected_player_data <- reactive({
    req(input$player_select)
    
    player_data() %>%
      filter(player_name == input$player_select)
  })
  
  output$player_points <- renderValueBox({
    req(selected_player_data())
    
    valueBox(
      round(selected_player_data()$avg_points, 1),
      "Points Per Game",
      icon = icon("chart-bar"),
      color = "blue"
    )
  })
  
  output$player_rebounds <- renderValueBox({
    req(selected_player_data())
    
    valueBox(
      round(selected_player_data()$avg_rebounds, 1),
      "Rebounds Per Game",
      icon = icon("basketball-ball"),
      color = "green"
    )
  })
  
  output$player_assists <- renderValueBox({
    req(selected_player_data())
    
    valueBox(
      round(selected_player_data()$avg_assists, 1),
      "Assists Per Game",
      icon = icon("hands-helping"),
      color = "yellow"
    )
  })
  
  output$player_minutes <- renderValueBox({
    req(selected_player_data())
    
    valueBox(
      round(selected_player_data()$avg_minutes, 1),
      "Minutes Per Game",
      icon = icon("clock"),
      color = "purple"
    )
  })
  
  output$player_stats_plot <- renderPlotly({
    req(selected_player_data())
    
    player <- selected_player_data()
    
    # Compare to position average
    position_avg <- player_data() %>%
      filter(player_position == player$player_position) %>%
      summarize(
        avg_points = mean(avg_points, na.rm = TRUE),
        avg_rebounds = mean(avg_rebounds, na.rm = TRUE),
        avg_assists = mean(avg_assists, na.rm = TRUE),
        avg_steals = mean(avg_steals, na.rm = TRUE),
        avg_blocks = mean(avg_blocks, na.rm = TRUE)
      )
    
    # Prepare data for radar chart
    stats <- c("Points", "Rebounds", "Assists", "Steals", "Blocks")
    player_values <- c(player$avg_points, player$avg_rebounds, player$avg_assists, 
                       player$avg_steals, player$avg_blocks)
    position_values <- c(position_avg$avg_points, position_avg$avg_rebounds, 
                         position_avg$avg_assists, position_avg$avg_steals, 
                         position_avg$avg_blocks)
    
    # Create data frames for plotting
    df_player <- data.frame(
      Statistic = stats,
      Value = player_values,
      Type = player$player_name
    )
    
    df_position <- data.frame(
      Statistic = stats,
      Value = position_values,
      Type = paste("Avg", player$player_position)
    )
    
    df <- rbind(df_player, df_position)
    
    # Create the plot
    p <- ggplot(df, aes(x = Statistic, y = Value, color = Type, group = Type)) +
      geom_line() +
      geom_point(size = 3) +
      theme_minimal() +
      labs(title = paste(player$player_name, "vs Average", player$player_position),
           color = "")
    
    ggplotly(p)
  })
  
  output$player_shooting_plot <- renderPlotly({
    req(selected_player_data())
    
    player <- selected_player_data()
    
    # Prepare shooting percentages
    df <- data.frame(
      Category = c("FG%", "3PT%", "FT%"),
      Percentage = c(player$career_fg_pct, player$career_3pt_pct, player$career_ft_pct)
    )
    
    # Create the plot
    p <- ggplot(df, aes(x = Category, y = Percentage, fill = Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "", y = "Percentage") +
      ylim(0, 100)
    
    ggplotly(p)
  })
  
  output$player_defense_plot <- renderPlotly({
    req(selected_player_data())
    
    player <- selected_player_data()
    
    # Prepare defensive stats
    df <- data.frame(
      Category = c("Steals", "Blocks"),
      Value = c(player$avg_steals, player$avg_blocks)
    )
    
    # Create the plot
    p <- ggplot(df, aes(x = Category, y = Value, fill = Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Value, 1)), vjust = -0.5) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "", y = "Per Game") +
      ylim(0, max(df$Value, na.rm = TRUE) * 1.2 + 0.1)
    
    ggplotly(p)
  })
  
  # Data Tables tab outputs
  output$players_table <- renderDataTable({
    player_data() %>%
      select(player_name, team_name, player_position, games_played, avg_points, 
             avg_rebounds, avg_assists, avg_steals, avg_blocks, avg_minutes) %>%
      arrange(desc(avg_points))
  })
  
  output$teams_table <- renderDataTable({
    team_data() %>%
      select(team_name, team_location, games_played, wins, losses, win_percentage,
             avg_points_scored, avg_rebounds, avg_assists, avg_steals, avg_blocks)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)