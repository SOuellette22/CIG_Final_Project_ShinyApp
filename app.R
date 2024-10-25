library(shiny)
library(tidyverse)
library(see)
library(scales)
NFLPassingStats <- readr::read_csv("NFLPassingStats.csv")
NFLPassStats <- NFLPassingStats
# G1 data ---- 
NFLPassingStatsG1 <- NFLPassingStats %>% 
  filter(Pos == "QB" & Att > 1 & GS > 1) %>% 
  mutate(Conf = if_else(Tm %in% c("MIN", "DET", "GNB", "CHI","SFO", "SEA", "LAR", "ARI","CAR", "ATL", "TAM", "NOR","PHI", "DAL", "NYG", "WAS"), "NFC",
                        "AFC"
  )
  ) %>% 
  rename(Competion_Percentage = `Cmp%`,
         Attempts = Att,
         Completions = Cmp,
         Touchdown = TD,
         Interception = Int,
         Games_Started = GS,
         Touchdown_Percentage = `TD%`,
         Interception_Percentage = `Int%`,
         QB_Rating = Rate,
         First_Downs = `1D`)
# ----

# G2 data ----
NFLPassingStatsG2 <- NFLPassingStats %>% 
  filter(Year >= 2002 & Pos == "QB") %>% 
  mutate(Tm = if_else(Tm == "OAK","LVR",
                      if_else(Tm == "STL", "LAR",
                              if_else(Tm == "SDG", "LAC", Tm)
                      )
  )
  ) %>% 
  rename(Competion_Percentage = `Cmp%`,
         Attempts = Att,
         Completions = Cmp,
         Touchdown = TD,
         Interception = Int,
         Games_Started = GS,
         Touchdown_Percentage = `TD%`,
         Interception_Percentage = `Int%`,
         QB_Rating = Rate,
         First_Downs = `1D`,
         Yards = Yds) %>% 
  group_by(Tm, Year) %>% 
  summarise(Attempts = sum(Attempts),
            Completions = sum(Completions),
            Touchdown = sum(Touchdown),
            Interception = sum(Interception),
            First_Downs = sum(First_Downs),
            Yards = sum(Yards))
# ----

# G3 data ----
NFLPassingStatsG3 <- NFLPassingStats %>% 
  filter(Pos == "QB" & Year >= 2002 &
           Player%in%c("Peyton Manning", "Patrick Mahomes", "Tom Brady", 
                       "Drew Brees", "Aaron Rodgers", "Ben Roethlisberger", 
                       "Russell Wilson", "Tony Romo", "Matt Ryan", 
                       "Philip Rivers", "Brett Farve", "Kurt Warner", 
                       "Andrew Luck", "Eli Manning", "Donavan McNabb", 
                       "Lamar Jackson", "Carson Palmer", "Daunte Culpepper", 
                       "Rich Gannon", "Matt Hasselbeck", "Dad Prescott", 
                       "Deshaun Watson", "Steve McNair", "Matthew Stafford", 
                       "Cam Newton", "Michael Vick", "Chad Pennington", 
                       "Trent Green", "Jeff Garcia", "Josh Allen")) %>% 
  mutate(Tm = if_else(Tm == "OAK","LVR",
                      if_else(Tm == "STL", "LAR",
                              if_else(Tm == "SDG", "LAC", Tm)
                      )
  )
  ) %>% 
  rename(Competion_Percentage = `Cmp%`,
         Attempts = Att,
         Completions = Cmp,
         Touchdown = TD,
         Interception = Int,
         Games_Started = GS,
         Touchdown_Percentage = `TD%`,
         Interception_Percentage = `Int%`,
         QB_Rating = Rate,
         First_Downs = `1D`,
         Yards = Yds) %>% 
  group_by(Player) %>% 
  summarise(Attempts = round(sum(Attempts)),
            Completions = round(sum(Completions)),
            Touchdown = round(sum(Touchdown)),
            Interception = round(sum(Interception)),
            First_Downs = round(sum(First_Downs)),
            Yards = round(sum(Yards)))
# ----

# G4 data ----
teamBarPlotData <- NFLPassStats %>% 
  filter(Year >= 2002 & Pos == "QB" & Tm != "2TM") %>% 
  mutate(Tm = if_else(Tm == "OAK","LVR",
                      if_else(Tm == "STL", "LAR",
                              if_else(Tm == "SDG", "LAC", Tm)
                      )
  )
  ) %>% 
  group_by(Tm) %>% 
  summarise(total_yards = sum(Yds)) %>% 
  mutate(Conf = if_else(Tm %in% c("MIN", "DET", "GNB", "CHI"), "NFC North",
                        if_else(Tm %in% c("SFO", "SEA", "LAR", "ARI"), "NFC West", 
                                if_else(Tm %in% c("CAR", "ATL", "TAM", "NOR"), "NFC South", 
                                        if_else(Tm %in% c("PHI", "DAL", "NYG", "WAS"), "NFC East",
                                                if_else(Tm %in% c("NWE", "BUF", "NYJ", "MIA"), "AFC East",
                                                        if_else(Tm %in% c("CIN", "CLE", "BAL", "PIT"), "AFC North",
                                                                if_else(Tm %in% c("HOU", "IND", "JAX", "TEN"), "AFC South", "AFC West")
                                                        )
                                                )
                                        )
                                )
                        )
  )
  )
# ----


ui <- navbarPage(
  
  title = "Final Project All Graphs",
  
  # title panel ----
  tabPanel(
     title = "Data Discription",
     
     h2("What is contained in the data:"),
     p("This dataset contains the passing stats of all of the teams from the years 2023 to 1993. This also includes players that are not quaterbacks that passed for any years during the given year."),
     p("The columns include:"),
     tags$div(
       tags$ul(
         tags$li("\"Year\" this is the year the stat were recorded"),
         tags$li("\"Player\" this contains player names"),
         tags$li("\"Tm\" this is the abviviation of the city the team is in"),
         tags$li("\"Age\" this is the age of the player listed"),
         tags$li("\"Pos\" this is the position the player plays in"),
         tags$li("\"G\" this is the number of games the player played in"),
         tags$li("\"GS\" this is the number of games started by the player"),
         tags$li("\"Cmp\" this is the number of completion that player had over the season"),
         tags$li("\"Att\" this is the number of passing attempts tha player had over the season"),
         tags$li("\"Cmp%\" this is the percent of passes completed this season"),
         tags$li("\"Yds\" this is the number of yards the player has thrown over the season"),
         tags$li("\"TD\" this is the number of touchdowns the player has thrown that season"),
         tags$li("\"TD%\" this is the percent of passes that season that were touchdowns"),
         tags$li("\"Int\" this is the number of interceptions that the player has thrown that season"),
         tags$li("\"Int%\" this is the percent of throwns were intercepted from this player that season"),
         tags$li("\"1D\" this is the number of first downs that the player has thrown that season")
       )
     ),
     h2("Why I choose this data:"),
     p("I choose this data set because I thought that it was a very interesting type of statistic to look at, since many people are into football esspecially in the USA. I thought that it would help bring to light of different teams line up based on their passing stats over the years. It definitly brought some light onto what teams are better than others when it comes to getting a generational quaterback. You can also see how different quaterbacks line up against one another in different stats over there respective careers. Its also interesting to see the change from running the ball a lot more to passing by the increase int he amount of yards going up and up and up since 1993 to 2023. Over all though this dataset is very interesting and sheds light on things that I didn't even know about the sport as a whole.")
  ),
  # ----
  
  # tabPanel 1 ----
  tabPanel(
    title = "Graph One",
    
    sidebarPanel(
      # Input: Selector for the X variable
      selectInput(inputId = "selectorXG1", 
                  label = "Choose X variable:",
                  choices = list("Attempts", "Completions",
                                 "Touchdown","Interception",
                                 "Games_Started","Competion_Percentage",
                                 "Touchdown_Percentage","Interception_Percentage",
                                 "QB_Rating","First_Downs"),
                  selected = "Attempts"),
      
      # Input: Selector for Y variable
      selectInput(inputId = "selectorYG1",
                  label = "Choose Y Varibale:",
                  choices = list("Attempts", "Completions",
                                 "Touchdown","Interception",
                                 "Games_Started","Competion_Percentage",
                                 "Touchdown_Percentage","Interception_Percentage",
                                 "QB_Rating","First_Downs"),
                  selected = "Touchdown"),
      
      # Input: Range of years the graph displays 
      sliderInput(inputId = "yearRange",
                  label = "Range of Years Observing:",
                  min = 1993, max = 2023,
                  value = c(1993,2023),
                  sep = ""),
      
      h4("Graph One:"),
      p("The readers of this graph are probably at least have a fifth grade reading level since football is a very popular sport. They will deffinetly want to be able to veiw stats in theis way since it allows you to see trends in the data over the range of year chosen."),
      p("The main task that this graph is intended to help with understanding the connections between diffrerent NFL passing statistics over the years.")
    ),
    
    mainPanel(
      h2("Finding Patterns in Different NFL Passing Stats"),
      plotOutput("plotG1")
    )
  ),
  # ----
  
  # tabPanel 2 ----
  tabPanel(
    title = "Graph Two",
    
    sidebarPanel(
      
      # Input: Slider for the number of bins
      selectizeInput("multiSelectG2",
                     "What teams (up to 4) do you want to look at?",
                     choices = list("ATL", "ARI", "BAL",
                                    "BUF", "CAR", "CHI", 
                                    "CIN", "CLE", "DAL", 
                                    "DEN", "DET", "GNB", 
                                    "HOU", "IND", "JAX", 
                                    "KAN", "LAC", "LAR", 
                                    "LVR", "MIA", "MIN", 
                                    "NOR", "NYG", "NYJ", 
                                    "NWE", "PHI", "PIT", 
                                    "SEA", "SFO", "TAM", 
                                    "TEN", "WAS"),
                     selected = "NWE",
                     multiple = TRUE,
                     options = list(maxItems = 4)),
      
      # Input: Selector for the X variable
      selectInput(inputId = "selectYG2", 
                  label = "Choose Y variable:",
                  choices = list("Yards","Attempts", "Completions",
                                 "Touchdown","Interception", "First_Downs")),
      
      h4("Graph Two:"),
      p("The people that would be looking at this graph would be at about a fifth grade reading level. These people would have a passion for understanding statistics in football and how over the years how teams compare to one another."),
      p("This graphic is to help the reader better understand how teams have changed through out the years comparably.")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      h2("Different Teams Compared Stats since 2002"),
      plotOutput("plotG2")
      
    )
  ),
  # ----
  
  # tabPanel 3 ----
  tabPanel(
    title = "Graph Three",
    
    sidebarPanel(
      
      p(span("Select up to 8", style = "color:grey")),
      selectizeInput("playerSelectG3",
                     "Select some of the top 30 QBs since 2002",
                     multiple = TRUE,
                     options = list(maxItems = 8),
                     choices = list("Peyton Manning", "Patrick Mahomes", "Tom Brady", 
                                    "Drew Brees", "Aaron Rodgers", "Ben Roethlisberger", 
                                    "Russell Wilson", "Tony Romo", "Matt Ryan", 
                                    "Philip Rivers", "Brett Farve", "Kurt Warner", 
                                    "Andrew Luck", "Eli Manning", "Donavan McNabb", 
                                    "Lamar Jackson", "Carson Palmer", "Daunte Culpepper", 
                                    "Rich Gannon", "Matt Hasselbeck", "Dad Prescott", 
                                    "Deshaun Watson", "Steve McNair", "Matthew Stafford", 
                                    "Cam Newton", "Michael Vick", "Chad Pennington", 
                                    "Trent Green", "Jeff Garcia", "Josh Allen"),
                     selected = c("Patrick Mahomes", "Tom Brady")),
      
      selectInput(inputId = "selectYG3", 
                  label = "Choose a stat to compare",
                  choices = list("Yards","Attempts", "Completions",
                                 "Touchdown","Interception", "First_Downs")),
      
      h4("Graph Three:"),
      p("This graph is for people of a fifth grade reading level and with at a leaast a little interest in football. I would hope that they also know the names of older quartar backs since this graph compares them to one another."),
      p("This graphic will help people understand how some of the top quartar backs of all time compare statisticly to one another.")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      h2("Comparing the NFLs top 30 QBs in the 21st Century"),
      plotOutput("plotG3")
      
    )
  ),
  # ----
  
  # tabPanel 4 ----
  tabPanel(
    title = "Graph Four",
    sidebarPanel(
      h4("Graph Four:"),
      p("This graph is used to show people the diffrence between teams total passing yards since 2002. Readers of this graph should be able to read at a fifth grade level. This is because many diffrent people would want to learn about these stats and possibly see were their team stands compared to the rest of the league."),
      p("This graph is to show people where there team stands and to understand the complexitys of how teams used to play compared to others.")
    ),
    mainPanel(
      h2("Total passing yards of all teams since 2002"),
      plotOutput("plotG4")
      )
  ),
  # ----
  
  # tabPanel Dataset ----
  tabPanel(
    title = "Dataset",
    h2("Here is the data that is used all the graphs."),
    DT::dataTableOutput("dataset")
  )
  # ----
  
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # G1 graph ----
  yearRangeDataG1 <- reactive({
    NFL <- NFLPassingStatsG1 %>% 
      filter(Year > input$yearRange[1] & Year < input$yearRange[2])
  })
  
  output$plotG1 <- renderPlot({
    
    NFL <- yearRangeDataG1()
    
    p <- ggplot(NFL, aes_string(input$selectorXG1, input$selectorYG1)) +
      geom_point(position = "jitter", aes(color = Conf), size = 2, alpha = 0.75) +
      labs(title = paste(paste(paste("Comparing", input$selectorXG1), "to"), input$selectorYG1),
           caption = "[All data is form: https://www.pro-football-reference.com]") +
      theme(panel.background = element_blank(),
            panel.grid.major = element_line(color = "grey80", size = 0.75),
            panel.grid.minor = element_line(color = "grey80", size = 0.75),
            axis.ticks = element_line(size = 0.75),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18),
            plot.caption = element_text(size = 10),
            legend.key.size = unit(0.75, 'cm'),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14)) +
      geom_line(stat = "smooth", color = "black", alpha = 0.75, size = 2) +
      scale_color_okabeito(order = c(5,6))
    
    plot(p)
    
  })
  # ----
  
  # G2 graph ----
  dataSet <- reactive({
    
    list <- input$multiSelectG2
    
    NFLPassingStatsG2 <- NFLPassingStatsG2 %>% 
      filter(Tm%in%list)
    
  })
  
  output$plotG2 <- renderPlot({
    
    list <- input$multiSelectG2
    NFL <- dataSet()
    year <- "Year"
    
    p <- ggplot(NFL, aes_string(year, input$selectYG2, color = "Tm")) +
      geom_line(aes(group = Tm), size = 2, alpha = 0.75) +
      labs(title = paste("Comparing:", paste(unlist(list), collapse = ", ")),
           colour = "Team",
           caption = "[All data is form: https://www.pro-football-reference.com]") +
      scale_colour_okabeito(order = c(1,2,3,4)) +
      theme(panel.background = element_blank(),
            panel.grid.major = element_line(color = "grey80", size = 0.75),
            panel.grid.minor = element_line(color = "grey80", size = 0.75),
            axis.ticks = element_line(size = 0.75),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18),
            plot.caption = element_text(size = 10),
            legend.key.size = unit(0.75, 'cm'),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14))
    
    plot(p)
    
  })
  # ----
  
  # G3 graph ----
  whatPlayers <- reactive({
    
    list <- input$playerSelectG3
    
    NFLPassingStatsG3 <- NFLPassingStatsG3 %>% 
      filter(Player%in%list)
    
  })
  
  stat <- reactive({
    
    NFL <- whatPlayers()
    
    result <- switch(input$selectYG3,
                     "Yards" = NFL$Yards,
                     "Attempts" = NFL$Attempts, 
                     "Completions" = NFL$Completions,
                     "Touchdown" = NFL$Touchdown,
                     "Interception" = NFL$Interception, 
                     "First_Downs" = NFL$First_Downs)
    result
    
  })
  
  output$plotG3 <- renderPlot({
    
    list <- input$playerSelectG3
    ySelected <- input$selectYG3
    stat <- stat()
    NFL <- whatPlayers()
    
    p <- ggplot(NFL, aes(x = "1", y = stat, fill = Player)) +
      geom_col(width = 1) +
      coord_polar("y", start = 0) +
      labs(caption = "[All data is form: https://www.pro-football-reference.com]") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            plot.title=element_text(size=14, face="bold"),
            legend.key.size = unit(0.75, 'cm'),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14)) +
      scale_fill_okabeito()
      
    
    plot(p)
    
  })
  # ----
  
  # G4 graph ----
    output$plotG4 <- renderPlot({
      
      NFLcolors = c("#385675","#4b6682","#5f7790","#73889e","#cc6a6a","#d17878","#d68787","#db9696")
      
      ggplot(teamBarPlotData, aes(reorder(Tm,total_yards),total_yards)) +
        geom_col(aes(fill = Conf), color = "white") +
        scale_fill_manual(values = NFLcolors) +
        scale_y_continuous(breaks = seq(0,100000, by=25000), limit = c(0,100000), expand = c(0,0)) +
        coord_flip() +
        theme(panel.background = element_blank(),
              panel.grid.major.x = element_line(color = "gray80"),
              panel.grid.minor.x = element_line(color = "gray80"),
              panel.grid.major = element_blank(),
              axis.ticks = element_line(size = 0.75),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 15),
              plot.title = element_text(size = 18),
              plot.caption = element_text(size = 10),
              legend.key.size = unit(0.75, 'cm'),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14)) +
        labs(y = "Total Yards",
             x = "Teams",
             fill = "NFL Conference",
             caption = "[All data is form: https://www.pro-football-reference.com]")
      
    })
  # ----
  
  # Dataset ----
  
    NFLPassingStatsDisplayed <- NFLPassingStats %>% 
      select(Year, Player, Tm, Age, Pos, G, GS, Cmp, Att, `Cmp%`, Yds, TD, `TD%`, Int, `Int%`, `1D`)
    
    output$dataset <- DT::renderDataTable({
      DT::datatable(NFLPassingStatsDisplayed, options = list(pagelength = 10))
    })
  
  # ----
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)