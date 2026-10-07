# ============================================================
# MUSIC LISTENING TRENDS AND SONG POPULARITY ANALYSIS
# Using R Shiny
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(corrplot)

# ============================================================
# 1. LOAD DATASET
# ============================================================

songs <- read.csv(
  "data/music_dataset.csv",
  stringsAsFactors = FALSE
)

# Data cleaning
songs <- songs[!duplicated(songs), ]

songs <- songs %>%
  filter(
    !is.na(track_name),
    !is.na(artist_name),
    !is.na(genre),
    !is.na(year),
    !is.na(popularity)
  )

genres <- sort(unique(songs$genre))

# ============================================================
# 2. USER INTERFACE
# ============================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Music Analysis"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Popularity",
        tabName = "popularity",
        icon = icon("star")
      ),
      
      menuItem(
        "Music Trends",
        tabName = "trends",
        icon = icon("music")
      ),
      
      menuItem(
        "Audio Analysis",
        tabName = "audio",
        icon = icon("volume-up")
      ),
      
      menuItem(
        "Data",
        tabName = "data",
        icon = icon("table")
      )
    ),
    
    hr(),
    
    selectInput(
      "genre",
      "Genre:",
      choices = c("All", genres),
      selected = "All"
    ),
    
    sliderInput(
      "year",
      "Year Range:",
      min = min(songs$year),
      max = max(songs$year),
      value = c(min(songs$year), max(songs$year)),
      step = 1,
      sep = ""
    ),
    
    sliderInput(
      "popularity",
      "Popularity:",
      min = min(songs$popularity),
      max = max(songs$popularity),
      value = c(min(songs$popularity), max(songs$popularity))
    )
  ),
  
  dashboardBody(
    
    tags$head(
      
      tags$style(HTML("

        .content-wrapper {
          background-color: #f4f6f9;
        }

        .small-box {
          border-radius: 12px;
        }

        .box {
          border-radius: 10px;
        }

        .main-header .logo {
          font-weight: bold;
        }

      "))
      
    ),
    
    tabItems(
      
      # ======================================================
      # DASHBOARD
      # ======================================================
      
      tabItem(
        tabName = "dashboard",
        
        h2("Music Listening Trends Dashboard"),
        
        fluidRow(
          
          valueBoxOutput(
            "totalSongs",
            width = 3
          ),
          
          valueBoxOutput(
            "totalArtists",
            width = 3
          ),
          
          valueBoxOutput(
            "totalGenres",
            width = 3
          ),
          
          valueBoxOutput(
            "avgPopularity",
            width = 3
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Songs Released by Year",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "yearPlot",
              height = "350px"
            )
          ),
          
          box(
            title = "Popularity Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "popularityPlot",
              height = "350px"
            )
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Songs by Genre",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "genrePlot",
              height = "350px"
            )
          ),
          
          box(
            title = "Song Duration",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "durationPlot",
              height = "350px"
            )
          )
          
        )
      ),
      
      # ======================================================
      # POPULARITY
      # ======================================================
      
      tabItem(
        tabName = "popularity",
        
        h2("Song Popularity Analysis"),
        
        fluidRow(
          
          box(
            title = "Top 10 Popular Songs",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "topSongs",
              height = "400px"
            )
          ),
          
          box(
            title = "Danceability vs Popularity",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "dancePlot",
              height = "400px"
            )
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Energy vs Popularity",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "energyPlot",
              height = "400px"
            )
          ),
          
          box(
            title = "Average Popularity by Genre",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "genrePopularity",
              height = "400px"
            )
          )
          
        )
      ),
      
      # ======================================================
      # MUSIC TRENDS
      # ======================================================
      
      tabItem(
        tabName = "trends",
        
        h2("Music Trends Over Time"),
        
        fluidRow(
          
          box(
            title = "Average Popularity by Year",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "avgYearPopularity",
              height = "400px"
            )
          ),
          
          box(
            title = "Number of Songs by Year",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "yearCount",
              height = "400px"
            )
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Genre Popularity Trend",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            plotlyOutput(
              "genreTrend",
              height = "450px"
            )
          )
          
        )
      ),
      
      # ======================================================
      # AUDIO ANALYSIS
      # ======================================================
      
      tabItem(
        tabName = "audio",
        
        h2("Audio Feature Analysis"),
        
        fluidRow(
          
          box(
            title = "Energy Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "energyDistribution",
              height = "400px"
            )
          ),
          
          box(
            title = "Tempo Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput(
              "tempoDistribution",
              height = "400px"
            )
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Correlation Heatmap",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            plotOutput(
              "correlationPlot",
              height = "600px"
            )
          )
          
        )
      ),
      
      # ======================================================
      # DATA
      # ======================================================
      
      tabItem(
        tabName = "data",
        
        h2("Music Dataset"),
        
        box(
          title = "Filtered Dataset",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          DTOutput("musicTable")
        )
        
      )
      
    )
  )
)

# ============================================================
# 3. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # FILTERED DATA
  # ----------------------------------------------------------
  
  filteredData <- reactive({
    
    data <- songs
    
    if (input$genre != "All") {
      
      data <- data %>%
        filter(genre == input$genre)
      
    }
    
    data <- data %>%
      filter(
        year >= input$year[1],
        year <= input$year[2],
        popularity >= input$popularity[1],
        popularity <= input$popularity[2]
      )
    
    data
    
  })
  
  # ==========================================================
  # DASHBOARD VALUE BOXES
  # ==========================================================
  
  output$totalSongs <- renderValueBox({
    
    valueBox(
      value = nrow(filteredData()),
      subtitle = "Total Songs",
      icon = icon("music"),
      color = "blue"
    )
    
  })
  
  output$totalArtists <- renderValueBox({
    
    valueBox(
      value = n_distinct(filteredData()$artist_name),
      subtitle = "Total Artists",
      icon = icon("users"),
      color = "green"
    )
    
  })
  
  output$totalGenres <- renderValueBox({
    
    valueBox(
      value = n_distinct(filteredData()$genre),
      subtitle = "Total Genres",
      icon = icon("list"),
      color = "yellow"
    )
    
  })
  
  output$avgPopularity <- renderValueBox({
    
    valueBox(
      value = round(
        mean(filteredData()$popularity),
        2
      ),
      subtitle = "Average Popularity",
      icon = icon("star"),
      color = "red"
    )
    
  })
  
  # ==========================================================
  # YEAR-WISE SONGS
  # ==========================================================
  
  output$yearPlot <- renderPlotly({
    
    data <- filteredData() %>%
      count(year)
    
    p <- ggplot(
      data,
      aes(
        x = year,
        y = n
      )
    ) +
      
      geom_col(fill = "steelblue") +
      
      labs(
        title = "Songs Released by Year",
        x = "Year",
        y = "Number of Songs"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ==========================================================
  # POPULARITY DISTRIBUTION
  # ==========================================================
  
  output$popularityPlot <- renderPlotly({
    
    p <- ggplot(
      filteredData(),
      aes(x = popularity)
    ) +
      
      geom_histogram(
        bins = 20,
        fill = "seagreen",
        color = "white"
      ) +
      
      labs(
        title = "Popularity Distribution",
        x = "Popularity",
        y = "Number of Songs"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ==========================================================
  # GENRE DISTRIBUTION
  # ==========================================================
  
  output$genrePlot <- renderPlotly({
    
    data <- filteredData() %>%
      count(genre) %>%
      arrange(desc(n))
    
    p <- ggplot(
      data,
      aes(
        x = reorder(genre, n),
        y = n
      )
    ) +
      
      geom_col(fill = "orange") +
      
      coord_flip() +
      
      labs(
        title = "Songs by Genre",
        x = "Genre",
        y = "Number of Songs"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ==========================================================
  # SONG DURATION
  # ==========================================================
  
  output$durationPlot <- renderPlotly({
    
    p <- ggplot(
      filteredData(),
      aes(x = duration_min)
    ) +
      
      geom_histogram(
        bins = 25,
        fill = "purple",
        color = "white"
      ) +
      
      labs(
        title = "Song Duration Distribution",
        x = "Duration (Minutes)",
        y = "Number of Songs"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ==========================================================
  # TOP 10 SONGS
  # ==========================================================
  
  output$topSongs <- renderPlotly({
    
    data <- filteredData() %>%
      arrange(desc(popularity)) %>%
      slice_head(n = 10) %>%
      arrange(popularity)
    
    p <- ggplot(
      data,
      aes(
        x = popularity,
        y = reorder(track_name, popularity),
        text = paste(
          "Artist:", artist_name,
          "<br>Genre:", genre,
          "<br>Year:", year,
          "<br>Popularity:", popularity
        )
      )
    ) +
      
      geom_col(fill = "tomato") +
      
      labs(
        title = "Top 10 Popular Songs",
        x = "Popularity",
        y = "Song"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # ==========================================================
  # DANCEABILITY VS POPULARITY
  # ==========================================================
  
  output$dancePlot <- renderPlotly({
    
    p <- ggplot(
      filteredData(),
      aes(
        x = danceability,
        y = popularity,
        text = paste(
          "Song:", track_name,
          "<br>Artist:", artist_name,
          "<br>Genre:", genre
        )
      )
    ) +
      
      geom_point(
        alpha = 0.5,
        color = "steelblue"
      ) +
      
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "black"
      ) +
      
      labs(
        title = "Danceability vs Popularity",
        x = "Danceability",
        y = "Popularity"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # ==========================================================
  # ENERGY VS POPULARITY
  # ==========================================================
  
  output$energyPlot <- renderPlotly({
    
    p <- ggplot(
      filteredData(),
      aes(
        x = energy,
        y = popularity,
        text = paste(
          "Song:", track_name,
          "<br>Artist:", artist_name,
          "<br>Genre:", genre
        )
      )
    ) +
      
      geom_point(
        alpha = 0.5,
        color = "darkorange"
      ) +
      
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "black"
      ) +
      
      labs(
        title = "Energy vs Popularity",
        x = "Energy",
        y = "Popularity"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  
  # ==========================================================
  # GENRE POPULARITY
  # ==========================================================
  
  output$genrePopularity <- renderPlotly({
    
    data <- filteredData() %>%
      group_by(genre) %>%
      summarise(
        avg_popularity = mean(popularity),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = reorder(genre, avg_popularity),
        y = avg_popularity
      )
    ) +
      
      geom_col(fill = "turquoise4") +
      
      coord_flip() +
      
      labs(
        title = "Average Popularity by Genre",
        x = "Genre",
        y = "Average Popularity"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ==========================================================
  # AVERAGE POPULARITY BY YEAR
  # ==========================================================
  
  output$avgYearPopularity <- renderPlotly({
    
    data <- filteredData() %>%
      group_by(year) %>%
      summarise(
        avg_popularity = mean(popularity),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = year,
        y = avg_popularity
      )
    ) +
      
      geom_line(
        linewidth = 1,
        color = "purple"
      ) +
      
      geom_point(
        size = 3,
        color = "purple"
      ) +
      
      labs(
        title = "Average Popularity by Year",
        x = "Year",
        y = "Average Popularity"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ==========================================================
  # SONG COUNT BY YEAR
  # ==========================================================
  
  output$yearCount <- renderPlotly({
    
    data <- filteredData() %>%
      count(year)
    
    p <- ggplot(
      data,
      aes(
        x = year,
        y = n
      )
    ) +
      
      geom_col(fill = "darkcyan") +
      
      labs(
        title = "Number of Songs by Year",
        x = "Year",
        y = "Number of Songs"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ==========================================================
  # GENRE TREND
  # ==========================================================
  
  output$genreTrend <- renderPlotly({
    
    data <- filteredData() %>%
      group_by(year, genre) %>%
      summarise(
        avg_popularity = mean(popularity),
        .groups = "drop"
      )
    
    p <- ggplot(
      data,
      aes(
        x = year,
        y = avg_popularity,
        color = genre
      )
    ) +
      
      geom_line(linewidth = 1) +
      
      geom_point() +
      
      labs(
        title = "Genre Popularity Trend",
        x = "Year",
        y = "Average Popularity",
        color = "Genre"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ==========================================================
  # ENERGY DISTRIBUTION
  # ==========================================================
  
  output$energyDistribution <- renderPlotly({
    
    p <- ggplot(
      filteredData(),
      aes(x = energy)
    ) +
      
      geom_histogram(
        bins = 25,
        fill = "darkorange",
        color = "white"
      ) +
      
      labs(
        title = "Energy Distribution",
        x = "Energy",
        y = "Number of Songs"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ==========================================================
  # TEMPO DISTRIBUTION
  # ==========================================================
  
  output$tempoDistribution <- renderPlotly({
    
    p <- ggplot(
      filteredData(),
      aes(x = tempo)
    ) +
      
      geom_histogram(
        bins = 25,
        fill = "royalblue",
        color = "white"
      ) +
      
      labs(
        title = "Tempo Distribution",
        x = "Tempo (BPM)",
        y = "Number of Songs"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ==========================================================
  # CORRELATION HEATMAP
  # ==========================================================
  
  output$correlationPlot <- renderPlot({
    
    data <- filteredData() %>%
      select(
        popularity,
        danceability,
        energy,
        loudness,
        acousticness,
        instrumentalness,
        valence,
        tempo,
        duration_min
      )
    
    correlation_matrix <- cor(
      data,
      use = "complete.obs"
    )
    
    corrplot(
      correlation_matrix,
      method = "color",
      type = "upper",
      addCoef.col = "black",
      tl.col = "black",
      tl.srt = 45,
      number.cex = 0.7
    )
    
  })
  
  # ==========================================================
  # INTERACTIVE DATA TABLE
  # ==========================================================
  
  output$musicTable <- renderDT({
    
    datatable(
      filteredData(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
}

# ============================================================
# 4. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)