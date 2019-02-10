n_total <- nrow(movies)
all_studios <- sort(unique(movies$studio))
movies <-
  movies %>% mutate(score_ratio = audience_score / critics_score)


ui <- navbarPage(theme = "style.css",
                 titlePanel("پروژه نگهداشت مشتری",
                            windowTitle = "نگهداشت مشتری"),
                 sidebarLayout(
                   sidebarPanel(
                     HTML(paste(
                       "Enter number of samples to show between 1 and ", n_total
                     )),
                     actionButton(inputId = "Show_Message", label = "show message"),
                     textInput(
                       inputId = "plot_title",
                       label = "Plot Title",
                       placeholder = "Enter Text For Plot Title"
                     ),
                     actionButton(inputId = "change_title", label = "change_title"),
                     numericInput(
                       inputId = "n",
                       label = "sample size",
                       min = 1 ,
                       max = n_total,
                       step = 1,
                       value = 651
                     ),
                     selectInput(
                       inputId = "selected_type",
                       label = "select movie type",
                       choices = levels(movies$title_type),
                       selected = "Feature Film"
                     ),
                     selectInput(
                       inputId = "studio",
                       label = "select studio:",
                       choices = all_studios,
                       selected = "20th Centry Fox",
                       multiple = TRUE,
                       selectize = TRUE
                     ),
                     selectInput(
                       inputId = "y",
                       label = "y-axis:",
                       choices = c(
                         "IMDB Rating" = "imdb_rating",
                         "IMDB num votes" = "imdb_num_votes",
                         "Critics Score" = "critics_score",
                         "Audience Score" = "audience_score",
                         "Run Time" = "runtime"
                       ),
                       selected = "audience_score"
                     ),
                     selectInput(
                       inputId = "x",
                       label = "x-axis:",
                       choices = c(
                         "IMDB Rating" = "imdb_rating",
                         "IMDB num votes" = "imdb_num_votes",
                         "Critics Score" = "critics_score",
                         "Audience Score" = "audience_score",
                         "Run Time" = "runtime"
                       ),
                       selected = "critics_score"
                     ),
                     selectInput(
                       inputId = "z",
                       label = "color-type:",
                       choices = c(
                         "Title Type" = "title_type",
                         "Genre" = "genre",
                         "MPAA Rating" = "mpaa_rating",
                         "Critics Rating" = "critics_rating",
                         "Audience Rating" = "audience_rating"
                       ),
                       selected = "title_type"
                     ),
                     sliderInput(
                       inputId = "alpha",
                       label = "Alpha: ",
                       min = 0,
                       max = 1,
                       value = 0.5
                     ),
                     checkboxInput(
                       inputId = "show_data_table",
                       label = "show Data Table",
                       value = TRUE
                     ),
                     checkboxGroupInput(
                       inputId = "select_title_type",
                       label = "select title type",
                       choices = names(movies),
                       selected = c("title")
                     ),
                     radioButtons(
                       inputId = "fileType",
                       label = "select file types",
                       choices = c("csv", "tsv"),
                       selected = "csv"
                     )
                   ),
                   mainPanel (
                     HTML("Select fielType and variables, then hit download button"),
                     tableOutput(outputId = "observer_show"),
                     br(),
                     downloadButton(outputId = "download_data", label = "Download data"),
                     br(),
                     textOutput(outputId = "correlation"),
                     br(),
                     plotOutput(outputId = "scatterPlot", hover = "plot_hover"),
                     DT::dataTableOutput(outputId = "selectedColumns"),
                     uiOutput(outputId = "n"),
                     textOutput(outputId = "avg_x"),
                     textOutput(outputId = "avg_y"),
                     verbatimTextOutput(outputId = "lmOutput"),
                     DT::dataTableOutput(outputId = "selectedByBrush"),
                     tableOutput(outputId = "summaryTable"),
                     plotOutput(outputId = "densityPlot"),
                     DT::dataTableOutput(outputId = "moviesTable")
                   ), position = c("right")
                 ))

server <- function(input, output) {
  pretty_plot_title <- reactive({
    req(input$plot_title)
    toTitleCase(input$plot_title)
  })
  movies_subset <- reactive({
    req(input$selected_type)
    filter(movies, title_type %in% input$selected_type)
  })
  movies_selected_columns <- reactive({
    req(input$select_title_type)
    movies %>% select(input$select_title_type)
  })
  new_plot_title <-
    eventReactive(
      eventExpr = input$change_title,
      valueExpr = input$plot_title,
      ignoreNULL = FALSE
    )
  show_Mesage <-
    observeEvent(input$Show_Message, {
      cat("Showing", ncol(movies_selected_columns()), "rows\n")
    })
  show_table  <-
    eventReactive(input$Show_Message, {
      head(movies, input$n)
    })
  output$observer_show <- renderTable({
    show_table()
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(data = movies_subset(), aes_string(
      x = input$x,
      y = input$y,
      color = input$z
    )) + geom_point(alpha = input$alpha) + labs(title = new_plot_title()) #isolate(pretty_plot_title())
  })
  
  output$selectedByBrush <- DT::renderDataTable({
    nearPoints(movies, input$plot_hover, x = input$x, y = input$y) %>% select(title , audience_score, critics_score)
  })
  
  output$selectedColumns <- DT::renderDataTable({
    DT::datatable(
      data = movies_selected_columns(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  output$densityPlot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x)) + geom_density()
  })
  
  output$moviesTable <- DT::renderDataTable({
    req(input$n)
    req(input$studio)
    moviesSample <-
      movies %>% sample_n(input$n) %>% select(title:studio)
    movies_from_selected_studio <-
      moviesSample %>% filter(studio %in% input$studio)
    if (input$show_data_table) {
      DT::datatable(
        data = movies_from_selected_studio,
        options = list(pageLength = 10),
        rownames = FALSE
      )
    }
  })
  output$summaryTable <- renderTable({
    movies %>% filter(title_type %in% input$select_title_type) %>% group_by(mpaa_rating) %>% summarise(
      Mean = mean(score_ratio),
      SD = sd(score_ratio),
      n = n()
    )
  }, striped = TRUE, spacing = "l", align = "lccr", digits = 4, width = "90%", caption = "score ration (audience/critics scores) summary statistics by MPAA ration.")
  
  output$correlation <- renderText({
    r <- round(cor(movies[, input$x], movies[, input$y]), 3)
    paste0("correlation = ", r)
  })
  output$avg_x <- renderText({
    avg_x <- movies %>% pull(input$x) %>% mean() %>% round(2)
    paste("Average", input$x, avg_x)
  })
  
  output$avg_y <- renderText({
    avg_y <- movies %>% pull(input$y) %>% mean() %>% round(2)
    paste("Average", input$y, avg_y)
  })
  
  output$lmOutput <- renderPrint({
    x <- movies %>% pull(input$x)
    y <- movies %>% pull(input$y)
    summ <- summary(lm(y ~ x, data = movies))
    print(summ, digits = 3, signif.stars = FALSE)
    
  })
  output$download_data <- downloadHandler(
    fileName <- function () {
      paste0("movies.", input$fileType)
    },
    content = function(file) {
      if (input$fileType == "csv") {
        write_csv(movies %>% select(input$select_title_type), path = file)
      }
      if (input$fileType == "tsv") {
        write_tsv(movies %>% select(input$select_title_type), path = file)
      }
    }
  )
  output$n <- renderUI({
    HTML(
      paste0(
        "The Plot displays the relationship between the <br> audience and critics scores of <br>",
        nrow(movies_subset()),
        " <br>",
        input$selected_type,
        "<br> movies."
      )
    )
  })
  
}

shinyApp(ui = ui, server = server)
