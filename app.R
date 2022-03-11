library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)

##########################################################
###################### UI.R #########################
##########################################################

if (interactive()) {
  
  ui <- fluidPage(
    
    titlePanel("Sudoku"),
    
    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        sidebarMenu(        
          radioButtons(inputId="radio",  #radio pour ne choisir qu'une difficulté à la fois
                       label ="Difficulté",
                       choices = c("Facile" = "easy", 
                                   "Moyen" = "medium", 
                                   "Difficile" = "hard"),
                       selected = "easy"),
          hr(),
          
          actionButton(inputId="load", label="Générer un nouveau Sudoku", icon = icon("cog")),
          hr(),
          actionButton(inputId="solve", label="Solution", icon = icon("check")),
          hr(),
          
          #input numéro ? 
          #  selectInput("row", label = "Row", 
          #              choices = list(1,2,3,4,5,6,7,8,9), 
          #              selected = 1),
          #  selectInput("col", label = "Column", 
          #              choices = list(1,2,3,4,5,6,7,8,9), 
          #              selected = 1),     
          #  selectInput("value", label = "Value", 
          #              choices = list(1,2,3,4,5,6,7,8,9," "=0), 
          #              selected = 1),
          #   actionButton("setButton", "Set"),
          #   hr()
          
          menuItem("", icon = icon(""))
          
        )),
      dashboardBody(  theme = shinytheme('superhero'),
                      selectInput('grille',label = '', choices = c(9)),
                      #  colorInput() ou data table https://onesixx.com/matrix-ggplot/
                      actionButton("add", "Jouer !"),
                      uiOutput("grid"),
                      
                      mainPanel(
                        plotOutput("plot_sudoku"),
                        plotOutput("plot__solve_sudoku")
                        
                      )
                      
      )))
  
  
  
  ##########################################################
  ###################### Server.R #########################
  ##########################################################
  
  
  server <- function(input, output, session) {
    
    # Création de la grille 9x9
    observe({
      if(!is.null(input$add)) {
        m = reactive({ matrix('',
                              ncol = as.numeric(input$grille),
                              nrow = as.numeric(input$grille)) })
        output$grid <- renderUI({
          div(matrixInput(inputId = "newGrid", value = m()),)
        })
      }
    })
    
    #Générer un nouveau Sudoku
    observeEvent(input$load,{
      sudoku <- sudoku_nina() # fonction a changer 
      output$plot_sudoku <- renderPlot({plot(sudoku)})
      
      #Générer un nouveau Sudoku selon la difficulté
      #  level_sudoku <- level_sudoku_nina() # fonction a changer 
      #  output$plot_diff <- renderPlot({plot(sudoku)})
    })
    
    #Générer la solution du Sudoku
    observeEvent(input$solve,{
      solution_sudoku <- solution_sudoku_nina # fonction a changer
      output$plot_solve_sudoku <- renderPlot({plot.sudoku(solution_sudoku_nina)})
    })
    
    
    
  }
  shinyApp(ui, server)
}