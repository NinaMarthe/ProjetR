shiny::runApp(
  list(
    ui = pageWithSidebar(
      
      
      
      headerPanel("Sudoku"),
      
      sidebarPanel(
        radioButtons(inputId="radio",  #radio pour ne choisir qu'une difficulté à la fois
                     label ="Difficulté",
                     choices = c("Facile" = "easy",
                                 "Moyen" = "medium",
                                 "Difficile" = "hard"),
                     selected = "easy"),
        
        actionButton(inputId="load", label="Générer un nouveau Sudoku"),
        hr(),
        actionButton(inputId="solve", label="Afficher la solution")
      ),
      
      mainPanel(    
        uiOutput('matrix')     
      )
    )
    , 
    server = function(input,output){

      
      observeEvent(input$load,{
        if(input$radio=="easy"){n<-1}
        if(input$radio=="medium"){n<-2}
        if(input$radio=="hard"){n<-3}
        
        
        sudoku <- grille_incomplete(n)
        assign("grille",sudoku,envir = .GlobalEnv)
       

        output$matrix <- renderTable({
          
          matrix <- sudoku
          
        },na="",matrix,rownames = FALSE,colnames = FALSE)
        
     
      })
      
      
      observeEvent(input$solve,{

        
        
        solution <- solveur(grille)
        
        
        output$matrix <- renderTable({
          
          matrix <- solution
          
        },matrix,rownames = FALSE,colnames = FALSE)
        
        
      })

      
     
    }
  )
)