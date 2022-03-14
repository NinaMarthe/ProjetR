####################################################################################################

# renvoie vrai si n est un candidat possible aux coordonées i,j dans la grille s
candidat<-function(s, i, j, n){
  ca<-candidats(s)
  return(n%in%ca[i,j][[1]])
}

####################################################################################################

# rend la liste des candidats de la case de coordonées i,j dans la grille s
candidatscase<-function(s,i,j){
  ca<-candidats(s)
  return(ca[i,j][[1]])
}

####################################################################################################

# fonction complète pour résoudre le sudoku
solveur <- function(s){
  
  if(complete(s)){
    
    return(s)
    
  }
  
  # récupérer les coordonées des cases vides : 
  
  # compter les cases vides pour construire la matrice
  m<-1
  for(k in 1:9){
    for (l in 1:9){
      if(is.na(s[k,l])){
        m<-m+1
      }
    }
  }
  a<-matrix(ncol=2,nrow=m)
  
  # remplir la matrice a avec les coordonnées des cases vides
  m<-1
  for(k in 1:9){
    for (l in 1:9){
      if(is.na(s[k,l])){
        a[m,1]<-k
        a[m,2]<-l
        m<-m+1
      }
    }
  }
  
  i <- a[1, 1]
  j <- a[1, 2]
  
  p <- candidatscase(s, i, j)
  
  for(n in p){
    # tester récursivement toutes les possibilités pour la case aux coordonnées i,j, 
    # jusqu'à en trouver une qui permette d'avoir une grille valide : 
    
    s[i, j] <- n
    solution <- solveur(s) 
    
     if(!is.null(solution)){
      # solveur renvoie null si une case vide n'a aucun candidat, donc si la grille n'est pas valide
       return(solution)
     }
  }
  
 return(NULL)
}
####################################################################################################
