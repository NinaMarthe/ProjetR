grille<-function(){
  s<-matrix(, 9, 9) # générer la matrice
  
  a<- c(sample(1:9,9)) # générer la première ligne
  
  # remplir la première ligne
  for  (i in 1:9) {
    s[1,i]<-a[i]
  }
  
  # remplir les lignes 2 et 3 en décalant de 3 positions les numéros de la ligne au dessus
  for(i in 2:3){
    for(j in 1:5){
      s[i,(j+3)%%9]<-s[i-1,j]
    }
    for(j in 7:9){
      s[i,(j+3)%%9]<-s[i-1,j]
    }
    s[i,9]<-s[i-1,6]
  }
  
  # remplir la première colonne
  for(i in 1:3){
    s[i+6,1]<-s[i,2]
    s[i+3,1]<-s[i,3]
  }
  
  # remplir les colonnes 2 et 3
  for(i in 2:3){
    for(j in 1:5){
      s[(j+3)%%9,i]<-s[j,i-1]
    }
    s[9,i]<-s[6,i-1]
  }
  
  # remplir les colonnes 4 à 6
  for(i in 1:3){
    s[i+6,4]<-s[i,5]
    s[i+3,4]<-s[i,6]
  }
  
  for(i in 5:6){
    for(j in 1:5){
      s[(j+3)%%9,i]<-s[j,i-1]
    }
    s[9,i]<-s[6,i-1]
  }
  
  # remplir les colonnes 7 à 9
  for(i in 1:3){
    s[i+6,7]<-s[i,8]
    s[i+3,7]<-s[i,9]
  }
  
  for(i in 8:9){
    for(j in 1:5){
      s[(j+3)%%9,i]<-s[j,i-1]
    }
    s[9,i]<-s[6,i-1]
  }
  
  s 
}

# échanger deux colonnes

echcol<-function(s){
  b<-c(sample(1:9,2))
  d<-c()
  e<-c()
 # récupérer les valeurs des deux colonnes dans des vecteurs 
  for(i in 1:9){
    d[i]<-s[i,b[1]]
  }
  for(i in 1:9){
    e[i]<-s[i,b[2]]
  }
  # remplacer les deux colonnes
  for(i in 1:9){
    s[i,b[2]]<-d[i]
  }
  for(i in 1:9){
    s[i,b[1]]<-e[i]
  }
  s
}


# échanger deux lignes

echrow<-function(s){
  f<-c()
  g<-c()
  c<-c(sample(1:9,2))
  # récupérer les valeurs des deux lignes dans des vecteurs
  for(i in 1:9){
    f[i]<-s[c[1],i]
  }
  for(i in 1:9){
    g[i]<-s[c[2],i]
  }
  # remplacer les deux lignes
  for(i in 1:9){
    s[c[2],i]<-f[i]
  }
  for(i in 1:9){
    s[c[1],i]<-g[i]
  }
  s
}

#"mélanger" la grille, effectuer des échanges de lignes et de colonnes
sudoku<-function(){
  s<-grille()
  for(i in 1:10){
    s<-echcol(s)
    s<-echrow(s)
  }
  s
}
