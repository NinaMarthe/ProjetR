# genere la grille incomplete aleatoirement. la fonction prend en parametre le nombre de 
# cases blanches souhaitees
grille2<-function(n){
s<-sudoku()
for(i in 1:n){
  a<-c(sample(1:9),2)
  s[a[1],a[2]]<-0
}
s
}
############################################################################

# renvoie vrai si la grille est valide (pas de repetition dans chaque ligne, colonne, carre)
valide<-function(s){
  # lignes
  for(i in 1:9){
    li<-c() # liste pour la ligne i
    for(j in 1:9){
      # tester si la valeur de la case est dans la liste
      if (s[i,j]!=0 & s[i,j]%in%li) {
        return(F)
      }
      # rentrer la valeur de la case dans la liste pour comparer avec les autres cases
      li<-c(li,s[i,j])
    }
  } # fin lignes
  
  # colonnes
  for(i in 1:9){
    li<-c(0) # liste pour la ligne i
    for(j in 1:9){
      # tester si la valeur de la case est dans la liste
      if (s[j,i]!=0 & s[j,i]%in%li) {
        return(F)
      }
      # rentrer la valeur de la case dans la liste pour comparer avec les autres cases
      li<-c(li,s[j,i])
    }
  } # fin colonnes
  
  # carres
  for(i in 0:2){
    for(j in 0:2){
      li<-c()
      for(k in (1:3)+i*3){
        for(l in (1:3)+j*3){
          if (s[k,l]!=0 & s[k,l]%in%li) {
            return(F)
          }
          li<-c(li,s[k,l])
        }
      }
      
    }
  } # fin carres
  
  return(T)
  
} # fin fonction valide
####################################################################################

# prend en parametre une grille incomplete et renvoie une matrice contenant les numeros candidats
# pour chaque case de la grille, sous forme de liste. la matrice ne contient rien pour les cases
# non vides de la grille d'entrée
candidats<-function(s){
  
#a<-matrix(rep(list(),81),9,9)
  a<-matrix(nrow=9,ncol=9)
  mode(a) <- "list"
  
for(i in 1:9){
  for(j in 1:9){
    if(s[i,j]==0){
    # constituer un vecteur avec les numéros non possibles dans cette case
    l<-c()
    
    # ligne : rentrer dans le vecteur les numéros présents dans la ligne
    for(k in 1:9){
      if(s[i,k]!=0){l<-c(l,s[i,k])}
    }
    
    # colonne : rentrer dans le vecteur les numéros présents dans la colonne
    for(k in 1:9){
      if(s[k,j]!=0){l<-c(l,s[k,j])}
    }
    
    # carré : 
    
    # trouver dans quel carré se trouve la case i,j
    carre<-c(0,0)
    if(i%in%1:3){
      carre[1]<-0
    }else if(i%in%4:6){
      carre[1]<-1
    }else{carre[1]<-2}
    
    if(j%in%1:3){
      carre[2]<-0
    }else if(j%in%4:6){
      carre[2]<-1
    }else{carre[2]<-2}
    
    # rentrer dans le vecteur les numéros présents dans ce carré
    for(x in (1:3)+carre[1]*3){
      for(y in (1:3)+carre[2]*3){
        if(s[x,y]!=0){l<-c(l,s[x,y])}
      }
    }
    
    # faire le "negatif" de cette liste
    m<-c()
    for(n in 1:9){
      if(!n%in%l){
        m<-c(m,n)
      }
    }
    
    # puis mettre cette liste dans la matrice a
    a[i,j]<-list(m)
    }
    }
}
a
}
#####################################################################

# renvoie vrai si la grille comporte des cases qui ont n candidats
candidatn<-function(te,n){
  p=F
  for(i in 1:9){
    for(j in 1:9){
      #si case non vide et de taille n
      if(!is.na(te[i,j]) & length(te[i,j][[1]])==n){
        p<-T
      }
    }
  }
  p
}
######################################################################

# renvoie vrai si la grille est complete
complet<-function(te){
  co=T
  for(i in 1:9){
    for(j in 1:9){
      if(!is.na(te[i,j])){co=F}
    }
  }
  co
}
#######################################################################

# prend une grille en parametre et la renvoie avec ses cases à un seul candidat remplies
remplir1<-function(s){
  ca<-candidats(s)
  while(complet(ca)==F & candidatn(ca,1)==T){
    ca<-candidats(s)
    #remplir les cases a 1 candidat.
    for(i in 1:9){
      for(j in 1:9){
        #si case non vide et de taille 1
        if(!is.na(ca[i,j]) & length(ca[i,j][[1]])==1){
          s[i,j]<-ca[i,j][[1]][[1]]
        }
      }
    }
  }
  s
}
#######################################################################