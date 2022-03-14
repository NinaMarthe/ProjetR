# genere la grille incomplete aleatoirement. la fonction prend en parametre le nombre de 
# cases blanches souhaitees
grille_incomplete<-function(d){
s<-grille_complete()
if(d==1){n<-30}
if(d==2){n<-50}
if(d==3){n<-70}
if(d>3){
  print('mauvaise difficulté choisie')
  break()
  }

for(i in 1:n){
  a<-c(sample(1:9),2)
  s[a[1],a[2]]<-NA
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
      if (!is.na(s[i,j]) & s[i,j]%in%li) {
        return(F)
      }
      # rentrer la valeur de la case dans la liste pour comparer avec les autres cases
      li<-c(li,s[i,j])
    }
  }
  
  # colonnes
  for(i in 1:9){
    li<-c(0) # liste pour la ligne i
    for(j in 1:9){
      # tester si la valeur de la case est dans la liste
      if (!is.na(s[j,i]) & s[j,i]%in%li) {
        return(F)
      }
      # rentrer la valeur de la case dans la liste pour comparer avec les autres cases
      li<-c(li,s[j,i])
    }
  }
  
  # carres
  for(i in 0:2){
    for(j in 0:2){
      li<-c()
      for(k in (1:3)+i*3){
        for(l in (1:3)+j*3){
          if (!is.na(s[k,l]) & s[k,l]%in%li) {
            return(F)
          }
          li<-c(li,s[k,l])
        }
      }
      
    }
  }
  
  return(T)
  
}
####################################################################################

# prend en parametre une grille incomplete et renvoie une matrice contenant les numeros candidats
# pour chaque case de la grille, sous forme de liste. la matrice ne contient rien pour les cases
# non vides de la grille d'entrée
candidats<-function(s){

  a<-matrix(nrow=9,ncol=9)
  mode(a) <- "list"
  
for(i in 1:9){
  for(j in 1:9){
    if(is.na(s[i,j])){
    # constituer un vecteur avec les numéros non possibles dans cette case
    l<-c()
    
    # ligne : rentrer dans le vecteur les numéros présents dans la ligne
    for(k in 1:9){
      if(!is.na(s[i,k])){l<-c(l,s[i,k])}
    }
    
    # colonne : rentrer dans le vecteur les numéros présents dans la colonne
    for(k in 1:9){
      if(!is.na(s[k,j])){l<-c(l,s[k,j])}
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
        if(!is.na(s[x,y])){l<-c(l,s[x,y])}
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

######################################################################

#renvoie vrai si la grille est complete
complete<-function(s){
  co=T
  for(i in 1:9){
    for(j in 1:9){
      if(is.na(s[i,j])){co=F}
    }
  }
  co
}
#######################################################################