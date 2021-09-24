
#prob-funkcja dokonujaca normalizacji naszej macierzy 
prob = function(m) {
  return(m/sum(m))
}

#prob oznacza nasza znormalizowana macierz przez funkcje wyzej
#boundary-zwraca wektor rozkładu brzegowego wzgledem zmiennej numer i
#wartosci wektora tego rozkadu liczymy sumujc po kolei każda kolumne lub wiersz, za kazdym razem dodajac je do naszego wektora  
boundary = function(prob, i) {
  bound = 0
  if (i == 1) {
    for (row in 1:dim(prob)[1]) {
      bound = c(bound, sum(prob[row,]))
    }
    return(bound)
  } 
  else {
    for(col in 1:dim(prob)[2]) {
      bound = c(bound, sum(prob[,col]))
    }
    return(bound)
  }
}

#conditional-wraca wektor rozkładu warunkowego wzgledem wartosci v zmiennej numer i
#v musi być mniejsze od n lub m w zależnosci od i
conditional = function(prob, i, v) {
  if (i == 1) {
    return(prob[v,])
  } 
  else {
    return(prob[,v])
  }
}

#mean-zwraca wartosc oczekiwana składowej numer i
#zmienna expect jest nasz wartoscia oczekiwana liczona w sposob pokazany na wykladzie
mean = function(prob, i= NA) {
  if (i == 1) {
    expect = 0
    for (row in 1:dim(prob)[1]) {
      expect = expect + sum(prob[row,]*row)
    }
    return(expect)
  } 
  else if(i==2){
    expect = 0
    for (col in 1:dim(prob)[2]) {
      expect = expect + sum(prob[,col]*col)
    }
    return(expect)
  }
  else if(is.na(i)){
    expect = c(0,0)
    for (row in 1:dim(prob)[1]) {
      for (col in 1:dim(prob)[2]) {
        expect[1] = expect[1] + row*prob[row,col]
        expect[2] = expect[2] + col*prob[row,col]
      }
    }
    return(expect)
  } 
}

#covariance-zwraca macierz kowariancji
covariance=function(prob){
  C<-matrix(nrow=dim(prob)[1], ncol=dim(prob)[2])
  for (row in 1:dim(prob)[1]) {
    for (col in 1:dim(prob)[2]) {
      if(row == col){
        var1=row*mean(prob,row)
        var2=col*mean(prob,row)
        r=mean(prob,i=row)
        c=mean(prob,i=col)
        C[row,col]=(row*col*r*c)/sqrt(var1*var2)
        
      }
      else{
        r=mean(prob,i=row)
        c=mean(prob,i=col)
        C[row,col] = row*col*r*c-r*c
      }
    }
  }
  return(C)
}
  

#independent
# jeżeli chociaz jeden iloczyn rozkładów brzegowych i,j nie jest równy wartosci maciezry i,j to nasza funkcja powinna zwrocic False
independent = function(prob) {
  r = boundary(prob, 1)
  c = boundary(prob, 2)
  for (row in 1:dim(prob)[1]) {
    for (col in 1:dim(prob)[2]) {
      if (r[row] * c[col] != prob[row,col]) {
        return(FALSE) 
      }
    }
  }
  return(TRUE)
}