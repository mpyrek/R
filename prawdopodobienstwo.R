#tworzymy sobie wektor reprezentujcy nasza przestrzeñ
t<-c((1:N)/sum(1:N))

event<-function(x,N=6){
  g<-rep(FALSE,length(N))
  if(all(x<=N)){
    for(i in 1:length(x)){
      g[x[i]]=TRUE
    }   
  }
  return(g)
}
#wszystkie funkcje bazuj na tym ¿e zdarzenia w naszej przestrzeni reprezentowane przez wektor logiczny typu: True, False;
#suma
union<-function(e1,e2){
  return(e1 | e2)
} 

#iloczyn
intersect<-function(e1,e2){
  return(e1 & e2)
}

#dopelnienie
complement<-function(e){
  return(!e)
}

#prawdopodobieñstwo zdarzenia event
prob<-function(space,event){
  g=0
  for(i in 1:6){
    if(event[i]) g<-g+space[i]
  }
  return(g)
}