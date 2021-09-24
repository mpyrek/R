#w zbiorze danych road z bibilioteki Mass mamy ramke danych z rocznymi ofiarami śmiertelnymi w wypadkach drogowych w połowie stanów USA.
#nasze rozwazania beda dt. analizy zmiennej "deaths"-oznaczajaca liczbe zabitych w danym stanie do pozostaych zmiennych

#1) Zależnoc zmiennej deaths od drivers(ilosci kierowców)(zmienna podawana w 10000)
drivers<-drivers*10000
lmFitSimple <- lm(deaths ~drivers, data = road)
summaryList <- summary(lmFitSimple)

x<-summaryList$coefficients[4]
x1<-(1-x)
#dany wspóczynnik zależy istotnie od predykatora, ponieważ nasz blad standardowy x jest rowny  0.2896782 => x1= 0.7103218 jest daleko od 0

lmFitSimple$coefficients[2]
#nasza zależnoć jest rosnca, bo wspóczynnik kierunkowy(drives) jest wiekszy od 0

summaryList$r.squared
#wartosc summaryList$r.squared jest bardzo bliska 1 co oznacza, ze jakosc dopasowania modelu liniowego jest wysoka i nie jest to zaleznosc nieliniowa.

plot(drivers,deaths)
abline(lmFitSimple)



#2)Zależnoc zmiennej deaths od popden-gestosci zaludnienia
lmFitSimple1 <- lm(deaths ~ popden, data = road)
summaryList1 <- summary(lmFitSimple1)

y<-summaryList1$coefficients[4]
y1<-(1-y)
#dany wspolczynnik nie zalezy istotnie od predykatora

lmFitSimple1$coefficients[2] #=> nasza zaleznosc malejaca
summaryList1$r.squared #=>wartosc bardzo bliska 0 => jakosc dopasowania liniowego jest niska i moga byc tu przeslanki o zaleznosci nieliniowej

plot(popden,deaths)
abline(lmFitSimple1)



#3)deaths od rural( dlugosc drog wiejskich w 1000 milach )
rural<-rural*1000
lmFitSimple2 <- lm(deaths ~rural, data = road)
summaryList2 <- summary(lmFitSimple2)

summaryList2$coefficients[2] #=> poniewaz nasz wspolczynik jest duzo wiekszy od 0 juz tutaj mozemy stwierdzic ze nasza zaleznosc nie jest zalenzna od predykatora
lmFitSimple2$coefficients[2]#=> nasza zaleznosc jest rosnaca
summaryList2$r.squared #=> jakosc dopasowania liniowego jest niska i moga byc tu przeslanki o zaleznosci nieliniowej

plot(rural,deaths)
abline(lmFitSimple2)



#4) deaths od temp(srednich max temp. w styczniu)
lmFitSimple3 <- lm(deaths ~ temp, data = road)
summaryList3 <- summary(lmFitSimple3)

summaryList3$coefficients[2] #=> poniewaz nasz wspolczynik jest duzo wiekszy od 0 juz tutaj mozemy stwierdzic ze nasza zaleznosc nie jest zalenzna od predykatora
lmFitSimple3$coefficients[2]#=> nasza zaleznosc jest rosnaca
summaryList3$r.squared #=>wartosc bardzo bliska 0 => jakosc dopasowania liniowego jest niska i moga byc tu przeslanki o zaleznosci nieliniowej

plot(temp,deaths)
abline(lmFitSimple3)


