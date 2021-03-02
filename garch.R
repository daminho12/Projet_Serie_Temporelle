library(data.table)
library(ggplot2)
library(dplyr)
library(tseries)
library(e1071)
library(lubridate)
library(rugarch)


hsi=fread("C:/Users/USER/Desktop/Master_2/Serie temporelle/ARMA/^HSI.csv")
attach(hsi)
library(ggplot2)

hsi[,5]<- as.numeric(hsi$Close)
#hsi[,1]<- as.numeric(hsi$Date)

#install.packages("tseries")
#library(tseries)

#Close<-na.omit(Close)
#Close<-as.numeric(Close)
#adf.test(Close)


dhsi <- hsi %>% select("Date","Close")
summary(dhsi)
dhsi$Close[dhsi$Close== "0"]<-NA
dhsi<-dhsi[!is.na(dhsi$Close),]

summary(dhsi)
dim(dhsi)
####bien

#Close <- as.numeric(dhsi$Close)
adf.test(dhsi$Close) #Test de Dickey-Fuller
## Effet levier, autocorrélation sur série négative puis en valeur absolue ----
## Graphique temporel ----
Base <- as.data.frame(dhsi)
gg <- ggplot(Base, aes(x=ymd(dhsi$Date), y=dhsi$Close)) +
  xlab("Time") + ylab("Close") + geom_line(size = 0.7, col="blue") 
gg

Close<-(dhsi$Close)

adf.test(dhsi$Close)


n <- length(Close)
rende <-rep(0,n); rende[2:n] <- log(Close[2:n]/Close[1:(n-1)])*100


## Graphique temporel ----
v <- rende ; ind <- 0:(length(v)-1)
Base <- data.frame( vals=v, index=ind)
colnames(Base) <- c("vals","ind")
gg1 <- ggplot(Base, aes(x=Base$ind, y=Base$vals)) +
  geom_line(size = 0.7) + xlab("Time") + ylab("rende") 
gg1


adf.test(rende)

###Fonction qui retourne les valeurs positives et negatives

PN <- function(rende) {
  res=matrix(0,length(rende),2)
  for (i in 1:length(rende)){
    if (rende[i] >= 0) {
      res[i,1]<-rende[i]
    }
    else {
      res[i,2]<-rende[i]
    }
  }
  return(res) }

## Les rendements positifs, négatifs et en valeurs absolues

RP <- PN(rende)[,1]
RN <- PN(rende)[,2]
Rend_abs <- abs(rende)

## Autocovariance et Autocorrélation d’une série univariée ----

acf.univ <- function (ser, h){
  n <- length(ser)
  autocov <- c(0)
  autocor <- c(0)
  h <- abs(h)
  centre <- ser-mean(ser)
  autocov <- sum(centre[1:(n-h)]*centre[(h+1):n])/n
  autocor <- autocov/(sum(centre[1:n]*centre[1:n])/n)
  list(autocov=autocov, autocor=autocor) }

## Fonction qui calcule les corrélations, centrées ----

Cor <- function(h,rend1,rend2){
  tmp1=0
  tmp2=0
  rend1 <- rend1-mean(rend1)
  rend2 <- rend2-mean(rend2)
  for(i in 1:(length(rend1)-h)){
    tmp1 <- tmp1+(rend1[i])*(rend2[i+h]) }
  gammah <-tmp1/length(rend1)
  gamma0rend1 <- acf.univ(rend1, 0)$autocov
  gamma0rend2 <- acf.univ(rend2, 0)$autocov
  gamma0 <- sqrt(gamma0rend1*gamma0rend2)
  return(gammah/gamma0) }

## Calcul des autocorrelations ----
h=30
Autocorr_rend <- rep(0,h) ; Autocorr_rend_carre <- rep(0,h)
for(i in 1:h){
  Autocorr_rend[i]=Cor(i,rende,rende)
  Autocorr_rend_carre[i]=Cor(i,rende^2,rende^2) }
Autocorr_rend; Autocorr_rend_carre

par(mfrow=c(2,2))

acf(rende,main="ACF pour rendement",rendlab="retard")
pacf(rende,main=" PACF pour rendement",rendlab="retard")
acf(rende^2,main="ACF pour rendement au carré",rendlab="retard")
pacf(rende^2,main=" PACF pour rendement au carré",rendlab="retard")

#plot.ts(rende^2,main="Carrée du rendement du cours")
#plot.ts(Rend_abs,main="Valeur absolu du rendement du cours ")


## Queues épaisses ----

kurtosis(rende)
gg <- ggplot(Base, aes(x=Base$vals)) +
  geom_histogram(aes(y=..density..), col="black", fill="white",alpha=.6) +
  geom_density(alpha=.4, fill="green") +
  geom_rug(col="blue") + xlab("Histogramm of rende") 
gg

## Test de normalité : Kolmogorov-Smirnov(rend) ----
ks.test(rende,"pnorm",mean(rende),sd(rende))

## Effet de levier ----
positivecor<-rep(0,8)
negativecor<-rep(0,8)
for (h in c(1,2,3,4,5,10,15,20)) {
  positivecor[h]=Cor(h,RP,Rend_abs)
  negativecor[h]=Cor(h,-RN,Rend_abs)
}
positivecor[c(1,2,3,4,5,10,15,20)]
negativecor[c(1,2,3,4,5,10,15,20)]

## Estimation des coefficients du modèle GARCH ----

base <- cbind.data.frame(Date=dhsi$Date,rende)
GARCH <- ugarchspec(variance.model =list(model="fGARCH",submodel ="GARCH",
                                         garchOrder = c(1, 1)))
modele1 <- ugarchfit(rende,spec=GARCH)
show(modele1)

gg <- ggplot(base,aes(x=ymd(base$Date), y=base$rende)) +
  geom_line(aes(y=2*modele3@fit[["sigma"]]), col="red")+
geom_line(aes(y=-2*modele3@fit[["sigma"]]), col="red")+
xlab("Time") + ylab("rende") + geom_line(size = 0.7)
gg

TGARCH <- ugarchspec(variance.model = list(model="fGARCH",submodel ="TGARCH",
                                           garchOrder = c(1, 1)))
modele2 <- ugarchfit(rende,spec=TGARCH)
show(modele2)
EGARCH <- ugarchspec(variance.model = list(model="eGARCH", garchOrder = c(1, 1)))
modele3 <- ugarchfit(rende, spec=EGARCH)
show(modele3)

## News impacts curves----
ni=newsimpact(z=NULL, modele1); ni2=newsimpact(z=NULL, modele2)
ni3=newsimpact(z=NULL, modele3)
n1 <- cbind.data.frame(zx=ni$zx, zy=ni$zy, models=rep("GARCH(1,1)",length(ni$zy)))
n2 <- cbind.data.frame(zx=ni2$zx, zy=ni2$zy, models=rep("TGARCH(1,1)",length(ni$zy)))
n3 <- cbind.data.frame(zx=ni3$zx, zy=ni3$zy, models=rep("EGARCH(1,1)",length(ni$zy)))
NIC <- rbind(n1,n2,n3)
gg <- ggplot(NIC) + aes(x=zx, y=zy, col=models) + geom_line(size = 0.7)+
  ggtitle("News Impact Curve") + xlab(ni$xexpr) + ylab(ni$yexpr)
gg





################################################################################
  
