#ThisIsKirooooo
#install.packages('quantmod')
library(quantmod)
#install.packages('dplyr')
library(dplyr)
#install.packages('infotheo')
library(infotheo)
#install.packages('caret')
library(caret)


#Main --------------
Filter <- DESI$Country == "Bulgaria"
DESI_BG <- DESI[Filter,]
Connectivity <- DESI_BG[which(DESI_BG$Indicator == "1 Connectivity"), ]
Connectivity <- Connectivity[,c(1,5)]
HumanCapital <- DESI_BG[which(DESI_BG$Indicator == "2 Human Capital"), ]
HumanCapital <- HumanCapital[,c(1,5)]
UseOfInternet <- DESI_BG[which(DESI_BG$Indicator == "3 Use of Internet"), ]
UseOfInternet <- UseOfInternet[,c(1,5)]
IntegrationOfDigitalTechnology <- DESI_BG[which(DESI_BG$Indicator == "4 Integration of Digital Technology"), ]
IntegrationOfDigitalTechnology <- IntegrationOfDigitalTechnology[,c(1,5)]
DigitalPublicServices <- DESI_BG[which(DESI_BG$Indicator == "5 Digital Public Services"), ]
DigitalPublicServices <- DigitalPublicServices[,c(1,5)]

Desi.Combined <- data.frame(Year = Connectivity$Year, Connectivity = Connectivity$`Weighted Score`, 
                            Human.Capital = HumanCapital$`Weighted Score`,
                            Use.Of.Internet = UseOfInternet$`Weighted Score`,
                            Integration.Of.Digital.Technology = IntegrationOfDigitalTechnology$`Weighted Score`,
                            Digital.Public.Services = DigitalPublicServices$`Weighted Score`)

#Ploting -----------------
windows()
plot(1, type = 'n', xlim = c(2012, 2020), ylim = c(-1, 20), 
     xlab = "Year", ylab = "Index Value")
lines(Desi.Combined$Year, Desi.Combined$Connectivity, col = "blue", xlab = "Year", ylab = "Index Value", main = "DESI Bulgaria", type = "l")
lines(Desi.Combined$Year, Desi.Combined$Human.Capital, col = "#FF5733", xlab = "Year", ylab = "Index Value", main = "DESI Bulgaria", type = "l")
lines(Desi.Combined$Year, Desi.Combined$Use.Of.Internet, col = "#33FF95", xlab = "Year", ylab = "Index Value", main = "DESI Bulgaria", type = "l")
lines(Desi.Combined$Year, Desi.Combined$Integration.Of.Digital.Technology, col = "#E42ABF", xlab = "Year", ylab = "Index Value", main = "DESI Bulgaria", type = "l")
lines(Desi.Combined$Year, Desi.Combined$Digital.Public.Services, col = "#44AA45", xlab = "Year", ylab = "Index Value", main = "DESI Bulgaria", type = "l")
GG <- lm(Connectivity + Human.Capital + Use.Of.Internet + Integration.Of.Digital.Technology
         +Digital.Public.Services ~ Year, data = Desi.Combined )
lines(Desi.Combined$Year, GG$fitted.values, col = "blue", xlab = "Year", ylab = "Index Value", main = "DESI Bulgaria", type = "l")


library(googleVis)
Line <- gvisLineChart(Desi.Combined, options=list(width=2000, height=1000))
plot(Line)

plot(Desi.Combined)

#install.packages("corrplot")
library(corrplot)
DC <- cor(Desi.Combined)
windows()
corrplot(DC, method="pie")
