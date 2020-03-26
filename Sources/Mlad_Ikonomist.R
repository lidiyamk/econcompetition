#Set wd----
setwd("C:\\Users\\Kiril Spiridonov\\Desktop\\WD\\Mlad_Ikonomist")

#HDI ------------
HDI=read.csv(file="HDI Human Development Index.csv",stringsAsFactors = FALSE,na.strings = c("NA",""," "))
HDI[is.na(HDI)] <- 0

library(googleVis)
HDI_2000 <- data.frame(Country = HDI$HDI, HDI_Value = HDI$X2000)
plot(Geo)
Intensity <- gvisIntensityMap(HDI_2000)
plot(Intensity)

HDI_sort <- data.frame(Country = HDI_sort$HDI, x1980 = HDI_sort$X1980, x1990=HDI_sort$X1990,x2000 = HDI_sort$X2000,
                      x2011 = HDI_sort$X2011)
HDI_top100 <- head(HDI_sort,100)
Geo=gvisGeoChart(HDI_top100, locationvar="Country", 
                 colorvar="x1980",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)


# DESI ------------

rm(gama, alfa, beta, HDI, HDI_2000_first50, Intensity, Line)
rm(HDI_2000, Geo)
DESI <- read.csv("DESI.csv")
DESI_formated <- data.frame(Year = DESI$ï..Year, Country = DESI$Country, Weighted.Score = DESI$Weighted.Score)
filter <- DESI_formated$Year == 2017
DESI_2018 <- DESI_formated[filter,]
DESI_2017 <- DESI_formated[filter,]
Geo=gvisGeoChart(HDI_top100, locationvar="Country",
                 colorvar="Weighted.Score",
                 options=list(projection="kavrayskiy-vii",width=1200, height=800 ))
plot(Geo)

GeoStates <- gvisGeoChart(DESI_2018, "Country", "Weighted.Score",
                          options=list(region="EU", 
                                       displayMode="countries", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)
