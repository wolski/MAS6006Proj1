rm(list=ls())
source("HelperFunctions.R")
yorkshireCrime2 <- getData()
yorkshireCrime2 <- countCrimes(yorkshireCrime2)
border <-getBorder()
xx <- filterTheData(yorkshireCrime2,loc = "All", crime_type = "All" )

xx <- filterTheData(yorkshireCrime2,loc = "Awaiting court outcome", crime_type = "All" )
xx <- filterTheData(yorkshireCrime2,loc = "All", crime_type = "Drugs" )
xx <- filterTheData(yorkshireCrime2,loc = "Awaiting court outcome", crime_type = "Drugs" )


xx <- filterTheData(yorkshireCrime2,loc = "All", crime_type = "All" )
makeMap(xx,border)


xx <- filterTheData(yorkshireCrime2,loc = "Awaiting court outcome", crime_type = "Vehicle crime" )
head(xx)

outfile <- tempfile(fileext = '.png')

makeMap(xx,border)


