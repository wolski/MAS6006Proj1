library(sp)
library(rgdal)
library(RColorBrewer)
library(tidyverse)

getData <- function(){
  yorkshireCrime <- read.csv("2017-11-south-yorkshire-street.csv",stringsAsFactors = FALSE)
  yorkshireCrime <- yorkshireCrime[!is.na(yorkshireCrime$Longitude)&!is.na(yorkshireCrime$Latitude),]  
  yorkshireCrime2 <- yorkshireCrime %>% mutate(Last.outcome.category = ifelse(Last.outcome.category == "", "none", Last.outcome.category)) %>% separate( LSOA.name, c("town"), sep=" ", remove=FALSE)
  yorkshireCrime2 <- yorkshireCrime2 %>% tidyr::unite(Location_LSOA , Location, LSOA.code, sep="_", remove=FALSE) 
  
  
  yorkshireCrime2 <- yorkshireCrime2 %>%
    group_by(town) %>%
    summarise(nrTown = n()) %>%
    left_join(., yorkshireCrime2)
  
  return(yorkshireCrime2)
}

countCrimes <- function(yorkshireCrime2){
  yorkshireCrime2 <- yorkshireCrime2 %>%
    group_by(town,Location_LSOA) %>%
    summarize(nrCrimes_Location= n()) %>% 
    ungroup() %>%
    left_join(.,yorkshireCrime2)
  
  yorkshireCrime2 <- yorkshireCrime2 %>%
    group_by(town,Location_LSOA,Crime.type) %>%
    summarize(nrCrimes_Location_Type= n()) %>%
    ungroup() %>%
    left_join(.,yorkshireCrime2)
  
  yorkshireCrime2 <- yorkshireCrime2 %>%
    group_by(town,Location_LSOA,Last.outcome.category) %>%
    summarize(nrCrimes_Location_LOC= n()) %>%
    ungroup() %>%
    left_join(.,yorkshireCrime2)
  
  yorkshireCrime2 <- yorkshireCrime2 %>%
    group_by(town, Location_LSOA,Crime.type,Last.outcome.category) %>%
    summarize(nrCrimes_Location_Type_LOC= n()) %>%
    ungroup() %>%
    left_join(.,yorkshireCrime2)
  return(yorkshireCrime2)
}


#'
#' @examples 
getAvailableCrimeTypes <- function(yorkshireCrime2, townSearch = "Sheffield", loc ="All"){
  if(loc != "All"){
    dataLastOutcomeFiltered <- yorkshireCrime2 %>% filter(Last.outcome.category == loc & town==townSearch)
  }else{
    dataLastOutcomeFiltered <- yorkshireCrime2 %>% filter( town==townSearch)
  }
  unique(dataLastOutcomeFiltered$Crime.type)
}

getAvailableLOC <- function(yorkshireCrime2, townSearch = "Sheffield", crime_type ="All"){
  if(crime_type != "All"){
    dataLastOutcomeFiltered <- yorkshireCrime2 %>% filter(Crime.type == crime_type & town==townSearch)
  }else{
    dataLastOutcomeFiltered <- yorkshireCrime2 %>% filter( town==townSearch)
  }
  unique(dataLastOutcomeFiltered$Last.outcome.category)
}


filterTheData <- function(yorkshireCrime2, townSearch = "Sheffield", loc ="All", crime_type="All"){
  if(loc == "All" & crime_type=="All"){
    dataLastOutcomeFiltered <- yorkshireCrime2 %>% 
      filter( town==townSearch) %>%
      select(town, Location, LSOA.code, nr_crimes = nrCrimes_Location ) %>%
      distinct() %>%
      arrange(desc(nr_crimes))
    
  }else if(crime_type == "All" & loc!="All"){
    dataLastOutcomeFiltered <- yorkshireCrime2 %>% 
      filter(Last.outcome.category == loc & town==townSearch) %>%
      select(town, Location, LSOA.code, Last.outcome.category, nr_crimes =  nrCrimes_Location_LOC ,total_nr_crimes = nrCrimes_Location ) %>%
      distinct() %>%
      arrange(desc(nr_crimes))
    
    
  }else if(crime_type != "All" & loc =="All"){
    dataLastOutcomeFiltered <- yorkshireCrime2 %>% 
      filter(Crime.type == crime_type & town==townSearch) %>%
      select(town, Location, LSOA.code, Crime.type, nr_crimes = nrCrimes_Location_Type ,total_nr_crimes = nrCrimes_Location ) %>%
      distinct() %>%
      arrange(desc(nr_crimes))
  }else{
    dataLastOutcomeFiltered <- yorkshireCrime2 %>%
      filter(Crime.type == crime_type & Last.outcome.category == loc & town==townSearch) %>%
      #,nrCrimes_Location_LOC,nrCrimes_Location_Type
      select(town, Location, LSOA.code,Crime.type, Last.outcome.category, nr_crimes = nrCrimes_Location_Type_LOC  , total_nr_crimes = nrCrimes_Location ) %>%
      distinct() %>%
      arrange(desc(nr_crimes))
  }
  return(dataLastOutcomeFiltered)
  
}

getBorder <- function(){
  border <-readOGR(dsn="./BoundaryData22", layer="england_lsoa_2011")
  #Point pattern
  wgs84 = '+proj=longlat +datum=WGS84'
  border <- spTransform(border, CRS(wgs84))
  towns <- data.frame(name = border$name,
                      town= sapply(strsplit(as.character(border$name),split=" "), function(x){x[1]})
  )
  border <- merge(border, towns)
  return(border)
}

makeMap <-function(xx, border, townSearch = "Sheffield"){
  toShow <- xx %>% dplyr::select(LSOA.code, nr_crimes)%>%
    group_by(LSOA.code) %>% summarize(nr_crimes = sum(nr_crimes))
  
  
  bdS <- border[border$town == townSearch,]
  bd <- merge(bdS, toShow, by.x="code", by.y="LSOA.code", all.x=TRUE)
  bd$nr_crimes[is.na(bd$nr_crimes)] <- 0
  nr_crimes <- bd@data$nr_crimes
  maxnrCrimes <- max(nr_crimes)
  if(maxnrCrimes >= 3){
    nrClasses <- max(3,min(max(nr_crimes),9))
    breaks <-ceiling(c(0,seq(1,max(nr_crimes),length.out = nrClasses-1)))
    classCut <-cut(nr_crimes , breaks)
    my_colours <- brewer.pal(nrClasses, "YlOrRd")
  } else if(maxnrCrimes ==2){
    my_colours <- c("white","orange","red")
    classCut <- as.factor(c("0","1","2")[nr_crimes + 1])
    
  } else if(maxnrCrimes == 1){
    my_colours <- c("white","red")
    classCut <- as.factor(c("0","1")[nr_crimes + 1])
  }
  plot(bd, col=my_colours[classCut], main=townSearch,border="gray")
  legend("bottomleft" ,legend = levels(classCut),
         fill = my_colours, bty = "n",cex=1.3, title="color coded number of crimes")
}
