library(maptools)
library(RColorBrewer)
library(ggmap)
library(rgdal)
library(ggplot2)
library(rgeos)
library(leaflet)
library(plyr)
library(gridExtra)
library(cowplot)
library(stringr)
library(stringi)

#PROPORTIONAL MAPS
map2<-readOGR("ShapeFiles","UK_Constituencies_2015Election_Hexagon")
map2@data$id = rownames(map2@data)
map2.points = fortify(map2)
map2.df = join(map2.points, map2@data, by="id")
info2017=read.csv("Output/ans2017.csv", header=TRUE)  
info2015=read.csv("Output/ans2015.csv", header=TRUE)
info2010=read.csv("Output/ans2010.csv", header=TRUE)

map.gb=map2.df[1:4527,]
map.ni=map2.df[4528:4660,]

#remove two newrys in shape file
map.ni.rem=map.ni[map.ni$Constituen!="Newry & Armagh",]
map.ni.newar=map.ni[map.ni$Constituen=="Newry & Armagh",]
map.ni.newar=map.ni.newar[8:14,]
map.ni=rbind(map.ni.rem,map.ni.newar)

map2.df=rbind(map.gb,map.ni)

#add CV and FPTP results for 2017 to dataframe with coordinates
for (i in 1:nrow(map2.df)){
  if(length(info2017$CV[info2017$Constituency %in% map2.df$Constituen[i]])==0){
    strhyp=str_split(as.character(map2.df$Constituen[i]), "-", simplify = TRUE)
    strspa=str_split(as.character(map2.df$Constituen[i]), " ", simplify = TRUE)
    if(length(info2017$CV[sapply(toupper(strhyp), grepl, toupper(as.character(info2017$Constituency)))])==0){
      match=sapply(strspa, grepl, as.character(info2017$Constituency), ignore.case=TRUE)
      map2.df[i, "CV2017"]=info2017$CV[which.max(rowSums(match))]
    }else{
      match=sapply(strhyp, grepl, as.character(info2017$Constituency), ignore.case=TRUE)
      map2.df[i, "CV2017"]=info2017$CV[which.max(rowSums(match))]
    }
  }else{  
    map2.df[i, "CV2017"]=info2017$CV[info2017$Constituency %in% map2.df$Constituen[i]]
  }
}
for (i in 1:nrow(map2.df)){
  if(length(info2017$FPTP[info2017$Constituency %in% map2.df$Constituen[i]])==0){
    strhyp=str_split(as.character(map2.df$Constituen[i]), "-", simplify = TRUE)
    strspa=str_split(as.character(map2.df$Constituen[i]), " ", simplify = TRUE)
    if(length(info2017$FPTP[sapply(toupper(strhyp), grepl, toupper(as.character(info2017$Constituency)))])==0){
      match=sapply(strspa, grepl, as.character(info2017$Constituency), ignore.case=TRUE)
      map2.df[i, "FPTP2017"]=info2017$FPTP[which.max(rowSums(match))]
    }else{
      match=sapply(strhyp, grepl, as.character(info2017$Constituency), ignore.case=TRUE)
      map2.df[i, "FPTP2017"]=info2017$FPTP[which.max(rowSums(match))]
    }
  }else{  
    map2.df[i, "FPTP2017"]=info2017$FPTP[info2017$Constituency %in% map2.df$Constituen[i]]
  }
}

#add CV and FPTP results for 2015 to dataframe with coordinates
for (i in 1:nrow(map2.df)){
  if(length(info2015$CV[info2015$Constituency %in% map2.df$Constituen[i]])==0){
    strhyp=str_split(as.character(map2.df$Constituen[i]), "-", simplify = TRUE)
    strspa=str_split(as.character(map2.df$Constituen[i]), " ", simplify = TRUE)
    if(length(info2015$CV[sapply(toupper(strhyp), grepl, toupper(as.character(info2015$Constituency)))])==0){
      match=sapply(strspa, grepl, as.character(info2015$Constituency), ignore.case=TRUE)
      map2.df[i, "CV2015"]=info2015$CV[which.max(rowSums(match))]
    }else{
      match=sapply(strhyp, grepl, as.character(info2015$Constituency), ignore.case=TRUE)
      map2.df[i, "CV2015"]=info2015$CV[which.max(rowSums(match))]
    }
  }else{  
    map2.df[i, "CV2015"]=info2015$CV[info2015$Constituency %in% map2.df$Constituen[i]]
  }
}
for (i in 1:nrow(map2.df)){
  if(length(info2015$FPTP[info2015$Constituency %in% map2.df$Constituen[i]])==0){
    strhyp=str_split(as.character(map2.df$Constituen[i]), "-", simplify = TRUE)
    strspa=str_split(as.character(map2.df$Constituen[i]), " ", simplify = TRUE)
    if(length(info2015$FPTP[sapply(toupper(strhyp), grepl, toupper(as.character(info2015$Constituency)))])==0){
      match=sapply(strspa, grepl, as.character(info2015$Constituency), ignore.case=TRUE)
      map2.df[i, "FPTP2015"]=info2015$FPTP[which.max(rowSums(match))]
    }else{
      match=sapply(strhyp, grepl, as.character(info2015$Constituency), ignore.case=TRUE)
      map2.df[i, "FPTP2015"]=info2015$FPTP[which.max(rowSums(match))]
    }
  }else{  
    map2.df[i, "FPTP2015"]=info2015$FPTP[info2015$Constituency %in% map2.df$Constituen[i]]
  }
}

#add CV and FPTP results for 2010 to dataframe with coordinates
for (i in 1:nrow(map2.df)){
  if(length(info2010$CV[info2010$Constituency %in% map2.df$Constituen[i]])==0){
    strhyp=str_split(as.character(map2.df$Constituen[i]), "-", simplify = TRUE)
    strspa=str_split(as.character(map2.df$Constituen[i]), " ", simplify = TRUE)
    if(length(info2010$CV[sapply(toupper(strhyp), grepl, toupper(as.character(info2010$Constituency)))])==0){
      match=sapply(strspa, grepl, as.character(info2010$Constituency), ignore.case=TRUE)
      map2.df[i, "CV2010"]=info2010$CV[which.max(rowSums(match))]
    }else{
      match=sapply(strhyp, grepl, as.character(info2010$Constituency), ignore.case=TRUE)
      map2.df[i, "CV2010"]=info2010$CV[which.max(rowSums(match))]
    }
  }else{  
    map2.df[i, "CV2010"]=info2010$CV[info2010$Constituency %in% map2.df$Constituen[i]]
  }
}
for (i in 1:nrow(map2.df)){
  if(length(info2010$FPTP[info2010$Constituency %in% map2.df$Constituen[i]])==0){
    strhyp=str_split(as.character(map2.df$Constituen[i]), "-", simplify = TRUE)
    strspa=str_split(as.character(map2.df$Constituen[i]), " ", simplify = TRUE)
    if(length(info2010$FPTP[sapply(toupper(strhyp), grepl, toupper(as.character(info2010$Constituency)))])==0){
      match=sapply(strspa, grepl, as.character(info2010$Constituency), ignore.case=TRUE)
      map2.df[i, "FPTP2010"]=info2010$FPTP[which.max(rowSums(match))]
    }else{
      match=sapply(strhyp, grepl, as.character(info2010$Constituency), ignore.case=TRUE)
      map2.df[i, "FPTP2010"]=info2010$FPTP[which.max(rowSums(match))]
    }
  }else{  
    map2.df[i, "FPTP2010"]=info2010$FPTP[info2010$Constituency %in% map2.df$Constituen[i]]
  }
}


write.csv(map2.df, "source/data/propmapFPTPCV2010-2017.csv")