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
library(installr)
library(devtools)
library(magick)
library(animation)

map2<-readOGR("ShapeFiles","UK_Constituencies_2015Election_Hexagon")
map2@data$id = rownames(map2@data)
map2.points = fortify(map2)
map2.df = join(map2.points, map2@data, by="id")

map.gb=map2.df[1:4527,]
map.ni=map2.df[4528:4660,]

#remove two newrys in shape file
map.ni.rem=map.ni[map.ni$Constituen!="Newry & Armagh",]
map.ni.newar=map.ni[map.ni$Constituen=="Newry & Armagh",]
map.ni.newar=map.ni.newar[8:14,]
map.ni=rbind(map.ni.rem,map.ni.newar)

#read gif csv files
gif2017ni=read.csv("gif/gif2017ni.csv", header=TRUE)
gif2017gb=read.csv("gif/gif2017gb.csv", header=TRUE)
gif2015ni=read.csv("gif/gif2015ni.csv", header=TRUE)
gif2015gb=read.csv("gif/gif2015gb.csv", header=TRUE)
gif2010ni=read.csv("gif/gif2010ni.csv", header=TRUE)
gif2010gb=read.csv("gif/gif2010gb.csv", header=TRUE)

#add to map.ni 2017
for (j in 10:ncol(gif2017ni)){
  for (i in 1:nrow(map.ni)){
    if(length(gif2017ni[gif2017ni$Constituency %in% map.ni$Constituen[i], j])==0){
      strhyp=str_split(as.character(map.ni$Constituen[i]), "-", simplify = TRUE)
      strspa=str_split(as.character(map.ni$Constituen[i]), " ", simplify = TRUE)
      if(length(gif2017ni[sapply(toupper(strhyp), grepl, toupper(as.character(gif2017ni$Constituency))),j])==0){
        match=sapply(strspa, grepl, as.character(gif2017ni$Constituency), ignore.case=TRUE)
        map.ni[i, paste("CV2017", j)]=gif2017ni[which.max(rowSums(match)), j]
      }else{
        match=sapply(strhyp, grepl, as.character(gif2017ni$Constituency), ignore.case=TRUE)
        map.ni[i, paste("CV2017", j)]=gif2017ni[which.max(rowSums(match)), j]
      }
    }else{  
      map.ni[i, paste("CV2017", j)]=gif2017ni[gif2017ni$Constituency %in% map.ni$Constituen[i],j]
    }
  }
}
#add to map.ni 2015
for (j in 13:ncol(gif2015ni)){
  for (i in 1:nrow(map.ni)){
    if(length(gif2015ni[gif2015ni$Constituency %in% map.ni$Constituen[i], j])==0){
      strhyp=str_split(as.character(map.ni$Constituen[i]), "-", simplify = TRUE)
      strspa=str_split(as.character(map.ni$Constituen[i]), " ", simplify = TRUE)
      if(length(gif2015ni[sapply(toupper(strhyp), grepl, toupper(as.character(gif2015ni$Constituency))),j])==0){
        match=sapply(strspa, grepl, as.character(gif2015ni$Constituency), ignore.case=TRUE)
        map.ni[i, paste("CV2015", j)]=gif2015ni[which.max(rowSums(match)), j]
      }else{
        match=sapply(strhyp, grepl, as.character(gif2015ni$Constituency), ignore.case=TRUE)
        map.ni[i, paste("CV2015", j)]=gif2015ni[which.max(rowSums(match)), j]
      }
    }else{  
      map.ni[i, paste("CV2015", j)]=gif2015ni[gif2015ni$Constituency %in% map.ni$Constituen[i],j]
    }
  }
}

#add to map.ni 2010
for (j in 12:ncol(gif2010ni)){
  for (i in 1:nrow(map.ni)){
    if(length(gif2010ni[gif2010ni$Constituency %in% map.ni$Constituen[i], j])==0){
      strhyp=str_split(as.character(map.ni$Constituen[i]), "-", simplify = TRUE)
      strspa=str_split(as.character(map.ni$Constituen[i]), " ", simplify = TRUE)
      if(length(gif2010ni[sapply(toupper(strhyp), grepl, toupper(as.character(gif2010ni$Constituency))),j])==0){
        match=sapply(strspa, grepl, as.character(gif2010ni$Constituency), ignore.case=TRUE)
        map.ni[i, paste("CV2010", j)]=gif2010ni[which.max(rowSums(match)), j]
      }else{
        match=sapply(strhyp, grepl, as.character(gif2010ni$Constituency), ignore.case=TRUE)
        map.ni[i, paste("CV2010", j)]=gif2010ni[which.max(rowSums(match)), j]
      }
    }else{  
      map.ni[i, paste("CV2010", j)]=gif2010ni[gif2010ni$Constituency %in% map.ni$Constituen[i],j]
    }
  }
}

#add to map.gb 2017
for (j in 12:ncol(gif2017gb)){
  for (i in 1:nrow(map.gb)){
    if(length(gif2017gb[gif2017gb$Constituency %in% map.gb$Constituen[i], j])==0){
      strhyp=str_split(as.character(map.gb$Constituen[i]), "-", simplify = TRUE)
      strspa=str_split(as.character(map.gb$Constituen[i]), " ", simplify = TRUE)
      if(length(gif2017gb[sapply(toupper(strhyp), grepl, toupper(as.character(gif2017gb$Constituency))),j])==0){
        match=sapply(strspa, grepl, as.character(gif2017gb$Constituency), ignore.case=TRUE)
        map.gb[i, paste("CV2017", j)]=gif2017gb[which.max(rowSums(match)), j]
      }else{
        match=sapply(strhyp, grepl, as.character(gif2017gb$Constituency), ignore.case=TRUE)
        map.gb[i, paste("CV2017", j)]=gif2017gb[which.max(rowSums(match)), j]
      }
    }else{  
      map.gb[i, paste("CV2017", j)]=gif2017gb[gif2017gb$Constituency %in% map.gb$Constituen[i],j]
    }
  }
}

#add to map.gb 2015
for (j in 15:ncol(gif2015gb)){
  for (i in 1:nrow(map.gb)){
    if(length(gif2015gb[gif2015gb$Constituency %in% map.gb$Constituen[i], j])==0){
      strhyp=str_split(as.character(map.gb$Constituen[i]), "-", simplify = TRUE)
      strspa=str_split(as.character(map.gb$Constituen[i]), " ", simplify = TRUE)
      if(length(gif2015gb[sapply(toupper(strhyp), grepl, toupper(as.character(gif2015gb$Constituency))),j])==0){
        match=sapply(strspa, grepl, as.character(gif2015gb$Constituency), ignore.case=TRUE)
        map.gb[i, paste("CV2015", j)]=gif2015gb[which.max(rowSums(match)), j]
      }else{
        match=sapply(strhyp, grepl, as.character(gif2015gb$Constituency), ignore.case=TRUE)
        map.gb[i, paste("CV2015", j)]=gif2015gb[which.max(rowSums(match)), j]
      }
    }else{  
      map.gb[i, paste("CV2015", j)]=gif2015gb[gif2015gb$Constituency %in% map.gb$Constituen[i],j]
    }
  }
}
#add to map.gb 2010
for (j in 21:ncol(gif2010gb)){
  for (i in 1:nrow(map.gb)){
    if(length(gif2010gb[gif2010gb$Constituency %in% map.gb$Constituen[i], j])==0){
      strhyp=str_split(as.character(map.gb$Constituen[i]), "-", simplify = TRUE)
      strspa=str_split(as.character(map.gb$Constituen[i]), " ", simplify = TRUE)
      if(length(gif2010gb[sapply(toupper(strhyp), grepl, toupper(as.character(gif2010gb$Constituency))),j])==0){
        match=sapply(strspa, grepl, as.character(gif2010gb$Constituency), ignore.case=TRUE)
        map.gb[i, paste("CV2010", j)]=gif2010gb[which.max(rowSums(match)), j]
      }else{
        match=sapply(strhyp, grepl, as.character(gif2010gb$Constituency), ignore.case=TRUE)
        map.gb[i, paste("CV2010", j)]=gif2010gb[which.max(rowSums(match)), j]
      }
    }else{  
      map.gb[i, paste("CV2010", j)]=gif2010gb[gif2010gb$Constituency %in% map.gb$Constituen[i],j]
    }
  }
}


#combine map.ni and map.gb 2017
for (i in (ncol(gif2017ni)+1):ncol(gif2017gb)){
  map.ni[,paste("CV2017", i)]=map.ni[,"CV2017 17"]
}
for (i in 10:11){
  map.gb[,paste("CV2017", i)]=map.gb[,"CV2017 12"]
}
#2015
for (i in (ncol(gif2015ni)+1):ncol(gif2015gb)){
  map.ni[,paste("CV2015", i)]=map.ni[,"CV2015 16"]
}
for (i in 13:14){
  map.gb[,paste("CV2015", i)]=map.gb[,"CV2015 15"]
}
#2010
for (i in (ncol(gif2010ni)+1):ncol(gif2010gb)){
  map.ni[,paste("CV2010", i)]=map.ni[,"CV2010 15"]
}
for (i in 12:20){
  map.gb[,paste("CV2010", i)]=map.gb[,"CV2010 21"]
}

#combine gb and ni
map.uk=rbind(map.gb, map.ni)


#create 2017 legend
cols <- c(AP="hotpink3",APNI="hotpink3", Alliance="hotpink3",   Alliance..Lib.="orange3", Alliance..SDP.="blue4",BNP="steelblue4",  British.National="steelblue4",Con="dodgerblue", Conservative="dodgerblue", Ch.P="darkorchid2", Democratic.Unionist.Party="firebrick3",      DUP="firebrick3",  Democratic.Unionist="firebrick3",Ecology="green3", ED="tomato4", 
          Green="green3",Grn="green3",KHHC="violetred2",Kidderminster.Hospital.and.Health.Concern="violetred2",Ind="plum1", Independent="plum1", Ind1="plum1",  Ind.="plum1",  Ind131="plum1", Ind86="plum1", Independent76="plum1", Ind127="plum1", Independent.Labour="red1",Ind.Labour="red1",Lab="red2",  Labour="red2",            LDem="orange2", Liberal.Democrat="orange2" , LD="orange2", Lib="orange3", Liberal="orange3", National.Front="lightskyblue", Natural.Law="lightsteelblue2", 
          Official.Unionist.Party="coral",Oth="grey40",PC="palegreen", Plaid.Cymru="palegreen", People.s.Labour="gold1",Referendum="indianred4",Republican.Clubs="limegreen",Respect="forestgreen", Resp="forestgreen",SSP="orangered3",Scottish.Socialist="orangered3", SDLP="green4",Social.Democratic.and.Labour.Party="green4",Social.Democratic.and.Labour="green4",Sinn.Fein="limegreen",SF="limegreen",Sinn.Fein="limegreen", Scottish.National.Party="yellow",SNP="yellow", Scottish.National="yellow", Speaker="wheat3",The.Speaker="wheat3",Socialist.Alliance="red4",Social.Democratic="blue4",Socialist.Labour="grey35", Trade.Unionist.and.Socialist.Coalition="deeppink4",TUV="royalblue", 
          UCUNF="cadetblue2", UK.Independence.Party="darkviolet", UKIP="darkviolet", United.Kingdom.Independence="darkviolet",United.Kingdom.Unionist="mediumpurple4",   United.Ulster.Unionist="deepskyblue1", Ulster.Unionist.Party="coral", UU="coral",UUP="coral", Official.Unionist="coral", Ulster.Unionist="coral",Ulster.Popular.Unionist="blue1", Ver="darkorchid1", The.Workers...NI.="red4",WRP="tomato2" )

brks <- c("AP","APNI", "Alliance",   "Alliance..Lib.", "Alliance..SDP.","BNP",  "British.National","Con", "Conservative", "Ch.P", "Democratic.Unionist.Party", "DUP",  "Democratic.Unionist","Ecology", "ED", 
          "Green","Grn","KHHC","Kidderminster.Hospital.and.Health.Concern","Ind", "Independent", "Ind1",  "Ind.",  "Ind131", "Ind86", "Independent76", "Ind127", "Independent.Labour","Ind.Labour","Lab",  "Labour",            "LDem", "Liberal.Democrat" , "LD", "Lib", "Liberal", "National.Front", "Natural.Law", 
          "Official.Unionist.Party","Oth","PC", "Plaid.Cymru", "People.s.Labour", "Referendum","Republican.Clubs","Respect","Resp","SSP","Scottish.Socialist", "SDLP","Social.Democratic.and.Labour.Party","Social.Democratic.and.Labour","Sinn.Fein","SF","Sinn.Fein", "Scottish.National.Party","SNP", "Scottish.National", "Speaker","The.Speaker","Socialist.Alliance","Social.Democratic","Socialist.Labour", "Trade.Unionist.and.Socialist.Coalition","TUV", 
          "UCUNF", "UK.Independence.Party", "UKIP", "United.Kingdom.Independence","United.Kingdom.Unionist",   "United.Ulster.Unionist", "Ulster.Unionist.Party", "UU","UUP", "Official.Unionist", "Ulster.Unionist","Ulster.Popular.Unionist", "Ver", "The.Workers...NI.","WRP")

labs <- c("Alliance NI","Alliance NI","Alliance NI",   "Alliance (Liberal)", "Alliance (SDP)","British National",  "British National","Conservative", "Conservative", "Christian Party", "DUP", "DUP",  "DUP","Ecology", "English Democrat", 
          "Green","Green","Health Concern","Health Concern","Independent", "Independent", "Independent",  "Independent",  "Independent", "Independent", "Independent", "Independent", "Independent Labour","Independent Labour","Labour",  "Labour", "Liberal Democrat", "Liberal Democrat" , "Liberal Democrat", "Liberal", "Liberal", "National Front", "Natural Law", 
          "Official Unionist","Other","Plaid Cymru", "Plaid Cymru", "People's Labour", "Referendum","Republican Clubs","Respect","Respect","Scottish Socialist","Scottish Socialist", "SDLP","SDLP","SDLP","Sinn Fein","Sinn Fein","Sinn Fein", "SNP","SNP", "SNP", "Speaker","Speaker","Socialist Alliance","Social Democratic","Socialist Labour", "TUSC","TUV", 
          "UCUNF", "UKIP", "UKIP", "UKIP","UK Unionist",   "United Ulster Unionist", "UUP", "UUP","UUP", "Official Unionist", "Ulster Unionist","Ulster Popular Unionist", "Veritas", "The Workers NI","Workers Revolutionary")
data=read.csv("source/data/propmapFPTPCV2010-2017.csv")
colCV=paste("CV", 2017, sep="")
pCV=ggplot(data) + 
  aes(long,lat,group=group,fill=data[[colCV]]) + 
  geom_polygon() +
  geom_path(color="black") +
  coord_equal()+
  scale_fill_manual(values = cols,name="Parties",
                    breaks=brks, labels=labs)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

legend_b <- get_legend(pCV + theme(legend.position="right", plot.margin = unit(c(0,0,0,0),"cm"))) 

#make gif UK 2017
#number of iterations in each year
it2017=ncol(gif2017gb)-10
it2017df <- data.frame(matrix(1, ncol = it2017, nrow = 1))
i=1
png(file="gif/input-%03d.png", width=800, height=800)
for (j in 10:ncol(gif2017gb)){
  colIt=paste("X", i, sep="")
  it2017df[[colIt]]=i
  itNum=ggplot(data=it2017df, aes(x=it2017df[[colIt]], y=X1)) +
    geom_bar(colour="black", stat="identity") +
    xlim(0, it2017)+
    xlab("Iteration Number") +
    theme(axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none")
  pCV=(ggplot(map.uk) + 
          aes(long,lat,group=group,fill=map.uk[[paste("CV2017", j)]]) + 
          geom_polygon() +
          geom_path(color="black") +
          coord_equal()+
          scale_fill_manual(values = cols,name="Parties",
                            breaks=brks, labels=labs)+
          theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                plot.margin = unit(c(0,0,0,0),"cm"),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())+
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)))
  grid.arrange(pCV,legend_b, itNum,ncol=2, nrow=2,heights=c(6,1))
  i=i+1
}
dev.off()
# convert pngs to one gif using ImageMagick
shell('"C:/Program Files/ImageMagick-7.0.6-Q16/magick.exe" -delay 20 gif/input-*.png gif/UK_CV_2017.gif')

#create 2015 legend
colCV=paste("CV", 2015, sep="")
pCV=ggplot(data) + 
  aes(long,lat,group=group,fill=data[[colCV]]) + 
  geom_polygon() +
  geom_path(color="black") +
  coord_equal()+
  scale_fill_manual(values = cols,name="Parties",
                    breaks=brks, labels=labs)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

legend_b <- get_legend(pCV + theme(legend.position="right", plot.margin = unit(c(0,0,0,0),"cm")))  

#make gif UK 2015
it2015=ncol(gif2015gb)-13
it2015df <- data.frame(matrix(1, ncol = it2015, nrow = 1))
i=1
png(file="gif/input-%03d.png", width=800, height=800)
for (j in 13:ncol(gif2015gb)){
  colIt=paste("X", i, sep="")
  it2015df[[colIt]]=i
  itNum=ggplot(data=it2015df, aes(x=it2015df[[colIt]], y=X1)) +
    geom_bar(colour="black", stat="identity") +
    xlim(0, it2015)+
    xlab("Iteration Number") +
    theme(axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none")
  pCV=(ggplot(map.uk) + 
          aes(long,lat,group=group,fill=map.uk[[paste("CV2015", j)]]) + 
          geom_polygon() +
          geom_path(color="black") +
          coord_equal()+
          scale_fill_manual(values = cols,name="Parties",
                            breaks=brks, labels=labs)+
          theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                plot.margin = unit(c(0,0,0,0),"cm"),
                axis.title.y=element_blank(),
                legend.position="none",
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())+
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)))
  grid.arrange(pCV,legend_b, itNum,ncol=2, nrow=2,heights=c(6,1))
  i=i+1
}
dev.off()
# convert pngs to one gif using ImageMagick
shell('"C:/Program Files/ImageMagick-7.0.6-Q16/magick.exe" -delay 20 gif/input-*.png gif/UK_CV_2015.gif')


#create 2010 legend
colCV=paste("CV", 2010, sep="")
pCV=ggplot(data) + 
  aes(long,lat,group=group,fill=data[[colCV]]) + 
  geom_polygon() +
  geom_path(color="black") +
  coord_equal()+
  scale_fill_manual(values = cols,name="Parties",
                    breaks=brks, labels=labs)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

legend_b <- get_legend(pCV + theme(legend.position="right", plot.margin = unit(c(0,0,0,0),"cm"))) 

#make gif UK 2010
it2010=ncol(gif2010gb)-12
it2010df <- data.frame(matrix(1, ncol = it2010, nrow = 1))
i=1
png(file="gif/input-%03d.png", width=800, height=800)
for (j in 12:ncol(gif2010gb)){
  colIt=paste("X", i, sep="")
  it2010df[[colIt]]=i
  itNum=ggplot(data=it2010df, aes(x=it2010df[[colIt]], y=X1)) +
    geom_bar(colour="black", stat="identity") +
    xlim(0, it2010)+
    xlab("Iteration Number") +
    theme(axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none")
  pCV=(ggplot(map.uk) + 
          aes(long,lat,group=group,fill=map.uk[[paste("CV2010", j)]]) + 
          geom_polygon() +
          geom_path(color="black") +
          coord_equal()+
          scale_fill_manual(values = cols,name="Parties",
                            breaks=brks, labels=labs)+
          theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                plot.margin = unit(c(0,0,0,0),"cm"),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())+
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)))
  grid.arrange(pCV,legend_b, itNum,ncol=2, nrow=2,heights=c(6,1))
  i=i+1
}
dev.off()
# convert pngs to one gif using ImageMagick
shell('"C:/Program Files/ImageMagick-7.0.6-Q16/magick.exe" -delay 20 gif/input-*.png gif/UK_CV_2010.gif')