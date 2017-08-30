#read votes
source("source/readvotes.R", local=TRUE)

#read output
ans2017=read.csv("Output/ans2017.csv",stringsAsFactors=FALSE)
ans2015=read.csv("Output/ans2015.csv",stringsAsFactors=FALSE)
ans2010=read.csv("Output/ans2010.csv",stringsAsFactors=FALSE)
ans2005=read.csv("Output/ans2005.csv",stringsAsFactors=FALSE)
ans2001=read.csv("Output/ans2001.csv",stringsAsFactors=FALSE)
ans1997=read.csv("Output/ans1997.csv",stringsAsFactors=FALSE)
ans1992=read.csv("Output/ans1992.csv",stringsAsFactors=FALSE)
ans1987=read.csv("Output/ans1987.csv",stringsAsFactors=FALSE)
ans1983=read.csv("Output/ans1983.csv",stringsAsFactors=FALSE)
ans1979=read.csv("Output/ans1979.csv",stringsAsFactors=FALSE)


#create dataframe
wastedVotes <- data.frame(Constituency=character(),
                        Year=double(),
                        System=character(),
                        Wasted=integer(),
                        AllVotes=integer(),
                        stringsAsFactors=FALSE)

#calculates the FPTP wasted votes
#2017
for (i in 1:nrow(votes2017)){
  wasted=sum(votes2017[i,1:ncol(votes2017)-1])-max(votes2017[i,1:ncol(votes2017)-1])+(max(votes2017[i,1:ncol(votes2017)-1])-(sort(votes2017[i,1:ncol(votes2017)-1], decreasing=T)[2]+1))
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=rownames(votes2017[i,])
  wastedVotes[y, "Year"]=2017
  wastedVotes[y, "System"]="FPTP"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes2017[i,1:ncol(votes2017)-1])
}
#2015
for (i in 1:nrow(votes2015)){
  wasted=sum(votes2015[i,1:ncol(votes2015)-1])-max(votes2015[i,1:ncol(votes2015)-1])+
    (max(votes2015[i,1:ncol(votes2015)-1])-(sort(votes2015[i,1:ncol(votes2015)-1], decreasing=T)[2]+1))
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=rownames(votes2015[i,])
  wastedVotes[y, "Year"]=2015
  wastedVotes[y, "System"]="FPTP"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes2015[i,1:ncol(votes2015)-1])
}
#2010
for (i in 1:nrow(votes2010)){
  wasted=sum(votes2010[i,1:ncol(votes2010)-1])-max(votes2010[i,1:ncol(votes2010)-1])+
    (max(votes2010[i,1:ncol(votes2010)-1])-(sort(votes2010[i,1:ncol(votes2010)-1], decreasing=T)[2]+1))
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=rownames(votes2010[i,])
  wastedVotes[y, "Year"]=2010
  wastedVotes[y, "System"]="FPTP"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes2010[i,1:ncol(votes2010)-1])
}
#2005
for (i in 1:nrow(votes2005)){
  wasted=sum(votes2005[i,1:ncol(votes2005)-1])-max(votes2005[i,1:ncol(votes2005)-1])+
    (max(votes2005[i,1:ncol(votes2005)-1])-(sort(votes2005[i,1:ncol(votes2005)-1], decreasing=T)[2]+1))
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=rownames(votes2005[i,])
  wastedVotes[y, "Year"]=2005
  wastedVotes[y, "System"]="FPTP"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes2005[i,1:ncol(votes2005)-1])
}
#2001
for (i in 1:nrow(votes2001)){
  wasted=sum(votes2001[i,1:ncol(votes2001)-1])-max(votes2001[i,1:ncol(votes2001)-1])+
    (max(votes2001[i,1:ncol(votes2001)-1])-(sort(votes2001[i,1:ncol(votes2001)-1], decreasing=T)[2]+1))
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=rownames(votes2001[i,])
  wastedVotes[y, "Year"]=2001
  wastedVotes[y, "System"]="FPTP"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes2001[i,1:ncol(votes2001)-1])
}
#1997
for (i in 1:nrow(votes1997)){
  wasted=sum(votes1997[i,1:ncol(votes1997)-1])-max(votes1997[i,1:ncol(votes1997)-1])+
    (max(votes1997[i,1:ncol(votes1997)-1])-(sort(votes1997[i,1:ncol(votes1997)-1], decreasing=T)[2]+1))
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=rownames(votes1997[i,])
  wastedVotes[y, "Year"]=1997
  wastedVotes[y, "System"]="FPTP"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes1997[i,1:ncol(votes1997)-1])
}
#1992
for (i in 1:nrow(votes1992)){
  wasted=sum(votes1992[i,1:ncol(votes1992)-1])-max(votes1992[i,1:ncol(votes1992)-1])+
    (max(votes1992[i,1:ncol(votes1992)-1])-(sort(votes1992[i,1:ncol(votes1992)-1], decreasing=T)[2]+1))
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=rownames(votes1992[i,])
  wastedVotes[y, "Year"]=1992
  wastedVotes[y, "System"]="FPTP"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes1992[i,1:ncol(votes1992)-1])
}
#1987
for (i in 1:nrow(votes1987)){
  wasted=sum(votes1987[i,1:ncol(votes1987)-1])-max(votes1987[i,1:ncol(votes1987)-1])+
    (max(votes1987[i,1:ncol(votes1987)-1])-(sort(votes1987[i,1:ncol(votes1987)-1], decreasing=T)[2]+1))
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=rownames(votes1987[i,])
  wastedVotes[y, "Year"]=1987
  wastedVotes[y, "System"]="FPTP"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes1987[i,1:ncol(votes1987)-1])
}
#1983
for (i in 1:nrow(votes1983)){
  wasted=sum(votes1983[i,1:ncol(votes1983)-1])-max(votes1983[i,1:ncol(votes1983)-1])+
    (max(votes1983[i,1:ncol(votes1983)-1])-(sort(votes1983[i,1:ncol(votes1983)-1], decreasing=T)[2]+1))
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=rownames(votes1983[i,])
  wastedVotes[y, "Year"]=1983
  wastedVotes[y, "System"]="FPTP"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes1983[i,1:ncol(votes1983)-1])
}
#1979
for (i in 1:nrow(votes1979)){
  wasted=sum(votes1979[i,1:ncol(votes1979)-1])-max(votes1979[i,1:ncol(votes1979)-1])+
    (max(votes1979[i,1:ncol(votes1979)-1])-(sort(votes1979[i,1:ncol(votes1979)-1], decreasing=T)[2]+1))
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=rownames(votes1979[i,])
  wastedVotes[y, "Year"]=1979
  wastedVotes[y, "System"]="FPTP"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes1979[i,1:ncol(votes1979)-1])
}

#Calculated the Concentrated Vote wasted votes
#2017
for (i in 1:nrow(ans2017)){
  nums <- sapply(ans2017, is.numeric)
  wasted=sum(ans2017[i,nums])-max(ans2017[i,nums])
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=ans2017$Constituency[i]
  wastedVotes[y, "Year"]=2017
  wastedVotes[y, "System"]="CV"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes2017[i,1:ncol(votes2017)-1])
}
#2015
for (i in 1:nrow(ans2015)){
  nums <- sapply(ans2015, is.numeric)
  wasted=sum(ans2015[i,nums])-max(ans2015[i,nums])
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=ans2015$Constituency[i]
  wastedVotes[y, "Year"]=2015
  wastedVotes[y, "System"]="CV"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes2015[i,1:ncol(votes2015)-1])
}
#2010
for (i in 1:nrow(ans2010)){
  nums <- sapply(ans2010, is.numeric)
  wasted=sum(ans2010[i,nums])-max(ans2010[i,nums])
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=ans2010$Constituency[i]
  wastedVotes[y, "Year"]=2010
  wastedVotes[y, "System"]="CV"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes2010[i,1:ncol(votes2010)-1])
}
#2005
for (i in 1:nrow(ans2005)){
  nums <- sapply(ans2005, is.numeric)
  wasted=sum(ans2005[i,nums])-max(ans2005[i,nums])
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=ans2005$Constituency[i]
  wastedVotes[y, "Year"]=2005
  wastedVotes[y, "System"]="CV"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes2005[i,1:ncol(votes2005)-1])
}
#2001
for (i in 1:nrow(ans2001)){
  nums <- sapply(ans2001, is.numeric)
  wasted=sum(ans2001[i,nums])-max(ans2001[i,nums])
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=ans2001$Constituency[i]
  wastedVotes[y, "Year"]=2001
  wastedVotes[y, "System"]="CV"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes2001[i,1:ncol(votes2001)-1])
}
#1997
for (i in 1:nrow(ans1997)){
  nums <- sapply(ans1997, is.numeric)
  wasted=sum(ans1997[i,nums])-max(ans1997[i,nums])
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=ans1997$Constituency[i]
  wastedVotes[y, "Year"]=1997
  wastedVotes[y, "System"]="CV"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes1997[i,1:ncol(votes1997)-1])
}
#1992
for (i in 1:nrow(ans1992)){
  nums <- sapply(ans1992, is.numeric)
  wasted=sum(ans1992[i,nums])-max(ans1992[i,nums])
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=ans1992$Constituency[i]
  wastedVotes[y, "Year"]=1992
  wastedVotes[y, "System"]="CV"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes1992[i,1:ncol(votes1992)-1])
}
#1987
for (i in 1:nrow(ans1987)){
  nums <- sapply(ans1987, is.numeric)
  wasted=sum(ans1987[i,nums])-max(ans1987[i,nums])
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=ans1987$Constituency[i]
  wastedVotes[y, "Year"]=1987
  wastedVotes[y, "System"]="CV"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes1987[i,1:ncol(votes1987)-1])
}
#1983
for (i in 1:nrow(ans1983)){
  nums <- sapply(ans1983, is.numeric)
  wasted=sum(ans1983[i,nums])-max(ans1983[i,nums])
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=ans1983$Constituency[i]
  wastedVotes[y, "Year"]=1983
  wastedVotes[y, "System"]="CV"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes1983[i,1:ncol(votes1983)-1])
}
#1979
for (i in 1:nrow(ans1979)){
  nums <- sapply(ans1979, is.numeric)
  wasted=sum(ans1979[i,nums])-max(ans1979[i,nums])
  y=(nrow(wastedVotes)+1)
  wastedVotes[y, "Constituency"]=ans1979$Constituency[i]
  wastedVotes[y, "Year"]=1979
  wastedVotes[y, "System"]="CV"
  wastedVotes[y, "Wasted"]=wasted
  wastedVotes[y, "AllVotes"]=sum(votes1979[i,1:ncol(votes1979)-1])
}

write.csv(wastedVotes, "wastedVotes.csv")


for (i in 1:nrow(wastedVotes)){
  wastedVotes[i, "PercentWasted"]=wastedVotes$Wasted[i]/sum(wastedVotes$AllVotes[which(wastedVotes$Year==wastedVotes$Year[i] & wastedVotes$System==wastedVotes$System[i])])
  
}

#Summarises wasted vote information for interface table
wastedVotesTable <- data.frame(Year=integer(),
                          FPTPwasted=integer(),
                          CVwasted=integer(),
                          FPTPpercent=character(),
                          CVpercent=character(),
                          stringsAsFactors=FALSE)

for(i in unique(wastedVotes$Year)){
  y=(nrow(wastedVotesTable)+1)
  wastedVotesTable[y, "Year"]=i
  wastedVotesTable[y, "FPTPwasted"]=prettyNum(sum(wastedVotes$Wasted[which(wastedVotes$Year==i 
                                              & wastedVotes$System=="FPTP")]),big.mark=",",scientific=FALSE)
  wastedVotesTable[y, "FPTPpercent"]=paste(round((sum(wastedVotes$PercentWasted[which(wastedVotes$Year==i 
                                  & wastedVotes$System=="FPTP")])*100),2),"%", sep="")
  wastedVotesTable[y, "CVwasted"]=prettyNum(sum(wastedVotes$Wasted[which(wastedVotes$Year==i &
                          wastedVotes$System=="CV")]),big.mark=",",scientific=FALSE)
  wastedVotesTable[y, "CVpercent"]=paste(round((sum(wastedVotes$PercentWasted[which(wastedVotes$Year==i 
                                  & wastedVotes$System=="CV")])*100),2),"%", sep="")
}
write.csv(wastedVotesTable, "source/data/wastedVotesTable.csv", row.names = FALSE)