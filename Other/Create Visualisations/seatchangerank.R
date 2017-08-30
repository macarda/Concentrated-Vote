switches <- data.frame( Constit=character(),
                        FPTP=character(),
                        FPTPVotes=integer(),
                       CV=character(),
                       CVVotes=integer(),
                       Year=integer(),
                       Rank=integer(),
                       stringsAsFactors=FALSE)

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

#read votes
source("source/readvotes.R", local=TRUE)


#2017
ans2017$FPTP=as.character(ans2017$FPTP)
ans2017$CV=as.character(ans2017$CV)
changes <- ans2017[ans2017$CV!=ans2017$FPTP,]
#Where CV and FPTP winners are different, find the rank of the new winner
for(i in 1:nrow(changes)){
  colIndex=which(colnames(votes2017) == changes$CV[i])
  rank=apply(-votes2017[as.character(changes$Constituency[i]),1:(ncol(votes2017)-1)], 1, rank)
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=changes[i,"FPTP"]
  switches[x, "FPTPVotes"]=votes2017[row.names(votes2017)==changes$Constituency[i],changes$FPTP[i]]
  switches[x, "CVVotes"]=votes2017[row.names(votes2017)==changes$Constituency[i],changes$CV[i]]
  switches[x, "CV"]=changes[i,"CV"]
  switches[x, "Year"]=2017
  switches[x, "Constit"]=as.character(changes$Constituency[i])
  switches[x, "Rank"]=rank[colIndex]
}
#Enter rank 1 for all seat winners that do not change from FPTP
nochanges <- ans2017[ans2017$CV==ans2017$FPTP,]
for(i in 1:nrow(nochanges)){
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=nochanges[i,"FPTP"]
  switches[x, "CV"]=nochanges[i,"CV"]
  switches[x, "Year"]=2017
  switches[x, "Constit"]=as.character(nochanges$Constituency[i])
  switches[x, "Rank"]=1
}

#2015
ans2015$FPTP=as.character(ans2015$FPTP)
ans2015$CV=as.character(ans2015$CV)
changes <- ans2015[ans2015$CV!=ans2015$FPTP,]
for(i in 1:nrow(changes)){
  colIndex=which(colnames(votes2015) == changes$CV[i])
  rank=apply(-votes2015[as.character(changes$Constituency[i]),1:(ncol(votes2015)-1)], 1, rank)
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=changes[i,"FPTP"]
  switches[x, "CV"]=changes[i,"CV"]
  switches[x, "Year"]=2015
  switches[x, "FPTPVotes"]=votes2015[row.names(votes2015)==changes$Constituency[i],changes$FPTP[i]]
  switches[x, "CVVotes"]=votes2015[row.names(votes2015)==changes$Constituency[i],changes$CV[i]]
  switches[x, "Constit"]=as.character(changes$Constituency[i])
  switches[x, "Rank"]=rank[colIndex]
}
nochanges <- ans2015[ans2015$CV==ans2015$FPTP,]
for(i in 1:nrow(nochanges)){
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=nochanges[i,"FPTP"]
  switches[x, "CV"]=nochanges[i,"CV"]
  switches[x, "Year"]=2015
  switches[x, "Constit"]=as.character(nochanges$Constituency[i])
  switches[x, "Rank"]=1
}

#2010
ans2010$FPTP=as.character(ans2010$FPTP)
ans2010$CV=as.character(ans2010$CV)
changes <- ans2010[ans2010$CV!=ans2010$FPTP,]
for(i in 1:nrow(changes)){
  colIndex=which(colnames(votes2010) == changes$CV[i])
  rank=apply(-votes2010[as.character(changes$Constituency[i]),1:(ncol(votes2010)-1)], 1, rank)
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=changes[i,"FPTP"]
  switches[x, "CV"]=changes[i,"CV"]
  switches[x, "Year"]=2010
  switches[x, "FPTPVotes"]=votes2010[row.names(votes2010)==changes$Constituency[i],changes$FPTP[i]]
  switches[x, "CVVotes"]=votes2010[row.names(votes2010)==changes$Constituency[i],changes$CV[i]]
  switches[x, "Constit"]=as.character(changes$Constituency[i])
  switches[x, "Rank"]=rank[colIndex]
}
nochanges <- ans2010[ans2010$CV==ans2010$FPTP,]
for(i in 1:nrow(nochanges)){
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=nochanges[i,"FPTP"]
  switches[x, "CV"]=nochanges[i,"CV"]
  switches[x, "Year"]=2010
  switches[x, "Constit"]=as.character(nochanges$Constituency[i])
  switches[x, "Rank"]=1
}


#2005
ans2005$FPTP=as.character(ans2005$FPTP)
ans2005$CV=as.character(ans2005$CV)
changes <- ans2005[ans2005$CV!=ans2005$FPTP,]
for(i in 1:nrow(changes)){
  colIndex=which(colnames(votes2005) == changes$CV[i])
  rank=apply(-votes2005[as.character(changes$Constituency[i]),1:(ncol(votes2005)-1)], 1, rank)
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=changes[i,"FPTP"]
  switches[x, "FPTPVotes"]=votes2005[row.names(votes2005)==changes$Constituency[i],changes$FPTP[i]]
  switches[x, "CVVotes"]=votes2005[row.names(votes2005)==changes$Constituency[i],changes$CV[i]]
  switches[x, "CV"]=changes[i,"CV"]
  switches[x, "Year"]=2005
  switches[x, "Constit"]=as.character(changes$Constituency[i])
  switches[x, "Rank"]=rank[colIndex]
}
nochanges <- ans2005[ans2005$CV==ans2005$FPTP,]
for(i in 1:nrow(nochanges)){
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=nochanges[i,"FPTP"]
  switches[x, "CV"]=nochanges[i,"CV"]
  switches[x, "Year"]=2005
  switches[x, "Constit"]=as.character(nochanges$Constituency[i])
  switches[x, "Rank"]=1
}


#2001
ans2001$FPTP=as.character(ans2001$FPTP)
ans2001$CV=as.character(ans2001$CV)
changes <- ans2001[ans2001$CV!=ans2001$FPTP,]
for(i in 1:nrow(changes)){
  colIndex=which(colnames(votes2001) == changes$CV[i])
  rank=apply(-votes2001[as.character(changes$Constituency[i]),1:(ncol(votes2001)-1)], 1, rank)
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=changes[i,"FPTP"]
  switches[x, "CV"]=changes[i,"CV"]
  switches[x, "Year"]=2001
  switches[x, "FPTPVotes"]=votes2001[row.names(votes2001)==changes$Constituency[i],changes$FPTP[i]]
  switches[x, "CVVotes"]=votes2001[row.names(votes2001)==changes$Constituency[i],changes$CV[i]]
  switches[x, "Constit"]=as.character(changes$Constituency[i])
  switches[x, "Rank"]=rank[colIndex]
}
nochanges <- ans2001[ans2001$CV==ans2001$FPTP,]
for(i in 1:nrow(nochanges)){
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=nochanges[i,"FPTP"]
  switches[x, "CV"]=nochanges[i,"CV"]
  switches[x, "Year"]=2001
  switches[x, "Constit"]=as.character(nochanges$Constituency[i])
  switches[x, "Rank"]=1
}

#1997
ans1997$FPTP=as.character(ans1997$FPTP)
ans1997$CV=as.character(ans1997$CV)
changes <- ans1997[ans1997$CV!=ans1997$FPTP,]
for(i in 1:nrow(changes)){
  colIndex=which(colnames(votes1997) == changes$CV[i])
  rank=apply(-votes1997[as.character(changes$Constituency[i]),1:(ncol(votes1997)-1)], 1, rank)
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=changes[i,"FPTP"]
  switches[x, "CV"]=changes[i,"CV"]
  switches[x, "Year"]=1997
  switches[x, "FPTPVotes"]=votes1997[row.names(votes1997)==changes$Constituency[i],changes$FPTP[i]]
  switches[x, "CVVotes"]=votes1997[row.names(votes1997)==changes$Constituency[i],changes$CV[i]]
  switches[x, "Constit"]=as.character(changes$Constituency[i])
  switches[x, "Rank"]=rank[colIndex]
}
nochanges <- ans1997[ans1997$CV==ans1997$FPTP,]
for(i in 1:nrow(nochanges)){
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=nochanges[i,"FPTP"]
  switches[x, "CV"]=nochanges[i,"CV"]
  switches[x, "Year"]=1997
  switches[x, "Constit"]=as.character(nochanges$Constituency[i])
  switches[x, "Rank"]=1
}

#1992
ans1992$FPTP=as.character(ans1992$FPTP)
ans1992$CV=as.character(ans1992$CV)
changes <- ans1992[ans1992$CV!=ans1992$FPTP,]
for(i in 1:nrow(changes)){
  colIndex=which(colnames(votes1992) == changes$CV[i])
  rank=apply(-votes1992[as.character(changes$Constituency[i]),1:(ncol(votes1992)-1)], 1, rank)
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=changes[i,"FPTP"]
  switches[x, "CV"]=changes[i,"CV"]
  switches[x, "Year"]=1992
  switches[x, "FPTPVotes"]=votes1992[row.names(votes1992)==changes$Constituency[i],changes$FPTP[i]]
  switches[x, "CVVotes"]=votes1992[row.names(votes1992)==changes$Constituency[i],changes$CV[i]]
  switches[x, "Constit"]=as.character(changes$Constituency[i])
  switches[x, "Rank"]=rank[colIndex]
}
nochanges <- ans1992[ans1992$CV==ans1992$FPTP,]
for(i in 1:nrow(nochanges)){
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=nochanges[i,"FPTP"]
  switches[x, "CV"]=nochanges[i,"CV"]
  switches[x, "Year"]=1992
  switches[x, "Constit"]=as.character(nochanges$Constituency[i])
  switches[x, "Rank"]=1
}

#1987 - Democratic Unionist Party - Democratic Unionist????
ans1987$FPTP=as.character(ans1987$FPTP)
ans1987$CV=as.character(ans1987$CV)
changes <- ans1987[ans1987$CV!=ans1987$FPTP,]
for(i in 1:nrow(changes)){
  colIndex=which(colnames(votes1987) == changes$CV[i])
  rank=apply(-votes1987[as.character(changes$Constituency[i]),1:(ncol(votes1987)-1)], 1, rank)
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=changes[i,"FPTP"]
  switches[x, "CV"]=changes[i,"CV"]
  switches[x, "Year"]=1987
  switches[x, "FPTPVotes"]=votes1987[row.names(votes1987)==changes$Constituency[i],changes$FPTP[i]]
  switches[x, "CVVotes"]=votes1987[row.names(votes1987)==changes$Constituency[i],changes$CV[i]]
  switches[x, "Constit"]=as.character(changes$Constituency[i])
  switches[x, "Rank"]=rank[colIndex]
}
nochanges <- ans1987[ans1987$CV==ans1987$FPTP,]
for(i in 1:nrow(nochanges)){
  colIndex=which(colnames(votes1987) == nochanges$CV[i])
  rank=apply(-votes1987[as.character(nochanges$Constituency[i]),1:(ncol(votes1987)-1)], 1, rank)
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=nochanges[i,"FPTP"]
  switches[x, "CV"]=nochanges[i,"CV"]
  switches[x, "Year"]=1987
  switches[x, "Constit"]=as.character(nochanges$Constituency[i])
  switches[x, "Rank"]=1
}


#1983
votes1983$Conservative=as.numeric(votes1983$Conservative)
votes1983$Ind=as.numeric(votes1983$Ind)
ans1983$FPTP=as.character(ans1983$FPTP)
ans1983$CV=as.character(ans1983$CV)
changes <- ans1983[ans1983$CV!=ans1983$FPTP,]
for(i in 1:nrow(changes)){
  colIndex=which(colnames(votes1983) == changes$CV[i])
  rank=apply(-votes1983[as.character(changes$Constituency[i]),1:(ncol(votes1983)-1)], 1, rank)
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=changes[i,"FPTP"]
  switches[x, "CV"]=changes[i,"CV"]
  switches[x, "Year"]=1983
  switches[x, "FPTPVotes"]=votes1983[row.names(votes1983)==changes$Constituency[i],changes$FPTP[i]]
  switches[x, "CVVotes"]=votes1983[row.names(votes1983)==changes$Constituency[i],changes$CV[i]]
  switches[x, "Constit"]=as.character(changes$Constituency[i])
  switches[x, "Rank"]=rank[colIndex]
}
nochanges <- ans1983[ans1983$CV==ans1983$FPTP,]
for(i in 1:nrow(nochanges)){
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=nochanges[i,"FPTP"]
  switches[x, "CV"]=nochanges[i,"CV"]
  switches[x, "Year"]=1983
  switches[x, "Constit"]=as.character(nochanges$Constituency[i])
  switches[x, "Rank"]=1
}


#1979
ans1979$FPTP=as.character(ans1979$FPTP)
ans1979$CV=as.character(ans1979$CV)
changes <- ans1979[ans1979$CV!=ans1979$FPTP,]
for(i in 1:nrow(changes)){
  colIndex=which(colnames(votes1979) == changes$CV[i])
  rank=apply(-votes1979[as.character(changes$Constituency[i]),1:(ncol(votes1979)-1)], 1, rank)
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=changes[i,"FPTP"]
  switches[x, "CV"]=changes[i,"CV"]
  switches[x, "Year"]=1979
  if(changes$FPTP[i]=="Ind8"){switches[x, "FPTPVotes"]=votes1979[row.names(votes1979)==changes$Constituency[i],"Ind"]}
  else{switches[x, "FPTPVotes"]=votes1979[row.names(votes1979)==changes$Constituency[i],changes$FPTP[i]]}
  switches[x, "CVVotes"]=votes1979[row.names(votes1979)==changes$Constituency[i],changes$CV[i]]
  switches[x, "Constit"]=as.character(changes$Constituency[i])
  switches[x, "Rank"]=rank[colIndex]
}
nochanges <- ans1979[ans1979$CV==ans1979$FPTP,]
for(i in 1:nrow(nochanges)){
  x=(nrow(switches)+1)
  switches[x, "FPTP"]=nochanges[i,"FPTP"]
  switches[x, "CV"]=nochanges[i,"CV"]
  switches[x, "Year"]=1979
  switches[x, "Constit"]=as.character(nochanges$Constituency[i])
  switches[x, "Rank"]=1
}

write.csv(switches, "seatRank.csv")