#create dataframe
constit <- data.frame(Constituency=character(),
                      Party=character(),
                      System=character(),
                      Votes=integer(),
                      Year=integer(),
                      stringsAsFactors=FALSE)

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


#Records the number of votes in the constituencies where the Conservatives and Labour won seats under FPTP and CV
#2017
votes2017[,"FPTP"] <- colnames(votes2017)[unlist(apply(votes2017, 1, which.max))]
for(i in 1:nrow(votes2017)){
  if(votes2017$FPTP[i]=="Con"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2017[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=2017
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes2017[i,1:(ncol(votes2017)-1)])
  }
  if(ans2017$CV[i]=="Con"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2017[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=2017
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes2017[i,1:(ncol(votes2017)-1)])
  }
}

for(i in 1:nrow(votes2017)){
  if(votes2017$FPTP[i]=="Lab"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2017[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=2017
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes2017[i,1:(ncol(votes2017)-1)])
  }
  if(ans2017$CV[i]=="Lab"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2017[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=2017
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes2017[i,1:(ncol(votes2017)-1)])
  }
}

#2015
votes2015[,"FPTP"] <- colnames(votes2015)[unlist(apply(votes2015, 1, which.max))]
for(i in 1:nrow(votes2015)){
  if(votes2015$FPTP[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2015[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=2015
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes2015[i,1:(ncol(votes2015)-1)])
  }
  if(ans2015$CV[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2015[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=2015
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes2015[i,1:(ncol(votes2015)-1)])
  }
}

for(i in 1:nrow(votes2015)){
  if(votes2015$FPTP[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2015[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=2015
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes2015[i,1:(ncol(votes2015)-1)])
  }
  if(ans2015$CV[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2015[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=2015
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes2015[i,1:(ncol(votes2015)-1)])
  }
}

#2010
votes2010[,"FPTP"] <- colnames(votes2010)[unlist(apply(votes2010, 1, which.max))]
for(i in 1:nrow(votes2010)){
  if(votes2010$FPTP[i]=="Con"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2010[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=2010
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes2010[i,1:(ncol(votes2010)-1)])
  }
  if(ans2010$CV[i]=="Con"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2010[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=2010
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes2010[i,1:(ncol(votes2010)-1)])
  }
}

for(i in 1:nrow(votes2010)){
  if(votes2010$FPTP[i]=="Lab"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2010[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=2010
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes2010[i,1:(ncol(votes2010)-1)])
  }
  if(ans2010$CV[i]=="Lab"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2010[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=2010
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes2010[i,1:(ncol(votes2010)-1)])
  }
}

#2005
votes2005[,"FPTP"] <- colnames(votes2005)[unlist(apply(votes2005, 1, which.max))]
for(i in 1:nrow(votes2005)){
  if(votes2005$FPTP[i]=="Con"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2005[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=2005
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes2005[i,1:(ncol(votes2005)-1)])
  }
  if(ans2005$CV[i]=="Con"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2005[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=2005
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes2005[i,1:(ncol(votes2005)-1)])
  }
}

for(i in 1:nrow(votes2005)){
  if(votes2005$FPTP[i]=="Lab"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2005[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=2005
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes2005[i,1:(ncol(votes2005)-1)])
  }
  if(ans2005$CV[i]=="Lab"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2005[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=2005
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes2005[i,1:(ncol(votes2005)-1)])
  }
}

#2001
votes2001[,"FPTP"] <- colnames(votes2001)[unlist(apply(votes2001, 1, which.max))]
for(i in 1:nrow(votes2001)){
  if(votes2001$FPTP[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2001[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=2001
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes2001[i,1:(ncol(votes2001)-1)])
  }
  if(ans2001$CV[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2001[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=2001
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes2001[i,1:(ncol(votes2001)-1)])
  }
}

for(i in 1:nrow(votes2001)){
  if(votes2001$FPTP[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2001[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=2001
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes2001[i,1:(ncol(votes2001)-1)])
  }
  if(ans2001$CV[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes2001[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=2001
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes2001[i,1:(ncol(votes2001)-1)])
  }
}

#1997
votes1997[,"FPTP"] <- colnames(votes1997)[unlist(apply(votes1997, 1, which.max))]
for(i in 1:nrow(votes1997)){
  if(votes1997$FPTP[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1997[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=1997
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes1997[i,1:(ncol(votes1997)-1)])
  }
  if(ans1997$CV[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1997[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=1997
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes1997[i,1:(ncol(votes1997)-1)])
  }
}

for(i in 1:nrow(votes1997)){
  if(votes1997$FPTP[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1997[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=1997
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes1997[i,1:(ncol(votes1997)-1)])
  }
  if(ans1997$CV[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1997[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=1997
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes1997[i,1:(ncol(votes1997)-1)])
  }
}

#1992
votes1992[,"FPTP"] <- colnames(votes1992)[unlist(apply(votes1992, 1, which.max))]
for(i in 1:nrow(votes1992)){
  if(votes1992$FPTP[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1992[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=1992
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes1992[i,1:(ncol(votes1992)-1)])
  }
  if(ans1992$CV[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1992[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=1992
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes1992[i,1:(ncol(votes1992)-1)])
  }
}

for(i in 1:nrow(votes1992)){
  if(votes1992$FPTP[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1992[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=1992
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes1992[i,1:(ncol(votes1992)-1)])
  }
  if(ans1992$CV[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1992[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=1992
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes1992[i,1:(ncol(votes1992)-1)])
  }
}

#1987
votes1987[,"FPTP"] <- colnames(votes1987)[unlist(apply(votes1987, 1, which.max))]
for(i in 1:nrow(votes1987)){
  if(votes1987$FPTP[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1987[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=1987
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes1987[i,1:(ncol(votes1987)-1)])
  }
  if(ans1987$CV[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1987[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=1987
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes1987[i,1:(ncol(votes1987)-1)])
  }
}

for(i in 1:nrow(votes1987)){
  if(votes1987$FPTP[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1987[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=1987
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes1987[i,1:(ncol(votes1987)-1)])
  }
  if(ans1987$CV[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1987[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=1987
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes1987[i,1:(ncol(votes1987)-1)])
  }
}

#1983
votes1983[,"FPTP"] <- colnames(votes1983)[unlist(apply(votes1983, 1, which.max))]
for(i in 1:nrow(votes1983)){
  if(votes1983$FPTP[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1983[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=1983
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes1983[i,1:(ncol(votes1983)-1)])
  }
  if(ans1983$CV[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1983[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=1983
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes1983[i,1:(ncol(votes1983)-1)])
  }
}

for(i in 1:nrow(votes1983)){
  if(votes1983$FPTP[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1983[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=1983
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes1983[i,1:(ncol(votes1983)-1)])
  }
  if(ans1983$CV[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1983[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=1983
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes1983[i,1:(ncol(votes1983)-1)])
  }
}

#1979
votes1979[,"FPTP"] <- colnames(votes1979)[unlist(apply(votes1979, 1, which.max))]
for(i in 1:nrow(votes1979)){
  if(votes1979$FPTP[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1979[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=1979
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes1979[i,1:(ncol(votes1979)-1)])
  }
  if(ans1979$CV[i]=="Conservative"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1979[i,])
    constit[y, "Party"]="Conservative"
    constit[y, "Year"]=1979
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes1979[i,1:(ncol(votes1979)-1)])
  }
}

for(i in 1:nrow(votes1979)){
  if(votes1979$FPTP[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1979[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=1979
    constit[y, "System"]="FPTP"
    constit[y, "Votes"]=rowSums(votes1979[i,1:(ncol(votes1979)-1)])
  }
  if(ans1979$CV[i]=="Labour"){
    y=(nrow(constit)+1)
    constit[y, "Constituency"]=rownames(votes1979[i,])
    constit[y, "Party"]="Labour"
    constit[y, "Year"]=1979
    constit[y, "System"]="CV"
    constit[y, "Votes"]=rowSums(votes1979[i,1:(ncol(votes1979)-1)])
  }
}

write.csv(constit, "constitPlots.csv")