#Create dataframe
votesperMP <- data.frame(Party=character(),
                         Votes=double(),
                         Year=integer(),
                         System=character(),
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

#Calculates the votes per MP elected for FPTP and Concentrated Vote
for(i in 1:ncol(ans2017)){
  if(colnames(ans2017[i])!="CV" & colnames(ans2017[i])!="Constituency" & colnames(ans2017[i])!="FPTP"){
    x=sum(ans2017[,i])/sum(ans2017$CV==colnames(ans2017[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(ans2017[i])
      votesperMP[y, "Year"]=2017
      votesperMP[y, "System"]="CV"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}
for(i in 1:ncol(votes2017)){
  if(colnames(votes2017[i])!="FPTP"){
    x=sum(votes2017[,i])/sum(votes2017$FPTP==colnames(votes2017[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(votes2017[i])
      votesperMP[y, "Year"]=2017
      votesperMP[y, "System"]="FPTP"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}

#2015
for(i in 1:ncol(ans2015)){
  if(colnames(ans2015[i])!="CV" & colnames(ans2015[i])!="Constituency" & colnames(ans2015[i])!="FPTP"){
    x=sum(ans2015[,i])/sum(ans2015$CV==colnames(ans2015[i]))
    if(x!=Inf && !is.na(x)){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(ans2015[i])
      votesperMP[y, "Year"]=2015
      votesperMP[y, "System"]="CV"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}  
for(i in 1:ncol(votes2015)){
  if(colnames(votes2015[i])!="FPTP"){
    x=sum(votes2015[,i])/sum(votes2015$FPTP==colnames(votes2015[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "System"]="FPTP"
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(votes2015[i])
      votesperMP[y, "Year"]=2015
      
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}

#2010
for(i in 1:ncol(ans2010)){
  if(colnames(ans2010[i])!="CV" & colnames(ans2010[i])!="Constituency" & colnames(ans2010[i])!="FPTP"){
    x=sum(ans2010[,i])/sum(ans2010$CV==colnames(ans2010[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(ans2010[i])
      votesperMP[y, "Year"]=2010
      votesperMP[y, "System"]="CV"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}
for(i in 1:ncol(votes2010)){
  if(colnames(votes2010[i])!="FPTP"){
    x=sum(votes2010[,i])/sum(votes2010$FPTP==colnames(votes2010[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(votes2010[i])
      votesperMP[y, "Year"]=2010
      votesperMP[y, "System"]="FPTP"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}

#2005
for(i in 1:ncol(ans2005)){
  if(colnames(ans2005[i])!="CV" & colnames(ans2005[i])!="Constituency" & colnames(ans2005[i])!="FPTP"){
    x=sum(ans2005[,i])/sum(ans2005$CV==colnames(ans2005[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(ans2005[i])
      votesperMP[y, "Year"]=2005
      votesperMP[y, "System"]="CV"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}
for(i in 1:ncol(votes2005)){
  if(colnames(votes2005[i])!="FPTP"){
    x=sum(votes2005[,i])/sum(votes2005$FPTP==colnames(votes2005[i]))
    if(x!=Inf & (sum(votes2005[,i]))!=0){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(votes2005[i])
      votesperMP[y, "Year"]=2005
      votesperMP[y, "System"]="FPTP"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}

#2001
for(i in 1:ncol(ans2001)){
  if(colnames(ans2001[i])!="CV" & colnames(ans2001[i])!="Constituency" & colnames(ans2001[i])!="FPTP"){
    x=sum(ans2001[,i])/sum(ans2001$CV==colnames(ans2001[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(ans2001[i])
      votesperMP[y, "Year"]=2001
      votesperMP[y, "System"]="CV"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}
for(i in 1:ncol(votes2001)){
  if(colnames(votes2001[i])!="FPTP"){
    x=sum(votes2001[,i])/sum(votes2001$FPTP==colnames(votes2001[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(votes2001[i])
      votesperMP[y, "Year"]=2001
      votesperMP[y, "System"]="FPTP"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}

#1997
for(i in 1:ncol(ans1997)){
  if(colnames(ans1997[i])!="CV" & colnames(ans1997[i])!="Constituency" & colnames(ans1997[i])!="FPTP"){
    x=sum(ans1997[,i])/sum(ans1997$CV==colnames(ans1997[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(ans1997[i])
      votesperMP[y, "Year"]=1997
      votesperMP[y, "System"]="CV"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}
for(i in 1:ncol(votes1997)){
  if(colnames(votes1997[i])!="FPTP"){
    x=sum(votes1997[,i])/sum(votes1997$FPTP==colnames(votes1997[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(votes1997[i])
      votesperMP[y, "Year"]=1997
      votesperMP[y, "System"]="FPTP"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}

#1992
for(i in 1:ncol(ans1992)){
  if(colnames(ans1992[i])!="CV" & colnames(ans1992[i])!="Constituency" & colnames(ans1992[i])!="FPTP"){
    x=sum(ans1992[,i])/sum(ans1992$CV==colnames(ans1992[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(ans1992[i])
      votesperMP[y, "Year"]=1992
      votesperMP[y, "System"]="CV"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}
for(i in 1:ncol(votes1992)){
  if(colnames(votes1992[i])!="FPTP"){
    x=sum(votes1992[,i])/sum(votes1992$FPTP==colnames(votes1992[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(votes1992[i])
      votesperMP[y, "Year"]=1992
      votesperMP[y, "System"]="FPTP"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}

#1987
for(i in 1:ncol(ans1987)){
  if(colnames(ans1987[i])!="CV" & colnames(ans1987[i])!="Constituency" & colnames(ans1987[i])!="FPTP"){
    x=sum(ans1987[,i])/sum(ans1987$CV==colnames(ans1987[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(ans1987[i])
      votesperMP[y, "Year"]=1987
      votesperMP[y, "System"]="CV"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}
for(i in 1:ncol(votes1987)){
  if(colnames(votes1987[i])!="FPTP"){
    x=sum(votes1987[,i])/sum(votes1987$FPTP==colnames(votes1987[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(votes1987[i])
      votesperMP[y, "Year"]=1987
      votesperMP[y, "System"]="FPTP"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}

#1983
for(i in 1:ncol(ans1983)){
  if(colnames(ans1983[i])!="CV" & colnames(ans1983[i])!="Constituency" & colnames(ans1983[i])!="FPTP"){
    x=sum(ans1983[,i])/sum(ans1983$CV==colnames(ans1983[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(ans1983[i])
      votesperMP[y, "Year"]=1983
      votesperMP[y, "System"]="CV"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}
for(i in 1:ncol(votes1983)){
  if(colnames(votes1983[i])!="FPTP"){
    x=sum(votes1983[,i])/sum(votes1983$FPTP==colnames(votes1983[i]))
    if(x!=Inf && (sum(votes1983[,i]))!=0){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(votes1983[i])
      votesperMP[y, "Year"]=1983
      votesperMP[y, "System"]="FPTP"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}

#1979
for(i in 1:ncol(ans1979)){
  if(colnames(ans1979[i])!="CV" & colnames(ans1979[i])!="Constituency" & colnames(ans1979[i])!="FPTP"){
    x=sum(ans1979[,i])/sum(ans1979$CV==colnames(ans1979[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(ans1979[i])
      votesperMP[y, "Year"]=1979
      votesperMP[y, "System"]="CV"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}
for(i in 1:ncol(votes1979)){
  if(colnames(votes1979[i])!="FPTP"){
    x=sum(votes1979[,i])/sum(votes1979$FPTP==colnames(votes1979[i]))
    if(x!=Inf){
      y=(nrow(votesperMP)+1)
      votesperMP[y, "Votes"]=x
      votesperMP[y, "Party"]=colnames(votes1979[i])
      votesperMP[y, "Year"]=1979
      votesperMP[y, "System"]="FPTP"
    }
  }
  votesperMP=votesperMP[complete.cases(votesperMP), ]
}

write.csv(votesperMP, "votesperMP.csv")