#Create dataframe
voteShare <- data.frame(Party=character(),
                        Year=integer(),
                        Percent=double(),
                        stringsAsFactors=FALSE)

#read votes
source("source/readvotes.R", local=TRUE)

#calculate vote Share
#Create other category to group parties that receive less than 0.0005 of the vote
oth=0
for (i in 1:(ncol(votes2017)-1)){
  x=(sum(votes2017[i]))/sum(votes2017[1:(ncol(votes2017)-1)])
  if(x<0.0005){
    oth=oth+x
  }else{
    y=(nrow(voteShare)+1)
    voteShare[y, "Party"]= colnames(votes2017[i])
    voteShare[y, "Year"]=2017
    voteShare[y, "Percent"]=x
  }  
}
y=(nrow(voteShare)+1)
voteShare[y, "Party"]= "Oth"
voteShare[y, "Year"]=2017
voteShare[y, "Percent"]=oth
oth=0
for (i in 1:(ncol(votes2015)-1)){
  x=(sum(votes2015[i]))/sum(votes2015[1:(ncol(votes2015)-1)])
  if(x<0.0005){
    oth=oth+x
  }else{
    y=(nrow(voteShare)+1)
    voteShare[y, "Party"]= colnames(votes2015[i])
    voteShare[y, "Year"]=2015
    voteShare[y, "Percent"]=x
  }
}
y=(nrow(voteShare)+1)
voteShare[y, "Party"]= "Oth"
voteShare[y, "Year"]=2015
voteShare[y, "Percent"]=oth
oth=0
for (i in 1:(ncol(votes2010)-1)){
  x=(sum(votes2010[i]))/sum(votes2010[1:(ncol(votes2010)-1)])
  if(x<0.0005){
    oth=oth+x
  }else{
    y=(nrow(voteShare)+1)
    voteShare[y, "Party"]= colnames(votes2010[i])
    voteShare[y, "Year"]=2010
    voteShare[y, "Percent"]=x
  }  
}
y=(nrow(voteShare)+1)
voteShare[y, "Party"]= "Oth"
voteShare[y, "Year"]=2010
voteShare[y, "Percent"]=oth
oth=0
for (i in 1:(ncol(votes2005)-1)){
  x=(sum(votes2005[i]))/sum(votes2005[1:(ncol(votes2005)-1)])
  if(x<0.0005){
    oth=oth+x
  }else{
    y=(nrow(voteShare)+1)
    voteShare[y, "Party"]= colnames(votes2005[i])
    voteShare[y, "Year"]=2005
    voteShare[y, "Percent"]=x
  }
}
y=(nrow(voteShare)+1)
voteShare[y, "Party"]= "Oth"
voteShare[y, "Year"]=2005
voteShare[y, "Percent"]=oth
oth=0
for (i in 1:(ncol(votes2001)-1)){
  x=(sum(votes2001[i]))/sum(votes2001[1:(ncol(votes2001)-1)])
  if(x<0.0005){
    oth=oth+x
  }else{
    y=(nrow(voteShare)+1)
    voteShare[y, "Party"]= colnames(votes2001[i])
    voteShare[y, "Year"]=2001
    voteShare[y, "Percent"]=x
  }  
}
y=(nrow(voteShare)+1)
voteShare[y, "Party"]= "Oth"
voteShare[y, "Year"]=2001
voteShare[y, "Percent"]=oth
oth=0
for (i in 1:(ncol(votes1997)-1)){
  x=(sum(votes1997[i]))/sum(votes1997[1:(ncol(votes1997)-1)])
  if(x<0.0005){
    oth=oth+x
  }else{
    y=(nrow(voteShare)+1)
    voteShare[y, "Party"]= colnames(votes1997[i])
    voteShare[y, "Year"]=1997
    voteShare[y, "Percent"]=x
  }  
}
y=(nrow(voteShare)+1)
voteShare[y, "Party"]= "Oth"
voteShare[y, "Year"]=1997
voteShare[y, "Percent"]=oth
oth=0
for (i in 1:(ncol(votes1992)-1)){
  x=(sum(votes1992[i]))/sum(votes1992[1:(ncol(votes1992)-1)])
  if(x<0.0005){
    oth=oth+x
  }else{
    y=(nrow(voteShare)+1)
    voteShare[y, "Party"]= colnames(votes1992[i])
    voteShare[y, "Year"]=1992
    voteShare[y, "Percent"]=x
  }
}
y=(nrow(voteShare)+1)
voteShare[y, "Party"]= "Oth"
voteShare[y, "Year"]=1992
voteShare[y, "Percent"]=oth
oth=0
for (i in 1:(ncol(votes1987)-1)){
  x=(sum(votes1987[i]))/sum(votes1987[1:(ncol(votes1987)-1)])
  if(x<0.0005){
    oth=oth+x
  }else{
    y=(nrow(voteShare)+1)
    voteShare[y, "Party"]= colnames(votes1987[i])
    voteShare[y, "Year"]=1987
    voteShare[y, "Percent"]=x
  }
}
y=(nrow(voteShare)+1)
voteShare[y, "Party"]= "Oth"
voteShare[y, "Year"]=1987
voteShare[y, "Percent"]=oth
oth=0
for (i in 1:(ncol(votes1983)-1)){
  x=(sum(votes1983[i]))/sum(votes1983[1:(ncol(votes1983)-1)])
  if(x<0.0005){
    oth=oth+x
  }else{
    y=(nrow(voteShare)+1)
    voteShare[y, "Party"]= colnames(votes1983[i])
    voteShare[y, "Year"]=1983
    voteShare[y, "Percent"]=x
  }  
}
y=(nrow(voteShare)+1)
voteShare[y, "Party"]= "Oth"
voteShare[y, "Year"]=1983
voteShare[y, "Percent"]=oth
oth=0
for (i in 1:(ncol(votes1979)-1)){
  x=(sum(votes1979[i]))/sum(votes1979[1:(ncol(votes1979)-1)])
  if(x<0.0005){
    oth=oth+x
  }else{
    y=(nrow(voteShare)+1)
    voteShare[y, "Party"]= colnames(votes1979[i])
    voteShare[y, "Year"]=1979
    voteShare[y, "Percent"]=x
  }
}
y=(nrow(voteShare)+1)
voteShare[y, "Party"]= "Oth"
voteShare[y, "Year"]=1979
voteShare[y, "Percent"]=oth

write.csv(voteShare, "voteShare.csv")

#Create dataframe
seatShare <- data.frame(Constituency=character(),
                        Year=double(),
                        CV=character(),
                        FPTP=character(),
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

#Calculate the Percentage Share of the Seats
for(i in 1:nrow(ans2017)){
      y=(nrow(seatShare)+1)
      seatShare[y, "Constituency"]=ans2017$Constituency[i]
      seatShare[y, "Year"]=2017
      seatShare[y, "CV"]=ans2017$CV[i]
      seatShare[y, "FPTP"]=ans2017$FPTP[i]
}
for(i in 1:nrow(ans2015)){
  y=(nrow(seatShare)+1)
  seatShare[y, "Constituency"]=ans2015$Constituency[i]
  seatShare[y, "Year"]=2015
  seatShare[y, "CV"]=ans2015$CV[i]
  seatShare[y, "FPTP"]=ans2015$FPTP[i]
}
for(i in 1:nrow(ans2010)){
  y=(nrow(seatShare)+1)
  seatShare[y, "Constituency"]=ans2010$Constituency[i]
  seatShare[y, "Year"]=2010
  seatShare[y, "CV"]=ans2010$CV[i]
  seatShare[y, "FPTP"]=ans2010$FPTP[i]
}
for(i in 1:nrow(ans2005)){
  y=(nrow(seatShare)+1)
  seatShare[y, "Constituency"]=ans2005$Constituency[i]
  seatShare[y, "Year"]=2005
  seatShare[y, "CV"]=ans2005$CV[i]
  seatShare[y, "FPTP"]=ans2005$FPTP[i]
}
for(i in 1:nrow(ans2001)){
  y=(nrow(seatShare)+1)
  seatShare[y, "Constituency"]=ans2001$Constituency[i]
  seatShare[y, "Year"]=2001
  seatShare[y, "CV"]=ans2001$CV[i]
  seatShare[y, "FPTP"]=ans2001$FPTP[i]
}
for(i in 1:nrow(ans1997)){
  y=(nrow(seatShare)+1)
  seatShare[y, "Constituency"]=ans1997$Constituency[i]
  seatShare[y, "Year"]=1997
  seatShare[y, "CV"]=ans1997$CV[i]
  seatShare[y, "FPTP"]=ans1997$FPTP[i]
}
for(i in 1:nrow(ans1992)){
  y=(nrow(seatShare)+1)
  seatShare[y, "Constituency"]=ans1992$Constituency[i]
  seatShare[y, "Year"]=1992
  seatShare[y, "CV"]=ans1992$CV[i]
  seatShare[y, "FPTP"]=ans1992$FPTP[i]
}
for(i in 1:nrow(ans1987)){
  y=(nrow(seatShare)+1)
  seatShare[y, "Constituency"]=ans1987$Constituency[i]
  seatShare[y, "Year"]=1987
  seatShare[y, "CV"]=ans1987$CV[i]
  seatShare[y, "FPTP"]=ans1987$FPTP[i]
}
for(i in 1:nrow(ans1983)){
  y=(nrow(seatShare)+1)
  seatShare[y, "Constituency"]=ans1983$Constituency[i]
  seatShare[y, "Year"]=1983
  seatShare[y, "CV"]=ans1983$CV[i]
  seatShare[y, "FPTP"]=ans1983$FPTP[i]
}
for(i in 1:nrow(ans1979)){
  y=(nrow(seatShare)+1)
  seatShare[y, "Constituency"]=ans1979$Constituency[i]
  seatShare[y, "Year"]=1979
  seatShare[y, "CV"]=ans1979$CV[i]
  seatShare[y, "FPTP"]=ans1979$FPTP[i]
}

write.csv(seatShare, "seatShare.csv")

#Calculate Percentage change from FPTP to CV
percentchang <- data.frame(Party=character(),
                           Year=integer(),
                           Change=double(),
                           stringsAsFactors=FALSE)
#2017
for(i in unique(colnames(votes2017))){
  x=(nrow(percentchang)+1)
  percentchang[x, "Party"]=i
  percentchang[x, "Year"]=2017
  percentFPTP=(length(which(ans2017$FPTP == i))/nrow(ans2017))
  percentCV=(length(which(ans2017$CV == i))/nrow(ans2017))
  percentchang[x, "Change"]=percentCV-percentFPTP
}

#2015
for(i in unique(colnames(votes2015))){
  x=(nrow(percentchang)+1)
  percentchang[x, "Party"]=i
  percentchang[x, "Year"]=2015
  percentFPTP=(length(which(ans2015$FPTP == i))/nrow(ans2015))
  percentCV=(length(which(ans2015$CV == i))/nrow(ans2015))
  percentchang[x, "Change"]=percentCV-percentFPTP
}
#2010
for(i in unique(colnames(votes2010))){
  x=(nrow(percentchang)+1)
  percentchang[x, "Party"]=i
  percentchang[x, "Year"]=2010
  percentFPTP=(length(which(ans2010$FPTP == i))/nrow(ans2010))
  percentCV=(length(which(ans2010$CV == i))/nrow(ans2010))
  percentchang[x, "Change"]=percentCV-percentFPTP
}
#2005
for(i in unique(colnames(votes2005))){
  x=(nrow(percentchang)+1)
  percentchang[x, "Party"]=i
  percentchang[x, "Year"]=2005
  percentFPTP=(length(which(ans2005$FPTP == i))/nrow(ans2005))
  percentCV=(length(which(ans2005$CV == i))/nrow(ans2005))
  percentchang[x, "Change"]=percentCV-percentFPTP
}
#2001
for(i in unique(colnames(votes2001))){
  x=(nrow(percentchang)+1)
  percentchang[x, "Party"]=i
  percentchang[x, "Year"]=2001
  percentFPTP=(length(which(ans2001$FPTP == i))/nrow(ans2001))
  percentCV=(length(which(ans2001$CV == i))/nrow(ans2001))
  percentchang[x, "Change"]=percentCV-percentFPTP
}
#1997
for(i in unique(colnames(votes1997))){
  x=(nrow(percentchang)+1)
  percentchang[x, "Party"]=i
  percentchang[x, "Year"]=1997
  percentFPTP=(length(which(ans1997$FPTP == i))/nrow(ans1997))
  percentCV=(length(which(ans1997$CV == i))/nrow(ans1997))
  percentchang[x, "Change"]=percentCV-percentFPTP
}
#1992
for(i in unique(colnames(votes1992))){
  x=(nrow(percentchang)+1)
  percentchang[x, "Party"]=i
  percentchang[x, "Year"]=1992
  percentFPTP=(length(which(ans1992$FPTP == i))/nrow(ans1992))
  percentCV=(length(which(ans1992$CV == i))/nrow(ans1992))
  percentchang[x, "Change"]=percentCV-percentFPTP
}
#1987
for(i in unique(colnames(votes1987))){
  x=(nrow(percentchang)+1)
  percentchang[x, "Party"]=i
  percentchang[x, "Year"]=1987
  percentFPTP=(length(which(ans1987$FPTP == i))/nrow(ans1987))
  percentCV=(length(which(ans1987$CV == i))/nrow(ans1987))
  percentchang[x, "Change"]=percentCV-percentFPTP
}
#1983
for(i in unique(colnames(votes1983))){
  x=(nrow(percentchang)+1)
  percentchang[x, "Party"]=i
  percentchang[x, "Year"]=1983
  percentFPTP=(length(which(ans1983$FPTP == i))/nrow(ans1983))
  percentCV=(length(which(ans1983$CV == i))/nrow(ans1983))
  percentchang[x, "Change"]=percentCV-percentFPTP
}
#1979
for(i in unique(colnames(votes1979))){
  x=(nrow(percentchang)+1)
  percentchang[x, "Party"]=i
  percentchang[x, "Year"]=1979
  percentFPTP=(length(which(ans1979$FPTP == i))/nrow(ans1979))
  percentCV=(length(which(ans1979$CV == i))/nrow(ans1979))
  percentchang[x, "Change"]=percentCV-percentFPTP
}

write.csv(percentchang, "percentChange.csv")