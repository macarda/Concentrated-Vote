library(data.table)
source("voting.R")

#Independent Candidates Separated Out
#2017
votes2017 <- read.csv("Data/2017.csv", stringsAsFactors=FALSE)
info <- votes2017[,1:6]
votes2017 <- votes2017[,-(1:6)]
rownames(votes2017) <- as.character(info$Constituency)
votes2017[is.na(votes2017)] <- 0

#Separate Independents
counter=1
for (i in 1:nrow(votes2017)){
  if (votes2017$Ind[i]!=0){
    x=paste("Ind",counter, sep="")
    votes2017[i,x]=votes2017$Ind[i]
    votes2017$Ind[i]=0
    counter=counter+1
  }
}
votes2017[is.na(votes2017)] <- 0

#remove column if no. of votes < min(consitituency)
min=abs(min(rowSums(votes2017))/2)
l<-c()
for(i in 1:ncol(votes2017)){
  if(sum(votes2017[,i])<min){
    l<-c(l,i)
  }  
}
votes2017=votes2017[,-l]

## Find NI seats - I can't find one party that stood in all NI seats
## that year, hence the "OR"
ni.seats <-
  (votes2017[,"DUP"] > 0) |
  (votes2017[,"SDLP"] > 0) |
  (votes2017[,"SF"]>0)

## Take out NI seats to get GB seats
dat.gb <- votes2017[!ni.seats,]
dat.gb <- dat.gb[,colSums(dat.gb)>0]
ans.gb <- shifter(dat.gb)

## And then do NI seats to get GB seats
dat.ni <- votes2017[ni.seats,]
dat.ni <- dat.ni[,colSums(dat.ni)>0]
ans.ni <- shifter(dat.ni)

#GB
lstData <- Map(as.data.frame, ans.gb)
ans.gb <- rbindlist(lstData)
ans.gb=ans.gb[1:632,]
ans.gb$Constituency <- row.names(dat.gb)
ans.gb[,"CV"] <- colnames(ans.gb)[unlist(apply(ans.gb, 1, which.max))]
#NI
lstData <- Map(as.data.frame, ans.ni)
ans.ni <- rbindlist(lstData)
ans.ni=ans.ni[1:18,]
ans.ni$Constituency <- row.names(dat.ni)
ans.ni[,"CV"] <- colnames(ans.ni)[unlist(apply(ans.ni, 1, which.max))]
ans.ni$FPTP=colnames(votes2017[ni.seats,])[unlist(apply(votes2017[ni.seats,], 1, which.max))]
ans.gb$FPTP=colnames(votes2017[!ni.seats,])[unlist(apply(votes2017[!ni.seats,], 1, which.max))]
ans.uk=rbind(ans.ni, ans.gb, fill=TRUE)
ans.uk[is.na(ans.uk)] <- 0
write.csv(ans.uk, file = "Output/ans2017.csv",row.names=FALSE)


#2015
votes2015 <- read.csv("Data/2015.csv", stringsAsFactors=FALSE)
info <- votes2015[,1:6]
votes2015 <- votes2015[,-(1:6)]
rownames(votes2015) <- as.character(info$Constituency)
votes2015[is.na(votes2015)] <- 0

#Separate Independents
counter=1
for (i in 1:nrow(votes2015)){
  if (votes2015$Independent[i]!=0){
    x=paste("Ind",counter, sep="")
    votes2015[i,x]=votes2015$Independent[i]
    votes2015$Independent[i]=0
    counter=counter+1
  }
}
votes2015[is.na(votes2015)] <- 0

#remove column if no. of votes < min(consitituency)
min=abs(min(rowSums(votes2015))/2)
l<-c()
for(i in 1:ncol(votes2015)){
  if(sum(votes2015[,i])<min){
    l<-c(l,i)
  }  
}
votes2015=votes2015[,-l]
## Find NI seats - I can't find one party that stood in all NI seats
## that year, hence the "OR"
ni.seats <-
  (votes2015[,"Ulster.Unionist.Party"] > 0) |
  (votes2015[,"Social.Democratic.and.Labour.Party"] > 0) |
  (votes2015[,"Sinn.Fein"]>0)

## Take out NI seats to get GB seats
dat.gb <- votes2015[!ni.seats,]
dat.gb <- dat.gb[,colSums(dat.gb)>0]
ans.gb <- shifter(dat.gb)

## And then do NI seats to get GB seats
dat.ni <- votes2015[ni.seats,]
dat.ni <- dat.ni[,colSums(dat.ni)>0]
ans.ni <- shifter(dat.ni)

#GB
lstData <- Map(as.data.frame, ans.gb)
ans.gb <- rbindlist(lstData)
ans.gb=ans.gb[1:632,]
ans.gb$Constituency <- row.names(dat.gb)
ans.gb[,"CV"] <- colnames(ans.gb)[unlist(apply(ans.gb, 1, which.max))]
#NI
lstData <- Map(as.data.frame, ans.ni)
ans.ni <- rbindlist(lstData)
ans.ni=ans.ni[1:18,]
ans.ni$Constituency <- row.names(dat.ni)
ans.ni[,"CV"] <- colnames(ans.ni)[unlist(apply(ans.ni, 1, which.max))]
ans.ni$FPTP=colnames(votes2015[ni.seats,])[unlist(apply(votes2015[ni.seats,], 1, which.max))]
ans.gb$FPTP=colnames(votes2015[!ni.seats,])[unlist(apply(votes2015[!ni.seats,], 1, which.max))]
ans.uk=rbind(ans.ni, ans.gb, fill=TRUE)
ans.uk[is.na(ans.uk)] <- 0
write.csv(ans.uk, file = "Output/ans2015.csv",row.names=FALSE)

#2010
votes2010 <- read.csv("Data/2010.csv")
votes2010 <- votes2010[-nrow(votes2010),]
info <- votes2010[,1:6]
votes2010 <- votes2010[,-(1:6)]
info[,"FPTP"] <- colnames(votes2010)[unlist(apply(votes2010, 1, which.max))]
rownames(votes2010) <- as.character(info$Constituency)
votes2010[is.na(votes2010)] <- 0
#Separate Independents
counter=1
for (i in 1:nrow(votes2010)){
  if (votes2010$Ind1[i]!=0){
    x=paste("Independent",counter, sep="")
    votes2010[i,x]=votes2010$Ind1[i]
    votes2010$Ind1[i]=0
    counter=counter+1
  }else if(votes2010$Ind2[i]!=0){
    x=paste("Independent",counter, sep="")
    votes2010[i,x]=votes2010$Ind2[i]
    votes2010$Ind2[i]=0
    counter=counter+1
  }else if(votes2010$Ind3[i]!=0){
    x=paste("Independent",counter, sep="")
    votes2010[i,x]=votes2010$Ind3[i]
    votes2010$Ind3[i]=0
    counter=counter+1
  }else if(votes2010$Ind4[i]!=0){
    x=paste("Independent",counter, sep="")
    votes2010[i,x]=votes2010$Ind4[i]
    votes2010$Ind4[i]=0
    counter=counter+1
  }else if(votes2010$Ind5[i]!=0){
    x=paste("Independent",counter, sep="")
    votes2010[i,x]=votes2010$Ind5[i]
    votes2010$Ind5[i]=0
    counter=counter+1
  }
}
votes2010[is.na(votes2010)] <- 0

#remove column if no. of votes < min(consitituency)
min=abs(min(rowSums(votes2010))/2)
l<-c()
for(i in 1:ncol(votes2010)){
  if(sum(votes2010[,i])<min){
    l<-c(l,i)
  }  
}
votes2010=votes2010[,-l]
## Find NI seats - I can't find one party that stood in all NI seats
## that year, hence the "OR"
ni.seats <-
  (votes2010[,"DUP"] > 0) |
  (votes2010[,"SDLP"] > 0) |
  (votes2010[,"SF"]>0)

## Take out NI seats to get GB seats
dat.gb <- votes2010[!ni.seats,]
dat.gb <- dat.gb[,colSums(dat.gb)>0]
ans.gb <- shifter(dat.gb)

## And then do NI seats to get GB seats
dat.ni <- votes2010[ni.seats,]
dat.ni <- dat.ni[,colSums(dat.ni)>0]
ans.ni <- shifter(dat.ni)

#GB
lstData <- Map(as.data.frame, ans.gb)
ans.gb <- rbindlist(lstData)
ans.gb=ans.gb[1:632,]
ans.gb$Constituency <- row.names(dat.gb)
ans.gb[,"CV"] <- colnames(ans.gb)[unlist(apply(ans.gb, 1, which.max))]
#NI
lstData <- Map(as.data.frame, ans.ni)
ans.ni <- rbindlist(lstData)
ans.ni=ans.ni[1:18,]
ans.ni$Constituency <- row.names(dat.ni)
ans.ni[,"CV"] <- colnames(ans.ni)[unlist(apply(ans.ni, 1, which.max))]
ans.ni$FPTP=colnames(votes2010[ni.seats,])[unlist(apply(votes2010[ni.seats,], 1, which.max))]
ans.gb$FPTP=colnames(votes2010[!ni.seats,])[unlist(apply(votes2010[!ni.seats,], 1, which.max))]
ans.uk=rbind(ans.ni, ans.gb, fill=TRUE)
ans.uk[is.na(ans.uk)] <- 0
write.csv(ans.uk, file = "Output/ans2010.csv",row.names=FALSE)

#2005
votes2005 <- read.csv("Data/2005.csv", stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")
info <- votes2005[,1:6]
votes2005 <- votes2005[,-(1:6)]
info[,"FPTP"] <- colnames(votes2005)[unlist(apply(votes2005, 1, which.max))]
rownames(votes2005) <- as.character(info$Constituency)
votes2005[is.na(votes2005)] <- 0
#Separate Independents
counter=1
for (i in 1:nrow(votes2005)){
  if (votes2005$Ind[i]!=0){
    x=paste("Ind",counter, sep="")
    votes2005[i,x]=votes2005$Ind[i]
    votes2005$Ind[i]=0
    counter=counter+1
  }
}
votes2005[is.na(votes2005)] <- 0

#remove column if no. of votes < min(consitituency)
min=abs(min(rowSums(votes2005))/2)
l<-c()
for(i in 1:ncol(votes2005)){
  if(sum(votes2005[,i])<min){
    l<-c(l,i)
  }  
}
votes2005=votes2005[,-l]
#remove column sum is 0
l<-c()
for(i in 1:ncol(votes2005)){
  if(sum(votes2005[,i])<1){
    #votes=votes[,-c(i)] 
    l<-c(l,i)
  }else{
    print("hello world")
  }  
}
votes2005=votes2005[,-l]
## Find NI seats - I can't find one party that stood in all NI seats
## that year, hence the "OR"
ni.seats <-
  (votes2005[,"DUP"] > 0) |
  (votes2005[,"SDLP"] > 0) |
  (votes2005[,"SF"]>0)

## Take out NI seats to get GB seats
dat.gb <- votes2005[!ni.seats,]
dat.gb <- dat.gb[,colSums(dat.gb)>0]
ans.gb <- shifter(dat.gb)

## And then do NI seats to get GB seats
dat.ni <- votes2005[ni.seats,]
dat.ni <- dat.ni[,colSums(dat.ni)>0]
ans.ni <- shifter(dat.ni)

#GB
lstData <- Map(as.data.frame, ans.gb)
ans.gb <- rbindlist(lstData)
ans.gb=ans.gb[1:628,]
ans.gb$Constituency <- row.names(dat.gb)
ans.gb[,"CV"] <- colnames(ans.gb)[unlist(apply(ans.gb, 1, which.max))]
#NI
lstData <- Map(as.data.frame, ans.ni)
ans.ni <- rbindlist(lstData)
ans.ni=ans.ni[1:18,]
ans.ni$Constituency <- row.names(dat.ni)
ans.ni[,"CV"] <- colnames(ans.ni)[unlist(apply(ans.ni, 1, which.max))]
ans.ni$FPTP=colnames(votes2005[ni.seats,])[unlist(apply(votes2005[ni.seats,], 1, which.max))]
ans.gb$FPTP=colnames(votes2005[!ni.seats,])[unlist(apply(votes2005[!ni.seats,], 1, which.max))]
ans.uk=rbind(ans.ni, ans.gb, fill=TRUE)
ans.uk[is.na(ans.uk)] <- 0
write.csv(ans.uk, file = "Output/ans2005.csv",row.names=FALSE)


#2001
votes2001 <- read.csv("Data/2001.csv", stringsAsFactors=FALSE)
info <- votes2001[,1:6]
votes2001 <- votes2001[,-(1:6)]
rownames(votes2001) <- as.character(info$Constituency)
votes2001[is.na(votes2001)] <- 0
#Separate Independents
counter=1
for (i in 1:nrow(votes2001)){
  if (votes2001$Independent[i]!=0){
    x=paste("Ind",counter, sep="")
    votes2001[i,x]=votes2001$Independent[i]
    votes2001$Independent[i]=0
    counter=counter+1
  }
}
votes2001[is.na(votes2001)] <- 0

#remove column if no. of votes < min(consitituency)
min=abs(min(rowSums(votes2001))/2)
l<-c()
for(i in 1:ncol(votes2001)){
  if(sum(votes2001[,i])<min){
    l<-c(l,i)
  }  
}
votes2001=votes2001[,-l]
## Find NI seats - I can't find one party that stood in all NI seats
## that year, hence the "OR"
ni.seats <-
  (votes2001[,"Ulster.Unionist"] > 0) |
  (votes2001[,"Social.Democratic.and.Labour"] > 0) |
  (votes2001[,"Sinn.Fein"]>0)

## Take out NI seats to get GB seats
dat.gb <- votes2001[!ni.seats,]
dat.gb <- dat.gb[,colSums(dat.gb)>0]
ans.gb <- shifter(dat.gb)

## And then do NI seats to get GB seats
dat.ni <- votes2001[ni.seats,]
dat.ni <- dat.ni[,colSums(dat.ni)>0]
ans.ni <- shifter(dat.ni)

#GB
lstData <- Map(as.data.frame, ans.gb)
ans.gb <- rbindlist(lstData)
ans.gb=ans.gb[1:641,]
ans.gb$Constituency <- row.names(dat.gb)
ans.gb[,"CV"] <- colnames(ans.gb)[unlist(apply(ans.gb, 1, which.max))]
#NI
lstData <- Map(as.data.frame, ans.ni)
ans.ni <- rbindlist(lstData)
ans.ni=ans.ni[1:18,]
ans.ni$Constituency <- row.names(dat.ni)
ans.ni[,"CV"] <- colnames(ans.ni)[unlist(apply(ans.ni, 1, which.max))]
ans.ni$FPTP=colnames(votes2001[ni.seats,])[unlist(apply(votes2001[ni.seats,], 1, which.max))]
ans.gb$FPTP=colnames(votes2001[!ni.seats,])[unlist(apply(votes2001[!ni.seats,], 1, which.max))]
ans.uk=rbind(ans.ni, ans.gb, fill=TRUE)
ans.uk[is.na(ans.uk)] <- 0
write.csv(ans.uk, file = "Output/ans2001.csv",row.names=FALSE)


#1997
votes1997 <- read.csv("Data/1997.csv", stringsAsFactors=FALSE)
info <- votes1997[,1:6]
votes1997 <- votes1997[,-(1:6)]
rownames(votes1997) <- as.character(info$Constituency)
votes1997[is.na(votes1997)] <- 0
#Separate Independents
counter=1
for (i in 1:nrow(votes1997)){
  if (votes1997$Independent[i]!=0){
    x=paste("Ind",counter, sep="")
    votes1997[i,x]=votes1997$Independent[i]
    votes1997$Independent[i]=0
    counter=counter+1
  }
}
votes1997[is.na(votes1997)] <- 0

#remove column if no. of votes < min(consitituency)
min=abs(min(rowSums(votes1997))/2)
l<-c()
for(i in 1:ncol(votes1997)){
  if(sum(votes1997[,i])<min){
    l<-c(l,i)
  }  
}
votes1997=votes1997[,-l]
## Find NI seats - I can't find one party that stood in all NI seats
## that year, hence the "OR"
ni.seats <-
  (votes1997[,"Ulster.Unionist"] > 0) |
  (votes1997[,"Social.Democratic.and.Labour"] > 0) |
  (votes1997[,"Sinn.Fein"]>0)

## Take out NI seats to get GB seats
dat.gb <- votes1997[!ni.seats,]
dat.gb <- dat.gb[,colSums(dat.gb)>0]
ans.gb <- shifter(dat.gb)

## And then do NI seats to get GB seats
dat.ni <- votes1997[ni.seats,]
dat.ni <- dat.ni[,colSums(dat.ni)>0]
ans.ni <- shifter(dat.ni)

#GB
lstData <- Map(as.data.frame, ans.gb)
ans.gb <- rbindlist(lstData)
ans.gb=ans.gb[1:641,]
ans.gb$Constituency <- row.names(dat.gb)
ans.gb[,"CV"] <- colnames(ans.gb)[unlist(apply(ans.gb, 1, which.max))]
#NI
lstData <- Map(as.data.frame, ans.ni)
ans.ni <- rbindlist(lstData)
ans.ni=ans.ni[1:18,]
ans.ni$Constituency <- row.names(dat.ni)
ans.ni[,"CV"] <- colnames(ans.ni)[unlist(apply(ans.ni, 1, which.max))]
ans.ni$FPTP=colnames(votes1997[ni.seats,])[unlist(apply(votes1997[ni.seats,], 1, which.max))]
ans.gb$FPTP=colnames(votes1997[!ni.seats,])[unlist(apply(votes1997[!ni.seats,], 1, which.max))]
ans.uk=rbind(ans.ni, ans.gb, fill=TRUE)
ans.uk[is.na(ans.uk)] <- 0
write.csv(ans.uk, file = "Output/ans1997.csv",row.names=FALSE)


#1992
votes1992 <- read.csv("Data/1992.csv", stringsAsFactors=FALSE)
info <- votes1992[,1:6]
votes1992 <- votes1992[,-(1:6)]
rownames(votes1992) <- as.character(info$Constituency)
votes1992[is.na(votes1992)] <- 0
#Separate Independents
counter=1
for (i in 1:nrow(votes1992)){
  if (votes1992$Independent[i]!=0){
    x=paste("Ind",counter, sep="")
    votes1992[i,x]=votes1992$Independent[i]
    votes1992$Independent[i]=0
    counter=counter+1
  }
}
votes1992[is.na(votes1992)] <- 0

#remove column if no. of votes < min(consitituency)
min=abs(min(rowSums(votes1992))/2)
l<-c()
for(i in 1:ncol(votes1992)){
  if(sum(votes1992[,i])<min){
    l<-c(l,i)
  }  
}
votes1992=votes1992[,-l]
## Find NI seats - I can't find one party that stood in all NI seats
## that year, hence the "OR"
ni.seats <-
  (votes1992[,"Ulster.Unionist"] > 0) |
  (votes1992[,"Social.Democratic.and.Labour"] > 0) |
  (votes1992[,"Sinn.Fein"]>0)|
  (votes1992[,"Democratic.Unionist"]>0)

## Take out NI seats to get GB seats
dat.gb <- votes1992[!ni.seats,]
dat.gb <- dat.gb[,colSums(dat.gb)>0]
ans.gb <- shifter(dat.gb)

## And then do NI seats to get GB seats
dat.ni <- votes1992[ni.seats,]
dat.ni <- dat.ni[,colSums(dat.ni)>0]
ans.ni <- shifter(dat.ni)

#GB
lstData <- Map(as.data.frame, ans.gb)
ans.gb <- rbindlist(lstData)
ans.gb=ans.gb[1:634,]
ans.gb$Constituency <- row.names(dat.gb)
ans.gb[,"CV"] <- colnames(ans.gb)[unlist(apply(ans.gb, 1, which.max))]
#NI
lstData <- Map(as.data.frame, ans.ni)
ans.ni <- rbindlist(lstData)
ans.ni=ans.ni[1:17,]
ans.ni$Constituency <- row.names(dat.ni)
ans.ni[,"CV"] <- colnames(ans.ni)[unlist(apply(ans.ni, 1, which.max))]
ans.ni$FPTP=colnames(votes1992[ni.seats,])[unlist(apply(votes1992[ni.seats,], 1, which.max))]
ans.gb$FPTP=colnames(votes1992[!ni.seats,])[unlist(apply(votes1992[!ni.seats,], 1, which.max))]
ans.uk=rbind(ans.ni, ans.gb, fill=TRUE)
ans.uk[is.na(ans.uk)] <- 0
write.csv(ans.uk, file = "Output/ans1992.csv",row.names=FALSE)


#1987
votes1987 <- read.csv("Data/1987.csv", stringsAsFactors=FALSE)
info <- votes1987[,1:6]
votes1987 <- votes1987[,-(1:6)]
rownames(votes1987) <- as.character(info$Constituency)
votes1987[is.na(votes1987)] <- 0
#Separate Independents
counter=1
for (i in 1:nrow(votes1987)){
  if (votes1987$ependent[i]!=0){
    x=paste("Ind",counter, sep="")
    votes1987[i,x]=votes1987$Independent[i]
    votes1987$Independent[i]=0
    counter=counter+1
  }
}
votes1987[is.na(votes1987)] <- 0

#remove column if no. of votes < min(consitituency)
min=abs(min(rowSums(votes1987))/2)
l<-c()
for(i in 1:ncol(votes1987)){
  if(sum(votes1987[,i])<min){
    l<-c(l,i)
  }  
}
votes1987=votes1987[,-l]

## Find NI seats - I can't find one party that stood in all NI seats
## that year, hence the "OR"
ni.seats <-
  (votes1987[,"Ulster.Unionist"] > 0) |
  (votes1987[,"Sinn.Fein"] > 0) |
  (votes1987[,"Ulster.Popular.Unionist"]>0)

## Take out NI seats to get GB seats
dat.gb <- votes1987[!ni.seats,]
dat.gb <- dat.gb[,colSums(dat.gb)>0]
ans.gb <- shifter(dat.gb)

## And then do NI seats to get GB seats
dat.ni <- votes1987[ni.seats,]
dat.ni <- dat.ni[,colSums(dat.ni)>0]
ans.ni <- shifter(dat.ni)

#GB
lstData <- Map(as.data.frame, ans.gb)
ans.gb <- rbindlist(lstData)
ans.gb=ans.gb[1:633,]
ans.gb$Constituency <- row.names(dat.gb)
ans.gb[,"CV"] <- colnames(ans.gb)[unlist(apply(ans.gb, 1, which.max))]
#NI
lstData <- Map(as.data.frame, ans.ni)
ans.ni <- rbindlist(lstData)
ans.ni=ans.ni[1:17,]
ans.ni$Constituency <- row.names(dat.ni)
ans.ni[,"CV"] <- colnames(ans.ni)[unlist(apply(ans.ni, 1, which.max))]
ans.ni$FPTP=colnames(votes1987[ni.seats,])[unlist(apply(votes1987[ni.seats,], 1, which.max))]
ans.gb$FPTP=colnames(votes1987[!ni.seats,])[unlist(apply(votes1987[!ni.seats,], 1, which.max))]
ans.uk=rbind(ans.ni, ans.gb, fill=TRUE)
ans.uk[is.na(ans.uk)] <- 0
write.csv(ans.uk, file = "Output/ans1987.csv",row.names=FALSE)


#1983
votes1983 <- read.csv("Data/1983.csv", stringsAsFactors=FALSE)
votes1983$Conservative=as.numeric(votes1983$Conservative)
votes1983$Ind=as.numeric(votes1983$Ind)
info <- votes1983[,1:6]
votes1983 <- votes1983[,-(1:6)]
rownames(votes1983) <- as.character(info$Constituency)
votes1983[is.na(votes1983)] <- 0
#Separate Independents
counter=1
for (i in 1:nrow(votes1983)){
  if (votes1983$Ind[i]!=0){
    x=paste("Ind",counter, sep="")
    votes1983[i,x]=votes1983$Ind[i]
    votes1983$Ind[i]=0
    counter=counter+1
  }
}
votes1983[is.na(votes1983)] <- 0

#remove column if no. of votes < min(consitituency)
min=abs(min(rowSums(votes1983))/2)
l<-c()
for(i in 1:ncol(votes1983)){
  if(sum(votes1983[,i])<min){
    l<-c(l,i)
  }  
}
votes1983=votes1983[,-l]
#remove column sum is 0
l<-c()
for(i in 1:ncol(votes1983)){
  if(sum(votes1983[,i])<1){
    #votes=votes[,-c(i)] 
    l<-c(l,i)
  }else{
    print("hello world")
  }  
}
votes1983=votes1983[,-l]

## Find NI seats - I can't find one party that stood in all NI seats
## that year, hence the "OR"
ni.seats <-
  (votes1983[,"Official.Unionist.Party"] > 0) |
  (votes1983[,"Sinn.Fein"] > 0) |
  (votes1983[,"Ulster.Popular.Unionist"]>0)

## Take out NI seats to get GB seats
dat.gb <- votes1983[!ni.seats,]
dat.gb <- dat.gb[,colSums(dat.gb)>0]
ans.gb <- shifter(dat.gb)

## And then do NI seats to get GB seats
dat.ni <- votes1983[ni.seats,]
dat.ni <- dat.ni[,colSums(dat.ni)>0]
ans.ni <- shifter(dat.ni)

#GB
lstData <- Map(as.data.frame, ans.gb)
ans.gb <- rbindlist(lstData)
ans.gb=ans.gb[1:633,]
ans.gb$Constituency <- row.names(dat.gb)
ans.gb[,"CV"] <- colnames(ans.gb)[unlist(apply(ans.gb, 1, which.max))]
#NI
lstData <- Map(as.data.frame, ans.ni)
ans.ni <- rbindlist(lstData)
ans.ni=ans.ni[1:17,]
ans.ni$Constituency <- row.names(dat.ni)
ans.ni[,"CV"] <- colnames(ans.ni)[unlist(apply(ans.ni, 1, which.max))]
ans.ni$FPTP=colnames(votes1983[ni.seats,])[unlist(apply(votes1983[ni.seats,], 1, which.max))]
ans.gb$FPTP=colnames(votes1983[!ni.seats,])[unlist(apply(votes1983[!ni.seats,], 1, which.max))]
ans.uk=rbind(ans.ni, ans.gb, fill=TRUE)
ans.uk[is.na(ans.uk)] <- 0
write.csv(ans.uk, file = "Output/ans1983.csv",row.names=FALSE)



#1979
votes1979 <- read.csv("Data/1979.csv", stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")
info <- votes1979[,1:6]
votes1979 <- votes1979[,-(1:6)]
rownames(votes1979) <- as.character(info$Constituency)
votes1979[is.na(votes1979)] <- 0
#Separate Independents
counter=1
for (i in 1:nrow(votes1979)){
  if (votes1979$Ind[i]!=0){
    x=paste("Ind",counter, sep="")
    votes1979[i,x]=votes1979$Ind[i]
    votes1979$Ind[i]=0
    counter=counter+1
  }else if(votes1979$Ind.[i]!=0){
    x=paste("Ind",counter, sep="")
    votes1979[i,x]=votes1979$Ind.[i]
    votes1979$Ind.[i]=0
    counter=counter+1
  }  
}
votes1979[is.na(votes1979)] <- 0

#remove column if no. of votes < min(consitituency)
min=abs(min(rowSums(votes1979))/2)
l<-c()
for(i in 1:ncol(votes1979)){
  if(sum(votes1979[,i])<min){
    l<-c(l,i)
  }  
}
votes1979=votes1979[,-l]

## Find NI seats - I can't find one party that stood in all NI seats
## that year, hence the "OR"
ni.seats <-
  (votes1979[,"Ulster.Unionist"] > 0) |
  (votes1979[,"SDLP"] > 0) |
  (votes1979[,"Unionist.of.NI"]>0)

## Take out NI seats to get GB seats
dat.gb <- votes1979[!ni.seats,]
dat.gb <- dat.gb[,colSums(dat.gb)>0]
ans.gb <- shifter(dat.gb)

## And then do NI seats to get GB seats
dat.ni <- votes1979[ni.seats,]
dat.ni <- dat.ni[,colSums(dat.ni)>0]
ans.ni <- shifter(dat.ni)

#GB
lstData <- Map(as.data.frame, ans.gb)
ans.gb <- rbindlist(lstData)
ans.gb=ans.gb[1:623,]
ans.gb$Constituency <- row.names(dat.gb)
ans.gb[,"CV"] <- colnames(ans.gb)[unlist(apply(ans.gb, 1, which.max))]
#NI
lstData <- Map(as.data.frame, ans.ni)
ans.ni <- rbindlist(lstData)
ans.ni=ans.ni[1:12,]
ans.ni$Constituency <- row.names(dat.ni)
ans.ni[,"CV"] <- colnames(ans.ni)[unlist(apply(ans.ni, 1, which.max))]
ans.ni$FPTP=colnames(votes1979[ni.seats,])[unlist(apply(votes1979[ni.seats,], 1, which.max))]
ans.gb$FPTP=colnames(votes1979[!ni.seats,])[unlist(apply(votes1979[!ni.seats,], 1, which.max))]
ans.uk=rbind(ans.ni, ans.gb, fill=TRUE)
ans.uk[is.na(ans.uk)] <- 0
write.csv(ans.uk, file = "Output/ans1979.csv",row.names=FALSE)
