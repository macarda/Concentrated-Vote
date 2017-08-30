source("voting.R")
source("connected.R")
library(data.table)

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

#find islands and separate if they exist
v17=data.matrix(votes2017, rownames.force = NA)
v17[v17 > 0] <- 1
connect17=connected_bipartite_graph(v17)
for (i in unique(connect17$rows)){
  votes = votes2017[connect17$rows==i,]
  votes = votes[,connect17$cols==i]
  ans <- shifter(votes)
  lstData <- Map(as.data.frame, ans)
  ans <- rbindlist(lstData)
  ans=ans[1:length(votes2017[connect17$rows==i,]),]
  ans$Constituency <- row.names(votes)
  ans[,"CV"] <- colnames(ans)[unlist(apply(ans, 1, which.max))]
  ans$FPTP=colnames(votes)[unlist(apply(votes, 1, which.max))]
  write.csv(ans, paste("Output/ans2017", i, ".csv"),row.names=FALSE)
}


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

#find islands and separate if they exist
v15=data.matrix(votes2015, rownames.force = NA)
v15[v15 > 0] <- 1
connect15=connected_bipartite_graph(v15)
for (i in unique(connect15$rows)){
  votes = votes2015[connect15$rows==i,]
  votes = votes[,connect15$cols==i]
  ans <- shifter(votes)
  lstData <- Map(as.data.frame, ans)
  ans <- rbindlist(lstData)
  ans=ans[1:length(votes2015[connect15$rows==i,]),]
  ans$Constituency <- row.names(votes)
  ans[,"CV"] <- colnames(ans)[unlist(apply(ans, 1, which.max))]
  ans$FPTP=colnames(votes)[unlist(apply(votes, 1, which.max))]
  write.csv(ans, paste("Output/ans2015", i, ".csv"),row.names=FALSE)
}

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

#find islands and separate if they exist
v10=data.matrix(votes2010, rownames.force = NA)
v10[v10 > 0] <- 1
connect10=connected_bipartite_graph(v10)
for (i in unique(connect10$rows)){
  votes = votes2010[connect10$rows==i,]
  votes = votes[,connect10$cols==i]
  ans <- shifter(votes)
  lstData <- Map(as.data.frame, ans)
  ans <- rbindlist(lstData)
  ans=ans[1:length(votes2010[connect10$rows==i,]),]
  ans$Constituency <- row.names(votes)
  ans[,"CV"] <- colnames(ans)[unlist(apply(ans, 1, which.max))]
  ans$FPTP=colnames(votes)[unlist(apply(votes, 1, which.max))]
  write.csv(ans, paste("Output/ans2010", i, ".csv"),row.names=FALSE)
}

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
  }
}
votes2005=votes2005[,-l]

#find islands and separate if they exist
v05=data.matrix(votes2005, rownames.force = NA)
v05[v05 > 0] <- 1
connect05=connected_bipartite_graph(v05)
for (i in unique(connect05$rows)){
  votes = votes2005[connect05$rows==i,]
  votes = votes[,connect05$cols==i]
  ans <- shifter(votes)
  lstData <- Map(as.data.frame, ans)
  ans <- rbindlist(lstData)
  ans=ans[1:length(votes2005[connect05$rows==i,]),]
  ans$Constituency <- row.names(votes)
  ans[,"CV"] <- colnames(ans)[unlist(apply(ans, 1, which.max))]
  ans$FPTP=colnames(votes)[unlist(apply(votes, 1, which.max))]
  write.csv(ans, paste("Output/ans2005", i, ".csv"),row.names=FALSE)
}

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

#find islands and separate if they exist
v01=data.matrix(votes2001, rownames.force = NA)
v01[v01 > 0] <- 1
connect01=connected_bipartite_graph(v01)
for (i in unique(connect01$rows)){
  votes = votes2001[connect01$rows==i,]
  votes = votes[,connect01$cols==i]
  ans <- shifter(votes)
  lstData <- Map(as.data.frame, ans)
  ans <- rbindlist(lstData)
  ans=ans[1:length(votes2001[connect01$rows==i,]),]
  ans$Constituency <- row.names(votes)
  ans[,"CV"] <- colnames(ans)[unlist(apply(ans, 1, which.max))]
  ans$FPTP=colnames(votes)[unlist(apply(votes, 1, which.max))]
  write.csv(ans, paste("Output/ans2001", i, ".csv"),row.names=FALSE)
}


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

#find islands and separate if they exist
v97=data.matrix(votes1997, rownames.force = NA)
v97[v97 > 0] <- 1
connect97=connected_bipartite_graph(v97)
for (i in unique(connect97$rows)){
  votes = votes1997[connect97$rows==i,]
  votes = votes[,connect97$cols==i]
  ans <- shifter(votes)
  lstData <- Map(as.data.frame, ans)
  ans <- rbindlist(lstData)
  ans=ans[1:length(votes1997[connect97$rows==i,]),]
  ans$Constituency <- row.names(votes)
  ans[,"CV"] <- colnames(ans)[unlist(apply(ans, 1, which.max))]
  ans$FPTP=colnames(votes)[unlist(apply(votes, 1, which.max))]
  write.csv(ans, paste("Output/ans1997", i, ".csv"),row.names=FALSE)
}


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

#find islands and separate if they exist
v92=data.matrix(votes1992, rownames.force = NA)
v92[v92 > 0] <- 1
connect92=connected_bipartite_graph(v92)
for (i in unique(connect92$rows)){
  votes = votes1992[connect92$rows==i,]
  votes = votes[,connect92$cols==i]
  ans <- shifter(votes)
  lstData <- Map(as.data.frame, ans)
  ans <- rbindlist(lstData)
  ans=ans[1:length(votes1992[connect92$rows==i,]),]
  ans$Constituency <- row.names(votes)
  ans[,"CV"] <- colnames(ans)[unlist(apply(ans, 1, which.max))]
  ans$FPTP=colnames(votes)[unlist(apply(votes, 1, which.max))]
  write.csv(ans, paste("Output/ans1992", i, ".csv"),row.names=FALSE)
}



#1987
votes1987 <- read.csv("Data/1987.csv", stringsAsFactors=FALSE)
info <- votes1987[,1:6]
votes1987 <- votes1987[,-(1:6)]
rownames(votes1987) <- as.character(info$Constituency)
votes1987[is.na(votes1987)] <- 0
#Separate Independents
counter=1
for (i in 1:nrow(votes1987)){
  if (votes1987$Independent[i]!=0){
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

#find islands and separate if they exist
v87=data.matrix(votes1987, rownames.force = NA)
v87[v87 > 0] <- 1
connect87=connected_bipartite_graph(v87)
for (i in unique(connect87$rows)){
  votes = votes1987[connect87$rows==i,]
  votes = votes[,connect87$cols==i]
  ans <- shifter(votes)
  lstData <- Map(as.data.frame, ans)
  ans <- rbindlist(lstData)
  ans=ans[1:length(votes1987[connect87$rows==i,]),]
  ans$Constituency <- row.names(votes)
  ans[,"CV"] <- colnames(ans)[unlist(apply(ans, 1, which.max))]
  ans$FPTP=colnames(votes)[unlist(apply(votes, 1, which.max))]
  write.csv(ans, paste("Output/ans1987", i, ".csv"),row.names=FALSE)
}


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

#find islands and separate if they exist
v83=data.matrix(votes1983, rownames.force = NA)
v83[v83 > 0] <- 1
connect83=connected_bipartite_graph(v83)
for (i in unique(connect83$rows)){
  votes = votes1983[connect83$rows==i,]
  votes = votes[,connect83$cols==i]
  ans <- shifter(votes)
  lstData <- Map(as.data.frame, ans)
  ans <- rbindlist(lstData)
  ans=ans[1:length(votes1983[connect83$rows==i,]),]
  ans$Constituency <- row.names(votes)
  ans[,"CV"] <- colnames(ans)[unlist(apply(ans, 1, which.max))]
  ans$FPTP=colnames(votes)[unlist(apply(votes, 1, which.max))]
  write.csv(ans, paste("Output/ans1983", i, ".csv"),row.names=FALSE)
}


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

#find islands and separate if they exist
v79=data.matrix(votes1979, rownames.force = NA)
v79[v79 > 0] <- 1
connect79=connected_bipartite_graph(v79)
for (i in unique(connect79$rows)){
  votes = votes1979[connect79$rows==i,]
  votes = votes[,connect79$cols==i]
  ans <- shifter(votes)
  lstData <- Map(as.data.frame, ans)
  ans <- rbindlist(lstData)
  ans=ans[1:length(votes1979[connect79$rows==i,]),]
  ans$Constituency <- row.names(votes)
  ans[,"CV"] <- colnames(ans)[unlist(apply(ans, 1, which.max))]
  ans$FPTP=colnames(votes)[unlist(apply(votes, 1, which.max))]
  write.csv(ans, paste("Output/ans1979", i, ".csv"),row.names=FALSE)
}