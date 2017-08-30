voteShare=read.csv("source/data/voteShare.csv")
seatShare=read.csv("source/data/seatShare.csv")
voteShare2017=voteShare[voteShare$Year == 2017,]
voteShare2017$Percent=voteShare2017$Percent*100
seatShare2017<-seatShare[seatShare$Year == 2017,]
seatShare2017["Percent"]=(1/nrow(seatShare2017))

seatsCV <- data.frame(Party=character(),
                        Percent=double(),
                    stringsAsFactors=FALSE)

for(i in unique(seatShare2017$CV)){
  y=(nrow(seatsCV)+1)
  seatsCV[y, "Party"]=as.character(i)
  seatsCV[y,"Percent"]=sum(seatShare2017$Percent[seatShare2017$CV==i])*100
}
seatsFPTP <- data.frame(Party=character(),
                      Percent=double(),
                      stringsAsFactors=FALSE)

for(i in unique(seatShare2017$FPTP)){
  y=(nrow(seatsFPTP)+1)
  seatsFPTP[y,"Party"]=i
  seatsFPTP[y,"Percent"]=sum(seatShare2017$Percent[seatShare2017$FPTP==i])*100
}


#LH Index
#FPTP
lhind=0
for(i in voteShare2017$Party){
  v=voteShare2017$Percent[voteShare2017$Party==i]
  if(i %in% seatsFPTP$Party){s=seatsFPTP$Percent[seatsFPTP$Party==i]}else{s=0}
  lhind=lhind+abs(v-s)
}
lhind=lhind/2
lhind

#CV
lhind=0
for(i in voteShare2017$Party){
  v=voteShare2017$Percent[voteShare2017$Party==i]
  if(i %in% seatsCV$Party){s=seatsCV$Percent[seatsCV$Party==i]}else{s=0}
  lhind=lhind+abs(v-s)
}
lhind=lhind/2
lhind

#Gallagher Index
#FPTP
gind=0
for(i in voteShare2017$Party){
  v=voteShare2017$Percent[voteShare2017$Party==i]
  if(i %in% seatsFPTP$Party){s=seatsFPTP$Percent[seatsFPTP$Party==i]}else{s=0}
  gind=gind+((v-s)^2)
}
gind=gind/2
gind=sqrt(gind)
gind

#CV
gind=0
for(i in voteShare2017$Party){
  v=voteShare2017$Percent[voteShare2017$Party==i]
  if(i %in% seatsCV$Party){s=seatsCV$Percent[seatsCV$Party==i]}else{s=0}
  gind=gind+((v-s)^2)
}
gind=gind/2
gind=sqrt(gind)
gind

#Calcullate the Gallagher Index for the alternative systems comparison
altSys=read.csv("altsystems.CSV")
gind=0
for(i in altSys$Party){
  v=altSys$Percent[altSys$Party==i]
  s=altSys$FPTP[altSys$Party==i]
  gind=gind+((v-s)^2)
}
gind=gind/2
gind=sqrt(gind)
gind

gind=0
for(i in altSys$Party){
  v=altSys$Percent[altSys$Party==i]
  s=altSys$CV[altSys$Party==i]
  gind=gind+((v-s)^2)
}
gind=gind/2
gind=sqrt(gind)
gind

gind=0
for(i in altSys$Party){
  v=altSys$Percent[altSys$Party==i]
  s=altSys$list[altSys$Party==i]
  gind=gind+((v-s)^2)
}
gind=gind/2
gind=sqrt(gind)
gind

gind=0
for(i in altSys$Party){
  v=altSys$Percent[altSys$Party==i]
  s=altSys$AV[altSys$Party==i]
  gind=gind+((v-s)^2)
}
gind=gind/2
gind=sqrt(gind)
gind

gind=0
for(i in altSys$Party){
  v=altSys$Percent[altSys$Party==i]
  s=altSys$STV[altSys$Party==i]
  gind=gind+((v-s)^2)
}
gind=gind/2
gind=sqrt(gind)
gind

#Sainte-Lague Index
#FPTP
slind=0
for(i in voteShare2017$Party){
  v=voteShare2017$Percent[voteShare2017$Party==i]
  if(i %in% seatsFPTP$Party){s=seatsFPTP$Percent[seatsFPTP$Party==i]}else{s=0}
  slind=slind+(((s-v)^2)/v)
}
slind

#CV
slind=0
for(i in voteShare2017$Party){
  v=voteShare2017$Percent[voteShare2017$Party==i]
  if(i %in% seatsCV$Party){s=seatsCV$Percent[seatsCV$Party==i]}else{s=0}
  slind=slind+(((s-v)^2)/v)
}
slind




#Analysis of marginal seats
data=read.csv("source/data/seatRank.csv",fileEncoding = "latin1")
data=data[data$Year == 2015,]
data=data[!is.na(data$CVVotes),]
x=data[abs(data$FPTPVotes-data$CVVotes)<(data$FPTPVotes*.1),]
