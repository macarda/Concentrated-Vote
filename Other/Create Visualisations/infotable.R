source("voteseatshare.R")

infoTable <- data.frame(Party=character(),
                        Year=integer(),
                        VotePercent=character(),
                        SeatPercentFPTP=character(),
                        SeatPercentCV=character(),
                        ChangeinSeats=integer(),
                        stringsAsFactors=FALSE)


for (i in unique(seatShare$CV[seatShare$Year==2017])){
  y=(nrow(infoTable)+1)
  infoTable[y, "Party"]= i
  infoTable[y, "Year"]=2017
  infoTable[y, "VotePercent"]=paste(round(sum(voteShare$Percent[which(voteShare$Party==i & voteShare$Year==2017)])*100,10),"%",sep="")
  infoTable[y, "SeatPercentFPTP"]=paste(round(((length(which(seatShare$FPTP[seatShare$Year==2017]==i)))/650)*100,1),"%", sep="")
  infoTable[y, "SeatPercentCV"]=paste(round(((length(which(seatShare$CV[seatShare$Year==2017]==i)))/650)*100,1),"%", sep="")
  infoTable[y, "ChangeinSeats"]=(length(which(seatShare$CV[seatShare$Year==2017]==i)))-(length(which(seatShare$FPTP[seatShare$Year==2017]==i)))
}

for (i in unique(seatShare$CV[seatShare$Year==2015])){
  y=(nrow(infoTable)+1)
  infoTable[y, "Party"]= i
  infoTable[y, "Year"]=2015
  infoTable[y, "VotePercent"]=paste(round(sum(voteShare$Percent[which(voteShare$Party==i & voteShare$Year==2015)])*100,3),"%",sep="")
  infoTable[y, "SeatPercentFPTP"]=paste(round(((length(which(seatShare$FPTP[seatShare$Year==2015]==i)))/650)*100,1),"%", sep="")
  infoTable[y, "SeatPercentCV"]=paste(round(((length(which(seatShare$CV[seatShare$Year==2015]==i)))/650)*100,1),"%", sep="")
  infoTable[y, "ChangeinSeats"]=(length(which(seatShare$CV[seatShare$Year==2015]==i)))-(length(which(seatShare$FPTP[seatShare$Year==2015]==i)))
}

for (i in unique(seatShare$CV[seatShare$Year==2010])){
  y=(nrow(infoTable)+1)
  infoTable[y, "Party"]= i
  infoTable[y, "Year"]=2010
  infoTable[y, "VotePercent"]=paste(round(sum(voteShare$Percent[which(voteShare$Party==i & voteShare$Year==2010)])*100,3),"%",sep="")
  infoTable[y, "SeatPercentFPTP"]=paste(round(((length(which(seatShare$FPTP[seatShare$Year==2010]==i)))/650)*100,1),"%", sep="")
  infoTable[y, "SeatPercentCV"]=paste(round(((length(which(seatShare$CV[seatShare$Year==2010]==i)))/650)*100,1),"%", sep="")
  infoTable[y, "ChangeinSeats"]=(length(which(seatShare$CV[seatShare$Year==2010]==i)))-(length(which(seatShare$FPTP[seatShare$Year==2010]==i)))
}
