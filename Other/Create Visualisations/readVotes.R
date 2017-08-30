#2017
votes2017 <- read.csv("Data/2017.csv", stringsAsFactors=FALSE)
info <- votes2017[,1:6]
votes2017 <- votes2017[,-(1:6)]
rownames(votes2017) <- as.character(info$Constituency)
votes2017[is.na(votes2017)] <- 0
votes2017[,"FPTP"] <- colnames(votes2017)[unlist(apply(votes2017, 1, which.max))]
#2015
votes2015 <- read.csv("Data/2015.csv", stringsAsFactors=FALSE)
info <- votes2015[,1:6]
votes2015 <- votes2015[,-(1:6)]
rownames(votes2015) <- as.character(info$Constituency)
votes2015[is.na(votes2015)] <- 0
votes2015[,"FPTP"] <- colnames(votes2015)[unlist(apply(votes2015, 1, which.max))]
#2010
votes2010 <- read.csv("Data/2010.csv")
votes2010 <- votes2010[-nrow(votes2010),]
info <- votes2010[,1:6]
votes2010 <- votes2010[,-(1:6)]
rownames(votes2010) <- as.character(info$Constituency)
votes2010[is.na(votes2010)] <- 0
votes2010[,"FPTP"] <- colnames(votes2010)[unlist(apply(votes2010, 1, which.max))]
#2005
votes2005 <- read.csv("Data/2005.csv", stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")
info <- votes2005[,1:6]
votes2005 <- votes2005[,-(1:6)]
rownames(votes2005) <- as.character(info$Constituency)
votes2005[is.na(votes2005)] <- 0
votes2005[,"FPTP"] <- colnames(votes2005)[unlist(apply(votes2005, 1, which.max))]
#2001
votes2001 <- read.csv("Data/2001.csv", stringsAsFactors=FALSE)
info <- votes2001[,1:6]
rownames(votes2001) <- as.character(info$Constituency)
votes2001 <- votes2001[,-(1:6)]
rownames(votes2001) <- as.character(info$Constituency)
votes2001[is.na(votes2001)] <- 0
votes2001[,"FPTP"] <- colnames(votes2001)[unlist(apply(votes2001, 1, which.max))]
#1997
votes1997 <- read.csv("Data/1997.csv", stringsAsFactors=FALSE)
info <- votes1997[,1:6]
rownames(votes1997) <- as.character(info$Constituency)
votes1997 <- votes1997[,-(1:6)]
votes1997[is.na(votes1997)] <- 0
votes1997[,"FPTP"] <- colnames(votes1997)[unlist(apply(votes1997, 1, which.max))]
#1992
votes1992 <- read.csv("Data/1992.csv", stringsAsFactors=FALSE)
info <- votes1992[,1:6]
rownames(votes1992) <- as.character(info$Constituency)
votes1992 <- votes1992[,-(1:6)]
votes1992[is.na(votes1992)] <- 0
votes1992[,"FPTP"] <- colnames(votes1992)[unlist(apply(votes1992, 1, which.max))]
#1987
votes1987 <- read.csv("Data/1987.csv", stringsAsFactors=FALSE)
info <- votes1987[,1:6]
rownames(votes1987) <- as.character(info$Constituency)
votes1987 <- votes1987[,-(1:6)]
votes1987[is.na(votes1987)] <- 0
votes1987[,"FPTP"] <- colnames(votes1987)[unlist(apply(votes1987, 1, which.max))]
#1983
votes1983 <- read.csv("Data/1983.csv", stringsAsFactors=FALSE)
votes1983$Conservative=as.numeric(votes1983$Conservative)
votes1983$Ind=as.numeric(votes1983$Ind)
info <- votes1983[,1:6]
rownames(votes1983) <- as.character(info$Constituency)
votes1983 <- votes1983[,-(1:6)]
votes1983[is.na(votes1983)] <- 0
votes1983[,"FPTP"] <- colnames(votes1983)[unlist(apply(votes1983, 1, which.max))]
#1979
votes1979 <- read.csv("Data/1979.csv", stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")
info <- votes1979[,1:6]
rownames(votes1979) <- as.character(info$Constituency)
votes1979 <- votes1979[,-(1:6)]
votes1979[is.na(votes1979)] <- 0
votes1979[,"FPTP"] <- colnames(votes1979)[unlist(apply(votes1979, 1, which.max))]