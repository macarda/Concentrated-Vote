output$share <- renderPlot({
  voteShare=read.csv("source/data/voteShare.csv")
  datvote=voteShare[voteShare$Year == input$yearshare,]
  seatShare=read.csv("source/data/seatShare.csv")
  dat<-seatShare[seatShare$Year == input$yearshare,]
  #percentage of seats
  dat["Percent"]=(1/nrow(dat))
  
  cols <- c(AP="hotpink3",APNI="hotpink3", Alliance="hotpink3",   Alliance..Lib.="orange3", Alliance..SDP.="blue4",BNP="steelblue4",  British.National="steelblue4",Con="dodgerblue", Conservative="dodgerblue", Ch.P="darkorchid2", Democratic.Unionist.Party="firebrick3",      DUP="firebrick3",  Democratic.Unionist="firebrick3",Ecology="green3", ED="tomato4", 
            Green="green3",Grn="green3",KHHC="violetred2",Kidderminster.Hospital.and.Health.Concern="violetred2",Ind="plum1", Independent="plum1", Ind1="plum1",  Ind.="plum1",  Ind131="plum1", Ind86="plum1", Independent76="plum1", Ind127="plum1", Independent.Labour="red1",Ind.Labour="red1",Lab="red2",  Labour="red2",            LDem="orange2", Liberal.Democrat="orange2" , LD="orange2", Lib="orange3", Liberal="orange3", National.Front="lightskyblue", Natural.Law="lightsteelblue2", 
            Official.Unionist.Party="coral",Oth="grey40",PC="palegreen", Plaid.Cymru="palegreen", People.s.Labour="gold1",Referendum="indianred4",Republican.Clubs="limegreen",Respect="forestgreen", Resp="forestgreen",SSP="orangered3",Scottish.Socialist="orangered3", SDLP="green4",Social.Democratic.and.Labour.Party="green4",Social.Democratic.and.Labour="green4",Sinn.Fein="limegreen",SF="limegreen",Sinn.Fein="limegreen", Scottish.National.Party="yellow",SNP="yellow", Scottish.National="yellow", Speaker="wheat3",The.Speaker="wheat3",Socialist.Alliance="red4",Social.Democratic="blue4",Socialist.Labour="grey35", Trade.Unionist.and.Socialist.Coalition="deeppink4",TUV="royalblue", 
            UCUNF="cadetblue2", UK.Independence.Party="darkviolet", UKIP="darkviolet", United.Kingdom.Independence="darkviolet",United.Kingdom.Unionist="mediumpurple4",   United.Ulster.Unionist="deepskyblue1", Ulster.Unionist.Party="coral", UU="coral",UUP="coral", Official.Unionist="coral", Ulster.Unionist="coral",Ulster.Popular.Unionist="blue1", Ver="darkorchid1", The.Workers...NI.="red4",WRP="tomato2" )
  
  brks <- c("AP","APNI", "Alliance",   "Alliance..Lib.", "Alliance..SDP.","BNP",  "British.National","Con", "Conservative", "Ch.P", "Democratic.Unionist.Party", "DUP",  "Democratic.Unionist","Ecology", "ED", 
            "Green","Grn","KHHC","Kidderminster.Hospital.and.Health.Concern","Ind", "Independent", "Ind1",  "Ind.",  "Ind131", "Ind86", "Independent76", "Ind127", "Independent.Labour","Ind.Labour","Lab",  "Labour",            "LDem", "Liberal.Democrat" , "LD", "Lib", "Liberal", "National.Front", "Natural.Law", 
            "Official.Unionist.Party","Oth","PC", "Plaid.Cymru", "People.s.Labour", "Referendum","Republican.Clubs","Respect","Resp","SSP","Scottish.Socialist", "SDLP","Social.Democratic.and.Labour.Party","Social.Democratic.and.Labour","Sinn.Fein","SF","Sinn.Fein", "Scottish.National.Party","SNP", "Scottish.National", "Speaker","The.Speaker","Socialist.Alliance","Social.Democratic","Socialist.Labour", "Trade.Unionist.and.Socialist.Coalition","TUV", 
            "UCUNF", "UK.Independence.Party", "UKIP", "United.Kingdom.Independence","United.Kingdom.Unionist",   "United.Ulster.Unionist", "Ulster.Unionist.Party", "UU","UUP", "Official.Unionist", "Ulster.Unionist","Ulster.Popular.Unionist", "Ver", "The.Workers...NI.","WRP")
  
  labs <- c("Alliance NI","Alliance NI","Alliance NI",   "Alliance (Liberal)", "Alliance (SDP)","British National",  "British National","Conservative", "Conservative", "Christian Party", "DUP", "DUP",  "DUP","Ecology", "English Democrat", 
            "Green","Green","Health Concern","Health Concern","Independent", "Independent", "Independent",  "Independent",  "Independent", "Independent", "Independent", "Independent", "Independent Labour","Independent Labour","Labour",  "Labour", "Liberal Democrat", "Liberal Democrat" , "Liberal Democrat", "Liberal", "Liberal", "National Front", "Natural Law", 
            "Official Unionist","Other","Plaid Cymru", "Plaid Cymru", "People's Labour", "Referendum","Republican Clubs","Respect","Respect","Scottish Socialist","Scottish Socialist", "SDLP","SDLP","SDLP","Sinn Fein","Sinn Fein","Sinn Fein", "SNP","SNP", "SNP", "Speaker","Speaker","Socialist Alliance","Social Democratic","Socialist Labour", "TUSC","TUV", 
            "UCUNF", "UKIP", "UKIP", "UKIP","UK Unionist",   "United Ulster Unionist", "UUP", "UUP","UUP", "Official Unionist", "Ulster Unionist","Ulster Popular Unionist", "Veritas", "The Workers NI","Workers Revolutionary")
  
  
  p1=ggplot(dat, aes(1, Percent, fill=FPTP)) + 
    geom_bar(stat="identity",position = position_stack(reverse = TRUE), width = 0.5)+
    coord_flip()+
    ggtitle(paste("First Past the Post Seat Share", input$yearshare))+
    scale_fill_manual(values=cols,breaks=brks, labels=labs)+
    theme(axis.text.x=element_text(hjust=1),axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())+ 
    scale_y_continuous(name="Percentage of Seats", labels = scales::percent)
  
  
  p2=ggplot(dat, aes(1, Percent, fill=CV)) + 
    geom_bar(stat="identity",position = position_stack(reverse = TRUE), width = 0.5)+
    coord_flip()+
    ggtitle(paste("Concentrated Vote Seat Share", input$yearshare))+
    scale_fill_manual(values=cols,breaks=brks, labels=labs)+
    theme(axis.text.x=element_text(hjust=1),axis.line=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())+ 
    scale_y_continuous(name="Percentage of Seats", labels = scales::percent)+ labs(fill="")
  
  p3=ggplot(datvote, aes(1, Percent, fill=Party)) + 
    geom_bar(stat="identity",position = position_stack(reverse = TRUE), width = 0.5)+
    coord_flip()+
    ggtitle(paste("General Election Vote Share", input$yearshare))+
    scale_fill_manual(values=cols,breaks=brks, labels=labs)+
    theme(axis.text.x=element_text(hjust=1),axis.line=element_blank(),axis.text.y=element_blank(),
          axis.ticks=element_blank(),axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())+ 
    scale_y_continuous(name="Percentage of Vote", labels = scales::percent)+ labs(fill="")
  
  legend_b <- get_legend(p3 + theme(legend.position="bottom"))
  grid.arrange(p3,p1,p2,legend_b, ncol=1, nrow=4)
  
})
output$pchange <- renderPlot({
  percentchang=read.csv("source/data/percentChange.csv")
  dat<-percentchang[percentchang$Year == input$yearshare,]

  cols <- c(AP="hotpink3",APNI="hotpink3", Alliance="hotpink3",   Alliance..Lib.="orange3", Alliance..SDP.="blue4",BNP="steelblue4",  British.National="steelblue4",Con="dodgerblue", Conservative="dodgerblue", Ch.P="darkorchid2", Democratic.Unionist.Party="firebrick3",      DUP="firebrick3",  Democratic.Unionist="firebrick3",Ecology="green3", ED="tomato4", 
            Green="green3",Grn="green3",KHHC="violetred2",Kidderminster.Hospital.and.Health.Concern="violetred2",Ind="plum1", Independent="plum1", Ind1="plum1",  Ind.="plum1",  Ind131="plum1", Ind86="plum1", Independent76="plum1", Ind127="plum1", Independent.Labour="red1",Ind.Labour="red1",Lab="red2",  Labour="red2",            LDem="orange2", Liberal.Democrat="orange2" , LD="orange2", Lib="orange3", Liberal="orange3", National.Front="lightskyblue", Natural.Law="lightsteelblue2", 
            Official.Unionist.Party="coral",Oth="grey40",PC="palegreen", Plaid.Cymru="palegreen", People.s.Labour="gold1",Referendum="indianred4",Republican.Clubs="limegreen",Respect="forestgreen", Resp="forestgreen",SSP="orangered3",Scottish.Socialist="orangered3", SDLP="green4",Social.Democratic.and.Labour.Party="green4",Social.Democratic.and.Labour="green4",Sinn.Fein="limegreen",SF="limegreen",Sinn.Fein="limegreen", Scottish.National.Party="yellow",SNP="yellow", Scottish.National="yellow", Speaker="wheat3",The.Speaker="wheat3",Socialist.Alliance="red4",Social.Democratic="blue4",Socialist.Labour="grey35", Trade.Unionist.and.Socialist.Coalition="deeppink4",TUV="royalblue", 
            UCUNF="cadetblue2", UK.Independence.Party="darkviolet", UKIP="darkviolet", United.Kingdom.Independence="darkviolet",United.Kingdom.Unionist="mediumpurple4",   United.Ulster.Unionist="deepskyblue1", Ulster.Unionist.Party="coral", UU="coral",UUP="coral", Official.Unionist="coral", Ulster.Unionist="coral",Ulster.Popular.Unionist="blue1", Ver="darkorchid1", The.Workers...NI.="red4",WRP="tomato2" )
  
  brks <- c("AP","APNI", "Alliance",   "Alliance..Lib.", "Alliance..SDP.","BNP",  "British.National","Con", "Conservative", "Ch.P", "Democratic.Unionist.Party", "DUP",  "Democratic.Unionist","Ecology", "ED", 
            "Green","Grn","KHHC","Kidderminster.Hospital.and.Health.Concern","Ind", "Independent", "Ind1",  "Ind.",  "Ind131", "Ind86", "Independent76", "Ind127", "Independent.Labour","Ind.Labour","Lab",  "Labour",            "LDem", "Liberal.Democrat" , "LD", "Lib", "Liberal", "National.Front", "Natural.Law", 
            "Official.Unionist.Party","Oth","PC", "Plaid.Cymru", "People.s.Labour", "Referendum","Republican.Clubs","Respect","Resp","SSP","Scottish.Socialist", "SDLP","Social.Democratic.and.Labour.Party","Social.Democratic.and.Labour","Sinn.Fein","SF","Sinn.Fein", "Scottish.National.Party","SNP", "Scottish.National", "Speaker","The.Speaker","Socialist.Alliance","Social.Democratic","Socialist.Labour", "Trade.Unionist.and.Socialist.Coalition","TUV", 
            "UCUNF", "UK.Independence.Party", "UKIP", "United.Kingdom.Independence","United.Kingdom.Unionist",   "United.Ulster.Unionist", "Ulster.Unionist.Party", "UU","UUP", "Official.Unionist", "Ulster.Unionist","Ulster.Popular.Unionist", "Ver", "The.Workers...NI.","WRP")
  
  labs <- c("Alliance NI","Alliance NI","Alliance NI",   "Alliance (Liberal)", "Alliance (SDP)","British National",  "British National","Conservative", "Conservative", "Christian Party", "DUP", "DUP",  "DUP","Ecology", "English Democrat", 
            "Green","Green","Health Concern","Health Concern","Independent", "Independent", "Independent",  "Independent",  "Independent", "Independent", "Independent", "Independent", "Independent Labour","Independent Labour","Labour",  "Labour", "Liberal Democrat", "Liberal Democrat" , "Liberal Democrat", "Liberal", "Liberal", "National Front", "Natural Law", 
            "Official Unionist","Other","Plaid Cymru", "Plaid Cymru", "People's Labour", "Referendum","Republican Clubs","Respect","Respect","Scottish Socialist","Scottish Socialist", "SDLP","SDLP","SDLP","Sinn Fein","Sinn Fein","Sinn Fein", "SNP","SNP", "SNP", "Speaker","Speaker","Socialist Alliance","Social Democratic","Socialist Labour", "TUSC","TUV", 
            "UCUNF", "UKIP", "UKIP", "UKIP","UK Unionist",   "United Ulster Unionist", "UUP", "UUP","UUP", "Official Unionist", "Ulster Unionist","Ulster Popular Unionist", "Veritas", "The Workers NI","Workers Revolutionary")
  
  
  ggplot(data=dat[which(dat$Change!=0),], aes(x=Party,y=Change,fill=Party)) +
    geom_bar(position="dodge",stat="identity") + 
    scale_fill_manual(values=cols,breaks=brks, labels=labs)+
    coord_flip() +
    ggtitle(paste("Percentage Change in Seats\nFirst Past the Post to Concentrated Vote",input$yearshare))+
    scale_y_continuous(name="Percentage Change", labels = scales::percent)+
    scale_x_discrete(breaks=brks, labels=labs)+guides(fill=FALSE)+
    geom_text(aes(label=paste(round((Change*100), 2), "%"), y = Change + 0.03), 
              position=position_dodge(width=0.9), size=4)
})
  