output$propmap <- renderPlot({
  data=read.csv("source/data/propmapFPTPCV2010-2017.csv")
  colCV=paste("CV", input$yearmap, sep="")
  colFPTP=paste("FPTP", input$yearmap, sep="")
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

  
  pCV=ggplot(data) + 
    aes(long,lat,group=group,fill=data[[colCV]]) + 
    geom_polygon() +
    geom_path(color="black") +
    coord_equal()+
    ggtitle(paste("Concentrated Vote \nResults", input$yearmap))+
    scale_fill_manual(values = cols,name="Parties",
                      breaks=brks, labels=labs)+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  pFPTP=ggplot(data) + 
    aes(long,lat,group=group,fill=data[[colFPTP]]) + 
    geom_polygon() +
    geom_path(color="black") +
    coord_equal()+
    scale_fill_manual(values = cols,name="FPTP Parties",
                      breaks=brks, labels=labs)+
    ggtitle(paste("First Past the Post \nResults", input$yearmap))+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  
  legend_b <- get_legend(pCV + theme(legend.position="right"))
  grid.arrange(pFPTP,legend_b,pCV,ncol=3, nrow=1)
})

output$tableinfo <- renderDataTable({ 
  data=read.csv("source/data/infoTable.csv")
  data=data[data$Year == input$yearmap,]
  datatable(data[order(data$Party),!names(data) %in% c("Year")], colnames = c('Party', '% of Vote', 'FPTP % of Seats', 'CV% of Seats', '+/- Seats'),rownames = FALSE)
})