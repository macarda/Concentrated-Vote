output$box <- renderPlot({
  dataconstit=read.csv("source/data/constitPlots.csv")
  dataconstit<-dataconstit[dataconstit$Year == input$yearbox,]
 
  dataFPTP=subset(dataconstit,System=="FPTP")
  p1=ggplot(data=dataFPTP,aes(x=Party,y=Votes))+
    geom_boxplot(color=c("royalblue4", "firebrick4"), 
                 fill=c("royalblue1", "firebrick2")) + 
    labs(x="Party", y="Votes per Constituency", title=paste("Constituency Sizes of First Past the Post \nSeats Won", input$yearbox))+
    scale_y_continuous(limits = c(20000, 80000))
  
  dataCV=subset(dataconstit,System=="CV")
  p2=ggplot(data=dataCV,aes(x=Party,y=Votes))+
    geom_boxplot(color=c("royalblue4", "firebrick4"), 
                 fill=c("royalblue1", "firebrick2")) + 
    labs(x="Party", y="Votes per Constituency", title=paste("Constituency Sizes of Concetrated \nVote Seats Won", input$yearbox))+
    scale_y_continuous(limits = c(20000, 80000))
  
  #display together in a grid
  grid.arrange(p1,p2,ncol=2, nrow=1)
  
})