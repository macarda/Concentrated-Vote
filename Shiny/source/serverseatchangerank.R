output$rank <- renderPlot({
  switches=read.csv("source/data/seatRank.csv")
  dat<-switches[switches$Year == input$yearrank,]
  dat$Rank=as.character(dat$Rank)
  ggplot(dat, aes(Rank)) +
    geom_histogram(stat="count",fill="maroon3", colour="black")+ 
    ggtitle(paste("Rank of MP elected by the Concentrated Vote \nElectoral System", input$yearrank))+
    scale_y_continuous(name="Number of MPs", labels = comma)+
    theme_light(base_size = 14)
})

output$tablerank <- renderDataTable({ 
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Constituency'),
        th(colspan = 2, 'First Past the Post'),
        th(colspan = 2, 'Concentrated Vote'),
        th(rowspan = 2, 'Rank')
      ),
      tr(
        lapply(rep(c('Winning Party','Votes Recieved'), 2), th)
      )
    )
  ))
  data=read.csv("source/data/ranktable.csv",fileEncoding = "latin1")
  data=data[data$Year == input$yearrank,]
  data=data[!is.na(data$CVVotes),]
  colnames(data) <- c("X", "Constituency", "FPTP", "Votes", "CV", "Votes", "Year", "Rank")
  datatable(data[order(data$Constit),!names(data) %in% c("X", "Year")], container=sketch,rownames = FALSE)
})