output$wastedvotes <- renderPlot({
  wasvotes=read.csv("source/data/wastedVotes.csv")
  ggplot(wasvotes, aes(x=as.character(Year), y=Wasted, fill=factor(System, levels=c("First Past the Post","Concentrated Vote" ))))+
    geom_bar(stat="identity", position=position_dodge())+
    ggtitle("Wasted Votes")+
    scale_y_continuous(name="Wasted Votes", labels = comma)+
    xlab("Year") +
    scale_fill_manual(values=c("violetred3", "dodgerblue1"),name="Electoral System")+
    theme(axis.text.x=element_text(angle=45,hjust=1))
})

output$tablewv <- renderDataTable({ 
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Year'),
        th(colspan = 2, 'Votes Wasted'),
        th(colspan = 2, '% Wasted')
      ),
      tr(
        lapply(rep(c('FPTP', 'CV'), 2), th)
      )
    )
  ))
  data=read.csv("source/data/wastedVotesTable.csv")
  colnames(data) = c('Year', 'Wasted Votes FPTP', '% Wasted FPTP', 'Wasted Votes CV', '% Wasted CV')
  datatable(data, container=sketch,rownames = FALSE, options = list(searching = FALSE, paging = FALSE))
})