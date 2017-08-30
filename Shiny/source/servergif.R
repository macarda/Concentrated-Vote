output$dynmap <- renderImage({
  filename <- normalizePath(file.path('./source/data',
                                      paste('UK_CV_', input$yeargif, '.gif', sep='')))
  list(src = filename,width = 530,
       height = 740,
       alt = paste("Image number", input$yeargif))
}, deleteFile = FALSE)  
