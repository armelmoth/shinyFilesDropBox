library(shiny)
library(shinyFilesDropBox)
library(rdrop2)
token = drop_auth(new_user = TRUE, cache=FALSE)
shinyServer(function(input, output, session) {

    shinyDropFileChoose(input, 'file',session = session, dtoken =token)
    output$filepath <- renderPrint({parseDropFilePaths(input$file)})
    
    shinyDropFileSave(input, 'save',session=session,dtoken=token)
    output$savefile <- renderPrint({parseDropSavePath(input$save)})
    
    shinyDropDirChoose(input, 'directory',session = session,dtoken=token)
    output$directorypath <- renderPrint({parseDropDirPath(input$directory)})
})
