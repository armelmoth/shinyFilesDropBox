library(shiny)
library(shinyFilesDropBox)
library(rdrop2)
token = drop_auth(new_user = FALSE, cache=TRUE)
shinyServer(function(input, output, session) {

    shinyDropFileChoose(input, 'files',roots= roots,session = session, dtoken =token,filetypes=c('', 'xlsx'))
    output$filepaths <- renderPrint({parseDropFilePaths(input$files)})
    
    shinyDropFileSave(input, 'save',roots=roots,session=session,dtoken=token)
    output$directorypath <- renderPrint({parseDropSavePath(input$save)})
    
    shinyDropDirChoose(input, 'folder',session = session,dtoken=token)
    output$savefile <- renderPrint({parseDropDirPath(input$folder)})
})
