source("R/aaa.R")
source("R/filechoose.R")
source("R/dirchoose.R")
source("R/filesave.R")

# library(shiny)
# library(shinyFilesDropBox)
# library(rdrop2)
token = drop_auth(new_user = FALSE, cache=FALSE)

ui <- shinyUI(bootstrapPage(
    shinyDropFilesButton('files', 'File select', 'Please select a file', FALSE),
    verbatimTextOutput('rawInputValue1'),
    verbatimTextOutput('filepaths1'),

    shinyDropSaveButton('save', 'Save', 'Save as...',filetype=c('xlsx')),
    verbatimTextOutput('rawInputValue2'),
    verbatimTextOutput('filepaths2'),

    shinyDropDirButton('folder', 'Folder select', 'Please select a folder', FALSE),
    verbatimTextOutput('rawInputValue3'),
    verbatimTextOutput('filepaths3')
))
server <- shinyServer(function(input, output,session) {
    roots = c(Home ="")
    shinyDropFileChoose(input, 'files',roots= roots,session = session, dtoken =token,filetypes=c('', 'xlsx'))
    output$rawInputValue1 <- renderPrint({str(input$files)})
    output$filepaths1 <- renderPrint({parseDropFilePaths(input$files)})
    
    shinyDropFileSave(input, 'save',roots=roots,session=session,dtoken=token)
    output$rawInputValue2 <- renderPrint({str(input$save)})
    output$filepaths2 <- renderPrint({parseDropSavePath(input$save)})
    
    shinyDropDirChoose(input, 'folder',session = session,dtoken=token)
    output$rawInputValue3 <- renderPrint({str(input$folder)})
    output$filepaths3 <- renderPrint({parseDropDirPath(input$folder)})
})

runApp(list(
    ui=ui,
    server=server
))



