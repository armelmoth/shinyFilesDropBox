library(shiny)
library(shinyFilesDropBox)

shinyUI(pageWithSidebar(
    headerPanel(
        'Selections with shinyFilesDropBox',
        'shinyFilesDropBox example'
        ),
    sidebarPanel(
        img(src='logo.png', style="float: left; width: 120px; margin-right: 10px; margin-top: 5px"),
        tags$p('The following buttons will expose the users R dropbox
               directory.'),
        tags$p('As each button is used multiple times, the last location is
               remembered, as well as any other states. Each button has its own
               memory.'),
        tags$hr(),
        shinyDropFilesButton('file', 'File select', 'Please select a file', FALSE),
        tags$p(),
        tags$p('The file selection button allows the user to select one or
               several files and get their absolute position communicated back
               to the shiny server. In this example the button has been set to
               single-file mode and the default path has been set to the dropbox home path.'),
        tags$hr(),
        shinyDropDirButton('directory', 'Folder select', 'Please select a folder'),
        tags$p(),
        tags$p('This button lets the user navigate his dropbox file system and select a
               folder. The absolute path of the selected folder is then send
               back to the server. While only folders can be selected, it is
               possible to get an overview of the content beforehand. Lastly it is possible to
               create folders on the fly'),
        tags$hr(),
        shinyDropSaveButton('save', 'Save file', 'Save file as...'),
        tags$p(),
        tags$p('The last type of button is the save button which allows the user
               to navigate to a position in his dropbox file system and specify the name
               of a new file to be send back to the server. It is
               possible to specify a range of different filetypes that the user
               can choose between.')
        ),
    mainPanel(
        tags$h4('The output of a file selection'),
        tags$p(HTML('When one or several files are chosen the result is made 
               available to the shinyServer instance. In order for it to get the
               formatting expected of a filepath it must first be fed into
               <code>parseDropFilePaths()</code> after which the output matches the formatting of
               that returned by shinys own fileInput widget.')),
        verbatimTextOutput('filepath'),
        tags$hr(),
        tags$h4('The output of a folder selection'),
        tags$p(HTML('When a folder is selected the position of the folder is sent to 
               the server and can be formatted with <code>parseDropDirPath()</code> to reflect a
               standard path string as returned by e.g. <code>choose.dir()</code> on windows
               systems.')),
        verbatimTextOutput('directorypath'),
        tags$hr(),
        tags$h4('The output of a file save'),
        tags$p(HTML('When a file is "saved" the name, path and type is sent back to
               the server, where it can be formatted with <code>parseDropSavePath()</code>. The 
               format after parsing is very similar to a file choise, except
               size information is omitted (often the file doesn\'t exist yet)
               and type is now available (provided that filetype information has
               been send from the server).')),
        verbatimTextOutput('savefile')
        )
))
