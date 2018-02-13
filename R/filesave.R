library(jsonlite)
#' @include aaa.R
source("R/aaa.R")
#' @include filechoose.R
source("R/filechoose.R")
#' @include dirchoose.R
source("R/dirchoose.R")
#' 
NULL


#'@rdname shinyFilesDropBox-fileGetter 
#' 
fileGetterSave <- function(restrictions, filetypes,session,id,dtoken,roots=c(Home="")) {
    if (missing(filetypes)) filetypes <- NULL
    if (missing(restrictions)) restrictions <- NULL
    
    function(dir, root) {
        
        session$sendCustomMessage('shinySaveProgress',list(width = "24",id=id))
        currentRoots <- if(class(roots) == 'function') roots() else roots
        
        if (is.null(names(currentRoots))) stop('Roots must be a named vector or a function returning one')
        if (is.null(root)) root <- names(currentRoots)[1]
        
        session$sendCustomMessage('shinySaveProgress',list(width = "24",id=id))
        if(substr(dir,1,1)=="/"){
            dir = substr(dir,2,nchar(dir))
        }
        
        if(substr(dir,nchar(dir),nchar(dir))=="/"){
            dir = substr(dir,1,(nchar(dir)-1))
        }

        fulldir <- file.path(currentRoots[root], dir)
        if(substr(fulldir,nchar(fulldir),nchar(fulldir))=="/" & nchar(fulldir)>1){
            fulldir = substr(fulldir,1,(nchar(fulldir)-1))
        }
        session$sendCustomMessage('shinySaveProgress',list(width = "24",id=id))
        drop_dir = drop_dir(fulldir,dtoken = dtoken)
        
        writable = TRUE
        
        res = list()
        
        if(nrow(drop_dir)==0){
            session$sendCustomMessage('shinySaveProgress',list(width = "158",id=id))
            breadcrumps <- strsplit(dir, .Platform$file.sep)[[1]]
            fileInfo = data.frame(filename=character(0), extension=character(0), isdir=logical(0), size=integer(0), mtime=character(0), ctime=character(0), atime=character(0))
            res = list(
                files=fileInfo[, c('filename', 'extension', 'isdir', 'size', 'mtime', 'ctime', 'atime')],
                writable=writable,
                exist = (fulldir == "/") || drop_exists(fulldir,dtoken = dtoken),
                breadcrumps=I(c('', breadcrumps[breadcrumps != ''])),
                roots=I(names(currentRoots)),
                root=root
            )
            
        }
        
        
        if(nrow(drop_dir)>0){
            
            if (length(drop_dir$.tag[drop_dir$.tag=='file'])==0){
                session$sendCustomMessage('shinySaveProgress',list(width = "28",id=id))

                files = drop_dir$path_display
                files <- gsub(pattern='//*', '/', files, perl=TRUE)
                lengthDir = length(files)
                infoDir = lapply(as.vector(files), getInfo, dtoken)
                session$sendCustomMessage('shinySaveProgress',list(width = "28",id=id))

                fileInfo = data.frame(filename = basename(files))
                fileInfo$size = sapply(1:lengthDir, function (i){infoDir[i][[1]]$size})
                fileInfo$mtime= sapply(1:lengthDir, function (i){infoDir[i][[1]]$mtime})
                fileInfo$atime= sapply(1:lengthDir, function (i){infoDir[i][[1]]$atime})
                fileInfo$ctime= sapply(1:lengthDir, function (i){infoDir[i][[1]]$ctime})
                fileInfo$isdir = TRUE
                fileInfo$extension = tolower(file_ext(files))
                
                session$sendCustomMessage('shinySaveProgress',list(width = "102",id=id))
                rownames(fileInfo) <- NULL
                breadcrumps <- strsplit(dir, .Platform$file.sep)[[1]]
                res = list(
                    files=fileInfo[, c('filename', 'extension', 'isdir', 'size', 'mtime', 'ctime', 'atime')],
                    writable=writable,
                    exist = (fulldir == "/") || drop_exists(fulldir,dtoken=dtoken),
                    breadcrumps=I(c('', breadcrumps[breadcrumps != ''])),
                    roots=I(names(currentRoots)),
                    root=root
                )
            }
            if (length(drop_dir$.tag[drop_dir$.tag=='file'])>0){
                
                session$sendCustomMessage('shinySaveProgress',list(width = "28",id=id))
                files = drop_dir$path_display
                files <- gsub(pattern='//*', '/', files, perl=TRUE)
                if (!is.null(restrictions) && length(files) != 0) {
                    if (length(files) == 1) {
                        keep <- !any(sapply(restrictions, function(x) {grepl(x, files, fixed=T)}))
                    } else {
                        keep <- !apply(sapply(restrictions, function(x) {grepl(x, files, fixed=T)}), 1, any)
                    }
                    files <- files[keep]
                }
                
                session$sendCustomMessage('shinySaveProgress',list(width = "28",id=id))
                fileInfo = data.frame(filename = basename(files))
                fileInfo$extension <- tolower(file_ext(files))
                
                drop_dir$client_modified = chartr(":T", "--", drop_dir$client_modified)
                drop_dir$client_modified = gsub("Z","", drop_dir$client_modified)
                
                drop_dir$server_modified = chartr(":T", "--", drop_dir$server_modified)
                drop_dir$server_modified = gsub("Z","", drop_dir$server_modified)
                
                session$sendCustomMessage('shinySaveProgress',list(width = "28",id=id))
                
                fileInfo$mtime <- substr(drop_dir$client_modified,1,(nchar(drop_dir$client_modified)-3))
                fileInfo$ctime <- substr(drop_dir$server_modified,1,(nchar(drop_dir$server_modified)-3))
                fileInfo$atime <- substr(drop_dir$client_modified,1,(nchar(drop_dir$client_modified)-3))
                
                
                session$sendCustomMessage('shinySaveProgress',list(width = "50",id=id))
                
                fileInfo$isdir = drop_dir$.tag == 'folder'
                fileInfo$size = drop_dir$size
                
                lengthIsdir = length(fileInfo$filename[fileInfo$isdir])
                infoIsdir = lapply(as.vector(drop_dir$path_display[drop_dir$.tag == 'folder']), getInfo,dtoken)
                session$sendCustomMessage('shinySaveProgress',list(width = "12",id=id))
                
                fileInfo$size[fileInfo$isdir][1:lengthIsdir] =  sapply(1:lengthIsdir, function (i){infoIsdir[i][[1]]$size})
                fileInfo$mtime[fileInfo$isdir][1:lengthIsdir] =  sapply(1:lengthIsdir, function (i){infoIsdir[i][[1]]$mtime})
                fileInfo$atime[fileInfo$isdir][1:lengthIsdir] =  sapply(1:lengthIsdir, function (i){infoIsdir[i][[1]]$atime})
                fileInfo$ctime[fileInfo$isdir][1:lengthIsdir] =  sapply(1:lengthIsdir, function (i){infoIsdir[i][[1]]$ctime})
                
                session$sendCustomMessage('shinySaveProgress',list(width = "12",id=id))
                
                if (!is.null(filetypes)) {
                    matchedFiles <- tolower(fileInfo$extension) %in% tolower(filetypes) & fileInfo$extension != ''
                    fileInfo$isdir[matchedFiles] <- FALSE
                    fileInfo <- fileInfo[matchedFiles | fileInfo$isdir,]
                }
                rownames(fileInfo) <- NULL
                breadcrumps <- strsplit(dir, .Platform$file.sep)[[1]]
                res = list(
                    files=fileInfo[, c('filename', 'extension', 'isdir', 'size', 'mtime', 'ctime', 'atime')],
                    writable=writable,
                    exist = (fulldir == "/") || drop_exists(fulldir,dtoken=dtoken),
                    breadcrumps=I(c('', breadcrumps[breadcrumps != ''])),
                    roots=I(names(currentRoots)),
                    root=root
                )
            } 
             
        }
        session$sendCustomMessage('shinySaveProgress',list(width = "10",id=id))    
        return(res)   
    }
}

#' @rdname shinyFilesDropBox-observers
#' 
#' @examples
#' \dontrun{
#' # File selections
#' ui <- shinyUI(bootstrapPage(
#'     shinyDropSaveButton('save', 'Save', 'Save as...'),
#'     verbatimTextOutput('rawInputValue'),
#'     verbatimTextOutput('filepaths')
#' ))
#' server <- shinyServer(function(input, output, session) {
#'     shinyDropFileSave(input, 'save',session=session,dtoken=token)
#'     output$rawInputValue <- renderPrint({str(input$save)})
#'     output$filepaths <- renderPrint({parseDropSavePath(input$save)})
#' })
#' 
#' runApp(list(
#'     ui=ui,
#'     server=server
#' ))
#' }
#' 
#' @export
#' 
#' @importFrom shiny observe invalidateLater
#' 
shinyDropFileSave <- function(input, id, updateFreq=20000, session=getDropSession(),
                          defaultPath='', defaultRoot=NULL,dtoken, ...) {
    clientId = session$ns(id)
    fileGet <- do.call('fileGetterSave', list(session = session, id = clientId, dtoken=dtoken,...))
    dirCreate <- do.call('dirDropCreator', list(dtoken=dtoken,...))
    currentDir <- list()
    lastDirCreate <- NULL
    
    
    return(observe({
        dir <- input[[paste0(id, '-modal')]]
        createDir <- input[[paste0(id, '-newDir')]]
        if(!identical(createDir, lastDirCreate)) {
            dirCreate(createDir$name, createDir$path, createDir$root)
            #dir$path <- c(dir$path, createDir$name)
            lastDirCreate <<- createDir
        }
        if(is.null(dir) || is.na(dir)) {
            dir <- list(dir=defaultPath, root=defaultRoot)
        } else {
            dir <- list(dir=dir$path, root=dir$root)
        }
        dir$dir <- do.call(file.path, as.list(dir$dir))
        newDir <- do.call('fileGet', dir)
        session$sendCustomMessage('shinySaveProgressEnd',list(id=clientId)) 
        if(!identical(currentDir, newDir) && newDir$exist) {
            currentDir <<- newDir
            session$sendCustomMessage('shinySave', list(id=clientId, dir=newDir))
        }
        invalidateLater(updateFreq, session)
    }))
}
#' @rdname shinyFilesDropBox-buttons
#' 
#' @importFrom htmltools tagList singleton tags
#' 
#' @export
#' 
shinyDropSaveButton <- function(id, label, title, filetype, buttonType='default', class=NULL, icon=NULL) {
    if(missing(filetype)) filetype <- NA
    filetype <- formatDropFiletype(filetype)
    
    tagList(
        singleton(tags$head(
            tags$script(src='sF/shinyFiles.js'),
            tags$link(
                rel='stylesheet',
                type='text/css',
                href='sF/styles.css'
            ),
            tags$link(
                rel='stylesheet',
                type='text/css',
                href='sF/fileIcons.css'
            )
        )),
        tags$button(
            id=id,
            type='button',
            class=paste(c('shinySave btn', paste0('btn-', buttonType), class), collapse=' '),
            'data-title'=title,
            'data-filetype'=filetype,
            list(icon, label)
        )
    )
}
#' Formats the value of the filetype argument
#' 
#' This function is intended to format the filetype argument of 
#' \code{\link{shinyDropSaveButton}} into a json string representation, so that it
#' can be attached to the button.
#' 
#' @param filetype A named list of file extensions or NULL or NA
#' 
#' @return A string describing the input value in json format
#' 
#' @importFrom jsonlite toJSON
#' 
formatDropFiletype <- function(filetype) {
    if(!is.na(filetype) && !is.null(filetype)) {
        filetype <- lapply(1:length(filetype), function(i) {
            list(name=names(filetype)[i], ext=I(filetype[[i]]))
        })
    }
    toJSON(filetype)
}
#' @rdname shinyFilesDropBox-parsers
#' 
#' @export
#' 
parseDropSavePath <- function(selection,roots=c(Home="")) {

    if(is.null(selection)) return(data.frame(name=character(), type=character(),
                                             datapath=character(), stringsAsFactors = FALSE))
    
    currentRoots <- if(class(roots) == 'function') roots() else roots
    
    if (is.null(names(currentRoots))) stop('Roots must be a named vector or a function returning one')
    
    root <- currentRoots[selection$root]

    location <- do.call('file.path', as.list(selection$path))

    savefile <- file.path(root, location, selection$name)
    savefile <- gsub(pattern='//*', '/', savefile, perl=TRUE)

    type <- selection$type
    if (is.null(type)) {
        type <- ""
    }
    #data.frame(name=selection$name, type=type, datapath=savefile, stringsAsFactors = FALSE)
    data.frame(name=selection$name, datapath=savefile, stringsAsFactors = FALSE)
}
