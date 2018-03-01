#' @include aaa.R
#' @include filechoose.R
NULL

#' @importFrom rdrop2 drop_dir drop_exists
#' @rdname shinyFilesDropBox-fileGetter 
#'
fileGetterDir <- function(restrictions, filetypes,session,id,dtoken,roots=c(Home="")) {
    if (missing(filetypes)) filetypes <- NULL
    if (missing(restrictions)) restrictions <- NULL
    
    function(dir, root) {
        
        session$sendCustomMessage('shinyDirProgress',list(width = "14",id=id))
        currentRoots <- if(class(roots) == 'function') roots() else roots
        
        if (is.null(names(currentRoots))) stop('Roots must be a named vector or a function returning one')
        if (is.null(root)) root <- names(currentRoots)[1]
        
        session$sendCustomMessage('shinyDirProgress',list(width = "14",id=id))
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
        
        session$sendCustomMessage('shinyDirProgress',list(width = "14",id=id))
        drop_dir = data.frame()
        repeat{
            drop_dir = tryCatch(drop_dir(fulldir,dtoken = dtoken),error=function(e) { e })
            if(is.data.frame(drop_dir)){
                break
            }
        }
        
        writable = TRUE
        
        res = list()
        
        drop_exists = FALSE
        repeat{
            drop_exists = tryCatch(drop_exists(fulldir,dtoken = dtoken),error=function(e) { e })
            if(is.logical(drop_exists)){
                break
            }
        }
        
        if(nrow(drop_dir)==0){
            session$sendCustomMessage('shinyDirProgress',list(width = "107",id=id))
            breadcrumps <- strsplit(dir, .Platform$file.sep)[[1]]
            fileInfo = data.frame(filename=character(0), extension=character(0), isdir=logical(0), size=integer(0), mtime=character(0), ctime=character(0), atime=character(0))
            res = list(
                files=fileInfo[, c('filename', 'extension', 'isdir', 'size', 'mtime', 'ctime', 'atime')],
                writable=writable,
                exist = (fulldir == "/") || drop_exists,
                breadcrumps=I(c('', breadcrumps[breadcrumps != ''])),
                roots=I(names(currentRoots)),
                root=root
            )
            
        }
        
        
        if(nrow(drop_dir)>0){
            
            if (length(drop_dir$.tag[drop_dir$.tag=='file'])==0){
                session$sendCustomMessage('shinyDirProgress',list(width = "18",id=id))
                
                files = drop_dir$path_display
                files <- gsub(pattern='//*', '/', files, perl=TRUE)
                lengthDir = length(files)
                infoDir = lapply(as.vector(files), getInfo, dtoken)
                session$sendCustomMessage('shinyDirProgress',list(width = "18",id=id))
                
                fileInfo = data.frame(filename = basename(files))
                fileInfo$size = sapply(1:lengthDir, function (i){infoDir[i][[1]]$size})
                fileInfo$mtime= sapply(1:lengthDir, function (i){infoDir[i][[1]]$mtime})
                fileInfo$atime= sapply(1:lengthDir, function (i){infoDir[i][[1]]$atime})
                fileInfo$ctime= sapply(1:lengthDir, function (i){infoDir[i][[1]]$ctime})
                fileInfo$isdir = TRUE
                fileInfo$extension = tolower(file_ext(files))
                
                session$sendCustomMessage('shinyDirProgress',list(width = "71",id=id))
                rownames(fileInfo) <- NULL
                breadcrumps <- strsplit(dir, .Platform$file.sep)[[1]]
                
                res = list(
                    files=fileInfo[, c('filename', 'extension', 'isdir', 'size', 'mtime', 'ctime', 'atime')],
                    writable=writable,
                    exist = (fulldir == "/") || drop_exists,
                    breadcrumps=I(c('', breadcrumps[breadcrumps != ''])),
                    roots=I(names(currentRoots)),
                    root=root
                )
            }
            if (length(drop_dir$.tag[drop_dir$.tag=='file'])>0){
                
                session$sendCustomMessage('shinyDirProgress',list(width = "13",id=id))
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
                
                session$sendCustomMessage('shinyDirProgress',list(width = "15",id=id))
                fileInfo = data.frame(filename = basename(files))
                fileInfo$extension <- tolower(file_ext(files))
                
                drop_dir$client_modified = chartr(":T", "--", drop_dir$client_modified)
                drop_dir$client_modified = gsub("Z","", drop_dir$client_modified)
                
                drop_dir$server_modified = chartr(":T", "--", drop_dir$server_modified)
                drop_dir$server_modified = gsub("Z","", drop_dir$server_modified)
                
                session$sendCustomMessage('shinyDirProgress',list(width = "15",id=id))
                
                fileInfo$mtime <- substr(drop_dir$client_modified,1,(nchar(drop_dir$client_modified)-3))
                fileInfo$ctime <- substr(drop_dir$server_modified,1,(nchar(drop_dir$server_modified)-3))
                fileInfo$atime <- substr(drop_dir$client_modified,1,(nchar(drop_dir$client_modified)-3))
                
                
                session$sendCustomMessage('shinyDirProgress',list(width = "40",id=id))
                
                fileInfo$isdir = drop_dir$.tag == 'folder'
                fileInfo$size = drop_dir$size
                
                lengthIsdir = length(fileInfo$filename[fileInfo$isdir])
                
                session$sendCustomMessage('shinyDirProgress',list(width = "12",id=id))
                if(lengthIsdir > 0){
                    infoIsdir = lapply(as.vector(drop_dir$path_display[drop_dir$.tag == 'folder']), getInfo,dtoken)
                    fileInfo$size[fileInfo$isdir][1:lengthIsdir] =  sapply(1:lengthIsdir, function (i){infoIsdir[i][[1]]$size})
                    fileInfo$mtime[fileInfo$isdir][1:lengthIsdir] =  sapply(1:lengthIsdir, function (i){infoIsdir[i][[1]]$mtime})
                    fileInfo$atime[fileInfo$isdir][1:lengthIsdir] =  sapply(1:lengthIsdir, function (i){infoIsdir[i][[1]]$atime})
                    fileInfo$ctime[fileInfo$isdir][1:lengthIsdir] =  sapply(1:lengthIsdir, function (i){infoIsdir[i][[1]]$ctime})
                }

                session$sendCustomMessage('shinyDirProgress',list(width = "12",id=id))
                
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
                    exist = (fulldir == "/") || drop_exists,
                    breadcrumps=I(c('', breadcrumps[breadcrumps != ''])),
                    roots=I(names(currentRoots)),
                    root=root
                )
            } 
            
        }
        session$sendCustomMessage('shinyDirProgress',list(width = "10",id=id))    
        return(res)   
    }
}

#' Traverse and update a tree representing the file system
#' 
#' This function takes a tree representing a part of the file system and updates
#' it to reflect the current state of the file system as well as the settings 
#' for each node. Children (contained folders) are recursed into if the parents
#' expanded element is set to TRUE, no matter if children are currently present.
#' 
#' @param tree A list representing the tree structure of the file system to 
#' traverse. Each element should at least contain the elements 'name' and 
#' 'expanded'. The elements 'empty' and 'children' will be created or updates if
#' they exist.
#' 
#' @param root A string with the location of the root folder for the tree
#' 
#' @param restrictions A vector of directories within the root that should be
#' filtered out of the results
#' 
#' @param dtoken The Dropbox token generated by drop_auth (package rdrop2). 
#' 
#' @return A list of the same format as 'tree', but with updated values to 
#' reflect the current file system state.
#' 
#' @importFrom rdrop2 drop_dir drop_exists
#' 
traverseDropDirs <- function(tree, root, restrictions,dtoken) {
    
    
    if(substr(tree$name,1,1)=="/"){
      tree$name = substr(tree$name,2,nchar(tree$name))
    }
    
    if(substr(tree$name,nchar(tree$name),nchar(tree$name))=="/"){
      tree$name = substr(tree$name,1,(nchar(tree$name)-1))
    }
    
    location <- file.path(root, tree$name)
    location = gsub(pattern='//*', '/', location, perl=TRUE)
    if(substr(location,nchar(location),nchar(location))=="/" & nchar(location)>1){
        location = substr(location,1,(nchar(location)-1))
    }
    
    drop_exists = FALSE
    repeat{
        drop_exists = tryCatch(drop_exists(location,dtoken=dtoken),error=function(e) { e })
        if(is.logical(drop_exists)){
            break
        }
    }
    
    if(!((location == "/") || drop_exists)){
        return(NULL)
    }  # A revoir
    
    
    drop_dir = data.frame()
    repeat{
        drop_dir = tryCatch(drop_dir(location,dtoken = dtoken),error=function(e) { e })
        if(is.data.frame(drop_dir)){
            break
        }
    }
    
    folders = character(0)
    if(nrow(drop_dir)>0){
        files =  drop_dir$path_display
        files <- gsub(pattern='//*', '/', files, perl=TRUE)
        if (!is.null(restrictions) && length(files) != 0) {
            if (length(files) == 1) {
                keep <- !any(sapply(restrictions, function(x) {grepl(x, files, fixed=T)}))
            } else {
                keep <- !apply(sapply(restrictions, function(x) {grepl(x, files, fixed=T)}), 1, any)
            }
            files <- files[keep]
        }
        fileInfo = data.frame(filename = basename(files))
        fileInfo$isdir = drop_dir$.tag == 'folder'

        folders <- basename(files)[fileInfo$isdir]
    }
    
    if(length(folders) == 0) {
        tree$empty <- TRUE
        tree$children <- list()
        tree$expanded <- FALSE
    }
    if(length(folders)>0) {
        tree$empty <- FALSE
        
        if(tree$expanded) {
            children <- updateDropChildren(tree$children, folders)
            tree$children <- lapply(children, traverseDropDirs, root=location, restrictions=restrictions,dtoken=dtoken)
        }
        else {
            tree$children <- list()
        }
    }
    
    tree
    
}

#' Update the children element to reflect current state
#' 
#' This function create new entries for new folders and remove entries for no
#' longer existing folders, while keeping the state of transient folders in the
#' children element of the tree structure. The function does not recurse into
#' the folders, but merely creates a shell that traverseDropDirs can take as input.
#' 
#' @param oldChildren A list of children folders from the parent$children 
#' element of tree in \code{\link{traverseDropDirs}}
#' 
#' @param currentChildren A vector of names of the folders that are currently
#' present in the parent of oldChildren
#' 
#' @return An updated list equal in format to oldChildren
#' 
updateDropChildren <- function(oldChildren, currentChildren) {
    oldNames <- sapply(oldChildren, `[[`, 'name')
    newChildren <- currentChildren[!currentChildren %in% oldNames]
    children <- oldChildren[oldNames %in% currentChildren]
    children <- append(children, lapply(newChildren, function(x) {
        list(name=x, expanded=FALSE, children=list())
    }))
    childrenNames <- sapply(children, `[[`, 'name')
    children[order(childrenNames)]
}

#' Create a function that updates a folder tree based on the given restrictions
#' 
#' This functions returns a new function that will handle updating the folder 
#' tree. It is the folder equivalent of \code{\link{fileGetter}} but functions
#' slightly different as it needs to handle expanded branches of the folder 
#' hierarchy rather than just the content of a single directory at a time. The
#' returned function takes a representation of a folder hierarchy along with the
#' root to where it belongs and updates the tree to correspond with the current
#' state of the file system, without altering expansions etc.
#' 
#' @param roots A named vector of absolute filepaths or a function returning a 
#' named vector of absolute filepaths (the latter is useful if the volumes
#' should adapt to changes in the filesystem).
#' 
#' @param restrictions A vector of directories within the root that should be
#' filtered out of the results
#' 
#' @param filetypes Currently unused
#' 
#' @param dtoken The Dropbox token generated by drop_auth (package rdrop2). 
#' 
#' @return A function taking a list representation of a folder hierarchy along
#' with the name of the root where it starts. See \code{\link{traverseDropDirs}} for
#' a description of the format for the list representation.
#' 
dirDropGetter <- function(restrictions, filetypes,session,id,dtoken,roots=c(Home="")) {
    if (missing(filetypes)) filetypes <- NULL
    if (missing(restrictions)) restrictions <- NULL
    
    function(tree, root,toFiles=TRUE) {
        width = c(11,70,0)
        if(!toFiles){
            width = width +53
        }
        session$sendCustomMessage('shinyDirProgress',list(width = width[1],id=id))
        currentRoots <- if(class(roots) == 'function') roots() else roots
        
        if (is.null(names(currentRoots))) stop('Roots must be a named vector or a function returning one')
        if (is.null(root)) root <- names(currentRoots)[1]
        session$sendCustomMessage('shinyDirProgress',list(width = width[2],id=id))
        tree <- traverseDropDirs(tree, currentRoots[root], restrictions,dtoken=dtoken)
        session$sendCustomMessage('shinyDirProgress',list(width = width[3],id=id))
        list(
            tree=tree,
            rootNames=I(names(currentRoots)),
            selectedRoot=root
        )
    }
}

#' Create a function that creates a new directory
#' 
#' This function returns a function that can be used to create new directories
#' based on the information returned from the client. The returned function 
#' takes the name, path and root of the directory to create and parses it into
#' a platform compliant format and then uses dir.create to create it. The 
#' returned function returns TRUE if the directory was created successfully and
#' FALSE otherwise.
#' 
#' @param roots A named vector of absolute filepaths or a function returning a 
#' named vector of absolute filepaths (the latter is useful if the volumes
#' should adapt to changes in the filesystem).
#' 
#' @param dtoken The Dropbox token generated by drop_auth (package rdrop2). 
#' 
#' @param ... Currently unused
#' 
#' @return A function that creates directories based on the information returned
#' by the client.
#' 
#' @importFrom rdrop2 drop_create
#' 
dirDropCreator <- function(dtoken,roots=c(Home=""),...) {
    function(name, path, root) {
        
        if(substr(name,1,1)=="/"){
            name = substr(name,2,nchar(name))
        }

        if(substr(name,nchar(name),nchar(name))=="/"){
            name = substr(name,1,(nchar(name)-1))
        }
        currentRoots <- if(class(roots) == 'function') roots() else roots
        
        if (is.null(names(currentRoots))) stop('Roots must be a named vector or a function returning one')

        location <- do.call('file.path', as.list(path))
        location <- file.path(currentRoots[root], location, name)
        location = gsub(pattern='//*', '/', location, perl=TRUE)
        
        #dir.create(location)
        
        drop_create = list()
        repeat{
            drop_create = tryCatch(drop_create(location, autorename = T,dtoken = dtoken),error=function(e) { e })
            if(is.list(drop_create)){
                break
            }
        }
    }
}

#' @rdname shinyFilesDropBox-observers
#' 
#' @examples
#' \dontrun{
#' # Folder selections
#' token = drop_auth(new_user = FALSE, cache=TRUE)
#' ui <- shinyUI(bootstrapPage(
#' shinyDropDirButton('folder', 'Folder select', 'Please select a folder', FALSE),
#' verbatimTextOutput('rawInputValue'),
#' verbatimTextOutput('filepaths')
#' ))
#' server <- shinyServer(function(input, output,session) {
#'
#' shinyDropDirChoose(input, 'folder', session = session,dtoken=token)
#' output$rawInputValue <- renderPrint({str(input$folder)})
#' output$filepaths <- renderPrint({parseDropDirPath(input$folder)})
#' })
# 
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
shinyDropDirChoose <- function(input, id, updateFreq=100000, session=getDropSession(),
                           defaultPath='', defaultRoot=NULL,dtoken,...) {
    clientId = session$ns(id)
    dirGet <- do.call('dirDropGetter', list(session=session,id=clientId,dtoken=dtoken,...))
    fileGet <- do.call('fileGetterDir', list(session=session,id=clientId,dtoken=dtoken,...))
    dirCreate <- do.call('dirDropCreator', list(dtoken=dtoken,...))
    currentDir <- list()
    currentFiles <- NULL
    lastDirCreate <- NULL
    
    
    return(observe({
        tree <- input[[paste0(id, '-modal')]]
        createDir <- input[[paste0(id, '-newDir')]]
        
        if(!identical(createDir, lastDirCreate)) {
            dirCreate(createDir$name, createDir$path, createDir$root)
            lastDirCreate <<- createDir
        }
        if(is.null(tree) || is.na(tree)) {
            dir <- list(tree=list(name=defaultPath, expanded=TRUE), root=defaultRoot)
            files <- list(dir=NA, root=tree$selectedRoot)
        } else {
            dir <- list(tree=tree$tree, root=tree$selectedRoot)
            files <- list(dir=unlist(tree$contentPath), root=tree$selectedRoot)
        }
        newDir <- do.call('dirGet', list(tree = dir$tree,root=dir$root,toFiles=!(is.null(files$dir) || is.na(files$dir))))
   
        if(is.null(files$dir) || is.na(files$dir)) {
            newDir$content <- NA
            newDir$contentPath <- NA
            newDir$writable <- FALSE
        } else {
            newDir$contentPath <- as.list(files$dir)
            files$dir <- do.call(file.path, as.list(files$dir))
            content <- do.call('fileGet', files)
            newDir$content <- content$files[, c('filename', 'extension', 'isdir', 'size'), drop=FALSE]
            newDir$writable <- content$writable
        }
        session$sendCustomMessage('shinyDirProgressEnd',list(id=clientId)) 
        if(!identical(currentDir, newDir)) {
            currentDir <<- newDir
            session$sendCustomMessage('shinyDirectories', list(id=clientId, dir=newDir))
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
shinyDropDirButton <- function(id, label, title, buttonType='default', class=NULL) {
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
            class=paste(c('shinyDirectories btn', paste0('btn-', buttonType), class), collapse=' '),
            'data-title'=title,
            as.character(label)
        )
    )
}

#' @rdname shinyFilesDropBox-parsers
#' 
#' @export
#' 
parseDropDirPath <- function(selection,roots=c(Home="")) {
    currentRoots <- if(class(roots) == 'function') roots() else roots
    
    if (is.null(names(currentRoots))) stop('Roots must be a named vector or a function returning one')
    
    root <- currentRoots[selection$root]
    
    location <- do.call('file.path', as.list(selection$path))
    gsub(pattern='//*', '/', file.path(root, location), perl=TRUE)
}
