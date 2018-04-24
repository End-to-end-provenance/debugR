#four kinds of type change
BASE <- "Initialize the variable"
NO_CHANGE <- "no type change"
IN_CHANGE <- "intentional type change"
UN_CHANGE <- "unintentional type change"

#list of function for intentionally changing the type
typeC.functions <- c("as.character", "as.numeric", "as.factor", "as.array", "data.frame", "list", "toString", "c")

#functions <- unique(find.calls(.Object@parsed[[1]]))
find.calls <- function(obj) 
{
  if( is.call(obj) && ! .ddg.is.functiondecl(obj) )
  {
    function.name <- toString(obj[[1]])
    call.list <- unlist( sapply(obj, find.calls, USE.NAMES=FALSE) )
    
    return( c(function.name, call.list) )
  }
}


#print all the values
printValue <- function(namedlist, varName)
{
  for (n in 1:length(namedlist)){
    
    #procLine, dataNodeValue, dataNodeValType
    print(paste("At line number ",namedlist$procLine[n]))
    print(paste(varName, " value is ", namedlist$dataNodeValue[n]))
    print(paste("And the type here is: ", namedlist$dataNodeValType[n]))
  }
  
}

#print one of the values
printValueOne <- function(namedlist, varName,n)
{
  
  #procLine, dataNodeValue, dataNodeValType
  print(paste("At line number ",namedlist$procLine[n]))
  print(paste(varName, " value is ", namedlist$dataNodeValue[n]))
  print(paste("And the type here is: ", namedlist$dataNodeValType[n]))
  
  
}


#get the history of a variable to a certain depth
getVarSet <- function(varName, filepath, depth = -1) {
  #get a list of all the data nodes
  datalist <<- .ddg.get("ddg.data.nodes")
  edgelist <<- .ddg.get("ddg.edges")
  proclist <<- .ddg.get("ddg.proc.nodes")
  
  #get the script path from ddg
  #filepath <- .ddg.get("ddg.r.script.path")
  #filepath <- "C:/Users/Nan/Desktop/MHC/now/IndeCS/RDataTracker/R/sample.R"
  
  
  
  
  #procName
  #get a list of data node numbers
  dataNodeNum <- which(datalist$ddg.name == varName)
  #if the variable name is not found
  
  if(length(dataNodeNum) == 0)
  {
    print("Variable is not found")
    return(0);
    
  }
  #get a list of data node values
  dataNodeValue <- datalist$ddg.value[dataNodeNum]
  #get a list of data node valtypes
  dataNodeValType <- datalist$ddg.val.type[dataNodeNum]
  #print(dataNodeNum)
  #add d to the numbers
  dataNodeNumD <- mapply(paste, "d", dataNodeNum, sep="", USE.NAMES = FALSE)
  
  
  procNodeNumP <- c()
  #find the procedure nodes names with p in the edge list
  for (m in 1:length(dataNodeNumD)){
    tempList <- edgelist$ddg.from[edgelist$ddg.to == dataNodeNumD[m]]
    procNodeNumP <- c(procNodeNumP,tempList)
  }
  #procNodeNumP <- which(edgelist$ddg.to == dataNodeNumD)
  #procNodeNumP <- mapply(which, edgelist$ddg.to == dataNodeNumD, dataNodeNumD, USE.NAMES = FALSE)
  #print(procNodeNumP)
  #delete the p before the proc num list
  procNode = mapply(substring, procNodeNumP, 2, USE.NAMES = FALSE)
  procNodeNum = sapply(procNode, as.numeric, USE.NAMES = FALSE)
  
  
  #get the line number of the procedure nodes from nums
  procLine = proclist$ddg.startLine[procNodeNum]
  procnames = proclist$ddg.name[procNodeNum]
  #get the endline
  procEnd = proclist$ddg.endLine[procNodeNum]
  encoding = getOption("encoding")
  lines <- ""
  
  if (is.character(filepath)) {
    #print("This is outer if")
    
    if (identical(encoding, "unknown")) {
      #print("This is e")
      enc <- utils::localeToCharset()
      encoding <- enc[length(enc)]
    }
    else enc <- encoding
    
    #if (length(enc) > 1L) {
      #encoding <- NA
      #print("inside if")
      #for (e in enc) {
        
        #print(e)
    #if (is.na(e))
    # next
    #zz <- file(filepath, encoding = e)
        #get the line number
        #print(zz)
        #res <- readLines(zz, n=procEnd[length(procEnd)], warn = FALSE)
    #res <- readLines(zz, warn = FALSE)
    # close(zz)
    #  }
    # }
    
    #else {
      file <- file(filepath, "r", encoding = encoding)
      on.exit(close(file))
      lines <- readLines(file, warn = FALSE)
      
      on.exit()
      close(file)
      
      # }
  }
  #get value, line number and type into a dataframe
  histFrame <<- data.frame(procLine, dataNodeValue, dataNodeValType, procnames, stringsAsFactors = FALSE)
  #print("This is lines")
  #print(lines)
  
  #for type changes
  #get the first variable type 
  temp <- histFrame$dataNodeValType[1]
  splitType <- unlist(strsplit(temp, "[,]" ))
  typeTemp <- splitType[1]
  typeTemp <- unlist(strsplit(typeTemp, "[:]" ))
  #print("strsplited")
  #print(length(splitType))
  one <- typeTemp[2]
  #print("one")
  
  one <- unlist(strsplit(one, "" ))
  #print(one)
  newword <- ""
  for (g in 1:length(one)){
    #if(is.character(one[g]) | is.numeric(one[g]))
    letint <- length(intersect(letters, one[g]))
    #print(one[g])
    #print("length")
    #print(letint)
    mynum <- c(0:9)
    mynum <- sapply(mynum, as.character, USE.NAMES = FALSE)
    numint <- length(intersect(mynum,one[g]))
    
    
    if( letint!=0  | numint!=0 )
    {
      newword <- paste(newword,one[g],sep = "")
      #print(one[g])
    }
    
    
    
  }
    
  #print(newword)
  #print(length(one))
  #split by ", \"
  #print(temp)
  type.change <-histFrame
  
  #go through the frame to find the changes
  #BASE NO_CHANGE IN_CHANGE UN_CHANGE
  #set the type of the first line to be base
  type.change <- c(BASE)
  
  #if there is only one entry of the variable
  if(length(histFrame$dataNodeValType) == 1)
  {
    type.change <- c(type.change,NO_CHANGE)
  }
  
  else
  {
    
  
  
  for (n in 2:length(histFrame$dataNodeValType))
  {
    #if this one is different from the last one print notice type change
    current = histFrame$dataNodeValType[n]
    #print("curr")
    #print(current)
    #print(n)
    #print(temp)
    #print(current)
    #print(histFrame)
    if(temp != current)
    {
      #find the functions calls
      functions <- unique(find.calls(parse(text=histFrame$procnames[n])[[1]]))
      
      #get the intersections
      intersection <- intersect(functions, typeC.functions)
      #if the intersections is length zero, unIN change
      #print(2)
      if(0 == length(intersection)){
        type.change <- c(type.change,UN_CHANGE)
      }
      #else no intersection = intentional type change
      else
      {
        type.change <- c(type.change,IN_CHANGE)
      }
    }
    
    #when there is no type change
    else
    {
      
      type.change <- c(type.change,NO_CHANGE)
      
    }
    temp = current
    
  }
  
  }
  
  histFrame <- data.frame(histFrame, type.change, stringsAsFactors = FALSE)
  
  
  
  #get the length
  len <- length(dataNodeValue)
  #print("1")
  if(depth == -1)
  {
    #printValue(histFrame,varName)
    #print("return")
    #print(histFrame)
    return(histFrame) 
    
  }
  else if(depth == 0)
  {
    print("Depth is zero, the entire history is returned.")
    return(histFrame)
  }
  else if(depth>len)
  {
    print("Request more than what is in the record, the entire history is returned.")
    return(histFrame)
    
  }
  
  
  else
  {
    
    returnList <- histFrame[(len - depth + 1):len]
    #printValue(returnList,varName)
    
    return (returnList)
    
    
  }
  
  #strsplit(valT, ', "') 
  #[[1]]
  #split : only
  #elements in array are by , only
  
  
}


