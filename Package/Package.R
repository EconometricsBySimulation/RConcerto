# RConcerto Package
# concerto <- list(db=list(name="concerto4_13"), sessionID=321, userIP="34.2.3.4.564")
# concerto.table.query <- function(sql) print(paste0("sql=",sql))
phi <- (5/2)^.5 # The Golden Ratio
minmax <- function(x) c(min(x),max(x))
p <- function(...) {
  ret <- paste0(...)
  class(ret) <- 'html'
  ret
}

pc <- function(...) p(..., collapse="")
recode <- function(x, m, r) {
  y <- x
  for (i in 1:length(m)) y[x==m[i]] <- r[i]
  y
}

pn <- function(...) p(..., collapse="\n")
recode <- function(x, m, r) {
  y <- x
  for (i in 1:length(m)) y[x==m[i]] <- r[i]
  y
}

pf <- function(x, ...) p(sprintf(x,...))


# Concact HTML objects
# p("hello")+p(" Bob")="Hello Bob"
`+.html` <- function(...) p(...)
# p("hello")^2="hellohello"
`^.html` <- function(e1, e2) pc(rep(e1, e2))
# Concact HTML objects collapsing vectors before combining
`*.html` <- function(e1, e2) pc(e1)+pc(e2)

# Concact a vector automatically naming values when names are not identified
cc <- function(...) {
  CALL <- match.call(expand.dots = FALSE)$...
  no.name <- names(CALL)==""
  names(CALL)[no.name]=CALL[no.name]
  for (i in 1:length(no.name)) if (no.name[i]==T) try(CALL[i] <- get(toString(CALL[i])))
  CALL
};

# This function creates a named list.
nl <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)),deparse)[-1]
  if (is.null(nm <- names(L))) nm <- snm
  if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
  setNames(L,nm)
};

# HTML Objects
# HTML drop down menu object that return selections with the name sel_field by default.
# Options
html.selectfield <- function(options, rows=4, name="sel_field") {
  return <- sprintf("<select name=\"%s\" size=\"%i\">", name, rows)
  for (i in 1:length(options)) return=p(return, "<option value=", i,">",options[i],"</option>")
  p(return, "</select>")
};

# Highlight the text
html.highlight <- function(text) paste0("<SPAN style=\"BACKGROUND-COLOR: #ffff00\">", text,"</SPAN>")
# Create a html button
html.button <- function(name="btn_name", value="Submit", text="")
  p(text,"<input name=\"", name, "\" type=\"submit\" value=\"",value,"\" />")
# Insert an html image
html.image <- function(targ, alt="", width="", height="", align="center") {
  # Modify the the width and height strings
  if ((width!="")&(height=="")) stop("Warning: Height must be specified if width is specified!")
  if (height!="") height <- sprintf('height: %spx;',height)
  if (width!="") width <- sprintf('width: %spx;',width)
  # Use sprintf to piece together the html command
  sprintf('<p style="text-align: %s;"><img alt="%s" src="%s" style="%s %s" />',align, alt, targ, width, height)
}
# HTML tags
tag <- list()
tag$center    <- function(...) p("<center>",list(...),"</center>")
tag$style     <- function(...) p('<style type="text/css">',list(...),"</style>")
tag$h1        <- function(...) p("<h1>",list(...),"</h1>")
tag$h2        <- function(...) p("<h2>",list(...),"</h2>")
tag$h3        <- function(...) p("<h3>",list(...),"</h3>")
tag$h4        <- function(...) p("<h4>",list(...),"</h4>")
tag$h5        <- function(...) p("<h5>",list(...),"</h5>")
tag$h6        <- function(...) p("<h6>",list(...),"</h6>")
tag$p         <- function(..., align='') {
  if (align!='')  pf('<p align="%s">%s</p>', align, list(...))
  if (align=='')  pf('<p>%s</p>', list(...))
}
tag$head      <- function(...) p("<head>", ..., "</head>")
tag$css       <- function(...) p('<link rel="stylesheet" href="', list(...), '">')
tag$script    <- function(..., scr='') p('<script src="%s">%s</script>',scr,...)
tag$comment   <- function(...) p('<!-- ', ..., ' -->')
tag$container <- function(...) p('<div class="container">', ..., '</div>')
tag$jumbotron <- function(...) p('<div class="jumbotron">', ..., '</div>')
tag$none      <- function(...) p(...)
tag$header    <- function(...) p('<div class="page-header">', ... , '</div>')
tag$a         <- function(x, href) pf('<a href="%s">%s</a>', href, x)
tag$row       <- function(...) p('<div class="row">\n', pn(...) ,'</div><!--End Row-->\n')
tag$col       <- function(..., size=4) pf('<div class="col-sm-%s">\n%s</div><!--End Col-->\n', size, pn(...)) 
tag$li <- function(..., active=FALSE) {
  if (!active) return(pf('<li>%s</li>', list(...)))
  pf('<li class="active">%s</li>', list(...))
}

tag$list      <- function(tags, ..., verbose=FALSE) {
  # Set verbose=TRUE to check if individual tags are having issues.
  ret <- p(...)
  for (i in 1:length(tags)) {
    ret <- tag[[tags[i]]](ret)
    if (verbose) print(ret)
  }
  ret
}

tag$list(c('h1', 'li', 'head', 'comment'), 'test')

# CSS Objects
css.get <- function(x) {
  getURL(
    pf("https://raw.githubusercontent.com/EconometricsBySimulation/RConcerto/master/CSS/%s.css",
       x),
         followlocation = TRUE,
         cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
}

# Twitter Bootstrap Objects
BS <- list()

BS$source <- function(theme='default') {
  min.css <- "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
  theme.min.css <- "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css"
  css2 <- css3 <- ''
  jquery.min.js <- "https://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
  min.js <- "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
  
  if (theme!='default') {
    css2 <- pf("http://bootswatch.com/%s/bootstrap.css", theme)
    css3 <- pf("http://bootswatch.com/%s/bootswatch.css", theme)
  }
  BS$SS <<- nl(min.css, theme.min.css, css2, css3, jquery.min.js, min.js)
}
BS$source()

BS$get <- function(x)
  getURL(p("https://raw.githubusercontent.com/EconometricsBySimulation/RConcerto/master/bootstrap/",
   x,".htm"), followlocation = TRUE,
   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

BS$head <- function() tag$head(pc(tag$css(BS$SS$min.css, BS$SS$theme.min.css, BS$SS$css2, BS$SS$css3)))+'\n\n'
BS$tail <- function() '\n\n'+tag$script(scr=c(BS$SS$jquery.min.js,BS$SS$min.js))

# Container jumbotron quick combo.
BS$cj <- function(..., title="", tags=c('center','jumbotron','container')) {
  if (title!="") return(tag$list(tags, tag$h2(title)*tag$p(...)))
  return(tag$list(tags, pc(tag$p(...))))
  }

BS$header <- function(...) tag$header(tag$h1(...))

BS$button <- 
# Defines a set of buttons which returns the name, displays the value
# Type is its color (primary, success, info, warning, danger, default)
# size it's size ('btn-lg', '', 'btn-sm', 'btn-xs')
  function(name="default_button", value='Click', type=NULL, size=NULL, accesskey='', keyhint=TRUE) {
    if (length(type)==0) type <- disp$btn.col
    if (length(size)==0) size <- disp$btn.size
    
    if (length(accesskey)>0) {
      value[accesskey!=''] <- p(value, ' (', accesskey[accesskey!=''], ')')
      accesskey[accesskey!=''] <- pf(' accesskey="%s"', accesskey[accesskey!=''])
    }
    pf('\n<button type="button" name="%s" class="btn %s btn-%s"%s>%s</button>\n',
       name, size, type, accesskey, value)
  }

# Create a textual input object. Left and right addons will cause the input box to span entire width
# as well as create textual clues.
BS$Tinput <- function(name, placeholder, type='text', left.addon=NULL, right.addon=NULL){
  left <- right <- p('')
  if (length(left.addon)>0) 
    left <- pf('<span class="input-group-addon">%s</span>\n', left.addon)
  if (length(right.addon)>0) 
      right <- pf('<span class="input-group-addon">%s</span>\n', right.addon)
  center <- pf('<input name="%s" type="%s" class="form-control" placeholder="%s">', 
               name, type, placeholder)
  pf('<div class="input-group">\n%s\n</div>\n',left+center+right)
}
  
BS$panel <-
  # Defines a set of buttons which returns the name, displays the value
  # Type is its color (primary, success, info, warning, danger, default)
  # size it's size ('btn-lg', '', 'btn-sm', 'btn-xs')
  function(head="Panel Heading", body='Panel Body', type='default') {
    pf('<div class="panel panel-%s">\n', type)+
      '<div class="panel panel-heading"><h3 class="panel-title">\n'+
      head+'\n</h3></div><!--Panel Heading-->\n'+
      '<div class="panel-body">\n'+body+'\n</div><!--Close Body-->\n'+
      '</div><!--Close Panel-->\n'
  }
  
# Use row, col, panel combinations to create stacked panels
# 1 | 2
# - - -
# 3 | 4

#tag$row(tag$col(BS$panel('Panel 1'), 
#                BS$panel('Panel 2', 'Body', 'primary')),
#        tag$col(BS$panel('Panel 3', BS$button(), 'info'), 
#                BS$panel('Panel 4', 'Body', 'warning')))



# Highest level functions are builds
BS$RespButton <- function(resp, collapse="") {
  # Takes a vector of possible responses and returns a button set.
  
  # Feed in a vector in of possible responses
  ir <- (resp)[resp!='']
  # Code responses from 1 to N
  #  responses <<- p('R',1:length(ir))
  responses <<- 1:length(ir)
  # Select shortcut keys
  shortAns  <<- disp$btn.short[1:length(ir)]
  
  p(BS$button(responses, ir, accesskey=shortAns), 
    collapse=collapse)
}

BS$a <- function(name='Default_link', value='Link')
  pf('<a href="#" onclick="test.submit(\'%s\')">%s</a>', name, value)


BS$top.navbar <- function(title='', name=NULL, value=NULL, active=1) {
  
  pass.links <- ''
  if (length(name)>0) {
    pass.links <- p('<li><a href="#" ', BS$a(name,value), '</li>')
    pass.links[active] <- 
      p('<li class="active">', BS$a(name[active],value[active]), '</li>')
  }
  
  p('<div class="navbar navbar-inverse navbar-fixed-top" role="navigation">',
     '<div class="container">\n<div class="header">',
     '<a class="navbar-brand" href="#">', title, 
     '</a></div>\n<ul class="nav navbar-nav">')+
     pc(pass.links)+'</ul>\n</div>\n</div>'
}


BS$stylizer <- function(theme=TRUE, size=TRUE, col=TRUE, key=TRUE){
  repeat {
  ### HTML Build
    # Define the body of the container
    body <- ''
    
    # If select button size is an option
    if (size) body <- body+tag$h4('Button Size')+
      tag$p(pn(BS$button(name=disp$btn.size.names, 
                         value=disp$btn.size.names, 
                         size=disp$btn.size.type)))
    
    # If select color is an option
    if (col) body <- body+
      tag$h4('Button Color')+
      tag$p(pn(BS$button(name=disp$btn.col.names, 
                         value=disp$btn.col.names, 
                         type=disp$btn.col.type)))
    
    # If select theme is an option
    if (theme) {
      # Identify current color
      current.col <- (1:length(disp$btn.col.type))[disp$btn.col==disp$btn.col.type]
      # Find next color
      next.col    <- (current.col %% length(disp$btn.col.type)) + 1
      # Create a vector of colors for the themes
      btn.col <- rep(disp$btn.col, length(disp$bootswatch))
      # Replace the current theme color with a different color
      # btn.col[disp$bootswatch==disp$theme] <- next.col
      btn.col[disp$bootswatch==disp$theme] <- ''
      
      body <- body+
        tag$h4('Themes From ' + tag$a('Bootswatch', 'http://bootswatch.com/')) +
        tag$p(pn(BS$button(name=disp$bootswatch,
                           value=disp$bootswatch, 
                           type=btn.col, 
                           size='btn-xs')))
     }
    
    # If shortcut key select is an option
     if (key) {
       key.choice <- sapply(disp$btn.short.set, pc)
       key.name   <- p('key', 1:length(key.choice))
       btn.col    <- rep(disp$btn.col, length(key.choice))
       btn.col[key.choice==pc(disp$btn.short)] <- ''
       
       body <- body+
         tag$h4('Select Shortcut Key Set')+
         tag$p(pn(BS$button(name=key.name,
                            value=key.choice, 
                            type=btn.col)))
         
     }
     
    # Defne the contents of the container    
    contents <- 
      tag$header(tag$h2("PersonalityPulse.com: Style Selector"))+
      tag$center(body)+'<hr>'+
      tag$p(BS$button('done', 'Return to Test') +
      BS$button('default', 'Revert to Default'))
    
    # Define that which will be passed to the browser
    StyleSelect <- BS$head()+tag$container(contents)
    
  ### Call HTML
    response <- concerto.template.show(HTML=StyleSelect)$LAST_PRESSED_BUTTON_NAME
    
  ### Define termination conditions
    if (response == 'done') break()
    
  ### Define other events
  
    # Change size
    if (response %in% disp$btn.size.names) 
      disp$btn.size <<- disp$btn.size.type[disp$btn.size.names==response]
    
    # Change color
    if (response %in% disp$btn.col.names)
      disp$btn.col <<- disp$btn.col.type[disp$btn.col.names==response]
    
    # Change theme
    if (response %in% disp$bootswatch) BS$source(disp$theme<<-response)
    
    # Change access keys
    if (substr(response, 1,3)=='key') {
      key <- as.numeric(substr(response, 4,5))
      disp$btn.short <<- disp$btn.short.set[[key]]
    }
    
    # Return to default
    if (response == 'default') {
      BS$source(disp$theme<<-'bootstrap.theme')
      disp$btn.col   <<- 'primary'
      disp$btn.size  <<- ''
      disp$btn.short <<- disp$btn.short.set[[1]]
    }
  } # End repeat
}


disp <-list(
 # Set default displays
  btn.col='primary',       #button type
  btn.size='',             #button size
  theme='default',         #default boostrap theme
  btn.short=c('a','s','d','f','g','h','j','k'), #button access keys.
 # Set the set of available options
  btn.size.names=c('Large', 'Regular', 'Small', 'Very Small'),
  btn.size.type=c('btn-lg', '', 'btn-sm', 'btn-xs'),
  btn.col.type=c('primary', 'success', 'info', 'warning', 'danger', 'default'),
  btn.col.names=1:6,
  bootswatch = c('cerulean','cosmo', 'cyborg', 'darkly', 'flatly', 'journal', 
                 'lumen', 'paper', 'readable', 'sandstone', 'simplex',
                 'slate', 'spacelab', 'superhero', 'united', 'yeti'),
  btn.short.set=list(
               c('a','s','d','f','g','h','j','k'),
               c('q','w','e','r','t','y','u','i'),
               c('z','x','c','v','b','n','m',','),
               1:8))

# rconcerto Objects
# A function for easily returning concerto default values to the screen.
rconcerto.show <- function()
  concerto.template.show(HTML=html.button(text=paste("concert:",
                                                     capture.output(concerto),"<br>" ,collapse="")))
# A function for creating a permenent HTML document for showing (mostly used for facebook share)
rconcerto.template.write <- function(template, param=list(), tag="") {
  HTMLtemp <- concerto.template.get(template) # Read the template HTML information
  HTMLtemp <- concerto.template.fillHTML(HTMLtemp, param) # Replace parameter information
  html.targ <- rconcerto.targ(paste0(tag,template,".HTML"))
  fileConn<-file(html.targ[1]) # Open a connection to a write file
  writeLines(HTMLtemp, fileConn) # Write HTML to file
  close(fileConn) # Close file
  return(html.targ[2]) # Return location of the write file
}

# Define a function to easily and uniquely generate file save locations.
rconcerto.targ <- function(name="",sep=".")
  paste(c(concerto$mediaPath,concerto$mediaURL),concerto$testID,concerto$sessionID,name,sep=sep)

# Make a facebook button that shares the link
mk.facebook <- function(link)
  p("Share &nbsp;<a href=\"http://www.facebook.com/sharer.php?u=",link,
    "\" target=\"_blank\">
    <img src=\"http://g-ecx.images-amazon.com/images/G/01/askville/bs/icn-facebook.png\"
    style=\"width: 28px; height: 28px;\" /></a>")

# Concact a list naming elements from names of elements when names are not identified
ll <- function(...) {
  CALL <- as.list(match.call(expand.dots = FALSE)$...)
  no.name <- names(CALL)==""
  names(CALL)[no.name]=CALL[no.name]
  for (i in 1:length(no.name)) if (no.name[i]==T) try(CALL[[i]] <- get(toString(CALL[i])))
  CALL
}

# A wrapper for inserting values into a MySQL table.
rconcerto.tinsert <- function(table, param, dbname=concerto$db$name, IP=T, ID=T, Ver=T) {
  command <- sprintf("INSERT INTO `%s`.`%s` SET ", dbname, table)
  # As default, save the user IP and the sessionID
  if (IP) param$userIP=concerto$userIP
  if (ID) param$sessionID=concerto$sessionID
  if (Ver) param$version=concerto$version
  arglist <- NULL
  for (i in 1:length(param)) arglist[i] <- sprintf("`%s`='%s'", names(param)[i], param[i])
  concerto.table.query(sql=paste(command, paste(arglist, collapse=",")))
}

# A wrapper for selecting (loading values from) a my SQL table.
rconcerto.tselect <- function(table, order="", dbname=concerto$db$name) {
  command <- sprintf("SELECT * FROM `%s`.`%s`", dbname, table)
  if (order!="") order <- sprintf(" ORDER BY `%s` ASC", order)
  concerto.table.query(sql=paste0(command, order))
}

# Function. Set dummy parameters for test running concerto code on the desktop R package.
rconcerto.dummy <- function() {
  # I am going to declare global objects that simulate the concerto objects.
  concerto <<- list(
    testID=1,
    sessionID=2021,
    workspaceID=3,
    workspacePrefix="concerto4_",
    templateFIFOPath="/var/www/vhosts/concerto4.e-psychometrics.com/httpdocs/data/3/fifo_2",
    sessionPath="/var/www/vhosts/concerto4.e-psychometrics.com/httpdocs/data/3/fifo_2.Rs",
    mediaPath="/var/www/vhosts/concerto4.e-psychometrics.com/httpdocs/media/3/",
    userIP="12.123.123.123",
    mediaURL = "http://concerto4.e-psychometrics.com/media/3/",
    db=list(connection="<MySQLConnection:(1234,0)>",
            name="concerto4_3"))
  rconcerto.template.show <<- function(template, param=NULL) print(paste("Template Show:", template))
}

# rconcerto.dummy()
# Generate a
rconcerto.bell <- function(
  thetahat=-.25, # Default is about 43%
  targ=paste(sample(letters, 10, replace=T), collapse=""), # Generate a random letter string
  saveplot=T, # Save the plot is standard
  col=c(grey(.4),grey(.8)), # Change colors to shades of grey
  width=600, # Specify the width of the plot
  main="You scored better than %s%% of those taking this test.") {
  # If plot is enabled plot open a dev.
  if (saveplot) {
    save_target <- rconcerto.targ(paste0(name=targ, ".png"))
    png(file=save_target[1], width = width, height = width/phi)
    print(paste("Graph saved to", save_target[2]))
  }
  # Map the range that the y axes can reach
  fullrange <- seq(-4, 4, .1)
  pfullrange <- dnorm(fullrange)
  # Specify the ranges for that achieved by the individual.
  thetarange <- seq(-3, thetahat, .1)
  pthetarange <- dnorm(thetarange)
  # Replace the text in the title with the %.
  main <- sprintf(main, round(pnorm(thetahat)*100,1))
  # Set the graph margins
  par(mar=c(1,1,3,1))
  # Plot the graph without any points
  plot(c(-3,3), minmax(pfullrange), type="n", lwd=3, main=main, ylab = "", xlab = "", xaxt='n', yaxt='n')
  # Plot the range of the population
  polygon(c(-4,fullrange,4),c(0,pfullrange,0), lwd=3, col=grey(.4))
  # Plot the range covered by the person
  polygon(c(-4,thetarange,max(thetarange)),c(0,pthetarange,0), lwd=3, col=grey(.8))
  if (saveplot) {
    dev.off()
    return(save_target[2])
  }
}

# graph1 <- rconcerto.bell()
# Evaluate code directly from a dropbox file
dropbox.eval <- function(x, noeval=F, printme=F, split=";", no_return=T) {
  require(RCurl)
  # Load the file into memory as a text file with getURL
  intext <- getURL(paste0("https://dl.dropboxusercontent.com/",x),
                   ssl.verifypeer = FALSE)
  # For some reason \r seem to be frequently inserted into
  # the script files that I save. They present a problem
  # so I remove them using gsub.
  intext <- gsub("\r","", intext)
  # First do some error checking. Count the number of {} and () to see if they match.
  checks <- c("(", ")", "{", "{")
  nchecks <- NA
  for (i in 1:length(checks)) nchecks[i]<-(nchar(gsub(sprintf("[^%s]", checks[i]),"",intext)))
  if (!all(nchecks[c(1,3)]==nchecks[c(2,4)]))
    print(paste0("Warning! Mis-matched counts:", paste0(nchecks, "'", checks,"'" ,collapse=" , ")))
  # Break the intext into a different value for each line.
  untext <- unlist(strsplit(intext, split, fixed = TRUE))
  # Evaluate the input file.
  if (!noeval) for (i in untext) {
    if (printme) cat(paste("", i,"\n"))
    eval(parse(text = i), envir= .GlobalEnv)
  }
  # Finally return the dropbox script as text.
  if (!no_return) return(intext)
}

# dropbox.eval("sh/1fjpw58gko634ye/C74hTEkknP/Demo.R")
# This function acts much the same as concerto.template.show except that it requires 
# one or more values from the template to be evaluated
rconcerto.check.show <- function(template, param=list(), vcheck="", 
                                 mess="Please check the box to continue.") {
  # Set these two values to be empty
  returner <- list(); usermess <- ""
  # This provides an infinite loop until the user satisfies the condition of the check box being clicked.
  while (is.null(returner[[vcheck]])) {
    returner <- concerto.template.show(template,
                                       param=c(param, vcheck=usermess))
    # Send the user the need to check the box message.
    usermess <- html.highlight(mess)
  }
  # Returns the values to the user
  returner
}




### TESTING
