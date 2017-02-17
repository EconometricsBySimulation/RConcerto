# ninja Package
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

`&.html` <- function(e1, e2) pn(e1,'<br>',e2)

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

html <- list()

html$selectfield <- function(options, rows=4, name="sel_field") {
  return <- sprintf("<select name=\"%s\" size=\"%i\">", name, rows)
  for (i in 1:length(options)) return=p(return, "<option value=", i,">",options[i],"</option>")
  p(return, "</select>")
};

# Highlight the text
html$highlight <- function(text) {
  ret <- paste0("<SPAN style=\"BACKGROUND-COLOR: #ffff00\">", text,"</SPAN>")
  class(ret) <- 'html'
  ret
}
# Create a html button
html$button <- function(name="btn_name", value="Submit", text="")
  p(text,"<input name=\"", name, "\" type=\"submit\" value=\"",value,"\" />")
  
# Insert an html image
html$image <- function(targ, alt="", width="", height="", align="center", dropbox=FALSE) {
  # Modify the the width and height strings
  if (dropbox) targ <- paste0('https://dl.dropboxusercontent.com/s/', targ)
  if ((width!="")&(height=="")) stop("Warning: Height must be specified if width is specified!")
  if (height!="") height <- sprintf('height: %spx;',height)
  if (width!="") width <- sprintf('width: %spx;',width)
  # Use sprintf to piece together the html command
  pf('<p style="text-align: %s;"><img alt="%s" src="%s" style="%s %s" /></p>',align, alt, targ, width, height)
}
    
# HTML tags
tag <- list()
tag$center    <- function(...) p("<center>",list(...),"</center>")
tag$left      <- function(...) p("<div align='left'>",list(...),"</div>")
tag$style     <- function(...) p('<style type="text/css">',list(...),"</style>")
tag$br        <- function(n=1) p(paste0(rep('<br>',n),collapse=""))
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
tag$pn         <- function(..., align='') {
  if (align!='')  pn(pf('<p align="%s">%s</p>', align, list(...)))
  if (align=='')  pn(pf('<p>%s</p>', list(...)))
}
tag$head      <- function(...) p("<head>", ..., "</head>")
tag$css       <- function(...) p('<link rel="stylesheet" href="', list(...), '">')
tag$script    <- function(..., scr='') p('<script src="%s">%s</script>',scr,...)
tag$comment   <- function(...) p('<!-- ', ..., ' -->')
tag$container <- function(...) p('<div class="container">', ..., '</div>')
tag$jumbotron <- function(...) p('<div class="jumbotron">', ..., '</div>')
tag$none      <- function(...) p(...)
tag$header    <- function(...) p('<div class="page-header">', ... , '</div>')
tag$a         <- function(x, href, target="") {
  if (target=="") pf('<a href="%s">%s</a>', href, x)
  if (target!="") pf('<a href="%s" target="%s">%s</a>', href, target, x)
}
tag$row       <- function(...) p('<div class="row">\n', pn(...) ,'</div><!--End Row-->\n')
tag$col       <- function(..., size=4) pf('<div class="col-sm-%s">\n%s</div><!--End Col-->\n', size, pn(...)) 
tag$li <- function(..., active=FALSE) {
  if (!active) return(pf('<li>%s</li>', list(...)))
  pf('<li class="active">%s</li>', list(...))
}
# Create a radio button
tag$radio <- function(name, value, text='', checked = '', collapse='') {
  check <- rep('', length(value))
  check[value==checked] <- 'checked'
  p(pf('<input type="radio" name="%s" value="%s" %s>%s</input>', 
       name, value, check, text), collapse=collapse)
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
    pf("https://raw.githubusercontent.com/EconometricsBySimulation/ninja/master/CSS/%s.css",
       x),
         followlocation = TRUE,
         cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
}

# Twitter Bootstrap Objects
BS <- list()

BS$a <- function(name='Default_link', value='Link')
  pf('<a href="#" onclick="test.submit(\'%s\')">%s</a>', name, value)

BS$alert <- function(x, type='warning') pf('<div class="alert alert-%s" role="alert">%s</div>', type, x)

# Container jumbotron quick combo.
BS$cj <- function(..., title="", tags=c('center','jumbotron','container')) {
  if (title!="") return(tag$list(tags, tag$h2(title)*tag$p(...)))
  return(tag$list(tags, pc(tag$p(...))))
  }

BS$get <- function(x)
  getURL(p("https://raw.githubusercontent.com/EconometricsBySimulation/ninja/master/bootstrap/",
   x,".htm"), followlocation = TRUE,
   cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

BS$glyph <- function(x) pf('<span class="glyphicon glyphicon-%s"></span>', x)

BS$head <- function() tag$head(pc(tag$css(BS$SS$min.css, BS$SS$theme.min.css, BS$SS$css2, BS$SS$css3)))+'\n\n'

BS$radio <- function(name, value, disp, checked='') {
  if (length(checked)==0) checked=''
  if (length(value)!=length(disp)) 
    warning('value and display should be equally long')
  ret <- p('')
  for (i in 1:length(value)) 
    ret <- ret & 
      BS$Ttext(disp[i], left.addon=tag$radio(name, value[i], checked=checked))
  ret
}

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

BS$header <- function(...) tag$header(tag$h1(...))

BS$progbar <- function(value=50, type='success', text='', striped=FALSE){
  stripe <- ''
  if (striped) stripe <- ' progress-bar-striped'
  p('<div class="progress">\n') +
  pf('<div class="progress-bar progress-bar-%s%s" role="progressbar" aria-valuenow="%s" aria-valuemin="0" aria-valuemax="100" style="width: %s%%">', type, stripe, value, value)+
  p(text)+p('\n</div>\n</div>')
}

# Create a textual input object. Left and right addons will cause the input box to span entire width
# as well as create textual clues.
BS$Tinput <- function(name, placeholder, type='text', 
  left.addon=NULL, left.button=NULL, right.addon=NULL, right.button=NULL)
{
  left <- right <- p('')
  if (length(left.button)>0) 
    left <- left + pf('<span class="input-group-btn">%s</span>\n', left.button)
  if (length(left.addon)>0) 
    left <- left + pf('<span class="input-group-addon">%s</span>\n', left.addon)
  if (length(right.addon)>0) 
    right <- right + pf('<span class="input-group-addon">%s</span>\n', right.addon)
  if (length(right.button)>0) 
    right <- right + pf('<span class="input-group-btn">%s</span>\n', right.button)
  center <- pf('<input name="%s" type="%s" class="form-control" placeholder="%s">', 
               name, type, placeholder)
  pf('<div class="input-group">\n%s\n</div>\n',left+center+right)
}

BS$Ttext <- function(x, type='text', 
left.addon=NULL, left.button=NULL, right.addon=NULL, right.button=NULL)
{
  left <- right <- p('')
  if (length(left.button)>0) 
    left <- left + pf('<span class="input-group-btn">%s</span>\n', left.button)
  if (length(left.addon)>0) 
    left <- left + pf('<span class="input-group-addon">%s</span>\n', left.addon)
  if (length(right.addon)>0) 
    right <- right + pf('<span class="input-group-addon">%s</span>\n', right.addon)
  if (length(right.button)>0) 
    right <- right + pf('<span class="input-group-btn">%s</span>\n', right.button)
  center <- pf('<span class="form-control">%s</span>', x)
  pf('<div class="input-group">\n%s\n</div>\n',left+center+right)
}
  
BS$panel <-
  # Defines a set of buttons which returns the name, displays the value
  # Type is its color (primary, success, info, warning, danger, default)
  # size it's size ('btn-lg', '', 'btn-sm', 'btn-xs')
  function(head="Panel Heading", body='Panel Body', type=NULL) {
    if (length(type)==0) type <- disp$btn.col
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

disp <-list(
 # Set default displays
  btn.col='primary',       #button type
  btn.size='',             #button size
  theme='default',         #default boostrap theme
  btn.short=c('a','s','d','f','g','h','j','k'), #button access keys.
  return.message='Return to Test',
  btn.size.names=c('Large', 'Regular', 'Small', 'Very Small'), # The set of available options
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

# graph1 <- ninja$bell()
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
    
sql <- list()

# A command builder for inserting values into a MySQL table.
sql$insert <- function(table, param, dbname=concerto$db$name, 
                       IP=TRUE, ID=TRUE, Ver=TRUE, verbose=FALSE, noID=TRUE) 
{
  Insert <- sprintf("INSERT INTO `%s`.`%s` SET", dbname, table)
  # As default, save the user IP and the sessionID
  if (IP) param$userIP=concerto$userIP
  if (ID) param$sessionID=concerto$sessionID
  if (Ver) param$version=concerto$version
  if (noID) param$id <- NULL
  Set <- p(pf("`%s`='%s'", names(param), param), collapse=',')
  command<-paste(Insert, Set)
  if (verbose) print(command)
  concerto.table.query(sql=command)
}


# A command builder for undating values into a MySQL table.
sql$update <- function(table, param, cond=c(ID=1), dbname=concerto$db$name, 
                       IP=TRUE, ID=TRUE, Ver=TRUE, verbose=FALSE, noID=TRUE) 
{
  # As default, save the user IP and the sessionID
  if (IP) param$userIP=concerto$userIP
  if (ID) param$sessionID=concerto$sessionID
  if (Ver) param$version=concerto$version
  if (noID) param$id <- NULL
  Update <- pf("UPDATE `%s`.`%s` SET", dbname, table)
  Set <- p(pf("`%s`='%s'", names(param), param), collapse=',')
  Where <- p("WHERE ", p(pf("`%s`='%s'", names(cond), cond), collapse=','))
  command<-paste(Update,Set,Where)
  if (verbose) print(command)
  concerto.table.query(sql=command)
}


# A wrapper for selecting (loading values from) a my SQL table.
sql$select <- function(table, what="*", order="", dbname=concerto$db$name, verbose=F) 
{
  Select <- sprintf("SELECT %s FROM `%s`.`%s`", what, dbname, table)
  if (order!="") order <- sprintf(" ORDER BY `%s` ASC", order)
  command <- paste(Select, order)
  if (verbose) print(command)
  concerto.table.query(sql=command)
}

# ninja Objects
    
ninja <- list()
    
# A function for easily returning concerto default values to the screen.
ninja$disp <- function(x=concerto)
  concerto.template.show(HTML=html$button(text=p(pn(as.list(match.call())),
                                                     capture.output(x),"<br>" ,collapse="")))

ninja$build <- function(...) {
  r0 <- concerto.template.show(...)
  r0$LPB <- r0$LAST_PRESSED_BUTTON_NAME
  r0$OT  <- r0$OUT_OF_TIME
  r0$TT  <- r0$TIME_TAKEN
  r0  
}

# A function for creating a permenent HTML document for showing (mostly used for facebook share)
ninja$template.write <- function(template, param=list(), tag="") {
  HTMLtemp <- concerto.template.get(template) # Read the template HTML information
  HTMLtemp <- concerto.template.fillHTML(HTMLtemp, param) # Replace parameter information
  html.targ <- ninja$targ(paste0(tag,template,".HTML"))
  fileConn<-file(html.targ[1]) # Open a connection to a write file
  writeLines(HTMLtemp, fileConn) # Write HTML to file
  close(fileConn) # Close file
  return(html.targ[2]) # Return location of the write file
}

# Define a function to easily and uniquely generate file save locations.
ninja$targ <- function(name="",sep=".")
  paste(c(concerto$mediaPath,concerto$mediaURL),concerto$testID,concerto$sessionID,name,sep=sep)

# Function. Set dummy parameters for test running concerto code on the desktop R package.
ninja$dummy <- function() {
  # I am going to declare global objects that simulate the concerto objects.
  concerto.template.show <<- function(template, param=NULL) print(paste0("Template Show:", template))
  concerto.table.query   <<- function(sql) print(paste0("SQL:", sql))
  
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
}

# Substitute default values when empty
ninja$dsub <- function(x, y, con='') {
    yn <- names(y)
    tf <- !sapply(x[yn], is.null)&(x[yn]==con)
    x[yn[tf]] <- y[tf]
  x
}

# ninja$dummy()
# Generate a
ninja$bell <- function(
  thetahat=-.25, # Default is about 43%
  targ=paste(sample(letters, 10, replace=T), collapse=""), # Generate a random letter string
  saveplot=T, # Save the plot is standard
  col=c(grey(.4),grey(.8)), # Change colors to shades of grey
  width=600, # Specify the width of the plot
  main="You scored better than %s%% of those taking this test.") {
  # If plot is enabled plot open a dev.
  if (saveplot) {
    save_target <- ninja$targ(paste0(name=targ, ".png"))
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

# dropbox.eval("sh/1fjpw58gko634ye/C74hTEkknP/Demo.R")
# This function acts much the same as concerto.template.show except that it requires 
# one or more values from the template to be evaluated
ninja$check.show <- function(template, param=list(), vcheck="", 
                                 mess="Please check the box to continue.") {
  # Set these two values to be empty
  returner <- list(); usermess <- ""
  # This provides an infinite loop until the user satisfies the condition of the check box being clicked.
  while (any(is.null(returner[[vcheck]]))) {
    returner <- concerto.template.show(template,
                                       param=c(param, vcheck=usermess))
    # Send the user the need to check the box message.
    usermess <- html$highlight(mess)
  }
  # Returns the values to the user
  returner
}

### TESTING
