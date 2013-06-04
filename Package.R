# RConcerto Package

# HTML builder functions
# heroes <- c("Rick Grimes (Andrew Lincoln) from the Walking Dead", "Jill Valentine from Resident Evil", "Ashley 'Ash' J. Williams (Bruce Campbell) from the Evil Dead (1981)", "Robert Neville (Will Smith) from I am Legend")

# concerto <- list(db=list(name="concerto4_13"), sessionID=321, userIP="34.2.3.4.564")
# concerto.table.query <- function(sql) print(paste0("sql=",sql))

# HTML Objects

# HTML drop down menu object that return selections with the name sel_field by default.
# Options 
html.selectfield <- function(options, rows=4, name="sel_field") {
  return <- sprintf("<select name=\"%s\" size=\"%i\">", name, rows)
  for (i in 1:length(options)) return=paste0(return, "<option value=", i,">",options[i],"</option>")
  paste0(return, "</select>")
}

# Highlight the text
html.highlight <- function(text) paste0("<SPAN style=\"BACKGROUND-COLOR: #ffff00\">", text,"</SPAN>")

# Create a html button
html.button <- function(name="btn_name", value="Submit") paste0("<input name=\"", name, "\" type=\"button\" value=\"",value,"\" />"

concerto.template.show(HTML="Please click the button: <button>click me</button>")

# Concact a vector automatically naming values when names are not identified
cc <- function(...) {
  CALL <- match.call(expand.dots = FALSE)$...
  no.name <- names(CALL)==""
  names(CALL)[no.name]=CALL[no.name]
  for (i in 1:length(no.name)) if (no.name[i]==T) try(CALL[i] <- get(toString(CALL[i])))
  CALL
}

# Concact a list naming elements from names of elements when names are not identified
ll <- function(...) {
  CALL <- as.list(match.call(expand.dots = FALSE)$...)
  no.name <- names(CALL)==""
  names(CALL)[no.name]=CALL[no.name]
  for (i in 1:length(no.name)) if (no.name[i]==T) try(CALL[[i]] <- get(toString(CALL[i])))
  CALL
}

# A wrapper for inserting values into a MySQL table.
rconcerto.tinsert <- function(table, param, dbname=concerto$db$name, noIP=F) {
  command <- sprintf("INSERT INTO `%s`.`%s` SET ", dbname, table)
  # As default, save the user IP and the sessionID
  if (!noIP) {
    param$userIP=concerto$userIP
    param$sessionID=concerto$sessionID
    param$version=concerto$version
  }
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
  
  concerto.template.show <<- function(template, param=NULL) print(paste("Template Show:", template))

}
# rconcerto.dummy()
