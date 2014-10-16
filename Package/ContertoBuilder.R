build <- list()

build$disp <-list(btn.col='primary', btn.size='', theme='bootstrap.theme',
             btn.size.names=c('Large', 'Regular', 'Small', 'Very Small'),
             btn.size.type=c('btn-lg', '', 'btn-sm', 'btn-xs'),
             btn.col.type=c('primary', 'success', 'info', 'warning', 'danger', 'default'),
             btn.col.names=1:6,
             bootswatch = 
                c('cerulean','cosmo', 'cyborg', 'darkly', 'flatly', 'journal', 
                  'lumen', 'paper', 'readable', 'sandstone', 'simplex',
                  'slate', 'spacelab', 'superhero', 'united', 'yeti'),
             btn.short.set=
                   list(c('a','s','d','f','g','h','j','k'),
                        c('q','w','e','r','t','y','u','i'),
                        c('z','x','c','v','b','n','m',','),
                        1:8),
             btn.short=c('a','s','d','f','g','h','j','k'))

build$RespButton <- function(resp) {
# Feed in a vector in of possible responses
  ir <- (resp)[resp!='']
  # Code responses from 1 to N
  responses <<- p('R',1:length(ir))
  # Select shortcut keys
  shortAns  <<- disp$btn.short[1:length(ir)]

  pc(BS$button(
  build$responses, 
  build$ir, 
  build$btn.col, 
  build$btn.size, 
  build$btn.shortcut))
}

build$stylizer <- function(theme=TRUE, size=TRUE, col=TRUE){
  repeat {
  # Define the body of the container
      body <- ''
      
      if (size) body <- body+
        tag$p('Button Size')+
        tag$p(pn(BS$button(build$disp$btn.size.names, 
                           build$disp$btn.size.names, 
                           build$disp$btn.col, 
                           build$disp$btn.size.type)))
        
      
      if (col) body <- body+
        tag$p('Button Color')+
        tag$p(pn(BS$button(build$disp$btn.col.names, 
                           build$disp$btn.col.names, 
                           build$disp$btn.col.type, 
                           build$disp$btn.size)))+
      
      if (theme) body <- body+
        tag$p('Themes From ' + tag$a('Bootswatch', 'http://bootswatch.com/'))+
        tag$p(pn(BS$button(bootswatch, bootswatch, disp$btn.col, 'btn-xs')))+
      
      body <- body+
        tag$p('Revert to Default')
        BS$button('bootstrap.theme', 'Default', 'primary', '')
        
    # Defne the contents of the container    
      contents <- 
        tag$header(tag$h2("PersonalityPulse.com: Style Selector"))+
        BS$cj("", body)+
        tag$h2('All Done')+
        tag$p(BS$button('done', 'Return to Test', build$disp$btn.col, build$disp$btn.size))
    
    # Define that which will be passed to the viewer
      StyleSelect <- BS$head()+tag$container(contents)
        
      # Display style selector
      response <- concerto.template.show(HTML=StyleSelect)$LAST_PRESSED_BUTTON_NAME
      
      if (response == 'done') break()
                                             
      if (response %in% btn.size.names) 
        build$disp$btn.size <<- build$disp$btn.size.type[build$disp$btn.size.names==response]
      
      if (response %in% build$disp$btn.col.names)
        build$disp$btn.col <<- build$disp$btn.col.type[build$disp$btn.col.names==response]
      
      if (response %in% bootswatch) BS$source(build$disp$theme<-response)

      # Return to default
      if (response == 'bootstrap.theme') {
        BS$source(build$disp$theme<<-'bootstrap.theme')
        disp$btn.col  <<- 'primary'
        disp$btn.size <<- ''
      }
    }
}
