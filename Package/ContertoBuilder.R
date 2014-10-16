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

BS$source(build$disp$theme)

build$preResp <- function(resp) {
  # Feed in a vector of possible responses.
  # Drop empty values.
}

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
      body <- ''
      
      if (size) body <- body+
        tag$p('Button Size')+
        tag$p(pn(BS$button(btn.size.names, btn.size.names, disp$btn.col, btn.size.type)))
        
      
        tag$p('Button Color')+
        tag$p(pn(BS$button(btn.col.names, btn.col.names, btn.col.type, disp$btn.size)))+
        tag$p('Themes From ' + tag$a('Bootswatch', 'http://bootswatch.com/'))+
        tag$p(pn(BS$button(bootswatch, bootswatch, disp$btn.col, 'btn-xs')))+
        tag$p('Revert to Default')+
        BS$button('bootstrap.theme', 'Default', 'primary', '')
        
      contents <- 
        tag$header(tag$h2("PersonalityPulse.com: Style Selector"))+
        BS$cj("", body)+
        tag$h2('All Done')+
        tag$p(BS$button('done', 'Return to Test', disp$btn.col, disp$btn.size))
      
      StyleSelect <- BS$head()+tag$container(contents)
        
      # Display style selector
      response <- concerto.template.show(HTML=StyleSelect)$LAST_PRESSED_BUTTON_NAME
      
      if (response == 'done') break()
                                             
      if (response %in% btn.size.names) 
        disp$btn.size <- btn.size.type[btn.size.names==response]
      
      if (response %in% btn.col.names)
        disp$btn.col <- btn.col.type[btn.col.names==response]
      
      if (response %in% bootswatch) BS$source(disp$theme<-response)

      # Return to default
      if (response == 'bootstrap.theme') {
        BS$source(disp$theme<<-'bootstrap.theme')
        disp$btn.col  <- 'primary'
        disp$btn.size <- ''
      }
    }
}
