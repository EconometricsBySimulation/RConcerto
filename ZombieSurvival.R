# Zombie Apocalyspe Preparedness Test

  # Test Parameters
  twidth <- 700         # Specify a width to start out with.
  phi <- (5/2)^.5       # The Golden Ratio
  initial.fixed <- 4    # Initial number of people minimum in each category
  initial.rand <- 1000  # Initial random people in different categories
  pdecoy <- 1.5           # Percent decoys in terms of % of peeps. This should only slow things down.
  zombies0 <- 1         # One zombie to begin with
  effectiveness <- 1 
  hscalar <- 1000
  concerto$version = .1 ; 

  # rconcerto.dummy() # Run this function if loading from desktop


# LOAD ITEMS
  item.list <- rconcerto.tselect("03-Zomb-01Items")
  item.list <- item.list[item.list$approve==1,]
  item.list$approve <- NULL ;

# INITIALIZE PARAMETERS
  nitems.you <- 15  ;
  nitems.hero <- 5 ;
  
  nitems <- nitems.you+nitems.hero ;

  titems <- nrow(item.list)
  
# SHOW INTRO
  concerto.template.show("03-Zomb-01-Into", param=nl(nitems.you, titems, nitems.hero, version=concerto$version));

# SHOW HERO Selection
  hero.list <- rconcerto.tselect("03-Zomb-02Heroes", "name")
  heroes <- hero.list$name[hero.list$approved==1]
  sender <- html.selectfield(heroes, rows=1)
    
  hero.selector <- concerto.template.show("03-Zomb-02-Pick-Hero", param=list(sender=sender));

  hero.selector$sel_field
  # Without inputing text hero is drawn from list
  (hero <- heroes[as.numeric(hero.selector$sel_field)])
   
  # If the user inputs text, assume it is inputing hero information.
  if (gsub(" ","", hero.selector$txt_hero_new , fixed=TRUE)!="") rconcerto.tinsert("03-Zomb-02Heroes", c(name=(hero <- hero.selector$txt_hero_new), approved=0))
  
# SELECT ITEMS - Initially select items at random
  
  # Select n items in a random order
  item.numb <- sample(1:nrow(item.list), nitems)
  # Construct a subsample of items
  item.selected <- item.list[item.numb,]
  
  item.selected$response <- item.selected$latency <- NULL;
  
# Initiate the test giving you questions
  # Item button names
  buttons <- c("btn_1", "btn_2", "btn_3", "btn_4")
  # This is the value if the item is positively tuned (ex: Do you like shooting things?)
  bvalue <- c(2,1,-1,-2)

  question_setup <- "Answer the following question for YOURSELF"
  for_hero<-0
  for (i in 1:(nitems)) {
    # Change factors if items for the individual have finished and hero have begun
    if (i>nitems.you) {
      question_setup <- paste0("Answer the following question for your hero/heroine:", html.highlight(hero))
      for_hero<-1
	  }
    
    # Solicit user response
    response <- concerto.template.show("03-Zomb-03-Item", param=list(question_setup=question_setup, question_text=item.selected$item[i], i=paste0(i,"/",nitems)))
    
    # Save the response to the item database
    item.selected$response[i] <- user_response <- bvalue[response$LAST_PRESSED_BUTTON_NAME==buttons]
    item.selected$latency[i] <- latency <- response[1]
     
    # Save results to response table     
  rconcerto.tinsert("03-Zomb-03Resp", param=c(user_response=user_response, latency=toString(latency), hero=hero, for_hero=for_hero, item=item.selected$item[i], itemn=item.numb[i], iorder=i))
  };
#########################################  
# Calculate score.
#########################################
  # Calculate person score
  print(item.selected) ;
  print(item.selected$response[1:nitems.you]);
  print((-1)^(1-item.selected$help[1:nitems.you]));
  score <- sum(item.selected$response[1:nitems.you]*((-1)^(1-item.selected$help[1:nitems.you])))
  print(paste("Your Score", score));
  
  # Calculate the minimum score and maximum
  score.min <- nitems.you*(min(bvalue))
  score.max <- nitems.you*(max(bvalue))

  # Calculate the range
  score.range <- score.max-score.min
  
  # Converto the score to a preparedness score
  preparedness <- round((score-score.min)/score.range*98)+1
  
  # Calculate the probability of survival which is just the prepareness + some random variation.
  
# Initial populations proportion
  popd <- abs(qnorm((1:initial.rand)/(initial.rand+1)))
  popd <- floor(popd/max(popd)*98)+1

  tab.popd <- table(popd)
  H <- data.frame(id=1:99, pop=initial.fixed, change=0)
  H$pop[as.numeric(names(tab.popd))] <- tab.popd+initial.fixed
  
  # H$pop*hscalar
  
  H$Start <- H$pop
  
  ### Build first plot
# Plot 1

  # Open up the file.
  plot1 <- rconcerto.targ("plot1.png")

  # Create a graph to be used by Concerto.
  png(file=plot1[1], width = twidth, height = twidth/phi)
  
  plot(c(1,99), c(0,33), col=grey(.25), main=paste0("General Human Population Distribution\nYou are ", preparedness, "% prepared for an encounter with a zombie!"), 
       xlab="Probability of Surving a Single Zombie Encounter (%)",
       ylab="Population", type="n")
       
       polygon(x=c(99,1, H$id), col=grey(.65), y=c(0,0,H$pop), lwd=3)

       abline(v=preparedness, col="red", lwd=4, lty=2)
       if (preparedness<30) text(preparedness+4 , 32000, "YOU", col="Red")
       if (preparedness>30) text(preparedness-4 , 32000, "YOU", col="Red")

  dev.off()

#########################################  
# SIR Model.
#########################################
  
  tpeeps0 <- tpeeps <- sum(H$pop)
  likeyou0 <- H$pop[preparedness]
  
  # Zombie population at day 1.
  nzombies <- vzombies <- zombies0
  
  ndecoy <- tpeeps*pdecoy
  
  to.zombie.rate <- function(ngroup) 
  
  tpeeps<-sum(H$pop)
  vpeeps <- tpeeps
  vcohort<-H$pop[preparedness]
  
  H$change<-0
  for (i in 1:360) {
  print(nzombies)
  H$change <- -nzombies * H$pop/(ndecoy+tpeeps)*(1-H$id/100) * effectiveness
  zchange <- sum(nzombies * H$pop/(ndecoy+tpeeps)*(effectiveness*(1-H$id/100)+(-H$id/100)))
  
  # Do the change
  H$pop <- apply(cbind(H$pop+H$change,0),1,max)
  nzombies <- nzombies+zchange
  
  vzombies <- c(vzombies, nzombies)
  
  tpeeps   <- sum(H$pop)
  vpeeps <- c(vpeeps, tpeeps)
  vcohort <- c(vcohort, H$pop[preparedness])
  
  }
  cohort.prob <- round(vcohort/H$Start[preparedness],3)*100
  
#########################################  
# Build result plots.
#########################################

# Plot 2 and 3
  
  # Create a graph to be used by Concerto.
  main<-"Short Run: First 60 days"
  day.range <- (0:6)*10
  
  cohort.prob.diff <- c(100,cohort.prob[(1:5)*30])-cohort.prob[(1:6)*30]
  highest.risk <- paste0("The 30 day period of greatest risk for your cohort is the period between ", 
      ((0:5)*30)[   cohort.prob.diff==max(cohort.prob.diff)], " days and ", 
      ((1:6)*30)[cohort.prob.diff==max(cohort.prob.diff)], " days, in which ",
      cohort.prob.diff[cohort.prob.diff==max(cohort.prob.diff)],
      "% of your original cohort are expected to perish.")

for (i in 2:3) {
  assign(paste0("plot",i), rconcerto.targ(paste0("plot",i,".png")))
  png(file=get(paste0("plot",i)), width = twidth, height = twidth/phi)
  
  par(mar=c(5, 1, 4, 1))
  
  if (i==3) {
    main<-"Long Run: 0-360 days"
    day.range <- 60*(0:6)
  }
  
  max.range <- max(day.range)
  range.count <- 1:max.range
    
  plot(x=c(0,max.range*1.2), y=c(0,max(vpeeps[range.count])*1.1425), type="n", axes=F, 
       ylab="Population", xlab="Days since subject 0 (the first zombie)",
       main=main)
  
  abline(v=day.range, col=grey(.85), lwd=5)
  abline(h=max(vpeeps[range.count])*1.1, col="yellow", lwd=45)
  abline(h=max(vpeeps[range.count])*1.1, col="white", lwd=38)
  
  text(0, max(vpeeps)*1.1, "100%")

  for (i in day.range[-1]) text(i, max(vpeeps[range.count])*1.1, paste0(cohort.prob[i],"%"))
      
  text(max.range*1.13, max(vpeeps[range.count])*1.1, "Your probability\nof survival")

  lines(vpeeps[range.count], lwd=3, col="darkblue")

  axis(1,col="black",lwd=2, at=day.range, ylab="Days since infection")
  
  text(max.range*1.05, vpeeps[max.range]+3.5, "Humans")

  par(new=T)
  
  plot(x=c(0,max.range*1.2), y=c(0,max(vzombies[range.count])*1.1425),
       type="n", axes=F, xlab="", ylab="")
  
  lines(vzombies[range.count], type="l", lwd=3, , col="limegreen")
  
  text(max.range*1.05, vzombies[max.range]-3.5, "Zombies")
  dev.off()
}

########################################
# Loop though and present final results
########################################

  psurvival <- cohort.prob[60]

  # Define a 
  descriptor <- "zombie bait"
  if (psurvival>15) descriptor <- "a helpless civilian"
  if (psurvival>30) descriptor <- "an asset"
  if (psurvival>55) descriptor <- "very dangerous"
  if (psurvival>60) descriptor <- "a mean SOB"
  if (psurvival>70) descriptor <- "insane"
  
  zadvice <- "Grab as much food as you can carry and hide under the mattress."
  if (psurvival>20) zadvice <- "Keep looking over your shoulder."
  if (psurvival>35) zadvice <- "Find a gun and stock up on ammo."
  if (psurvival>50) zadvice <- "Find a blunt weapon."
  if (psurvival>65) zadvice <- "Finally your time has come to shine! Expunge the zombies hordes from the Earth!"
  if (psurvival>80) zadvice <- "Do whatever you want.  You are untouchable." 
 
  hadvice <- "Build a bomb shelter.  Look into hiring a toothless bodyguard."
  if (psurvival>25) hadvice <- "Get a gun.  Practice shooting."
  if (psurvival>40) hadvice <- "Practice sprinting.  Imagine there is someone following you.  Practice."
  if (psurvival>55) hadvice <- "Start collecting blunt wooden or metal tools.  Join a baseball team."
  if (psurvival>70) hadvice <- "Keep up the good work!"
  if (psurvival>90) hadvice <- "Perhaps you should spend less energy preparing for the end of the world and more energy spending time with your fellow human beings." 

########################################
# Loop though and present final results
########################################

  submitted <- "Please consider submitting a question to be added to the database"

  while (1==1) {
    
    param <- nl(submitted=html.highlight(submitted), plot1=plot1[2], plot2=plot2[2], plot3=plot3[2], likeyou=likeyou0, tpeeps=tpeeps0, preparedness, psurvival=cohort.prob[60], zadvice, hadvice, descriptor, highest.risk)
    
    facelink <- rconcerto.template.write("03-Zomb-03-Summary", param)
    
    facebook <- mk.facebook(facelink)
    
    submit.item <- concerto.template.show("03-Zomb-03-Summary", param=c(facebook=facebook,param))
    
    submitted <- submit.item$txt_submit
    
    # Accept submissions from 
    if(submitted != "Please consider submitting a question to be added to the database!") {
      rconcerto.tinsert("03-Zomb-01Items", param=c(item=submitted, approve=0))
      submitted <- paste(submitted, "Submitted! Thanks! <br> Submit another!")
    }
  }
  
  # What was first degree
  # What was your last degree
