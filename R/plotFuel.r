#' @export
plotFuel <- function(s){
  s$freq = s$freq/sum(s$freq)
  print(  ggplot(s,aes(y=freq,x=fuel))+geom_bar(position = 'dodge',stat='identity')+labs(x = 'Fuel Consumed per Trip (l)', y='Percent'))
  }






