#' @export
plotBoatWidths <- function(s,fp){
  fp$B = round(fp$BREADTH)
  fp = subset(fp,B>0)
  f1 = as.data.frame(table(fp$B)/nrow(fp)*100)
  s1 = as.data.frame(s[,2:3])
  names(s1)[1]='Var1'
  s1$freq = s1$freq/sum(s1$freq)*100
  uf = unique(as.character(f1$Var1))
  us = unique(as.character(s1$Var1))
  x = data.frame(Var1 = min(as.numeric(c(uf,us)),na.rm=T): max(as.numeric(c(uf,us)),na.rm=T))
  x = merge(merge(x,f1,all.x = T),s1,all.x=T)
  x = bio.utilities::na.zero(x)
  names(x) = c('Length','Fishery','Survey')
  xr = reshape(x,direction='long',idvar="Length",varying=list(2:3))
  xr$time = ifelse(xr$time==1,'Fishery','Survey')
print(  ggplot(xr,aes(fill=time,y=Fishery,x=Length))+geom_bar(position = 'dodge',stat='identity')+labs(x = 'Boat Width (ft)', y='Percent'))
  return(xr)
}






