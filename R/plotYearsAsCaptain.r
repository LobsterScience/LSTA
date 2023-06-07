#' @export
plotYearsAsCaptain <- function(s,fp){

  f1 = as.data.frame(table(fp$yrH)/nrow(fp)*100)
  s1 = as.data.frame(s)
  names(s1)[1]='Var1'
  f1$Group = 'Fishery'
  s1$Group = 'Survey'
  uf = unique(as.character(f1$Var1))
  us = unique(as.character(s1$Var1))
  if(!all(uf %in% us)) {
    re = uf[which(!uf %in% us)]
    s1 = data.frame(rbind(s1,data.frame(Var1=re,Freq=rep(0,times=length(re)),Group=rep('Survey',times=length(re)))))

    }
  if(!all(us %in% uf)) {
    re = us[which(!us %in% uf)]
    f1 = data.frame(rbind(f1,data.frame(Var1=re,Freq=rep(0,times=length(re)),Group=rep('Fishery',times=length(re)))))

  }

  c1 = dplyr::bind_rows(list(f1,s1))
  c1$Var1 = factor(c1$Var1,levels=c('<5','5-14','15-29','30+'))
print(  ggplot(c1,aes(fill=Group,y=Freq,x=Var1))+geom_bar(position = 'dodge',stat='identity')+labs(x = 'Years As Captain', y='Percent'))
  return(c1)
}






