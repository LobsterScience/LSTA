#' @export
plotAges <- function(s,f){

  f1 = as.data.frame(table(f$ageBin)/nrow(f)*100)
  s1 = as.data.frame(s)
  names(s1)[1]='Var1'
  f1$Group = 'Fishery'
  s1$Group = 'Survey'
  c1 = dplyr::bind_rows(list(f1,s1,data.frame(Var1=c('30-39','80-89'),Freq=c(0,0),Group=c('Survey','Survey'))))

  ggplot2::ggplot(c1,aes(fill=Group,y=Freq,x=Var1))+geom_bar(position = 'dodge',stat='identity')
}
