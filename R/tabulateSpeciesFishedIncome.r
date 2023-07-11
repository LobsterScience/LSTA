#' @export

tabulateSpeciesFishedIncome <- function(x,by.lfa=T){
  x1 = strsplit(x$Species,"_")
  x6 = lapply(x1,FUN=function(x) sapply(strsplit(x,","),'[',2))
  x2 = lapply(x1,FUN=function(x) sapply(strsplit(x,","),'[',1))

  names(x2) = x$Source.Name
  nam <- names(x2)
  len <- sapply(x2, length)
  lm = max(len)
  out <- vector("list", length(len))

   for (i in seq_along(len)) {
        if(length(x2[[i]])<lm)  out[[i]] <- c(x2[[i]],rep(NA,times=(lm-length(x2[[i]]))))
        if(length(x2[[i]])==lm)  out[[i]] <- x2[[i]]
      }
  browser()
    x3 = as.data.frame(do.call(rbind, out), stringsAsFactors=FALSE)
  x3$Source.Name = nam
  x4 = merge(x,x3)
  y1 = tidyr::gather(x4,Var1,Var2,V1:V3)

  names(x6) = x$Source.Name
  nam <- names(x6)
  len <- sapply(x6, length)
  lm = max(len)
  out <- vector("list", length(len))

  for (i in seq_along(len)) {
    if(length(x6[[i]])<lm)  out[[i]] <- c(x6[[i]],rep(NA,times=(lm-length(x6[[i]]))))
    if(length(x6[[i]])==lm)  out[[i]] <- x6[[i]]
  }
  x3 = as.data.frame(do.call(rbind, out), stringsAsFactors=FALSE)
  x3$Source.Name = nam
  y2 = tidyr::gather(x3,Var1,Var2,V1:V3)
  names(y2)[3] = 'Value'
  y3 = merge(y1,y2)
  y3 = subset(y3,select=c(Source.Name,PrimaryLFA,Var2,Value))

  if(!by.lfa) print(ggplot2::ggplot(as.data.frame(table(y3$Value,y3$Var2)),aes(Var2,Freq,fill=Var1))+geom_bar(stat='identity')+ylab('Responses')+ xlab('Species')+ theme(axis.text.x = element_text(angle=90))+guides(fill=guide_legend(title='Income')))
  if(by.lfa) print(ggplot2::ggplot(as.data.frame(table(y3$Value,y3$Var2,y3$PrimaryLFA)),aes(Var2,Freq,fill=Var1))+geom_bar(stat='identity')+facet_wrap(~Var3)+ylab('Responses')+ xlab('Species')+ theme(axis.text.x = element_text(angle=90))+guides(fill=guide_legend(title='Income')))

  return(out)
  }
