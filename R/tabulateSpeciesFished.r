#' @export

tabulateSpeciesFished <- function(x, groupVariable='PrimaryLFA'){
  x1 = strsplit(x$Species,"_")
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
  x3 = as.data.frame(do.call(rbind, out), stringsAsFactors=FALSE)
  x3$Source.Name = nam
  x4 = merge(x,x3)
  co = grep('V',names(x4))
  x5 = split(x4,f=x4[,groupVariable])

  out=list()
  par(mfrow=c(2,2),las=2)

  for(i in 1:length(x5)){
         m = tools::toTitleCase(unlist(x5[[i]][,co]))
         l = nrow(x5[[i]])
      if(any(!is.na(m))){
          v = table(m)/l*100
              if(any(names(v)=='Lobster')) {
                o = which(names(v)=='Lobster'); v = v[-o]
              }
              if(any(names(v)=='0')) {
                o = which(names(v)=='0'); v = v[-o]
              }
              if(any(names(v)=='None')) {
                o = which(names(v)=='None'); v = v[-o]
              }
    out[[i]] = v
    names(out)[[i]] = unique(x5[[i]]$PrimaryLFA)

    barplot(v,ylab='Percent of Respondants',main=paste(unique(unique(x5[[i]]$PrimaryLFA)),'; n=',l,sep=" "))
    }
  }
  return(out)
  }
