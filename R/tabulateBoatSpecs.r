#' @export

tabulateBoatSpecs <- function(x,variableOfInterest=c('length','width','livewell','fuel'), groupVariable='PrimaryLFA'){
  le = grep(variableOfInterest,colnames(x))

  x5 = split(x,f=x[,groupVariable])
  out=list()
  for(i in 1:length(x5)){
    m = unlist(x5[[i]][,le])
    l = nrow(x5[[i]])
    x4 = x5[[i]][-which(is.na(x5[[i]][,variableOfInterest])),]

    lr = nrow(x4)
    if(variableOfInterest %in% c('lengthCorrected','width','fuel')){
      if(any(!is.na(m))){
        m = round(m)
       v=table(m)
        v5 = data.frame(LFA = unique(x5[[i]]$PrimaryLFA),v1=as.numeric(names(v)),freq=as.numeric(v)) ##not done
        names(v5)[2] = variableOfInterest
        out[[i]] = v5
        names(out)[[i]] = unique(x5[[i]][,groupVariable])
      }
    } else {
      if(any(!is.na(m))){
        v = c(sum(m,na.rm=T),lr)
        names(v5)[1] = variableOfInterest
        names(v5)[2] = 'N_not_NA'
        out[[i]] = v
        names(out)[[i]] = unique(x5[[i]][,groupVariable])
      }

    }
  }
  return(out)
}
