#' @export
splitOnline <- function(an=x){

  ##if splitting files
  for(i in 1:nrow(an)){
    an1 = an[i,]
    fn=paste(paste(as.character(an1[1,1]),'xlsx',sep='.'))
    write_xlsx(an1,path=fn)
  }

}
