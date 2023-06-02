#' @export

responsesReadIn <- function(su='C:/Users/cooka/OneDrive - DFO-MPO/Fishing_Behaviour/Responses/',onefile=T) {
if(onefile) {
  pa=file.path(su,'Combined.xlsx')
  return( readxl::read_excel(pa,sheet=1))

  }
    pa=file.path(su,'Online/Changes in Fishing Practices Over Time.xlsx')
  x = readxl::read_excel(pa,sheet=1)
  re = x
  pa=file.path(su,'MailIn')
  fi = dir(pa)
  li = list()
  for(i in 1:length(fi)){
    if(i==1){ x = readxl::read_excel(file.path(pa, fi[i]),sheet=1)
    x[1,1] = fi[i]
    li[[i]] = x[1,1:159]
    } else {
      xi = readxl::read_excel(file.path(pa, fi[i]),sheet=1)
      xi[1,1]= fi[i]
      xi = xi[1,1:159]
      li[[i]] = xi
    }
  }

  xi = do.call(rbind,li)
  names(xi)[1:2] = names(re)[1:2]
  x = do.call(rbind,list(re,xi))

  return(x)
}
