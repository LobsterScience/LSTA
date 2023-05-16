#' @export

surveyReadIn <- function(su='C:/Users/cooka/OneDrive - DFO-MPO/Fishing_Behaviour/Survey.xlsx') {
          sR = readxl::read_excel(su,sheet=1,col_names = F)
        naR = which(apply(sR,1,function(x) all(is.na(x))))
        survey = list()
        for(i in 1:length(naR)){
          if(i==1) ind = 1: (naR[i]-1)
          if(i>1)ind = (naR[i-1]+1): (naR[i]-1)
          if(i==length(naR))ind = (naR[i]+1): (nrow(sR))
          x = sR[ind,]
          names(x)[1]=x[1,1]
          x = x[-1,]
          survey[[i]] = x
        }
return(survey)
}
