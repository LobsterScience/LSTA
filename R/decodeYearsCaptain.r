#' @export

decodeYearsCaptain <- function(x){
      x = bio.utilities::recode(x,"'Less than 5 years'='<5';
    '5-14 years.'='5-14';
    '15-29 years.'='15-29';
    '30 years or more.'='30+';
    '5-14 years'='5-14';
    '15-29 years'='15-29';
    '30 years or more'='30+';")
      return(x)
}
