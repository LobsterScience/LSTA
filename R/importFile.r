#' @export

importFile <- function(pa){
    x = readxl::read_excel(pa,sheet=1)
    questions = names(x)
    responses = x
  return(list(questions, responses))
}
