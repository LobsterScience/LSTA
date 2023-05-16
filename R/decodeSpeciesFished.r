#' @export

decodeSpeciesFished <- function(x){
      x = bio.utilities::recode(x,"'snow crab'='snow crab';
    'Snow crab'='snow crab';
    'Scallop'='Scallop';
    'Herring'='Herring';
    'Green crab'='Green crab';
    'GASPREAU'='Gaspareau';
    'Lobster'='Lobster';
    'Scallops'='Scallop';
    'Scollups'='Scallop';
    'scallops'='Scallop';
    'Mackerel'='Mackerel';
    'scallop'='Scallop';
    'Snow crabs'='snow crab';
    'Crab quota'='Snow crab';
    'Gaspereau'='Gaspareau';
    'SHAD'='Shad';
    'Longline'='Groundfish';
    'Groundfish'='Groundfish';
    'Tuna'='Tuna';
    'Longline Halibut'='Halibut';
    'herring weir'='Herring';
    'Sea urchin'='Sea urchin';
    'Halibut'='Halibut';")

      return(x)



}
