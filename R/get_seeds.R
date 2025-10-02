#' Generate a reproducible vector of random seeds
#' 
#' @description
#' Use the user-specified `masterseed` to generate a vector of randomly sampled 
#' seeds that is reproducible by calling the same `masterseed` at a future time.
#'
#' @inheritParams sim_kid
#'
#' @param nseed A positive integer that specifies the number of subjects to 
#' simulate. Defaults to 1000.
#' 
#' @return A vector of numeric integers of length nseed
#' @export
#' 
#' @examples
#' get_seeds(masterseed = 513, nseed = 10)
get_seeds <- function(masterseed = NULL, nseed = 1000){
  
  if(length(nseed) != 1L || !is.numeric(nseed) || nseed%%1!=0 || nseed <= 0){
    warning(paste0(
      "nseed must be a positive integer of length one; ",
      "The default of nseed = 1000 will be used instead"
    ))
  }
  
  if(length(nseed) != 1L || !is.numeric(nseed) || nseed%%1!=0 || nseed <= 0){
    nseed <- 1000
  }
  
  if(is.null(masterseed)){
    seedl <- sample(seq_len(.Machine$integer.max), nseed)
  }else{
    seedl <- withr::with_seed(
      masterseed, 
      sample(seq_len(.Machine$integer.max), nseed)
    )
  }
}