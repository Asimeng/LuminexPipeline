#' @export
as.data.frame.scluminex <- function(x, row.names = NULL,
                                    optional = FALSE, ...){
  data <- ldply(lapply(x,function(y){y$data}),rbind)
  data$.id <- NULL
  return(data)
}


#' @export
as.data.frame.summary.scluminex <- function(x,row.names = NULL,
                                            optional = FALSE, ...) {
  if (inherits(x,"summary.scluminex")){
    return(data.frame(unclass(x),...))
  } else {
    stop("'x' is not an object of class summary.scluminex")
  }
}
