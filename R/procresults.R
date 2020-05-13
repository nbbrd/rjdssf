#' @include rslts.R
#' @import rJava
NULL

#' Title
#'
#' @slot internal jobjRef.
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_Object",
  representation = representation(internal = "jobjRef" )
)

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_ProcResults",
  contains = "JD3_Object"
)

#' Title
#'
#' @param object
#' @param id
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
setGeneric(name="result", def = function( object, id, ... ){standardGeneric("result")})

#' Title
#'
#' @param object
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
setGeneric(name="dictionary", def = function( object, ... ){standardGeneric("dictionary")})

#' Title
#'
#' @slot internal jobjRef. 
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_Object",
  representation = representation(internal = "jobjRef" )
)

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_ProcResults",
  contains = "JD3_Object"
)

#' Title
#'
#' @param JD3_ProcResults 
#'
#' @return
#' @export
#'
#' @examples
setMethod("dictionary", "JD3_ProcResults", function(object){
  if (is.null(object@internal)){
    NULL
  }else{
    proc_dictionary(.jclass(object@internal))
  }

})

#' Title
#'
#' @param object JD3_ProcResults. 
#' @param id character. 
#'
#' @return
#' @export
#'
#' @examples
setMethod("result", signature = c(object="JD3_ProcResults", id="character"), function(object, id){
  if (is.null(object@internal)){
    NULL
  }else{
    proc_data(object@internal, id)}
})

