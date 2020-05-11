#' @include ts.R procresults.R 
#' @import rJava
NULL

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_Ssf",
  contains = "JD3_ProcResults"
)

setGeneric(name="compute", def = function( object, ...){standardGeneric("compute")})

setGeneric(name="estimate", def = function( object,...){standardGeneric("estimate")})

setGeneric(name="add"	, def = function( object, item, ...){standardGeneric("add")})

setGeneric(name="loading", def = function( object,...){standardGeneric("loading")})

setGeneric(name="signal", def = function( object,...){standardGeneric("signal")})

setGeneric(name="states", def = function( object,...){standardGeneric("states")})

setGeneric(name="statedim", def = function( object,...){standardGeneric("statedim")})

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_SsfModel",
  contains = "JD3_Object"
)

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_SsfModelEstimation",
  contains = "JD3_ProcResults"
)

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_SsfItem",
  contains = "JD3_Object"
)

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_SsfStateBlock",
  contains = "JD3_SsfItem"
)

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_SsfEquation",
  contains = "JD3_SsfItem"
)


#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_SsfDynamics",
  contains = "JD3_Object"
)

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_SsfInitialization",
  contains = "JD3_Object"
)

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_SsfMeasurement",
  contains = "JD3_Object"
)

#' Title
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class="JD3_SsfLoading",
  contains = "JD3_Object"
)

#' Title
#'
#' @param object JD3_SsfModel. 
#' @param item JD3_SsfStateBlock. 
#'
#' @return
#' @export
#'
#' @examples
setMethod("add", signature = c(object="JD3_SsfModel", item = "JD3_SsfStateBlock"), function(object, item){
  if ( is.jnull(object@internal) || is.jnull(item@internal) ){
    return
  }else{
    .jcall(object@internal, "V", "add", item@internal )
  }
})

setMethod("statedim", signature = c(object="JD3_SsfStateBlock"), function(object){
  if ( is.jnull(object@internal) ){
    return
  }else{
    .jcall(object@internal, "I", "stateDim")
  }
})

#' Title
#'
#' @param object JD3_SsfModelEstimation. 
#'
#' @return
#' @export
#'
#' @examples
setMethod("signal", signature = c(object="JD3_SsfModelEstimation"), function(object, obs=1, pos=NULL, loading=NULL, stdev=F){
  if ( is.jnull(object@internal)){
    return
  }else{
    if (! is.null(loading)){
      if (stdev){
        return (.jcall(object@internal, "[D", "stdevSignal", matrix_r2jd(loading)))
      }else{
        return (.jcall(object@internal, "[D", "signal", matrix_r2jd(loading)))
      }
    }else{
      if (is.null(pos))
        jpos<-.jnull("[I")
      else
        jpos<-.jarray(as.integer(pos-1))
      if (stdev){
        return (.jcall(object@internal, "[D", "stdevSignal", as.integer(obs-1), jpos))
      }else{
        return (.jcall(object@internal, "[D", "signal", as.integer(obs-1), jpos))
      }
    }
  }
})

#' Title
#'
#' @param object JD3_SsfModelEstimation. 
#'
#' @return
#' @export
#'
#' @examples
setMethod("loading", signature = c(object="JD3_SsfModelEstimation"), function(object, obs=1){
  if ( is.jnull(object@internal)){
    return
  }else{
      jm<-.jcall(object@internal, "Ldemetra/math/matrices/MatrixType;", "loading", as.integer(obs-1))
      return (matrix_jd2r(jm))
  }
})

#' Title
#'
#' @param object JD3_SsfModel. 
#' @param item JD3_SsfEquation. 
#'
#' @return
#' @export
#'
#' @examples
setMethod("add", signature = c(object="JD3_SsfModel", item = "JD3_SsfEquation"), function(object, item){
  if ( is.jnull(object@internal) || is.jnull(item@internal) ){
    return
  }else{
    .jcall(object@internal, "V", "add", item@internal )
  }
})

#' Title
#'
#' @param JD3_SsfModel 
#'
#' @return
#' @export
#'
#' @examples
setMethod("estimate", "JD3_SsfModel", function(object, data, marginal=F, concentrated=T,
              initialization=c("Diffuse", "SqrtDiffuse", "Augmented"), optimizer=c("LevenbergMarquardt", "MinPack", "BFGS", "LBFGS"), precision=1e-15, initialParameters=NULL){
  initialization=match.arg(initialization)
  optimizer=match.arg(optimizer)
  if ( is.jnull(object@internal) ){
    return(NULL)
  }else{
    jparams<-.jnull("[D")
    if (! is.null(initialParameters))
      jparams<-.jarray(initialParameters)
    jdata<-matrix_r2jd(data)
    jrslt<-.jcall("rssf/CompositeModels", "Lrssf/CompositeModels$Results;", "estimate",object@internal, jdata, marginal, concentrated, initialization, optimizer, precision, jparams)
    return( new(Class= "JD3_SsfModelEstimation", internal=jrslt))
  }
})

#' Title
#'
#' @param object JD3_SsfModel. 
#'
#' @return
#' @export
#'
#' @examples
setMethod("compute", signature = c(object="JD3_SsfModel"), function(object, data, parameters, marginal=FALSE, concentrated=TRUE){
  
  
  if ( is.jnull(object@internal) ){
    return(NULL)
  }else{
    jdata<-matrix_r2jd(data)
    
    
    jrslt<-.jcall("rssf/CompositeModels", "Lrssf/CompositeModels$Results;", "compute", object@internal, jdata, .jarray(parameters), marginal, concentrated)
    
    
    return( new(Class= "JD3_ProcResults", internal=jrslt))
  }
})

#' Title
#'
#' @param object JD3_SsfEquation. 
#' @param item character. 
#'
#' @return
#' @export
#'
#' @examples
setMethod("add", signature = c(object="JD3_SsfEquation", item="character"), function(object, item, coeff=1, fixed=TRUE, loading=NULL){

  if (is.null(loading))
    .jcall(object@internal, "V", "add", item, coeff, as.logical(fixed), .jnull("jdplus/ssf/ISsfLoading"))
  else
    
    
    .jcall(object@internal, "V", "add", item, coeff, as.logical(fixed), loading@internal)
  
})


#' Title
#'
#' @param name 
#' @param ar 
#' @param fixedar 
#' @param variance 
#' @param fixedvariance 
#' @param nlags 
#' @param zeroinit 
#'
#' @return
#' @export
#'
#' @examples
ar<-function(name, ar, fixedar=FALSE, variance=.01, fixedvariance=FALSE, nlags=0, zeroinit=FALSE){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "ar", name, .jarray(ar), fixedar, variance, fixedvariance, as.integer(nlags), zeroinit)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param factor 
#' @param period 
#' @param fixed 
#' @param variance 
#' @param fixedvariance 
#'
#' @return
#' @export
#'
#' @examples
cycle<-function(name, factor=.9, period=60, fixed=F, variance=.01, fixedvariance=FALSE){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "cycle", name, factor, period, fixed, variance, fixedvariance)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}



#' Title
#'
#' @param name 
#' @param ar 
#' @param fixedar 
#' @param variance 
#' @param fixedvariance 
#' @param nlags 
#' @param nfcasts 
#'
#' @return
#' @export
#'
#' @examples
ar2<-function(name, ar, fixedar=FALSE, variance=.01, fixedvariance=FALSE, nlags=0, nfcasts=0){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "ar", name, .jarray(ar), fixedar, variance, fixedvariance, as.integer(nlags), as.integer(nfcasts))
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}




#' Title
#'
#' @param name 
#' @param ar 
#' @param fixedar 
#' @param lag 
#' @param zeroinit 
#'
#' @return
#' @export
#'
#' @examples
sae<-function(name, ar, fixedar=FALSE, lag=1, zeroinit=FALSE){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "sae", name, .jarray(ar), fixedar, as.integer(lag), zeroinit)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param nwaves 
#' @param ar 
#' @param fixedar 
#' @param lag 
#'
#' @return
#' @export
#'
#' @examples
msae<-function(name, nwaves, ar, fixedar=TRUE, lag=1){

  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "waveSpecificSurveyError", name, as.integer(nwaves), matrix_r2jd(ar), fixedar, as.integer(lag))
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param vars 
#' @param fixedvars 
#' @param ar 
#' @param fixedar 
#' @param lag 
#'
#' @return
#' @export
#'
#' @examples
msae2<-function(name, vars, fixedvars=F, ar, fixedar=T, lag=1){
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "waveSpecificSurveyError", name, vars, fixedvars, matrix_r2jd(ar), fixedar, as.integer(lag))
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param vars 
#' @param fixedvars 
#' @param ar 
#' @param fixedar 
#' @param k 
#' @param lag 
#'
#' @return
#' @export
#'
#' @examples
msae3<-function(name, vars, fixedvars=F, ar, fixedar=T, k, lag=1){
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "waveSpecificSurveyError", name, vars, fixedvars, .jarray(ar), fixedar, matrix_r2jd(k), as.integer(lag))
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param variance 
#' @param fixed 
#' @param initial 
#'
#' @return
#' @export
#'
#' @examples
locallevel<-function(name, variance=.01, fixed=FALSE, initial=NaN){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "localLevel", name, variance, fixed, initial)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}


#' Title
#'
#' @param name 
#' @param levelVariance 
#' @param slopevariance 
#' @param fixedLevelVariance 
#' @param fixedSlopeVariance 
#'
#' @return
#' @export
#'
#' @examples
locallineartrend<-function(name, levelVariance=.01, slopevariance=.01, fixedLevelVariance=FALSE, fixedSlopeVariance=FALSE ){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "localLinearTrend", name, levelVariance, slopevariance, fixedLevelVariance, fixedSlopeVariance)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param period 
#' @param type 
#' @param variance 
#' @param fixed 
#'
#' @return
#' @export
#'
#' @examples
seasonal<-function(name, period, type=c("Trigonometric", "Crude", "HarrisonStevens", "Dummy"), variance=.01, fixed=FALSE){
  
  type=match.arg(type)
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "seasonalComponent", name, type, as.integer(period), variance, fixed)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param variance 
#' @param fixed 
#'
#' @return
#' @export
#'
#' @examples
noise<-function(name, variance=.01, fixed=FALSE){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "noise", name, variance, fixed)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
model<-function(){
  jrslt<-.jnew("jdplus/msts/CompositeModel")
  new (Class = "JD3_SsfModel", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param variance 
#' @param fixed 
#'
#' @return
#' @export
#'
#' @examples
equation<-function(name, variance=0, fixed=T){
  jrslt<-.jnew("jdplus/msts/ModelEquation", name, variance, fixed)
  new (Class = "JD3_SsfEquation", internal = jrslt)
}

#' Title
#'
#' @param pos 
#' @param weights 
#'
#' @return
#' @export
#'
#' @examples
loading<-function(pos=NULL, weights=NULL){
  
  
  if (is.null(pos)){	jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "fromPosition", as.integer(0))
  
  
  return (new (Class = "JD3_SsfLoading", internal =jrslt))
  }
  else if (length(pos) == 1){ if (is.null(weights))
    
    
    jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "fromPosition", as.integer(pos))
  else{
    
    
    if (length(pos) != length(weights))
      
      
      return (NULL)
    
    
    jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "from", as.integer(pos), weights[1])
  }
  
  
  return (new (Class = "JD3_SsfLoading", internal =jrslt))
  }else{
    
    
    if (is.null(weights))
      jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "fromPositions", as.integer(pos))
    else{
      
      
      if (length(pos) != length(weights))
        return (NULL)
      
      
      jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "from", as.integer(pos), weights)
    }
    
    
    return (new (Class = "JD3_SsfLoading", internal =jrslt))
  }
}

#' Title
#'
#' @param pos 
#' @param weights 
#'
#' @return
#' @export
#'
#' @examples
varloading<-function(pos, weights){
  if (is.null(pos)){
    jl<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "fromPosition", as.integer(0))
  }
  else if (length(pos) == 1){
    jl<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "fromPosition", as.integer(pos))
  }else{
    jl<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "fromPositions", as.integer(pos))
  }
  jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "rescale", jl, weights)
  return (new (Class = "JD3_SsfLoading", internal =jrslt))
}

#' Title
#'
#' @param length 
#'
#' @return
#' @export
#'
#' @examples
loading_sum<-function(length=0){
  if (length == 0)
    jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "sum")
  else
    jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "createPartialSum", as.integer(length))
  return (new (Class = "JD3_SsfLoading", internal =jrslt))
}

#' Title
#'
#' @param period 
#' @param startpos 
#'
#' @return
#' @export
#'
#' @examples
loading_cyclical<-function(period, startpos){
  jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "cyclical", as.integer(period), as.integer(startpos-1))
  return (new (Class = "JD3_SsfLoading", internal =jrslt))
}

#' Title
#'
#' @param period 
#' @param startpos 
#'
#' @return
#' @export
#'
#' @examples
loading_periodic<-function(period, startpos){
  jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "periodic", as.integer(period), as.integer(startpos-1))
  return (new (Class = "JD3_SsfLoading", internal =jrslt))
}

#' Title
#'
#' @param initialization 
#' @param dynamics 
#' @param measurement 
#'
#' @return
#' @export
#'
#' @examples
ssf<-function(initialization, dynamics, measurement){
  jrslt<-.jcall("rssf/Ssf", "Ljdplus/ssf/univariate/Issf;", "of", initialization@internal, dynamics@internal, measurement@internal)
  new (Class = "JD3_Ssf", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param ar 
#' @param diff 
#' @param ma 
#' @param var 
#' @param fixed 
#'
#' @return
#' @export
#'
#' @examples
arima<-function(name, ar, diff, ma, var=1, fixed =FALSE){
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "arima", name, as.double(ar), as.double(diff), as.double(ma), var, fixed)
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}


#' Title
#'
#' @param name 
#' @param ar 
#' @param fixedar 
#' @param ma 
#' @param fixedma 
#' @param var 
#' @param fixedvar 
#'
#' @return
#' @export
#'
#' @examples
arma<-function(name, ar, fixedar=F, ma, fixedma=F, var=1, fixedvar =FALSE){
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "arma", name, as.double(ar), fixedar,
                as.double(ma), fixedma, var, fixedvar)
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param period 
#' @param orders 
#' @param seasonal 
#' @param parameters 
#' @param fixedparameters 
#' @param var 
#' @param fixedvariance 
#'
#' @return
#' @export
#'
#' @examples
sarima<-function(name, period, orders, seasonal, parameters=NULL, fixedparameters=FALSE, var=1, fixedvariance =FALSE){
  if (is.null(parameters))
    jp<-.jnull("[D")
  else
    jp<-.jarray(parameters)
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "sarima", name, as.integer(period), as.integer(orders), as.integer(seasonal), jp, fixedparameters, var, fixedvariance)
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param core 
#' @param period 
#' @param start 
#'
#' @return
#' @export
#'
#' @examples
cumul<-function(name, core, period, start=0){
  jrslt<-.jcall("jdplus/msts/DerivedModels", "Ljdplus/msts/StateItem;", "cumulator", name, core@internal
                , as.integer(period), as.integer(start))
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param components 
#'
#' @return
#' @export
#'
#' @examples
aggregation<-function(name, components){
  if(!is.list(components) || length(components)<2 ) {
    stop("incorrect argument, components should be a list of at least 2 items")}
  plist<-list()
  for (i in 1:length(components)){
    plist[[i]]<-components[[i]]@internal
  }
  jcmps<-.jarray(plist, contents.class = "jdplus/msts/StateItem")
  jrslt<-.jcall("jdplus/msts/DerivedModels", "Ljdplus/msts/StateItem;", "aggregation", name, jcmps)
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @param name 
#' @param x 
#' @param var 
#' @param fixed 
#'
#' @return
#' @export
#'
#' @examples
reg<-function(name, x, var=NULL, fixed=F){
  
  if (is.null(var)){
    jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "regression", name, matrix_r2jd(x))
  }else{
    jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "timeVaryingRegression", name, matrix_r2jd(x), as.numeric(var), fixed)
  }
  return (new (Class = "JD3_SsfStateBlock", internal = jrslt))
}

#' Title
#'
#' @param name 
#' @param period 
#' @param start 
#' @param length 
#' @param groups 
#' @param contrast 
#' @param variance 
#' @param fixed 
#'
#' @return
#' @export
#'
#' @examples
td<-function(name, period, start, length, groups=c(1,2,3,4,5,6,0), contrast=TRUE, variance=1, fixed=FALSE){
  jdomain<-tsdomain_r2jd(period, startYear = start[1], startPeriod = start[2], length = length)
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "tdRegression", name, jdomain, as.integer(groups), contrast, variance, fixed)
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

#' Title
#'
#' @param rslt 
#'
#' @return
#' @export
#'
#' @examples
smoothedstates<-function(rslt){
  return(result(rslt, "ssf.smoothing.states"))
}

#' Title
#'
#' @param rslt 
#'
#' @return
#' @export
#'
#' @examples
smoothedstatesstdev<-function(rslt){
  return(sqrt(result(rslt, "ssf.smoothing.vstates")))
}

#' Title
#'
#' @param rslt 
#'
#' @return
#' @export
#'
#' @examples
filteredstates<-function(rslt){
  return(result(rslt, "ssf.filtered.states"))
}

#' Title
#'
#' @param rslt 
#'
#' @return
#' @export
#'
#' @examples
filteredstatesstdev<-function(rslt){
  return(sqrt(result(rslt, "ssf.filtered.vstates")))
}

#' Title
#'
#' @param rslt 
#'
#' @return
#' @export
#'
#' @examples
filteringstatesstdev<-function(rslt){
  return(sqrt(result(rslt, "ssf.filtering.vstates")))
}

#' Title
#'
#' @param rslt 
#'
#' @return
#' @export
#'
#' @examples
filteringstates<-function(rslt){
  return(result(rslt, "ssf.filtering.states"))
}

