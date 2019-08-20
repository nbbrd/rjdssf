setClass(
  Class="JD3_Ssf",
  contains = "JD3_ProcResults"
)

if (! isGeneric("compute")){
  setGeneric(name="compute", def = function( object, ...){standardGeneric("compute")})
}

if (! isGeneric("estimate")){
  setGeneric(name="estimate", def = function( object,...){standardGeneric("estimate")})
}

if (! isGeneric("add")){
  setGeneric(name="add"	, def = function( object, item, ...){standardGeneric("add")})
}

setClass(
  Class="JD3_SsfModel",
  contains = "JD3_Object"
)

setClass(
  Class="JD3_SsfItem",
  contains = "JD3_Object"
)

setClass(
  Class="JD3_SsfStateBlock",
  contains = "JD3_SsfItem"
)

setClass(
  Class="JD3_SsfEquation",
  contains = "JD3_SsfItem"
)


setClass(
  Class="JD3_SsfDynamics",
  contains = "JD3_Object"
)

setClass(
  Class="JD3_SsfInitialization",
  contains = "JD3_Object"
)

setClass(
  Class="JD3_SsfMeasurement",
  contains = "JD3_Object"
)

setClass(
  Class="JD3_SsfLoading",
  contains = "JD3_Object"
)

setMethod("add", signature = c(object="JD3_SsfModel", item = "JD3_SsfStateBlock"), function(object, item){
  if ( is.jnull(object@internal) || is.jnull(item@internal) ){
    return
  }else{
    .jcall(object@internal, "V", "add", item@internal )
  }
})

setMethod("add", signature = c(object="JD3_SsfModel", item = "JD3_SsfEquation"), function(object, item){
  if ( is.jnull(object@internal) || is.jnull(item@internal) ){
    return
  }else{
    .jcall(object@internal, "V", "add", item@internal )
  }
})

setMethod("estimate", "JD3_SsfModel", function(object, data, precision=1e-15, likelihood=c("Diffuse", "Marginal", "Augmented"), 
                                               optimizer=c("LevenbergMarquardt", "MinPack", "BFGS", "LBFGS"), 
                                               concentrated=TRUE, initialParameters=NULL){
  likelihood=match.arg(likelihood)
  optimizer=match.arg(optimizer)
  if ( is.jnull(object@internal) ){
    return(NULL)
  }else{
    jparams<-.jnull("[D")
    if (! is.null(initialParameters))
      jparams<-.jarray(initialParameters)
    jdata<-matrix_r2jd(data)
    jrslt<-.jcall("rssf/CompositeModels", "Lrssf/CompositeModels$Results;", "estimate",object@internal, jdata, precision, likelihood, optimizer, concentrated, jparams)
    return( new(Class= "JD3_ProcResults", internal=jrslt))
  }
})



setMethod("compute", signature = c(object="JD3_SsfModel"), function(object, data, parameters, marginal=FALSE, concentrated=TRUE){
  
  
  if ( is.jnull(object@internal) ){
    return(NULL)
  }else{
    jdata<-matrix_r2jd(data)
    
    
    jrslt<-.jcall("rssf/CompositeModels", "Lrssf/CompositeModels$Results;", "compute", object@internal, jdata, .jarray(parameters), marginal, concentrated)
    
    
    return( new(Class= "JD3_ProcResults", internal=jrslt))
  }
})





setMethod("add", signature = c(object="JD3_SsfEquation", item="character"), function(object, item, coeff=1, fixed=TRUE, loading=NULL){
  
  
  if (is.null(loading))
    .jcall(object@internal, "V", "add", item, coeff, as.logical(fixed), .jnull("jdplus/ssf/ISsfLoading"))
  else
    
    
    .jcall(object@internal, "V", "add", item, coeff, as.logical(fixed), loading@internal)
  
})





jd3_ssf_ar<-function(name, ar, fixedar=FALSE, variance=.01, fixedvariance=FALSE, nlags=0, zeroinit=FALSE){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "ar", name, .jarray(ar), fixedar, variance, fixedvariance, as.integer(nlags), zeroinit)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

jd3_ssf_cycle<-function(name, factor=.9, period=60, fixed=F, variance=.01, fixedvariance=FALSE){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "cycle", name, factor, period, fixed, variance, fixedvariance)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}



jd3_ssf_ar2<-function(name, ar, fixedar=FALSE, variance=.01, fixedvariance=FALSE, nlags=0, nfcasts=0){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "ar", name, .jarray(ar), fixedar, variance, fixedvariance, as.integer(nlags), as.integer(nfcasts))
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}




jd3_ssf_sae<-function(name, ar, fixedar=FALSE, lag=1, zeroinit=FALSE){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "sae", name, .jarray(ar), fixedar, as.integer(lag), zeroinit)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}




jd3_ssf_msae<-function(name, nwaves, ar, fixedar=TRUE, lag=1){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "waveSpecificSurveyError", name, as.integer(nwaves), matrix_r2jd(ar), fixedar, as.integer(lag))
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

jd3_ssf_msae2<-function(name, vars, fixedvars=F, ar, fixedar=T, lag=1){
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "waveSpecificSurveyError", name, vars, fixedvars, matrix_r2jd(ar), fixedar, as.integer(lag))
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

jd3_ssf_msae3<-function(name, vars, fixedvars=F, ar, fixedar=T, k, lag=1){
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "waveSpecificSurveyError", name, vars, fixedvars, .jarray(ar), fixedar, matrix_r2jd(k), as.integer(lag))
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}



jd3_ssf_locallevel<-function(name, variance=.01, fixed=FALSE, initial=NaN){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "localLevel", name, variance, fixed, initial)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}


jd3_ssf_locallineartrend<-function(name, levelVariance=.01, slopevariance=.01, fixedLevelVariance=FALSE, fixedSlopeVariance=FALSE ){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "localLinearTrend", name, levelVariance, slopevariance, fixedLevelVariance, fixedSlopeVariance)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

jd3_ssf_seasonal<-function(name, period, type=c("Trigonometric", "Crude", "HarrisonStevens", "Dummy"), variance=.01, fixed=FALSE){
  
  type=match.arg(type)
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "seasonalComponent", name, type, as.integer(period), variance, fixed)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}




jd3_ssf_noise<-function(name, variance=.01, fixed=FALSE){
  
  
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "noise", name, variance, fixed)
  
  
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}

jd3_ssf_model<-function(){
  jrslt<-.jnew("jdplus/msts/CompositeModel")
  new (Class = "JD3_SsfModel", internal = jrslt)
}




jd3_ssf_equation<-function(name, variance=0, fixed=T){
  jrslt<-.jnew("jdplus/msts/ModelEquation", name, variance, fixed)
  new (Class = "JD3_SsfEquation", internal = jrslt)
}




jd3_ssf_loading<-function(pos=NULL, weights=NULL){
  
  
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




jd3_ssf_varloading<-function(pos, weights){
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



jd3_ssf_loading_sum<-function(length=0){
  if (length == 0)
    jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "sum")
  else
    jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "createPartialSum", as.integer(length))
  return (new (Class = "JD3_SsfLoading", internal =jrslt))
}



jd3_ssf_loading_cyclical<-function(period, startpos){
  jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "cyclical", as.integer(period), as.integer(startpos-1))
  return (new (Class = "JD3_SsfLoading", internal =jrslt))
}



jd3_ssf_loading_periodic<-function(period, startpos){
  jrslt<-.jcall("jdplus/ssf/implementations/Loading", "Ljdplus/ssf/ISsfLoading;", "periodic", as.integer(period), as.integer(startpos-1))
  return (new (Class = "JD3_SsfLoading", internal =jrslt))
}



jd3_ssf<-function(initialization, dynamics, measurement){
  jrslt<-.jcall("rssf/Ssf", "Ljdplus/ssf/univariate/Issf;", "of", initialization@internal, dynamics@internal, measurement@internal)
  new (Class = "JD3_Ssf", internal = jrslt)
}



jd3_ssf_arima<-function(name, ar, diff, ma, var=1, fixed =FALSE){
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "arima", name, as.double(ar), as.double(diff), as.double(ma), var, fixed)
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}


jd3_ssf_arma<-function(name, ar, fixedar=F, ma, fixedma=F, var=1, fixedvar =FALSE){
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "arma", name, as.double(ar), fixedar,
                as.double(ma), fixedma, var, fixedvar)
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}


jd3_ssf_sarima<-function(name, period, orders, seasonal, parameters=NULL, fixedparameters=FALSE, var=1, fixedvariance =FALSE){
  if (is.null(parameters))
    jp<-.jnull("[D")
  else
    jp<-.jarray(parameters)
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "sarima", name, as.integer(period), as.integer(orders), as.integer(seasonal), jp, fixedparameters, var, fixedvariance)
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}



jd3_ssf_reg<-function(name, x, var=NULL, fixed=F){
  
  if (is.null(var)){
    jrslt<-.jcall("demetra/msts/AtomicModels", "Ldemetra/msts/ModelItem;", "regression", name, matrix_r2jd(x))
  }else{
    jrslt<-.jcall("demetra/msts/AtomicModels", "Ldemetra/msts/ModelItem;", "timeVaryingRegression", name, matrix_r2jd(x), as.numeric(var), fixed)
  }
  return (new (Class = "JD3_SsfStateBlock", internal = jrslt))
}



jd3_ssf_td<-function(name, period, start, length, groups=c(1,2,3,4,5,6,0), contrast=TRUE, variance=1, fixed=FALSE){
  jdomain<-tsdomain_r2jd(period, startYear = start[1], startPeriod = start[2], length = length)
  jrslt<-.jcall("jdplus/msts/AtomicModels", "Ljdplus/msts/StateItem;", "tdRegression", name, jdomain, as.integer(groups), contrast, variance, fixed)
  new (Class = "JD3_SsfStateBlock", internal = jrslt)
}



jd3_smoothedstates<-function(rslt){
  return(result(rslt, "ssf.smoothing.states"))
}



jd3_filteredstates<-function(rslt){
  return(result(rslt, "ssf.filtered.states"))
}



jd3_filteringstates<-function(rslt){
  return(result(rslt, "ssf.filtering.states"))
}

