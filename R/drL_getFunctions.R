## Wrap for test goodnes of fit as it can produce an error
get_modelfit <- function(object, x, method, grouping) {
  ans <- try(get_neilltest(object, x, method, grouping),silent=TRUE)
  if(inherits(ans,"try-error")){
    ans <- NA
  } else{
    if(is.character(ans)){
      if(ans=="error1") ans <- "Too many groups in 'grouping'"
      if(ans=="error2") ans <- "Too few groups in 'grouping'"
    } else{
      ans <- as.numeric(round(ans[,2],3))
    }
  }
  return(ans)
}

## Get fitted values
get_fitted<-function(model, bkg.method, bkg.mean, fct) {
  lhs <- as.character(model$m$formula()[[2]])
  parameters <- names(model$m$getPars())
  allobj <- ls(model$m$getEnv())
  rhs <- allobj[-match(c(parameters,lhs),allobj)]
  ndf <- data.frame(get(lhs, model$m$getEnv()), get(rhs, model$m$getEnv()))
  names(ndf) <- c(lhs, rhs)

  if(bkg.method=="constraint")  log10.bkgmean <- log10(bkg.mean)

  yvalue <- ndf[,lhs]
  if(bkg.method!="constraint"){
    inv <- invest.fun(model,"noconstraint", fct, yvalue, parameters, NULL)
  }
  if(bkg.method=="constraint"){
    inv <- invest.fun(model,"constraint",
                      fct, yvalue, parameters, log10.bkgmean)
  }
  est <- unlist(lapply(1:length(yvalue), function(x) inv$inv[[x]]$est))

  form <- unlist(lapply(1:length(yvalue), function(x) inv$form[[x]]))

  se <- lapply(form,
               function(x) msm::deltamethod(x, coef(model), vcov(model)))
  se <- unlist(se)

  ans <- as.data.frame(cbind(est, se))
  names(ans) <- c("log10_concentration.fit", "log10_concentratrion.se")
  return(ans)
}

## Estimate of model fit based on neill.test
## The code is an adapatation of drc::neill.test but has been
## changed in some sections because some errors occurs in the original
## function with 'nls' class models. The drc::neill.test, version 2.3-96
get_neilltest <- function(object, x, method, grouping){
  noCluster <- floor(length(x)/2)
  if(is.null(grouping)){
    if (method == "finest") {
      lenx <- length(x)
      grouping <- floor((1 + 1:lenx)/2)
      grouping[lenx] <- grouping[lenx - 1]
    }
    if (method == "c-finest") {
      for (i in noCluster:(length(coef(object)) + 1)) {
        grouping <- cutree(hclust(dist(x)), k = i)
        if (all(tapply(x, grouping, length) > 1)) {
          break
        }
      }
    }
    if (method == "percentiles") {
      cutVar <- c(-Inf, quantile(x, c(0.2, 0.4, 0.6, 0.8)),
                  Inf)
      grouping <- cut(x, cutVar)
    }
  }




  M <- length(unique(grouping))
  lhs <- as.character(object$m$formula()[[2]])
  parameters <- names(object$m$getPars())
  allobj <- ls(object$m$getEnv())
  rhs <- allobj[-match(c(parameters,lhs),allobj)]
  ndf <- data.frame(get(lhs, object$m$getEnv()), get(rhs, object$m$getEnv()))
  names(ndf) <- c(lhs, rhs)
  norder <- order(ndf[,rhs], decreasing=TRUE)
  ndf <- ndf[norder,]

  N <- nrow(ndf)
  denDF <- N - M

  ## Checking the number of groups
  mes <- NULL
  if (denDF <= 0)  # (N <= M)
  {
    # "error1.Too many groups in 'grouping'"
    mes <- "error1"
  }
  p <- N - df.residual(object)
  numDF <- M - p

  if (numDF <= 0)  # (M <= p)
  {
    # "error2. Too few groups in 'grouping'"
    mes <- "error2"
  }

  if(is.null(mes)){
    ## Calculating the test statistic
    resVec <- residuals(object)
    resVec <- resVec[norder]
    resAver0 <- tapply(resVec, grouping, mean)
    resAver <- rep(resAver0, tapply(grouping, grouping, length))

    resDiff <- resVec - resAver
    FF <- (denDF/numDF)*(sum(resAver*resAver)/(sum(resDiff*resDiff)))
    p <- pf(FF ,numDF, denDF, lower.tail = FALSE)
    ans <- matrix(c(FF, p), 1, 2)
    colnames(ans) <- c("F","p")
  } else {
    ans <- mes
  }
  return(ans)
}

compair <- function(x, predlog10mfi.var = "predicted.log10_mfi",
                    residual.var = "residuals"){
  if( all(c(predlog10mfi.var,residual.var)%in%names(x)) ) {
    mat <- apply( x[,c(predlog10mfi.var,residual.var)], 1,
                  function(y) is.na(y))
    ans <- apply(mat,2,function(y) sum(y))
    ans <- all(ans>=1)
  } else {
    ans <- TRUE
  }
  return(ans)
}
