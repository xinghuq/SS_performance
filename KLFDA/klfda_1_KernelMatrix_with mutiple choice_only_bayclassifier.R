klfda_1=function (k, y,kernel=polydot(degree = 1, scale = 1, offset = 1), r=20, tol,prior, CV=FALSE,usekernel = TRUE, fL = 0.5,metric = c('weighted', 'orthonormalized', 'plain'),
          knn = 6, reg = 0.001,...) {## reg regularization parameter (default: 0.001)
  
  #
  
  tol=tol
  obj.trainData = k
  obj.trainClass = y
  obj.classes = sort(unique(obj.trainClass));
  obj.nClasses = length(obj.classes)
  k=kernelMatrix(kernel, obj.trainData,obj.trainData)
  #k=multinomial_kernel(obj.trainData,obj.trainData,order=2)
  
  obj.nObservations=dim(obj.trainData)[1]
  obj.nFeatures = dim(obj.trainData)[2]
  #k=k/obj.nObservations
  
  cl <- match.call()
  metric <- match.arg(metric) # the type of the transforming matrix (metric)
  x=as.matrix(k)
  p <- ncol(k)
  n <- nrow(k) # number of samples
  
  
  if(n != length(y))
    stop("nrow(x) and length(y) are different")
  g <- as.factor(y)
  lev <- lev1 <- levels(g)
  counts <- as.vector(table(g))
  
  if(is.null(prior)==FALSE) {
    if(any(prior < 0) || round(sum(prior), 5) != 1) stop("invalid 'prior'")
    if(length(prior) != nlevels(g)) stop("'prior' is of incorrect length")
    prior <- prior[counts > 0L]
    
  }
  
  
  if(any(counts == 0L)) {
    empty <- lev[counts == 0L]
    warning(sprintf(ngettext(length(empty),
                             "group %s is empty",
                             "groups %s are empty"),
                    paste(empty, collapse = " ")), domain = NA)
    lev1 <- lev[counts > 0L]
    g <- factor(g, levels = lev1)
    counts <- as.vector(table(g))
  }
  
  proportions <- counts/n
  ng <- length(proportions)
  
  
  group.means <- tapply(c(x), list(rep(g, p), col(x)), mean)
  
  ### new section  from local da
  
  y <- t(as.matrix(y)) # transpose of original class labels
  d=nrow(k)
  if(is.null(r)) r <- n # if no dimension reduction requested, set r to n
  
  tSb <- mat.or.vec(n, n) # initialize between-class scatter matrix (to be maximized)
  tSw <- mat.or.vec(n, n) # initialize within-class scatter matrix (to be minimized)
  
  # compute the optimal scatter matrices in a classwise manner
  for (i in unique(as.vector(t(y)))) {
    
    Kcc <- k[y == i, y == i] # data for this class
    Kc <- k[, y == i]
    nc <- nrow(Kcc)
    
    # Define classwise affinity matrix
    Kccdiag <- diag(Kcc) # diagonals of the class-specific data
    distance2 <- repmat(Kccdiag, 1, nc) + repmat(t(Kccdiag), nc, 1) - 2 * Kcc
    
    # Get affinity matrix
    A <- getAffinityMatrix(distance2, knn, nc)
    
    Kc1 <- as.matrix(rowSums(Kc))
    Z <- Kc %*% (repmat(as.matrix(colSums(A)), 1, n) * t(Kc)) - Kc %*% A %*% t(Kc)
    tSb <- tSb + (Z/n) + Kc %*% t(Kc) * (1 - nc/n) + Kc1 %*% (t(Kc1)/n)
    tSw <- tSw + Z/nc
  }
  
  K1 <- as.matrix(rowSums(k))
  tSb <- tSb - K1 %*% t(K1)/n - tSw
  
  tSb <- (tSb + t(tSb))/2 # final between-class cluster matrix
  tSw <- (tSw + t(tSw))/2 # final within-class cluster matrix
  
  # find generalized eigenvalues and normalized eigenvectors of the problem
  eigTmp <- suppressWarnings(rARPACK::eigs(A = solve(tSw + reg * diag(1, nrow(tSw), ncol(tSw)),tol=tol,bounds = list(a=c(X> 0))) %*% tSb,
                                           k = r,which ='LM')) # r largest magnitude eigenvalues
  eigVec <- Re(eigTmp$vectors) # the raw transforming matrix
  eigVal <- as.matrix(Re(eigTmp$values))
  
  # options to require a particular type of returned transform matrix
  # transforming matrix (do not change the "=" in the switch statement)
  Tr <- getMetricOfType(metric, eigVec, eigVal, n)
  
  
  Z <- t(t(Tr) %*% k) # transformed data
  
  classVecsTrain = matrix(nrow=obj.nObservations, ncol=obj.nClasses);
  obj.nObsPerClas = matrix(nrow=1, ncol=obj.nClasses);
  for (i in 1:obj.nClasses) {
    clas = obj.classes[i]
    classVecsTrain[, i] = match(obj.trainClass, clas,nomatch = 0)
    
    obj.nObsPerClas[i] = sum(classVecsTrain[,i])
  }
  if (is.null(prior)==TRUE){
    obj.priors = matrix(data=0,nrow=1, ncol=obj.nClasses);
    for (i in  1 : obj.nClasses){
      obj.priors[,i] = obj.nObsPerClas[,i] / obj.nObservations;
    }
  }
  if(is.null(prior)==FALSE) {
    priorsTmp = prior;
    obj.priors = matrix(data=0,nrow=1, ncol=obj.nClasses)
    for (i in 1 : obj.nClasses){
      clas = obj.classes[i]
      obj.priors[,i]= priorsTmp[i]
    }
  }
  prior=obj.priors
  names(prior) <- names(counts) <- lev1
  
  require(WMDB) ## FOR dbayes function the  function
  bayes_judgement=dbayes0(Z,obj.trainClass,var.equal = FALSE,tol=tol)
  require(klaR)
  bayes=NaiveBayes(Z, obj.trainClass, prior=prior, usekernel, fL = fL,...)
  bayes_assigment=predict(bayes,threshold = 0.001,dkernel=kernel)
 # t_lda=lda(Z,obj.trainClass,...)
  cl <- match.call()
  cl[[1L]] <- as.name("klfda")
  structure(list(kernel=kernel, tol=tol,obj.trainData=obj.trainData,usekernel=usekernel,fL=fL,obj.classes=obj.classes,obj.nObservations=obj.nObservations, eigTmp=eigTmp,eigVec=eigVec,eigVal=eigVal, prior = prior, bayes=bayes, bayes_judgement=bayes_judgement,bayes_assigment=bayes_assigment,means=group.means,
                 obj.priors=obj.priors,T=Tr,obj.nClasses=obj.nClasses, obj.trainClass=obj.trainClass, Z=Z, call = cl),class = "klfda")
}

predict.kfda=function(object,newdata,dimen,...){
  tol=object$tol
  nObsTest=dim(newdata)[1]
  nFeaturesTest = dim(newdata)[2]
  obj.nClasses=object$obj.nClasses
  obj.nFeatures=object$obj.nFeatures
  obj.nObservations=object$obj.nObservations
  require(kernlab)
  kernel=object$kernel
  knewdata=kernelMatrix(kernel, object$obj.trainData,newdata)
  #knewdata=multinomial_kernel(object$obj.trainData,newdata,order=2)
  #newdata=as.matrix(newdata)
  knewdata=knewdata/obj.nObservations
  Z=object$Z
  Y=object$obj.trainClass
  Trans=as.matrix(object$T)
  #Z2=as.matrix(knewdata) %*% Trans 
   Z2=t(as.matrix(knewdata)) %*% Trans 
  # if (is.null(prior)==TRUE){
  prior=object$obj.priors
  # }
  usekernel=object$usekernel
  fL=object$fL
 # t_lda=object$t_lda
  require(klaR) ## FOR Nativebaye function
  bayes_jud_pred=dbayes0(Z,Y,TstX = Z2,var.equal = FALSE,tol=tol)
  # bayes=NaiveBayes(Z, Y, prior, usekernel, fL,kernel,bw = "nrd0", adjust = 1,weights = NULL, window = kernel, give.Rkern = FALSE,...)
  Nbayes_assig_pred=predict(object$bayes,Z2,threshold = 0.001,dkernel=kernel,...)
#  p_lda=predict(t_lda,Z2,...)
  
  
  list(bayes_jud_pred=bayes_jud_pred,bayes_assig_pred=Nbayes_assig_pred)
}

