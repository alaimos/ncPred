########################################################
# Developed by S. Alaimo (alaimos at dmi . unict . it) #
# Released under licence GNU GENERAL PUBLIC LICENSE 3  #
# Date: 2015-06-01                                     #
########################################################

graphWeights <- function (n, m, A, lambda=0.5) {
  if (nrow(A) != n || ncol(A) != m) {
    stop("The matrix A should be an n by m matrix.")
  }
  Ky <- diag(1/colSums(A))
  Ky[is.infinite(Ky) | is.na(Ky)] <- 0 #BugFix: 1/0=Infinite replaced with 0
  kx <- rowSums(A)
  Nx <- 1/(matrix(kx, nrow=n, ncol=n, byrow=TRUE)^(lambda) * 
             matrix(kx, nrow=n, ncol=n, byrow=FALSE)^(1-lambda))
  Nx[is.infinite(Nx) | is.na(Nx)] <- 0 #BugFix: 1/0=Infinite replaced with 0
  kx[is.infinite(kx) | is.na(kx)] <- 0 #BugFix: 1/0=Infinite replaced with 0
  
  W <- t(A %*% Ky)
  W <- A %*% W
  W <- Nx * W
  rownames(W) <- rownames(A)
  colnames(W) <- rownames(A)  
  W[is.na(W)] <- 0 #This should never happen
  return (W)
}

computeRecommendation <- function (AOT, ATD, lambda1=0.5, lambda2=0.5) {
  n <- nrow(AOT)
  m <- ncol(AOT)
  o <- nrow(ATD)
  p <- ncol(ATD)
  if (m != o) {
    stop(paste0("The number of columns in ncRNA-Target matrix should ",
                "match the number of rows in the Target-Disease matrix"))
  }
  WT <- graphWeights(n=m, m=n, A=t(AOT), lambda=lambda1)
  WD <- graphWeights(n=p, m=o, A=t(ATD), lambda=lambda2)
  WC <- WT %*% (ATD %*% WD)
  R  <- AOT %*% WC
  return (R)
}

output.table <- function (data) {
  library(xtable)
  return (HTML(paste(
    capture.output(
      print(
        xtable(data, digits=4), 
        type = "html", 
        html.table.attributes="class=\"data table table-bordered table-condensed table-hover\"")), 
    collapse = "\n")))
}

read.input.file <- function(data, what="") {
  
  file.path <- data[1,"datapath"]
  file.type <- tolower(data[1,"type"])
  
  tmp <- NULL
  
  if (file.type == "text/plain") {
    tmp <- data.matrix(read.delim(file=file.path, check.names=FALSE))
  } else {
    obj <- try(readRDS(file.path))
    if (is.data.frame(obj)) {
      tmp <- data.matrix(obj)
    } else if (is.matrix(obj) && all(is.numeric(obj))) {
      tmp <- obj
    }
  }
  if (!is.matrix(tmp)) {
    stop(paste0(what, "Invalid file format: file does not contain a matrix."))
  }
  if (nrow(tmp) <= 1 || ncol(tmp) <= 1) {
    stop(paste0(what, "Invalid file format: You must load a matrix with more than one row or column."))
  }
  return (tmp)
}

test.crossvalidation <- function (M1, M2, algorithm, test.runs=1, k.fold=10, 
                                  top.number=20, progress=NULL, ...) {
  n         <- nrow(M1)
  m         <- ncol(M2)
  p         <- ncol(M1)
  A         <- M1 %*% M2
  A[A != 0] <- 1
  ns        <- rownames(A)
  ms        <- colnames(A)
  ps        <- colnames(M1)
  k.n       <- rowSums(A)
  k.m       <- colSums(A)
  valid.n   <- unname(which(k.n > 2))
  valid.m   <- unname(which(k.m > 2))
  pairs     <- which(A == 1, arr.ind=TRUE, useNames=FALSE)
  ######
  nvalid.pairs <- pairs[which(!(pairs[,1] %in% valid.n)),]
  valid.pairs  <- pairs[which(  pairs[,1] %in% valid.n) ,]
  nvalid.pairs <- rbind(nvalid.pairs, valid.pairs[which(!(valid.pairs[,2] %in% valid.m)),])
  valid.pairs  <- valid.pairs[which(valid.pairs[,2] %in% valid.m),]  
  n.v.pairs    <- nrow(valid.pairs)
  ######
  test.r   <- numeric(test.runs*k.fold); test.eP  <- numeric(test.runs*k.fold)
  test.eR  <- numeric(test.runs*k.fold); test.IL  <- numeric(test.runs*k.fold)
  test.hL  <- numeric(test.runs*k.fold)
  test.rac <- vector("list", test.runs*k.fold)
  real.index <- 1
  for (t.i in 1:test.runs) {
    folds        <- 1:k.fold
    id           <- sample(folds, n.v.pairs, replace=TRUE)
    for (test.fold in folds) {
      if (!all(is.null(progress))) {
        progress$inc(1/(test.runs*k.fold), detail=paste("Test",t.i,"Fold",test.fold))
      }
      test.set     <- subset(valid.pairs, id %in% test.fold)
      train.set    <- subset(valid.pairs, id %in% folds[-test.fold])
      tmp.M1       <- M1
      tmp.M2       <- M2
      for (s in 1:nrow(test.set)) {
        f <- ns[test.set[s,1]]
        e <- ms[test.set[s,2]]
        f.e <- names(which(M2[,e] == 1))
        tmp.M1[f,f.e][which(M1[f,f.e] == 1)] <- 0
      }
      new.A        <- tmp.M1 %*% tmp.M2
      new.A[new.A != 0] <- 1
      new.R        <- do.call(algorithm, list(tmp.M1, tmp.M2, ...))
      rem.e.usrs <- sort(unique(test.set[,2]))
      r  <- 0; rs <- 0; Pl <- 0; Rl <- 0; TP <- 0; FN <- 0
      for (i in rem.e.usrs) {
        RR <- sort(new.R[new.A[,i] == 0,i], decreasing=TRUE) #Sort list of i-th user
        D  <- test.set[test.set[,2] == i, ]; #Get deleted links for i-th user
        if (is.matrix(D)) { d.o <- ns[D[,1]]; } else { d.o <- ns[D[1]]; }
        p  <- which(names(RR) %in% d.o)        #recovered links for i-th user
        r  <- r + sum(p/(n-k.m[i]))                 #Recovery of deleted links
        rs <- rs + sum(RR[1:top.number]/(n-k.m[i])) #Average ranking score
        di <- length(p[p <= top.number])       #Mean precision and recall P(top.number) and R(top.number)
        Pl <- Pl + (di / top.number)           #Mean precision and recall P(top.number) and R(top.number)
        Rl <- Rl + (di / length(p))            #Mean precision and recall P(top.number) and R(top.number)
        TP <- TP + di                          #True Positive
        FN <- FN + length(p[p > top.number])   #False Positive
      }
      r  <- r  / nrow(test.set)       #Recovery of deleted links
      rs <- rs / length(rem.e.usrs)   #Average ranking score
      Pl <- Pl / length(rem.e.usrs)   #Mean precision and recall P(top.number) and R(top.number)
      Rl <- Rl / length(rem.e.usrs)   #Mean precision and recall P(top.number) and R(top.number)
      eP <- Pl * (n*m/nrow(test.set)) #Precision and recall enhancement, eP(top.number) and eR(top.number)
      eR <- Rl * (n/top.number)       #Precision and recall enhancement, eP(top.number) and eR(top.number)
      ###################################################################################################
      #Personalization h(top.number)
      hL <- 0; c  <- 0
      for (i in rem.e.usrs) {
        RRi <- names(sort(new.R[new.A[,i] == 0,i], decreasing=TRUE)[1:top.number])
        for (j in rem.e.usrs) {
          if (i != j) {
            RRj <- names(sort(new.R[new.A[,j] == 0,j], decreasing=TRUE)[1:top.number])
            qij <- length(intersect(RRi, RRj))
            hL  <- hL + (1-(qij/top.number))
            c   <- c+1
          }
        }
      }
      hL <- hL / c
      ###################################################################################################
      #Surprisal/novelty, I(L)
      I <- log2(m/k.n); IL <- 0
      I[is.infinite(I)] <- 0
      for (i in rem.e.usrs) {
        RR <- names(sort(new.R[new.A[,i] == 0,i], decreasing=TRUE)[1:top.number])
        IL <- IL + mean(I[RR])
      }
      IL <- IL / length(rem.e.usrs)
      test.r[real.index]     <- r ; test.eP[real.index] <- eP
      test.eR[real.index]    <- eR; test.IL[real.index] <- IL
      test.hL[real.index]    <- hL;
      test.rac[[real.index]] <- new.R
      real.index <- real.index + 1 
    }
  }
  return (data.frame(r=test.r, eP=test.eP, 
                     eR=test.eR, hL=test.hL, IL=test.IL))
}