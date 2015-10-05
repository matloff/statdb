
# code to implement NeighborSynth method for data disclosure avoidance

# arguments:

#    z:  database, n x p
#    eps:  radius of each neighborhood
#    modprop:  proportion of records that will be changed 
#    wts:  vector, stating which variables are to be weighted, and with
#          what weights; e.g. (5,12,13,0.2,0.2,0.5) would indicate that
#          the variables in columns 5, 12 and 13 are to be weighted by
#          factors 0.2, 0.2 and 0.5, respectively, relative to the other
#          variables in the distance calculations
#    cls: R 'parallel' cluster

# value:  perturbed database

nbrs <- 
      function(z,eps,modprop=1.00,wts=NULL,cls=NULL) {
   require(pdist)
   if (!is.null(cls)) {
      require(partools)
      mypdist <- parpdist
   } else mypdist <- pdist
   n <- nrow(z)
   p <- ncol(z)
   zperturb <- z  # eventual output
   zsave <- z
   z <- scale(z)
   if (!is.null(wts)) {
      nwtd <- length(wts) / 2
      wtd <- wts[1:nwtd]
      for (k in 1:nwtd) {
         j <- wts[k]
         r <- wts[k+nwtd]
         z[,j] <- r * z[,j]
      }
   }
   # how many records in z will be changed?
   modn <- floor(n * modprop)
   # which ones?
   modidxs <- sample(1:n,modn)
   for (i in modidxs) {
      # find the neighbors of this vector
      x <- z[i,]
      # find distances from x to all rows of z
      dxz <- as.matrix(mypdist(x,z)@dist)
      # who is near x, other than itself?
      xnb <- which(dxz < eps)
      xnb <- setdiff(xnb,i)
      # nobody?
      if (length(xnb) == 0) {
         zperturb[i,] <- NA
         next
      }
      # back in the original matrix, select the desired neighborhood
      zneigh <- zsave[xnb,,drop=FALSE]
      nzn <- nrow(zneigh)
      for (j in 1:p) {
         newi <- sample(1:nzn,1)
         zperturb[i,j] <- zneigh[newi,j]
      }
   }
   # return the perturbed data, including unperturbed records; note that
   # row names will be original
   zperturb
}

sim <- function(p1,eps,wts,wks) {
   p1p <- nbrs(p1,eps=eps,wts=wts)
   print(eps)
   print(wts)
   print(lm(wageinc ~ .,data=p1))
   print(lm(wageinc ~ .,data=p1p))
   p1pc <- na.omit(p1p)
   print(p1[p1$wkswrkd == wks,])
   print(p1pc[p1pc$wkswrkd == wks,])
}

