
# author:  N. Matloff

# code to implement the data swapping method for statistical disclosure
# limitation

# arguments:
# 
#    ind: input data, a data frame or matrix
#    sk: swap keys
#    sa: swap attributes
#    sp: swap proportion

# return value:

#    the perturbed data set

# a proportion sp of the records are chosen for swapping; each selected record 
# is swapped with another record have the same values of sk; the
# variables that are swapped are specified in sa

dswap <- function(ind,sk,sa,sp=1.00) {
   inds <- ind[orderbycols(ind[,sk]),]
   n <- nrow(inds)
   blocks <- getblocks(inds,sk)
   nb <- nrow(blocks)
   for (i in 1:nb) {
      s <- blocks[i,1]
      e <- blocks[i,2]
      # which indices will be swapped?
      nswap <- ceiling(sp*(e-s+1))
      swapindices <- sample(s:e,nswap)
      # and where will they go?
      newindices <- sample(swapindices,nswap)
      # do the swap
      inds[swapindices,sa] <- inds[newindices,sa]
   }
   inds
}

# since inds has been sorted, records with like keys will be blocked
# together; this function finds the start and stop row numbers for the
# blocks
getblocks <- function(inds,sk) {
   n <- nrow(inds)
   blocks <- NULL
   startblock <- 1
   while (1) {  # each iteration deals with a new block
      # keys combination for this block
      blockkey <- as.numeric(inds[startblock,sk])
      # find end of this block
      if (startblock >= n) return(blocks)
      for (i in (startblock+1):n) {
# if (i == 100) browser()
         # each iteration looks at next record in block
         if (!identical(as.numeric(inds[i,sk]),blockkey)) {
            endblock <- i - 1
            break
         }
         if (i == n) endblock <- n
      }
      blocks <- rbind(blocks,c(startblock,endblock))
      startblock <- i + 1
   }
}

# sort data frame dfr, columns-within-columns, leftmost column most
# major, etc.
orderbycols <- function(dfr) {
   do.call(order, as.list(dfr))
}
