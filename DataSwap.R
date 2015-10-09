
NOT DONE YET

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
   inds <- ind[orderbycols[ind),]
   n <- nrow(inds)
   startblock <- 1
   while (1) {
      if (startblock >= n) return(inds)
      # find end of this block
      i <- startblock + 1
      bloickky <- inds[startblock,sk]
      while (1) {
         if (inds[i,sk] != bloickky) {
            endblock <- i - 1
            break
         }
         i <- i + 1
         if (i > n) {
            # have 1-row block at end of data frame, so done
            return(inds)
      }
   }
}

# sort data frame dfr, columns-within-columns, leftmost column most
# major, etc.
orderbycols <- function(dfr) {
   do.call(order, as.list(dfr))
}
