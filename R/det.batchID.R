det.batchID <- function (geno.files)
{
       batchID = NULL
 
        for (i in 1:seq_along(geno.files))
                 batchID = c(batchID,rep(i,nrow(get(geno.files[i]))))
      return(batchID)
}
