det.set.ind <- function(train,i)
{
   if(train){
           curr.ind = 0
           ind = NULL
          for (m in i){
		cat (geno.files[m], " ")
		if (m == 1)
                        ind = c(ind,1:nrow(get(geno.files[1])))
                else{
                        curr.ind = curr.ind+nrow(get(geno.files[m-1]))

                        ind = c(ind,(curr.ind+1):(curr.ind+nrow(get(
geno.files[m]))))
                }    
	}
 }
    else
               if(i == 1)
                   ind = 1:nrow(get(geno.files[1]))
               else{
                     curr.ind = 0
                     for(k in 2:i)
                        curr.ind = curr.ind+nrow(get(geno.files[k-1]))
                     ind = curr.ind:(curr.ind+nrow(get(geno.files[i])))      
               }
     return(ind)
}
