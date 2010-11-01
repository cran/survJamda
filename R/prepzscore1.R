prepzscore1 <-
function (common.gene, geno.files, surv.data,x,y)
{
	x = union (y,x)

	fLst = vector("list", length(x))
	m = NULL
	surv = censor = NULL

	for (i in x){
		phyno = comb.surv.censor(geno.files,c(i),surv.data)
		lst = excl.missing(get(geno.files[i]),phyno)

		surv = c(surv, lst$phyno$surv)
		censor = c(censor, lst$phyno$censor)

		fLst[[i]] = znorm(lst$mat[,common.gene])
	
		m = rbind(m, fLst[[i]])
	}
	phyno = list(surv = surv, censor = censor)

        return(list(mat = m, phyno = phyno))
}

