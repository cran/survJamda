prepcombat <-
function (common.gene,geno.files,surv.data, batchID,x,y)
{
	x = union(y,x)	
	m = NULL
	for (i in x)
		m = rbind(m, get(geno.files[i])[,common.gene])
	
	phyno = comb.surv.censor(geno.files,x,surv.data)

	lst = excl.missing(m,phyno)
	writeSamples(lst$mat, batchID, "sampleMerge")

       writeGeno(lst$mat, "genoMerge")
       mat = compute.combat ("genoMerge", "sampleMerge")
        return(list(mat = t(mat), phyno = lst$phyno))
}

