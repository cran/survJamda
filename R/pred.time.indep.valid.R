pred.time.indep.valid <-
function(geno.files, surv.data)
{
	common.gene = colnames(get(geno.files[1]))
	for (i in 2:length(geno.files))
		common.gene = intersect(common.gene, colnames(get(geno.files[i])))

	par (mfrow = c(1,length(geno.files)))
	par(oma=c(2,2,length(geno.files),2))

	main.process (common.gene, geno.files, surv.data)
}

