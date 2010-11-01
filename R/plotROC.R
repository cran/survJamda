plotROC <-
function(test.ind,all.surv,all.censor, lp, file.name,col)
{
	roc.fit = NULL
	surv = all.surv[test.ind]
	censor = all.censor[test.ind]

	file.name = detFileName(file.name)

	excl.ind = excl.samples(test.ind,surv,censor)
	
	if (is.null(excl.ind)){
		ind = test.ind
		lp.ind = 1:length(test.ind)
	}
	else{
		first.ind = test.ind[1]-1
		ind = setdiff(test.ind,first.ind+excl.ind)
		lp.ind = ind-first.ind
	}


	surv.sorted = sort(all.surv[ind])

	for (t in surv.sorted){
		tmp = survivalROC (Stime = all.surv[ind], status =all.censor[ind], marker=lp[lp.ind], predict.time = t, span = 0.25*length(test.ind)^(-0.20))$AUC
		roc.fit = c(roc.fit, round(tmp,2))

	}

	if(col == "black")
		plot(surv.sorted,roc.fit,xlim = c(0, 120), ylim = c(0,1), xlab= "Time (months)", ylab="AUC", main = file.name, col = col,type = "l") 
	else			
		lines(surv.sorted,roc.fit,col = col)     
	
}

