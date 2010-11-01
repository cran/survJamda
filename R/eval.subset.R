eval.subset <-
function (x, y, censor,iter, method, gn.nb, train.nb)
{
   call <- match.call()
    x <- as.matrix(x)
    n <- length(y)

	gn.lst = NULL
	 options(warn=-1) 

      	lst.samples = shuffle.samples(n, censor, train.nb)	
	train.ind = lst.samples$train.ind
	test.ind = lst.samples$test.ind
	
	lst = featureselection(x[train.ind,], y[train.ind],censor[train.ind])

	p.list <- p.adjust(lst$p,method=method)
	if (method == "none")
		p.list = order(p.list)[1:gn.nb]
	else{
		p.list = (p.list<= .05)	
		gn.nb = sum(p.list)
		cat ("Nb selected genes: ", gn.nb, "\n")
	}

  	
	lp.train = lst$coef[p.list]%*%t(x[train.ind,p.list])

	if (is.vector(x[test.ind,p.list]) && length(x[test.ind,p.list]) == length(lst$coef[p.list]))
		m = x[test.ind,p.list]
	else
		m = t(x[test.ind,p.list])

	lp = lst$coef[p.list]%*%m
    
	roc.fit =survivalROC (Stime = as.vector(y[test.ind]), status = as.vector(censor[test.ind]), marker=lp, predict.time = mean(y[test.ind]), span = 0.25*NROW(x[test.ind,])^(-0.20))

	sgn = ifelse (lp < median(lp.train),0, 1)
	sgn = as.vector(sgn)
	
	cox.hr = coxph(Surv(as.vector (y[test.ind]),as.vector (censor[test.ind]))~sgn)
        if(summary (cox.hr)[[6]][2] < 500){
		cat ("Iteration", iter,"\t", sprintf("%.2f",roc.fit$AUC), "\t", sprintf("%.2f",summary (cox.hr)[[6]][2]), "(", sprintf("%.2f",summary (cox.hr)[[7]][3]), "-", sprintf("%.2f",summary (cox.hr)[[7]][4]),")\t", summary (cox.hr)[[6]][5],"\n", sep = "")

		val = c(roc.fit$AUC, summary (cox.hr)[[6]][2])
	}
	else
		 val = c(0,0)

    	return(val)
}

