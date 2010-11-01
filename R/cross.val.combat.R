cross.val.combat <-
function (x,y,censor,batchID, method,gn.nb,plot.roc, ngroup, iter)
{
	require(survivalROC)
	require(survival)
	
    call <- match.call()
    x <- as.matrix(x)
    n <- length(y)

    if (ngroup < 2) {
        stop("ngroup should be greater than or equal to 2")
    }
    if (ngroup > n) {
        stop("ngroup should be less than or equal to the number of observations"
)
    }
    if (ngroup == n) {
        groups <- sample(1:n)
        leave.out <- 1
    }
    if (ngroup < n) 
	groups = groups.cv (n, ngroup,censor) 

	 pred.fit = vector (length = ngroup)
 	sign = NULL
	all.fp = NULL
	all.tp = NULL
    	all.group = NULL

 	options(warn=-1) 

   for (j in 1:ngroup) {
	writeSamples (x[-groups[[j]],], batchID[-groups[[j]]], "trainSample")
	writeGeno(x[-groups[[j]],], "train")

	writeSamples (x[groups[[j]],], batchID[groups[[j]]], "testSample")
	writeGeno(x[groups[[j]],], "test")

	train.adj = compute.combat("train", "trainSample")
	train.adj = t(train.adj)

	test.adj = compute.combat("test", "testSample")
	test.adj = t(test.adj)

	lst = featureselection(train.adj, y[-groups[[j]]],censor[-groups[[j]]])

	p.list <- p.adjust(lst$p,method=method)
	if (method == "none")
		p.list = order(p.list)[1:gn.nb]
	else{
		p.list = (p.list<= .05)	
		gn.nb = sum(p.list)
		cat ("Selected genes nb: ", gn.nb, "\n")
	}

  	lp.train = lst$coef[p.list]%*%t(train.adj)[p.list,]

	if (is.vector(test.adj[,p.list]) && length(test.adj[,p.list]) ==length(lst$coef[p.list]))
		m = test.adj[,p.list]
	else
		m = t(test.adj[,p.list])

	lp = lst$coef[p.list]%*%m
	
	predict.time =  mean(y[groups[[j]]][censor[groups[[j]]]==1])

	roc.fit =survivalROC (Stime = y[groups[[j]]], status = censor[groups[[j]]], marker =lp, predict.time = predict.time, span = 0.25*NROW(test.adj)^(-0.20))
	pred.fit[j] = roc.fit$AUC

	all.fp = rbind(all.fp,roc.fit$FP)
	all.tp = rbind(all.tp,roc.fit$TP)

	sgn = ifelse (lp < median(lp.train),0, 1)
	sgn = as.vector(sgn)
	sign = c(sign, sgn)

	all.group = c(all.group,groups[[j]])
   	}
	if (plot.roc) lines(mean(as.data.frame(all.fp)),mean(as.data.frame(all.tp)), lty = 3)

	cox.hr = coxph(Surv(as.vector (y[all.group]),as.vector (censor[all.group]))~sign)

	cat ("Iteration", iter, "\t",sprintf("%.2f",mean(pred.fit)), "\t", sprintf("%.2f",summary (cox.hr)[[6]][2]), "(", sprintf("%.2f",summary (cox.hr)[[7]][3]), "-", sprintf("%.2f",summary (cox.hr)[[7]][4]),")\t", summary (cox.hr)[[6]][5],"\n", sep = "")

	val = c(mean(pred.fit), summary (cox.hr)[[6]][2])

    return(val)
}

