calPerformance.merge.indep <-
function(lst, train.ind, test.ind, method,gn.nb)
{
	train = lst$mat[train.ind,]
	test = lst$mat[test.ind,]
	options(warn = -1)

        res = featureselection (train, lst$phyno$surv[train.ind], lst$phyno$censor[train.ind])
       	list.p = p.adjust (res$p, method = method)

	if (method == "none")
		list.p = order(res$p)[1:gn.nb]
	else
		list.p = (list.p <= 0.05)
	lp.train = res$coef[list.p]%*%t(train[,list.p])
	lp = res$coef[list.p]%*%t(test[,list.p])

	roc.fit =survivalROC (Stime = lst$phyno$surv[test.ind], status =lst$phyno$censor[test.ind], marker=lp, predict.time = mean(lst$phyno$surv[test.ind]), span = 0.25*NROW(test)^(-0.20))

	sgn = ifelse (lp < median(lp.train),0, 1)
	sgn = as.vector(sgn)

	cox.hr = coxph(Surv(lst$phyno$surv[test.ind],lst$phyno$censor[test.ind])~sgn)

	cat ("AUC\tHR(CI)\t\tP-val\n")
	cat (sprintf("%.2f",roc.fit$AUC), "\t", sprintf("%.2f",summary (cox.hr)[[6]][2]), "(", sprintf("%.2f",summary (cox.hr)[[7]][3]), "-", sprintf("%.2f",summary (cox.hr)[[7]][4]), ")\tp=", summary (cox.hr)[[6]][5],"\n",sep = "")
}

