calPerformance.auc.plot <-
function(lst, train.ind, test.ind, file.name,col, method)
{
	train = lst$mat[train.ind,]
	test = lst$mat[test.ind,]
	options(warn = -1)
        res = featureselection (train, lst$phyno$surv[train.ind], lst$phyno$censor[train.ind])

   	list.p = p.adjust (res$p, method = method)

	if (method == "none")
		list.p = order(res$p)[1:100]
	else
		list.p = (list.p <= 0.05)
	lp.train = res$coef[list.p]%*%t(train[,list.p])
	lp = res$coef[list.p]%*%t(test[,list.p])

	plotROC(test.ind,lst$phyno$surv,lst$phyno$censor, lp, file.name,col)
}

