calPerformance.single.indep <-
function(lst1, lst2, method,gn.nb)
{
	train.vec = lst1$phyno
	test.vec = lst2$phyno
	options(warn = -1)

	lst = featureselection (lst1$mat, train.vec$surv,train.vec$censor)

	list.p = p.adjust (lst$p, method = method)

	if (method == "none")
		list.p = order(lst$p)[1:gn.nb]
	else
		list.p = list.p <= 0.05
	lp.train = lst$coef[list.p]%*%t(lst1$mat[,list.p])
	lp = lst$coef[list.p]%*%t(lst2$mat[,list.p])

	roc.fit =survivalROC (Stime = test.vec$surv, status = test.vec$censor, marker=lp, predict.time = mean(test.vec$surv), span = 0.25*NROW(lst2$mat)^(-0.20))

	sgn = ifelse (lp < median(lp.train),0, 1)
	sgn = as.vector(sgn)

	cox.hr = coxph(Surv(test.vec$surv, test.vec$censor)~sgn)

	cat ("AUC\tHR(CI)\t\t\tP-val\n")
        cat (sprintf("%.2f",roc.fit$AUC), "\t", sprintf("%.2f",summary (cox.hr)[[6]][2]), "(", sprintf("%.2f",summary (cox.hr)[[7]][3]), "-", sprintf("%.2f",summary (cox.hr)[[7]][4]),")\t\tp=", summary (cox.hr)[[6]][5],"\n",sep = "")
}

