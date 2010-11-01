featureselection <-
function (gnExpMat, survivaltime, censor){
	require (survival)

        ploglik = NULL
        cox.coef = NULL

        max.col = ifelse (is.matrix(gnExpMat), ncol(gnExpMat), 1)

        for (i in 1:max.col){
                if(is.matrix(gnExpMat))
                        var = gnExpMat[,i]
                else
                        var = gnExpMat
                cox.t = coxph(Surv (survivaltime, censor)~var)          
                ltest <- -2 * (cox.t$loglik[1] - cox.t$loglik[2])
                pv <- 1 - pchisq(ltest, 1)
                ploglik = c (ploglik, pv)
                cox.coef = c(cox.coef, cox.t$coef)
        }
  return (list(coef = cox.coef, p = ploglik))
}

