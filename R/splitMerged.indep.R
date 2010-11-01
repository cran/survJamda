splitMerged.indep <-
function (files,lst, i,j, method,gn.nb)
{
	cat ("Train data sets: ")
	train.ind = det.set.ind(1,i)

	cat("Test data set: ", files[j], "\n")

	test.ind = det.set.ind(0,j)
	
	calPerformance.merge.indep(lst, train.ind, test.ind, method,gn.nb)
}

