splitMerged.auc.plot <-
function (geno.files,lst, i,j,col, method)
{
	normalization = ifelse(col == "black", "ComBat", "Zscore1")
	cat ("Normalization = ", normalization, "\n")

	cat ("Train data sets: ")
	train.ind = det.set.ind(1,i)

	cat("Test data set: ", geno.files[j], "\n")

	test.ind = det.set.ind(0,j)


	calPerformance.auc.plot(lst, train.ind, test.ind, geno.files[j],col, method)
}

