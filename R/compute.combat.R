compute.combat <-
function (fileGeno, fileSample)
{
	mat.adj  = ComBat(fileGeno, fileSample, write = FALSE, filter = FALSE, prior.plots= FALSE, skip = 1, par.prior = TRUE)

	mat.adj = as.matrix(mat.adj[,2:ncol(mat.adj)])
	return (mat.adj)
}

