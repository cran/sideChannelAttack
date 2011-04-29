dpa1.default <-
function (x, y, ...) 
{
    if (!is.matrix(x)) {
        stop("'x' has to be a matrix")
        return(-1)
    }
	if (!is.factor(y)) {
        stop("'y' has to be a factor")
        return(-1)
    }
    if (dim(x)[1] != length(y)) {
        stop("the number of output has to be the same as the number of input")
        return(-1)
    }
    MoyenneDeLa <- list()
    for (cle in 1:2) {
        MoyenneDeLa[[cle]] <- matrix(ncol = dim(x)[2], nrow = 2, 
            data = 0)
        MoyenneDeLa[[cle]] = apply(as.matrix(x[which(y == (cle - 
            1)), ], ncol = dim(x)[2]), MARGIN = 2, FUN = mean)
    }
    res = list(mean = MoyenneDeLa)
    class(res) <- "dpa1"
    return(res)
}