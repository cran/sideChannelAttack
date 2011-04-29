verify.loo.default <-
function (model, filter, X, Y, nbreVarX, ...) 
{
    if (!is.vector(Y)) {
        stop("'Y' has to be a vector")
        return(-1)
    }
    if (!is.matrix(X)) {
        stop("'X' has to be a matrix")
        return(-1)
    }
    if (length(Y) != dim(X)[1]) {
        stop("the number of output has to be the same as the number of input")
        return(-1)
    }
    TP <- rep(0, length(nbreVarX))
    TN <- rep(0, length(nbreVarX))
    FN <- rep(0, length(nbreVarX))
    FP <- rep(0, length(nbreVarX))
    nbreVarX_dansV = 0
    for (nombreDeDimension in nbreVarX) {
        nbreVarX_dansV = nbreVarX_dansV + 1
        for (b in 1:length(Y)) {
            apprends = matrix(X[-b, ], ncol = dim(X)[2])
            verif = matrix(X[b, ], ncol = dim(X)[2])
            f = filter(X = apprends, Y = Y[-b], nbreVarX = nombreDeDimension)
			
            predicteur = model(x = matrix(predict(f, apprends),ncol=nombreDeDimension), y = factor(Y[-b]))
            p <- predict(predicteur, c(predict(f, verif)))
            p <- c((as.matrix(p))[1, 1])
            if (p == c("0")) 
                p <- 0
            else p <- 1
            if (zero(p) && zero(Y[b])) {
                TN[nbreVarX_dansV] <- TN[nbreVarX_dansV] + 1
            }
            if (zero(p) && !zero(Y[b])) {
                FN[nbreVarX_dansV] <- FN[nbreVarX_dansV] + 1
            }
            if (!zero(p) && zero(Y[b])) {
                FP[nbreVarX_dansV] <- FP[nbreVarX_dansV] + 1
            }
            if (!zero(p) && !zero(Y[b])) {
                TP[nbreVarX_dansV] <- TP[nbreVarX_dansV] + 1
            }
        }
    }
    res = list(TP = TP, TN = TN, FN = FN, FP = FP, dim = nbreVarX)
    class(res) <- "verify.loo"
    return(res)
}
verify.ho.default <-
function (model, filter, Xlearn, Ylearn, Xval, Yval, nbreVarX, ...) 
{
    if (!is.vector(Ylearn) || !is.vector(Yval)) {
        stop("'Y' has to be a vector")
        return(-1)
    }
    if (!is.matrix(Xlearn) || !is.matrix(Xval)) {
        stop("'X' has to be a matrix")
        return(-1)
    }
    if (( length(Ylearn) != dim(Xlearn)[1]) || ( length(Yval) != dim(Xval)[1])) {
        stop("the number of output has to be the same as the number of input")
        return(-1)
    }
    TP <- rep(0, length(nbreVarX))
    TN <- rep(0, length(nbreVarX))
    FN <- rep(0, length(nbreVarX))
    FP <- rep(0, length(nbreVarX))
    nbreVarX_dansV = 0
    for (nombreDeDimension in nbreVarX) {
        nbreVarX_dansV = nbreVarX_dansV + 1
		f = filter(X = Xlearn, Y = Ylearn, nbreVarX = nombreDeDimension)

		predicteur = model(x = matrix(predict(f, Xlearn),ncol=nombreDeDimension), y = factor(Ylearn))
		p <- predict(predicteur, matrix(predict(f, Xval),ncol=nombreDeDimension))
		p <- as.integer(p)-1
		TN[nbreVarX_dansV] <- TN[nbreVarX_dansV] + length( which(p==0 & Yval==0))
        FN[nbreVarX_dansV] <- FN[nbreVarX_dansV] + length( which(p==0 & Yval==1))
        FP[nbreVarX_dansV] <- FP[nbreVarX_dansV] + length( which(p==1 & Yval==0))
        TP[nbreVarX_dansV] <- TP[nbreVarX_dansV] + length( which(p==1 & Yval==1))
    }
    res = list(TP = TP, TN = TN, FN = FN, FP = FP, dim = nbreVarX)
    class(res) <- "verify.ho"
    return(res)
}
zero <-
function (X) 
{
    return(X < 0.5 && X > -0.5)
}
