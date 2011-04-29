predict.filter.NULL <-
function (object, newdata, ...) 
{
    return(newdata)
}
predict.filter.PCA <-
function (object, newdata, ...) 
{
    X_test = (data.frame(t(newdata)) - ((object$mod)$cent))/((object$mod)$norm)
    X_test_RF = (as.matrix(t(X_test))) %*% (as.matrix((object$mod)$co))/sqrt((object$mod)$eig[1:(object$nbreVarX)])
    return(X_test_RF)
}
predict.filter.RegressionTreeFilter <-
function (object, newdata, ...) 
{
    Nmax = object$nbreVarX
    a = newdata
    resume = TRUE
    shortResume = TRUE
    D = list()
    L = list()
    t = c(1:length(a))
    D[[1]] = t[1]
    D[[2]] = t[length(t)]
    L[[1]] = struct(g = integer(), d = integer())
    L[[1]]$g = t[1]
    L[[1]]$d = t[length(t)]
    ac = rep(mean(a), (t[length(t)]))
    Np = 1
    while (Np != Nmax) {
        if (is.struct(L[[1]])) {
            max = (L[[1]]$d - L[[1]]$g) * var(a[c(L[[1]]$g:L[[1]]$d)])
            ti = L[[1]]$g
            tj = L[[1]]$d
            LIndex = 1
            if (length(L) != 1) {
                for (i in 2:length(L)) {
                  if (is.struct(L[[i]])) {
                    tmp = (L[[i]]$d - L[[i]]$g) * var(a[c(L[[i]]$g:L[[i]]$d)])
                    if (tmp > max) {
                      max = tmp
                      ti = L[[i]]$g
                      tj = L[[i]]$d
                      LIndex = i
                    }
                  }
                }
            }
            L[[LIndex]] = NA
        }
        max = 0
        maxE = -1
        vartitj = (tj - ti) * var(a[which(t == ti):which(t == 
            tj)])
        for (e in which(t == ti):which(t == tj)) {
            varIE = var(a[ti:t[e]])
            if (is.na(varIE)) {
                varIE = 0
            }
            varEJ = var(a[t[e]:tj])
            if (is.na(varEJ)) {
                varEJ = 0
            }
            tmp = vartitj - (t[e] - ti) * varIE - (tj - t[e]) * 
                varEJ
            if (tmp > max) {
                max = tmp
                maxE = e
            }
        }
        e = maxE
        te = t[e]
        distIE = length(which(t == ti):which(t == te))
        distEJ = length(which(t == te):which(t == tj))
        ac[ti:te] = rep(mean(a[ti:te]), distIE)
        ac[te:tj] = rep(mean(a[te:tj]), distEJ)
        Np = Np + 1
        trouve = FALSE
        trouveI = -1
        for (i in 1:length(L)) {
            if (!is.struct(L[[i]]) && !trouve) {
                trouve = TRUE
                trouveI = i
            }
        }
        if (!trouve) {
            trouveI = length(L) + 1
        }
        L[[trouveI]] = struct(g = integer(), d = integer())
        L[[trouveI]]$g = ti
        L[[trouveI]]$d = te
        trouve = FALSE
        trouveI = -1
        for (i in 1:length(L)) {
            if (!is.struct(L[[i]]) && !trouve) {
                trouve = TRUE
                trouveI = i
            }
        }
        if (!trouve) {
            trouveI = length(L) + 1
        }
        L[[trouveI]] = struct(g = integer(), d = integer())
        L[[trouveI]]$g = te
        L[[trouveI]]$d = tj
        D[[length(D) + 1]] = te
    }
    if (!resume) {
        return(ac)
    }
    else {
        if (shortResume) {
            return(unique(ac))
        }
        else {
            resTmp = unique(ac)
            res = resTmp
            for (i in resTmp) {
                res = c(res, length(which(ac == i)))
            }
            return(res)
        }
    }
}
predict.filter.mRMR <-
function (object, newdata, ...) 
{
    return(newdata[, object$filter])
}
