predict.dpa1 <-
function (object, newdata, ...) 
{
    maximum = -1
    maximumIndice = -1
    for (supposonsLaclCle in 1:2) {
        corr <- cor(newdata, (object$mean)[[supposonsLaclCle]])
        if (maximum < corr) {
            maximumIndice = supposonsLaclCle
            maximum = corr
        }
    }
    return(maximumIndice - 1)
}
