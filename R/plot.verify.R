plot.verify.loo <-
function (x, ...) 
{
    object = x
    Y = (object$TP + object$TN)/(object$TP + object$TN + object$FP + 
        object$FN)
    X = object$dim
    plot(x = X, y = Y, type = "l", main = "% of good answers", 
        xlab = "number of points of each side channel", ylab = "% of good answers")
    points(x = X, y = Y)
}
plot.verify.ho <-
function (x, ...) 
{
    object = x
    Y = (object$TP + object$TN)/(object$TP + object$TN + object$FP + 
								 object$FN)
    X = object$dim
    plot(x = X, y = Y, type = "l", main = "% of good answers", 
		 xlab = "number of points of each side channel", ylab = "% of good answers")
    points(x = X, y = Y)
}
