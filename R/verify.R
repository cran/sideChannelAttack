verify.loo <-
function (model, filter, X, Y, nbreVarX, ...) 
{
    UseMethod("verify.loo")
}
verify.ho <-
function (model, filter, Xlearn, Ylearn, Xval, Yval, nbreVarX, ...) 
{
    UseMethod("verify.ho")
}
verify.cv <-
function (model, filter, X,Y, nbreVarX,k, ...) 
{
    UseMethod("verify.cv")
}