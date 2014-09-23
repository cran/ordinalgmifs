plot.ordinalgmifs <-
function(x, type="trace", ...) {
	if (is.null(x$x)) {
		stop("Penalized model not requested\n")
	} else if (type=="trace") {
		matplot(x$beta, ylab=expression(hat(beta)), xlab="Step", type="l")
	} else if (type=="AIC") {
		plot(x$AIC, xlab="Step", ylab="AIC")
	} else if (type=="BIC") {
		plot(x$BIC, xlab="Step", ylab="BIC")
	} else if (type=="logLik") {
		plot(x$logLik, xlab="Step", ylab="logLikelihood")
	}
	if (x$probability.model=="Cumulative" | x$probability.model=="ForwardCR" | x$probability.model=="BackwardCR") {		
		title(paste(x$probability.model, "model using a", x$link, "link", sep=" "))
	} else if (x$probability.model=="AdjCategory") {
		title(paste(x$probability.model, "model using a loge link", sep=" "))		
	} else if (x$probability.model=="Stereotype") {
		title(paste(x$probability.model, "model using a logit link", sep=" "))		
	}
}
