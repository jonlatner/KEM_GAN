.MPinverse <- function(eta, tol = sqrt(.Machine$double.eps)) {
	cov_eta <- cov(eta)
	ev <- eigen(cov_eta, TRUE)
	ev$values <- ifelse(ev$values > tol, 1/ev$values, 0)
	Sigma_inv <- crossprod(sqrt(ev$values)*(t(ev$vectors)))
	return(Sigma_inv)
}

.amelia.pmm <- function(dgp, imp, type, v, cats, command){ # v = column number of the variable being imputed; cats = columns for the indicator variables for a nominal variable
	cmeans <- vector("list", 5)
	mark <- vector("list", 5)
	draws <- vector("list", 5)
	if(type=="nominal") V <- cats else V <- v
	for(k in seq_along(imp$imputations)){
		data.cmu <- imp$imputations[[k]][,-V]
	 	mu <- imp$mu[,k]
	 	sigma <- imp$covMatrices[,,k]	
	 	B <- sigma[V,V]
	 	C <- sigma[V,-V,drop=FALSE]
	 	D <- sigma[-V,-V]
	 	CDinv <- C%*%solve(D)
		cMu <- apply(data.cmu, 1, FUN=function(x){c(mu[V] + CDinv %*% (x - mu[-V]))})
		cmeans[[k]] <- t(cMu)  #for each of 1000 rows, conditional for choice B, conditional for choice C this is also way there are 500 marks
		if(type=="nominal"){ 
			mark[[k]] <- apply(cmeans[[k]][dgp$miss,], 1, FUN=function(x){
				Sigma_inv <- .MPinverse(x-cmeans[[k]][!dgp$miss,])
				mdiff <- (x-cmeans[[k]][!dgp$miss,])
				MD <- mahalanobis(mdiff, rep(0,ncol(mdiff)), Sigma_inv, inverted=TRUE)
				return(which.min(MD))
			})
		} else mark[[k]] <- sapply(cmeans[[k]][dgp$miss], FUN=function(x){which.min(abs(x-cmeans[[k]][!dgp$miss]))}) 
		data2.cmu <- dgp$data$y_1[!dgp$miss]
		draws[[k]] <- data2.cmu[mark[[k]]]
		imp$imputations[[k]]$y_1 <- dgp$data$y_1
		imp$imputations[[k]]$y_1[dgp$miss] <- draws[[k]]
	 }
	 imp$mark <- mark
	 return(imp)
}
		
.dgp <- function(N, N_FULL, N_PARTIAL, restrict, ncat, type, pr_miss=.25, imp_meth="ppd", strong=0){
	if(type!="continuous" & type!="binary" & type!="ordinal" & type!="nominal") stop("Type must be continuous, binary, ordinal, or nominal")
	cpc <- (restrict=="none")
	
	rdf.names <- c(paste("x_", 1:(N_FULL + N_PARTIAL), sep=""), "y_1")
	f1 <- as.formula(paste("x_1 ~ y_1", paste(paste("x", 2:(N_FULL + N_PARTIAL), sep="_"), collapse="+"), sep="+"))
	f2 <- as.formula(paste("y_1", paste(paste("x", 1:(N_FULL + N_PARTIAL), sep = "_"), collapse = " + "), sep = " ~ "))
	
	if(type=="continuous"|type=="binary") rdf <- rdata.frame(N=N, n_full = N_FULL, n_partial=(N_PARTIAL+1), restrictions = restrict, 
		types = c(rep("continuous", (N_FULL+N_PARTIAL)), type), pr_miss=c(rep(.1, N_PARTIAL), .25), strong=strong, estimate_CPCs=cpc)
	if(type=="ordinal"|type=="nominal") rdf <- rdata.frame(N=N, n_full = N_FULL, n_partial=(N_PARTIAL+1), restrictions = restrict, 
		types = c(rep("continuous", (N_FULL+N_PARTIAL)), type), pr_miss=c(rep(.1, N_PARTIAL), .25), n_cat=ncat, strong=strong, estimate_CPCs=cpc)	
	
	if(cpc) nmar <- sqrt(mean(rdf$empirical_CPCs^2)) else nmar <- NA
	data <- rdf$obs; true <- rdf$true
	colnames(data) <- colnames(true) <- rdf.names
	obs <- !apply(data, 1, FUN=function(x) any(is.na(x))); miss <- is.na(data[,ncol(data)])
	if(type!="continuous") col <- cbind(1:sum(miss), sapply(true[["y_1"]][miss], FUN=function(x){which(x==levels(true[["y_1"]][miss]))}))
	
	RHS <- bayesglm(f1, data=true); RHS.fitted <- fitted(RHS)

	if (type=="continuous") {
		LHS <- bayesglm(f2, data=true)
		LHS.fitted <- fitted(LHS) 	
		ev <- eigen(vcov(LHS), symmetric = TRUE)	
		params <- (coef(LHS) + (ev$vectors %*% (sqrt(ev$values) * rnorm(length(coef(LHS)))))[,1])
		draws <- model.matrix(LHS)%*%params
		truematch <- sqrt(mean((draws[miss] - true$y_1[miss])^2))
		if(imp_meth=="pmm"){
			mark <- sapply((model.matrix(LHS)%*%params)[miss], FUN=function(m){which.min(abs(m-true$y_1[!miss]))})
			truematch <- sqrt(mean((true$y_1[miss] - true$y_1[!miss][mark])^2))
		}
		Pr <- NA
	} 
	if (type=="binary") {
		LHS <- bayesglm(f2, data=true, family=binomial(link="logit"))
		LHS.fitted <- model.matrix(LHS)%*%coef(LHS)
		ev <- eigen(vcov(LHS), symmetric = TRUE)
		params <- (coef(LHS) + (ev$vectors %*% (sqrt(ev$values) * rnorm(length(coef(LHS)))))[,1])
		eta <- model.matrix(LHS)%*%params
		Pr <- plogis(eta)
		draws <- Pr > runif(length(Pr))
		truematch <- mean(draws[miss] == true$y_1[miss])
		Pr <- cbind((1-Pr), Pr)[miss,]
		if(imp_meth=="pmm"){
			mark <- sapply(eta[miss], FUN=function(m){which.min(abs(m-eta[!miss]))})
			truematch <- mean(true$y_1[miss] == true$y_1[!miss][mark])
			Pr <- cbind((1-plogis(eta)), plogis(eta))[!miss,][mark,]	
		}
		Pr <- Pr[col]
	} 
	if (type=="ordinal") {
		LHS <- bayespolr(f2, data=true, drop.unused.levels=FALSE)
		LHS.fitted <- sapply(LHS$zeta, FUN=function(x){x - model.matrix(LHS)[,-1]%*%coef(LHS)}, simplify=TRUE)
		ev <- eigen(vcov(LHS), symmetric = TRUE)
		params <- (c(coef(LHS), LHS$zeta) + (ev$vectors %*% (sqrt(ev$values) * rnorm(length(c(coef(LHS), LHS$zeta)))))[,1])
		coef <- params[1:(length(params)-ncat+1)]; zeta <- params[(length(params)-ncat+2):length(params)]
		eta <- sapply(zeta, FUN=function(x){x - model.matrix(LHS)[,-1]%*%coef}, simplify=TRUE)
		eta <- cbind(0, plogis(eta), 1)
		Pr <- t(diff(t(eta)))
		draws <- apply(Pr, 1, FUN = function(p) which(rmultinom(1, 1, p) == 1))
		draws <-  factor(draws, ordered=TRUE)
		truematch <- mean(factor(toupper(letters[draws]), ordered=TRUE)[miss] == true$y_1[miss])
		Pr <- Pr[miss,]
		if(imp_meth=="pmm"){
			mark <- sapply((model.matrix(LHS)[,-1]%*%coef)[miss], FUN=function(m){which.min(abs(m-(model.matrix(LHS)[,-1]%*%coef)[!miss]))})
			truematch <- mean(true$y_1[miss] == true$y_1[!miss][mark])
			Pr <- cbind((1-t(diff(t(eta)))), t(diff(t(eta))))[!miss,][mark,]			
		}
		Pr <- Pr[col]
	} 
	if (type=="nominal") {
		text <- capture.output(LHS <- multinom(f2, data=true, maxit=1000))
		LHS.fitted <- model.matrix(LHS)%*%t(coef(LHS))
		params <- matrix(mvrnorm(1, c(t(coef(LHS))), vcov(LHS), tol=1e-10), ncol = nrow(as.matrix(coef(LHS))), byrow = FALSE)
		eta <- model.matrix(LHS) %*% params
		exp_eta <- matrix(pmin(.Machine$double.xmax / ncol(eta), cbind(1, exp(eta))), ncol = ncol(eta) + 1)
		denom <- rowSums(exp_eta)
		Pr <- exp_eta / denom
		draws <- apply(Pr, 1, FUN = function(p) which(rmultinom(1, 1, p) == 1))
		truematch <- mean(true$y_1[miss]==letters[draws[miss]])		
		Pr <- Pr[miss,]
		if(imp_meth=="pmm"){
			Sigma_inv <- .MPinverse(eta)      		
      		dist <- apply(eta[miss,], 1, FUN=function(x){
				mahalanobis(eta[!miss,], x, Sigma_inv, inverted=TRUE)
			})
			draws <- apply(dist, 2, which.min)
			truematch <- mean(rdf$true$y_1[miss]==rdf$true$y_1[!miss][draws])
      		Pr <- (exp_eta / denom)[!miss,][draws,]
		}		
		Pr <- Pr[col]
	}
	dgp <- list()
	dgp$true <- true$y_1
	dgp$truedata <- true
	dgp$data <- data
	dgp$miss <- miss
	dgp$obs <- rowSums(is.na(data))==0
	dgp$col <- col
	dgp$rhs <- RHS
	dgp$rhs.fitted <- RHS.fitted
	dgp$lhs <- LHS
	dgp$lhs.fitted <- LHS.fitted
	dgp$pr <- Pr
	dgp$truematch <- truematch
	dgp$f1 <- f1; dgp$f2 <- f2
	dgp$nvars <- N_FULL + N_PARTIAL +1
	dgp$type <- type
	dgp$imp_meth <- imp_meth
	dgp$nmar <- nmar
	dgp$rdf <- rdf
	return(dgp)	
}

.amelia <- function(dgp, imp_meth="ppd", m, ncat=3, command="amelia"){
	type <- dgp$type; imp_meth <- dgp$imp_meth
	data <- dgp$data
	v <- which(names(data)=="y_1")
	if(type=="binary") {
		data$y_1 <- as.numeric(data$y_1) - 1 
		ncat <- 2	
	}
	if(type=="ordinal") data$y_1 <- as.numeric(data$y_1)
	if(type=="nominal") {
		data <- cbind(data[,-v], sapply(levels(data$y_1), FUN=function(x){as.integer(data$y_1==x)}, simplify=TRUE)[,-1])
		cats <- (v:ncol(data))
		}					

	time1 <- proc.time() 
	if(command=="amelia") imp <- amelia(data, m=m, p2s = 0) else {
		imp <- list()
		imp$imputations <- vector("list", m)
		imp$mu <- matrix(, nrow=ncol(data), ncol=m)
		imp$covMatrices <- array(, dim = c(ncol(data), ncol(data), m))
		if(command=="mlest"){
			mvn <- mlest(data)
			for(k in 1:m){
				imp$imputations[[k]] <- data
				draws <- mvrnorm(n=nrow(data), mu=mvn$muhat, Sigma=mvn$sigmahat)
				imp$imputations[[k]][is.na(data)] <- draws[is.na(data)]
				imp$mu[,k] <- colMeans(imp$imputations[[k]])
				imp$covMatrices[,,k] <- cov(imp$imputations[[k]])
			}
		}
		if(command=="norm"){
			for(k in 1:m){
				prelim <- prelim.norm(as.matrix(data))
				text <- capture.output(thetahat <- em.norm(prelim))
				rngseed(round(runif(1, min=0, max=10000000)))
				draws <- imp.norm(prelim, thetahat, data)
				imp$imputations[[k]] <- data.frame(draws)
				imp$mu[,k] <- colMeans(draws)
				imp$covMatrices[,,k] <- cov(draws)
			}
		}
	}
	time2 <- proc.time()
	time <- as.numeric((time2-time1)[3])			
	class(imp$imputations) <- "list"
	
	if(type=="binary" | type=="ordinal"){
		res <- lapply(imp$imputations, FUN=function(x){
			p <- x$y_1
			if (type=="ordinal") p <- (p - min(data$y_1, na.rm=T))/(max(data$y_1, na.rm=T)-min(data$y_1, na.rm=T))
			p <- p*(p>0)*(p<1) + (p>1)
			d <- sapply(p, FUN=function(y){rbinom(1, (ncat-1), y)+1})
			pr <- t(sapply(p, FUN=function(y){dbinom(0:(ncat-1), (ncat-1), y)}, simplify=TRUE))
			res <- list()
			res$draws <- d[dgp$miss]
			res$pr <- pr
			return(res)
		})
	}

	if(type=="nominal") {
		res <- lapply(imp$imputations, FUN=function(x){
			g <- apply(x[,cats], 2, FUN=function(k){k*(k>0)*(k<1)+(k>1)})
			rs <- rowSums(g)
			rs <- (rs<=1) + rs*(rs>1)
			g <- g/rs
			gzero <- 1-rowSums(g)
			g <- cbind(gzero, g)
			if(any(g<0)) {
				g[g<0] <- 0
				g <- g/rowSums(g)
			}
			res <- list()
			res$draws <- apply(g, 1, FUN=function(y){which(rmultinom(1,1,y)==1)})[dgp$miss]
			res$pr <- g
			return(res)
		})
	}

	if(imp_meth=="pmm"){
		imp <- .amelia.pmm(dgp, imp, type=type, v=v, cats=cats, command=command)
		if(type!="continuous"){
			for(k in seq_along(res$pr)){
				res$pr[[k]][dgp$miss,] <- res$pr[[k]][!dgp$miss,][imp$mark,]
			}	
		}
	}	
	
	if(type=="binary"|type=="ordinal"){
		for(k in seq_along(imp$imputations)) {
			if(type=="binary") {
				if(imp_meth=="pmm") imp$imputations[[k]]$y_1[dgp$miss] <- as.logical(res[[k]]$draws - 1) else {
					imp$imputations[[k]]$y_1[dgp$miss] <- res[[k]]$draws
					imp$imputations[[k]]$y_1 <- as.logical(imp$imputations[[k]]$y_1)	
				}
			}
			if(type=="ordinal") {
				if(imp_meth=="pmm")	imp$imputations[[k]]$y_1[dgp$miss] <- toupper(letters[res[[k]]$draws]) else {
					imp$imputations[[k]]$y_1[dgp$miss] <- res[[k]]$draws
					imp$imputations[[k]]$y_1 <- ordered(imp$imputations[[k]]$y_1, labels=toupper(letters[1:ncat]))
				}
			}
		}
	}
	if(type=="nominal") {		
		for(k in seq_along(imp$imputations)){
			imp$imputations[[k]] <- imp$imputations[[k]][,-cats]
			imp$imputations[[k]]$y_1 <- dgp$data$y_1
			imp$imputations[[k]]$y_1[dgp$miss] <- letters[res[[k]]$draws]
		}
	}	
	
	if(type!="continuous"){
		for(k in seq_along(res)){
			res[[k]]$pr <- res[[k]]$pr[dgp$miss,][dgp$col]
		}	
		Pr.rmse <- sqrt(mean(sapply(res, FUN=function(x){(x$pr-dgp$pr)^2})))
		Pr.bias <- abs(mean(sapply(res, FUN=function(x){abs(mean(x$pr)-mean(dgp$pr))})))
	} 

	if (type=="continuous") {
		mvmatch <- mean(sapply(imp$imputations, FUN = function(d){sqrt(mean(((d$y_1 - dgp$true)^2)[dgp$miss]))}))
		mvmatch.bias <- mean(sapply(imp$imputations, FUN = function(d){abs(mean(d$y_1[dgp$miss]) - mean(dgp$true[dgp$miss]))}))
	} else {
		mvmatch <- mean(sapply(imp$imputations, FUN = function(d){mean((d$y_1==dgp$true)[dgp$miss])}))
		mvmatch.bias <- NA	
	}
	
	RHS <- pool(dgp$f1, data=imp$imputations, m=5, FUN=bayesglm)
	coef1 <- sqrt(mean((RHS@coefficients - coef(dgp$rhs))^2))
	coef1.bias <- abs(mean(RHS@coefficients) - mean(coef(dgp$rhs)))
	coef1.mns <- mahalanobis(x=coef(dgp$rhs), center=RHS@coefficients, cov=vcov(dgp$rhs))
	eta <- model.matrix(dgp$rhs)%*%RHS@coefficients 
	fit1.av <- sqrt(mean((eta[dgp$obs] - dgp$rhs.fitted[dgp$obs])^2))
	fit1.av.bias <- abs(mean(eta[dgp$obs]) - mean(dgp$rhs.fitted[dgp$obs]))
	fit1.all <- sqrt(mean((eta - dgp$rhs.fitted)^2))
	fit1.all.bias <- abs(mean(eta) - mean(dgp$rhs.fitted))
	
	if(type=="continuous"){
		Pr.rmse <- NA; Pr.bias <- NA
		LHS <- pool(dgp$f2, data=imp$imputations, m=5, FUN=bayesglm)
		coef2 <- sqrt(mean((LHS@coefficients - coef(dgp$lhs))^2))
		coef2.bias <- abs(mean(LHS@coefficients) - mean(coef(dgp$lhs)))
		coef2.mns <- mahalanobis(x=coef(dgp$lhs), center=LHS@coefficients, cov=vcov(dgp$lhs))
		eta <- model.matrix(dgp$lhs)%*%LHS@coefficients 
		fit2.av <- sqrt(mean((eta[dgp$obs] - dgp$lhs.fitted[dgp$obs])^2))
		fit2.av.bias <- abs(mean(eta[dgp$obs]) - mean(dgp$lhs.fitted[dgp$obs]))	
		fit2.all <- sqrt(mean((eta - dgp$lhs.fitted)^2))
		fit2.all.bias <- abs(mean(eta) - mean(dgp$lhs.fitted))		
	}
	if(type=="binary"){
		LHS <- pool(dgp$f2, data=imp$imputations, m=5, FUN=bayesglm, family=binomial(link="logit"))
		coef2 <- sqrt(mean((LHS@coefficients - coef(dgp$lhs))^2))
		coef2.bias <- abs(mean(LHS@coefficients) - mean(coef(dgp$lhs)))
		coef2.mns <- mahalanobis(x=coef(dgp$lhs), center=LHS@coefficients, cov=vcov(dgp$lhs))
		eta <- model.matrix(dgp$lhs)%*%LHS@coefficients 
		fit2.av <- sqrt(mean((eta[dgp$obs] - dgp$lhs.fitted[dgp$obs])^2))
		fit2.av.bias <- abs(mean(eta[dgp$obs]) - mean(dgp$lhs.fitted[dgp$obs]))	
		fit2.all <- sqrt(mean((eta - dgp$lhs.fitted)^2))
		fit2.all.bias <- abs(mean(eta) - mean(dgp$lhs.fitted))		
	}
	if(type=="ordinal"){
		LHS <- pool(dgp$f2, data=imp$imputations, m=5, FUN=bayespolr) 
		coef <- LHS@coefficients[1:(dgp$nvars-1)]
		zeta <- LHS@coefficients[(dgp$nvars):length(LHS@coefficients)]
		coef2 <- sqrt(mean((coef - coef(dgp$lhs))^2)) 
		coef2.bias <- abs(mean(coef) - mean(coef(dgp$lhs))) 
		coef2.mns <- mahalanobis(x=coef(dgp$lhs)[1:(dgp$nvars-1)], center=LHS@coefficients[1:(dgp$nvars-1)], cov=vcov(dgp$lhs)[1:(dgp$nvars-1),1:(dgp$nvars-1)])
		eta <- sapply(zeta, FUN=function(x){x - model.matrix(dgp$lhs)[,-1]%*%coef}, simplify=TRUE)
		fit2.av <- sqrt(mean((eta[dgp$obs,] - dgp$lhs.fitted[dgp$obs,])^2))
		fit2.av.bias <- abs(mean(eta[dgp$obs,]) - mean(dgp$lhs.fitted[dgp$obs,]))	
		fit2.all <- sqrt(mean((eta - dgp$lhs.fitted)^2))
		fit2.all.bias <- abs(mean(eta) - mean(dgp$lhs.fitted))	
	}
	if(type=="nominal"){
		text <- capture.output(LHS <- pool(dgp$f2, data=imp$imputations, m=5, FUN=multinom, maxit=1000))
		coef <- matrix(LHS@coefficients,(ncat-1),(dgp$nvars),byrow=TRUE)
		coef2 <- sqrt((mean((coef - coef(dgp$lhs))^2)))
		coef2.bias <- abs(mean(coef) - mean(coef(dgp$lhs)))
		coef2.mns <- mahalanobis(x=c(t(coef(dgp$lhs))), center=LHS@coefficients, cov=vcov(dgp$lhs))
		eta <- model.matrix(dgp$lhs)%*%t(coef)
		fit2.av <- sqrt(mean((eta[dgp$obs,] - dgp$lhs.fitted[dgp$obs,])^2))
		fit2.av.bias <- abs(mean(eta[dgp$obs,]) - mean(dgp$lhs.fitted[dgp$obs,]))	
		fit2.all <- sqrt(mean((eta - dgp$lhs.fitted)^2))
		fit2.all.bias <- abs(mean(eta) - mean(dgp$lhs.fitted))	
	}
	res <- list()
	res$time <- time
	res$mvmatch <- mvmatch; res$mvmatch.bias <- mvmatch.bias
	res$pr.rmse <- Pr.rmse; res$pr.bias <- Pr.bias
	res$coef1 <- coef1; res$coef1.bias <- coef1.bias
	res$coef2 <- coef2; res$coef2.bias <- coef2.bias
	res$coef1.mns <- coef1.mns; res$coef2.mns <- coef2.mns
	res$fit1.all <- fit1.all; res$fit1.all.bias <- fit1.all.bias
	res$fit2.all <- fit2.all; res$fit2.all.bias <- fit2.all.bias
	res$fit1.av <- fit1.av; res$fit1.av.bias <- fit1.av.bias
	res$fit2.av <- fit2.av; res$fit2.av.bias <- fit2.av.bias
	res$data <- imp$imputations
	res$lhs <- LHS
	res$rhs <- RHS
	return(res)
}
									
.mi <- function(dgp, m, imp_meth="ppd", chains=5, iter=30, est="MNL", usena=FALSE, ncat=3, mcar=FALSE){
	type <- dgp$type; imp_meth <- dgp$imp_meth
	mdf <- missing_data.frame(dgp$data)
	if(!mcar){
		mdf@variables[["y_1"]]@imputation_method <- imp_meth
		if(type=="nominal"){
			mdf@variables[["y_1"]]@estimator <- est
			mdf@variables[["y_1"]]@use_NA <- usena
		}	
	} else iter <- 0
	time1 <- proc.time(); imp <- mi(mdf, n.chains=chains, n.iter=iter, verbose = FALSE, save_RAM=TRUE, debug=TRUE); time2 <- proc.time()
	time <- as.numeric((time2-time1)[3])			
	complete <- complete(imp, m)

	Pr <- vector("list", chains)
	for(k in 1:chains){
		if(type=="binary") Pr[[k]] <- cbind(1-imp@data[[k]]@variables$y_1@fitted, imp@data[[k]]@variables$y_1@fitted)[dgp$miss]
		else if(type=="ordinal"|type=="nominal") Pr[[k]] <- (imp@data[[k]]@variables$y_1@fitted)[dgp$miss,]
		if(type!="continuous") Pr[[k]] <- Pr[[k]][dgp$col]
	}

	if (type=="continuous") {
		Pr.rmse <- NA; Pr.bias <- NA
		mvmatch <- mean(sapply(complete, FUN = function(d){sqrt(mean(((d$y_1 - dgp$true)^2)[dgp$miss]))}))
		mvmatch.bias <- mean(sapply(complete, FUN = function(d){abs(mean(d$y_1[dgp$miss]) - mean(dgp$true[dgp$miss]))}))
											
	} else {
		Pr.rmse <- sqrt(mean(sapply(Pr, FUN=function(x){(x-dgp$pr)^2})))
		Pr.bias <- abs(mean(sapply(Pr, FUN=function(x){abs(mean(x)-mean(dgp$pr))})))
		mvmatch <- mean(sapply(complete, FUN = function(d){mean((d$y_1==dgp$true)[dgp$miss])})) 
		mvmatch.bias <- NA
	}
	
	RHS <- pool(dgp$f1, data=imp, m=5, FUN=bayesglm)
	coef1 <- sqrt(mean((RHS@coefficients - coef(dgp$rhs))^2))
	coef1.bias <- abs(mean(RHS@coefficients) - mean(coef(dgp$rhs)))
	coef1.mns <- mahalanobis(x=coef(dgp$rhs), center=RHS@coefficients, cov=vcov(dgp$rhs))
	eta <- model.matrix(dgp$rhs)%*%RHS@coefficients 
	fit1.av <- sqrt(mean((eta[dgp$obs] - dgp$rhs.fitted[dgp$obs])^2))
	fit1.av.bias <- abs(mean(eta[dgp$obs]) - mean(dgp$rhs.fitted[dgp$obs]))
	fit1.all <- sqrt(mean((eta - dgp$rhs.fitted)^2))
	fit1.all.bias <- abs(mean(eta) - mean(dgp$rhs.fitted))
	
	if(type=="continuous"){
		LHS <- pool(dgp$f2, data=imp, m=5, FUN=bayesglm)
		coef2 <- sqrt(mean((LHS@coefficients - coef(dgp$lhs))^2))
		coef2.bias <- abs(mean(LHS@coefficients) - mean(coef(dgp$lhs)))
		coef2.mns <- mahalanobis(x=coef(dgp$lhs), center=LHS@coefficients, cov=vcov(dgp$lhs))
		eta <- model.matrix(dgp$lhs)%*%LHS@coefficients 
		fit2.av <- sqrt(mean((eta[dgp$obs] - dgp$lhs.fitted[dgp$obs])^2))
		fit2.av.bias <- abs(mean(eta[dgp$obs]) - mean(dgp$lhs.fitted[dgp$obs]))	
		fit2.all <- sqrt(mean((eta - dgp$lhs.fitted)^2))
		fit2.all.bias <- abs(mean(eta) - mean(dgp$lhs.fitted))			
	}
	if(type=="binary"){
		LHS <- pool(dgp$f2, data=imp, m=5, FUN=bayesglm, family=binomial(link="logit"))
		coef2 <- sqrt(mean((LHS@coefficients - coef(dgp$lhs))^2))
		coef2.bias <- abs(mean(LHS@coefficients) - mean(coef(dgp$lhs)))
		coef2.mns <- mahalanobis(x=coef(dgp$lhs), center=LHS@coefficients, cov=vcov(dgp$lhs))
		eta <- model.matrix(dgp$lhs)%*%LHS@coefficients 
		fit2.av <- sqrt(mean((eta[dgp$obs] - dgp$lhs.fitted[dgp$obs])^2))
		fit2.av.bias <- abs(mean(eta[dgp$obs]) - mean(dgp$lhs.fitted[dgp$obs]))	
		fit2.all <- sqrt(mean((eta - dgp$lhs.fitted)^2))
		fit2.all.bias <- abs(mean(eta) - mean(dgp$lhs.fitted))		
	}
	if(type=="ordinal"){
		LHS <- pool(dgp$f2, data=imp, m=5, FUN=bayespolr) 
		coef <- LHS@coefficients[1:(dgp$nvars-1)]
		zeta <- LHS@coefficients[(dgp$nvars):length(LHS@coefficients)]
		coef2 <- sqrt(mean((coef - coef(dgp$lhs))^2)) 
		coef2.bias <- abs(mean(coef) - mean(coef(dgp$lhs)))
		coef2.mns <-mahalanobis(x=coef(dgp$lhs)[1:(dgp$nvars-1)], center=LHS@coefficients[1:(dgp$nvars-1)], cov=vcov(dgp$lhs)[1:(dgp$nvars-1),1:(dgp$nvars-1)])
		eta <- sapply(zeta, FUN=function(x){x - model.matrix(dgp$lhs)[,-1]%*%coef}, simplify=TRUE)
		fit2.av <- sqrt(mean((eta[dgp$obs,] - dgp$lhs.fitted[dgp$obs,])^2))
		fit2.av.bias <- abs(mean(eta[dgp$obs,]) - mean(dgp$lhs.fitted[dgp$obs,]))	
		fit2.all <- sqrt(mean((eta - dgp$lhs.fitted)^2))
		fit2.all.bias <- abs(mean(eta) - mean(dgp$lhs.fitted))	
	}
	if(type=="nominal"){
		text <- capture.output(LHS <- pool(dgp$f2, data=imp, m=5, FUN=multinom, maxit=1000))
		coef <- matrix(LHS@coefficients,(ncat-1),(dgp$nvars),byrow=TRUE)
		coef2 <- sqrt(mean((coef - coef(dgp$lhs))^2))
		coef2.bias <- abs(mean(coef) - mean(coef(dgp$lhs)))
		coef2.mns <- mahalanobis(x=c(t(coef(dgp$lhs))), center=LHS@coefficients, cov=vcov(dgp$lhs))
		eta <- model.matrix(dgp$lhs)%*%t(coef)
		fit2.av <- sqrt(mean((eta[dgp$obs,] - dgp$lhs.fitted[dgp$obs,])^2))
		fit2.av.bias <- abs(mean(eta[dgp$obs,]) - mean(dgp$lhs.fitted[dgp$obs,]))	
		fit2.all <- sqrt(mean((eta - dgp$lhs.fitted)^2))
		fit2.all.bias <- abs(mean(eta) - mean(dgp$lhs.fitted))	
	}			
	res <- list()
	res$time <- time
	res$mvmatch <- mvmatch; res$mvmatch.bias <- mvmatch.bias
	res$pr.rmse <- Pr.rmse; res$pr.bias <- Pr.bias
	res$coef1 <- coef1; res$coef1.bias <- coef1.bias
	res$coef2 <- coef2; res$coef2.bias <- coef2.bias
	res$coef1.mns <- coef1.mns; res$coef2.mns <- coef2.mns
	res$fit1.all <- fit1.all; res$fit1.all.bias <- fit1.all.bias
	res$fit2.all <- fit2.all; res$fit2.all.bias <- fit2.all.bias
	res$fit1.av <- fit1.av; res$fit1.av.bias <- fit1.av.bias
	res$fit2.av <- fit2.av; res$fit2.av.bias <- fit2.av.bias
	return(res)		
}

.completecase <- function(dgp){
	type <- dgp$type; imp_meth <- dgp$imp_meth
	RHS <- bayesglm(dgp$f1, data=dgp$data)
	if(type=="continuous") LHS <- bayesglm(dgp$f2, data=dgp$data)
	if(type=="binary") LHS <- bayesglm(dgp$f2, data=dgp$data, family=binomial(link="logit"), drop.unused.levels=FALSE)
	if(type=="ordinal") LHS <- bayespolr(dgp$f2, data=dgp$data, drop.unused.levels=FALSE)
	if(type=="nominal") text <- capture.output(LHS <- multinom(dgp$f2, data=dgp$data, maxit=1000))
	coef1 <- sqrt(mean((coef(RHS) - coef(dgp$rhs))^2))
	coef1.bias <- abs(mean(coef(RHS)) - mean(coef(dgp$rhs)))
	coef1.mns <- mahalanobis(x=coef(dgp$rhs), center=coef(RHS), cov=vcov(dgp$rhs))
	coef2 <- sqrt(mean((coef(LHS) - coef(dgp$lhs))^2))
	coef2.bias <- abs(mean(coef(LHS)) - mean(coef(dgp$lhs)))
	if(type!="nominal") {
		coef2.mns <- mahalanobis(x=coef(dgp$lhs)[1:(dgp$nvars-1)], center=coef(LHS)[1:(dgp$nvars-1)], cov=vcov(dgp$lhs)[1:(dgp$nvars-1),1:(dgp$nvars-1)])
	} else coef2.mns <- mahalanobis(x=c(t(coef(dgp$lhs))), center=c(t(coef(LHS))), cov=vcov(dgp$lhs)) 
	fit1.av <- sqrt(mean((fitted(RHS) - dgp$rhs.fitted[dgp$obs])^2))
	fit1.av.bias <- abs(mean(fitted(RHS)) - mean(dgp$rhs.fitted[dgp$obs]))
	
	if(type=="continuous" | type=="binary") {
		fit2.av <- sqrt(mean((fitted(LHS) - dgp$lhs.fitted[dgp$obs])^2))
		fit2.av.bias <- abs(mean(fitted(LHS)) - mean(dgp$lhs.fitted[dgp$obs]))	
	}
	if(type=="ordinal"){
		ld <- sapply(LHS$zeta, FUN=function(x){x - model.matrix(LHS)[,-1]%*%coef(LHS)}, simplify=TRUE)
		fit2.av <- sqrt(mean((ld - dgp$lhs.fitted[dgp$obs])^2))	
		fit2.av.bias <- abs(mean(ld[dgp$miss]) - mean(dgp$lhs.fitted[dgp$obs]))		
	}
	if(type=="nominal"){
		ld <- model.matrix(LHS)%*%t(coef(LHS))
		fit2.av <- sqrt(mean((ld - dgp$lhs.fitted[dgp$obs])^2))
		fit2.av.bias <- abs(mean(ld) - mean(dgp$lhs.fitted[dgp$obs]))		
	}
	res <- list()
	res$coef1 <- coef1; res$coef1.bias <- coef1.bias
	res$coef2 <- coef2; res$coef2.bias <- coef2.bias
	res$coef1.mns <- coef1.mns; res$coef2.mns <- coef2.mns
	res$fit1.av <- fit1.av; res$fit1.av.bias <- fit1.av.bias
	res$fit2.av <- fit2.av; res$fit2.av.bias <- fit2.av.bias
	return(res)
}
