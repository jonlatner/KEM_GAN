# Top commands ----

# Create empty R application (no figures, data frames, packages, etc.)
# Get a list of all loaded packages
packages <- search()[grepl("package:", search())]
# Unload each package
for (package in packages) {
  unloadNamespace(package)
}

rm(list=ls(all=TRUE))

rm(list=ls())
library(mi)
library(Amelia)
library(mvnmle)
library(MASS)
library(norm)
library(nnet)
library(arm)


# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"

setwd(main_dir)

source("do_files/gelman/helper.R")

nfull <- 3L
part <- 1
iterations <- 5
restrict <- "triangular"
if(restrict=="triangular") NMAR <- FALSE
if(restrict=="none") NMAR <- TRUE
N <- 1000
str <- 0

results <- as.numeric()
resfile <- paste(c("results",part,".csv"), collapse="")

resnames <- c("iteration", "variable.type", "N", "n.cat", "n.partial", "NMAR", "strong", "mean.CPC", "method", "imputation.method", 
	"mv.match", "mv.match.bias", "Pr.rmse", "Pr.bias", "coef1", "coef1.bias", "coef1.mns", "coef2", "coef2.bias", "coef2.mns", 
	"fit1.all", "fit1.all.bias", "fit2.all", "fit2.all.bias", "fit1.av", "fit1.av.bias", "fit2.av", "fit2.av.bias", "time")

for(it in 1:iterations) {
	itnum <- it + (part-1)*iterations
	for(npart in c(0,2,4,6)){
		for(im in c("ppd", "pmm")){
				
	#CONTINUOUS
			dgp.co <- .dgp(N=N, N_FULL=nfull, N_PARTIAL=npart, restrict=restrict, type="continuous", pr_miss=.25, imp_meth=im, strong=str)
			
			cc.res <- .completecase(dgp.co)
			results <- rbind(results, c(itnum, dgp.co$type, N, NA, npart, NMAR, str, dgp.co$nmar, "Complete Cases", im,  NA, 
					NA, NA, NA, cc.res$coef1, cc.res$coef1.bias, cc.res$coef1.mns, cc.res$coef2, cc.res$coef2.bias, cc.res$coef2.mns, 
					NA, NA, NA, NA, cc.res$fit1.av, cc.res$fit1.av.bias, cc.res$fit2.av, cc.res$fit2.av.bias, NA))
					
			amelia.res <- .amelia(dgp.co, m=5, command="amelia")
			results <- rbind(results, c(itnum, dgp.co$type, N, NA, npart, NMAR, str, dgp.co$nmar, "Amelia", im, 
				amelia.res$mvmatch, amelia.res$mvmatch.bias, amelia.res$pr.rmse, amelia.res$pr.bias, 
				amelia.res$coef1, amelia.res$coef1.bias, amelia.res$coef1.mns, 
				amelia.res$coef2, amelia.res$coef2.bias, amelia.res$coef2.mns,
				amelia.res$fit1.all, amelia.res$fit1.all.bias, amelia.res$fit2.all, amelia.res$fit2.all.bias, 
				amelia.res$fit1.av, amelia.res$fit1.av.bias, amelia.res$fit2.av, amelia.res$fit2.av.bias, amelia.res$time))

			norm.res <- .amelia(dgp.co, m=5, ncat=ncat, command="norm")
			results <- rbind(results, c(itnum, dgp.co$type, N, NA, npart, NMAR, str, dgp.co$nmar, "NORM", im, 
				norm.res$mvmatch, norm.res$mvmatch.bias, norm.res$pr.rmse, norm.res$pr.bias, 
				norm.res$coef1, norm.res$coef1.bias, norm.res$coef1.mns, 
				norm.res$coef2, norm.res$coef2.bias, norm.res$coef2.mns,
				norm.res$fit1.all, norm.res$fit1.all.bias, norm.res$fit2.all, norm.res$fit2.all.bias, 
				norm.res$fit1.av, norm.res$fit1.av.bias, norm.res$fit2.av, norm.res$fit2.av.bias, norm.res$time))
				
			mcar.res <- .mi(dgp=dgp.co, m=5, chains=5, iter=30, est="MNL", usena=FALSE, , mcar=TRUE)
			results <- rbind(results, c(itnum, dgp.co$type, N, NA, npart, NMAR, str, dgp.co$nmar, "MCAR", im, 
				mcar.res$mvmatch, mcar.res$mvmatch.bias, mcar.res$pr.rmse, mcar.res$pr.bias, 
				mcar.res$coef1, mcar.res$coef1.bias, mcar.res$coef1.mns, 
				mcar.res$coef2, mcar.res$coef2.bias, mcar.res$coef2.mns,
				mcar.res$fit1.all, mcar.res$fit1.all.bias, mcar.res$fit2.all, mcar.res$fit2.all.bias, 
				mcar.res$fit1.av, mcar.res$fit1.av.bias, mcar.res$fit2.av, mcar.res$fit2.av.bias, mcar.res$time))
				
			mnl.res <- .mi(dgp=dgp.co, m=5, chains=5, iter=30, est="MNL", usena=FALSE, , mcar=FALSE)
			results <- rbind(results, c(itnum, dgp.co$type, N, NA, npart, NMAR, str, dgp.co$nmar, "MI: MNL", im, 
				mnl.res$mvmatch, mnl.res$mvmatch.bias, mnl.res$pr.rmse, mnl.res$pr.bias, 
				mnl.res$coef1, mnl.res$coef1.bias, mnl.res$coef1.mns, 
				mnl.res$coef2, mnl.res$coef2.bias, mnl.res$coef2.mns,
				mnl.res$fit1.all, mnl.res$fit1.all.bias, mnl.res$fit2.all, mnl.res$fit2.all.bias, 
				mnl.res$fit1.av, mnl.res$fit1.av.bias, mnl.res$fit2.av, mnl.res$fit2.av.bias, mnl.res$time))
			
		#BINARY
			dgp.bi <- .dgp(N=N, N_FULL=nfull, N_PARTIAL=npart, restrict=restrict, type="binary", pr_miss=.25, imp_meth=im)
			
			cc.res <- .completecase(dgp.bi)
			results <- rbind(results, c(itnum, dgp.bi$type, N, 2, npart, NMAR, 0, dgp.bi$nmar, "Complete Cases", im,  NA, 
					NA, NA, NA, cc.res$coef1, cc.res$coef1.bias, cc.res$coef1.mns, cc.res$coef2, cc.res$coef2.bias, cc.res$coef2.mns, 
					NA, NA, NA, NA, cc.res$fit1.av, cc.res$fit1.av.bias, cc.res$fit2.av, cc.res$fit2.av.bias, NA))
					
			amelia.res <- .amelia(dgp.bi, m=5, command="amelia")
			results <- rbind(results, c(itnum, dgp.bi$type, N, 2, npart, NMAR, 0, dgp.bi$nmar, "Amelia", im, 
				amelia.res$mvmatch, amelia.res$mvmatch.bias, amelia.res$pr.rmse, amelia.res$pr.bias, 
				amelia.res$coef1, amelia.res$coef1.bias, amelia.res$coef1.mns, 
				amelia.res$coef2, amelia.res$coef2.bias, amelia.res$coef2.mns,
				amelia.res$fit1.all, amelia.res$fit1.all.bias, amelia.res$fit2.all, amelia.res$fit2.all.bias, 
				amelia.res$fit1.av, amelia.res$fit1.av.bias, amelia.res$fit2.av, amelia.res$fit2.av.bias, amelia.res$time))

			norm.res <- .amelia(dgp.bi, m=5, ncat=ncat, command="norm")
			results <- rbind(results, c(itnum, dgp.bi$type, N, 2, npart, NMAR, 0, dgp.bi$nmar, "NORM", im, 
				norm.res$mvmatch, norm.res$mvmatch.bias, norm.res$pr.rmse, norm.res$pr.bias, 
				norm.res$coef1, norm.res$coef1.bias, norm.res$coef1.mns, 
				norm.res$coef2, norm.res$coef2.bias, norm.res$coef2.mns,
				norm.res$fit1.all, norm.res$fit1.all.bias, norm.res$fit2.all, norm.res$fit2.all.bias, 
				norm.res$fit1.av, norm.res$fit1.av.bias, norm.res$fit2.av, norm.res$fit2.av.bias, norm.res$time))
				
			mcar.res <- .mi(dgp=dgp.bi, m=5, chains=5, iter=30, est="MNL", usena=FALSE, , mcar=TRUE)
			results <- rbind(results, c(itnum, dgp.bi$type, N, 2, npart, NMAR, 0, dgp.bi$nmar, "MCAR", im, 
				mcar.res$mvmatch, mcar.res$mvmatch.bias, mcar.res$pr.rmse, mcar.res$pr.bias, 
				mcar.res$coef1, mcar.res$coef1.bias, mcar.res$coef1.mns, 
				mcar.res$coef2, mcar.res$coef2.bias, mcar.res$coef2.mns,
				mcar.res$fit1.all, mcar.res$fit1.all.bias, mcar.res$fit2.all, mcar.res$fit2.all.bias, 
				mcar.res$fit1.av, mcar.res$fit1.av.bias, mcar.res$fit2.av, mcar.res$fit2.av.bias, mcar.res$time))
				
			mnl.res <- .mi(dgp=dgp.bi, m=5, chains=5, iter=30, est="MNL", usena=FALSE, , mcar=FALSE)
			results <- rbind(results, c(itnum, dgp.bi$type, N, 2, npart, NMAR, 0, dgp.bi$nmar, "MI: MNL", im, 
				mnl.res$mvmatch, mnl.res$mvmatch.bias, mnl.res$pr.rmse, mnl.res$pr.bias, 
				mnl.res$coef1, mnl.res$coef1.bias, mnl.res$coef1.mns, 
				mnl.res$coef2, mnl.res$coef2.bias, mnl.res$coef2.mns,
				mnl.res$fit1.all, mnl.res$fit1.all.bias, mnl.res$fit2.all, mnl.res$fit2.all.bias, 
				mnl.res$fit1.av, mnl.res$fit1.av.bias, mnl.res$fit2.av, mnl.res$fit2.av.bias, mnl.res$time))
									
		for(ncat in 3:10){
			
			#ORDINAL
				dgp.or <- .dgp(N=1000, N_FULL=nfull, N_PARTIAL=npart, restrict=restrict, ncat=ncat, type="ordinal", pr_miss=.25, imp_meth=im)

				cc.res <- .completecase(dgp.or)
				results <- rbind(results, c(itnum, dgp.or$type, N, ncat, npart, NMAR, 0, dgp.or$nmar, "Complete Cases", im,  NA, 
					NA, NA, NA, cc.res$coef1, cc.res$coef1.bias, cc.res$coef1.mns, cc.res$coef2, cc.res$coef2.bias, cc.res$coef2.mns, 
					NA, NA, NA, NA, cc.res$fit1.av, cc.res$fit1.av.bias, cc.res$fit2.av, cc.res$fit2.av.bias, NA))
										
				amelia.res <- .amelia(dgp.or, m=5, ncat=ncat, command="amelia")
				results <- rbind(results, c(itnum, dgp.or$type, N, ncat, npart, NMAR, 0, dgp.or$nmar, "Amelia", im, 
					amelia.res$mvmatch, amelia.res$mvmatch.bias, amelia.res$pr.rmse, amelia.res$pr.bias, 
					amelia.res$coef1, amelia.res$coef1.bias, amelia.res$coef1.mns, 
					amelia.res$coef2, amelia.res$coef2.bias, amelia.res$coef2.mns,
					amelia.res$fit1.all, amelia.res$fit1.all.bias, amelia.res$fit2.all, amelia.res$fit2.all.bias, 
					amelia.res$fit1.av, amelia.res$fit1.av.bias, amelia.res$fit2.av, amelia.res$fit2.av.bias, amelia.res$time))

				norm.res <- .amelia(dgp.or, m=5, ncat=ncat, command="norm")
				results <- rbind(results, c(itnum, dgp.or$type, N, ncat, npart, NMAR, 0, dgp.or$nmar, "NORM", im, 
					norm.res$mvmatch, norm.res$mvmatch.bias, norm.res$pr.rmse, norm.res$pr.bias, 
					norm.res$coef1, norm.res$coef1.bias, norm.res$coef1.mns, 
					norm.res$coef2, norm.res$coef2.bias, norm.res$coef2.mns,
					norm.res$fit1.all, norm.res$fit1.all.bias, norm.res$fit2.all, norm.res$fit2.all.bias, 
					norm.res$fit1.av, norm.res$fit1.av.bias, norm.res$fit2.av, norm.res$fit2.av.bias, norm.res$time))
					
				mcar.res <- .mi(dgp=dgp.or, m=5, chains=5, iter=30, est="MNL", ncat=ncat, usena=FALSE, , mcar=TRUE)
				results <- rbind(results, c(itnum, dgp.or$type, N, ncat, npart, NMAR, 0, dgp.or$nmar, "MCAR", im, 
					mcar.res$mvmatch, mcar.res$mvmatch.bias, mcar.res$pr.rmse, mcar.res$pr.bias, 
					mcar.res$coef1, mcar.res$coef1.bias, mcar.res$coef1.mns, 
					mcar.res$coef2, mcar.res$coef2.bias, mcar.res$coef2.mns,
					mcar.res$fit1.all, mcar.res$fit1.all.bias, mcar.res$fit2.all, mcar.res$fit2.all.bias, 
					mcar.res$fit1.av, mcar.res$fit1.av.bias, mcar.res$fit2.av, mcar.res$fit2.av.bias, mcar.res$time))
				
				mnl.res <- .mi(dgp=dgp.or, m=5, chains=5, iter=30, est="MNL", ncat=ncat, usena=FALSE, , mcar=FALSE)
				results <- rbind(results, c(itnum, dgp.or$type, N, ncat, npart, NMAR, 0, dgp.or$nmar, "MI: MNL", im, 
					mnl.res$mvmatch, mnl.res$mvmatch.bias, mnl.res$pr.rmse, mnl.res$pr.bias, 
					mnl.res$coef1, mnl.res$coef1.bias, mnl.res$coef1.mns, 
					mnl.res$coef2, mnl.res$coef2.bias, mnl.res$coef2.mns,
					mnl.res$fit1.all, mnl.res$fit1.all.bias, mnl.res$fit2.all, mnl.res$fit2.all.bias, 
					mnl.res$fit1.av, mnl.res$fit1.av.bias, mnl.res$fit2.av, mnl.res$fit2.av.bias, mnl.res$time))
							
			#NOMINAL
				dgp.un <- .dgp(N=1000, N_FULL=nfull, N_PARTIAL=npart, restrict=restrict, ncat=ncat, type="nominal", pr_miss=.25, imp_meth=im)
				
				cc.res <- .completecase(dgp.un)
				results <- rbind(results, c(itnum, dgp.un$type, N, ncat, npart, NMAR, 0, dgp.un$nmar, "Complete Cases", im,  
				NA, 
					NA, NA, NA, cc.res$coef1, cc.res$coef1.bias, cc.res$coef1.mns, cc.res$coef2, cc.res$coef2.bias, cc.res$coef2.mns, 
					NA, NA, NA, NA, cc.res$fit1.av, cc.res$fit1.av.bias, cc.res$fit2.av, cc.res$fit2.av.bias, NA))
										
				amelia.res <- .amelia(dgp.un, m=5, ncat=ncat, command="amelia")
				results <- rbind(results, c(itnum, dgp.un$type, N, ncat, npart, NMAR, 0, dgp.un$nmar, "Amelia", im, 
					amelia.res$mvmatch, amelia.res$mvmatch.bias, amelia.res$pr.rmse, amelia.res$pr.bias, 
					amelia.res$coef1, amelia.res$coef1.bias, amelia.res$coef1.mns, 
					amelia.res$coef2, amelia.res$coef2.bias, amelia.res$coef2.mns,
					amelia.res$fit1.all, amelia.res$fit1.all.bias, amelia.res$fit2.all, amelia.res$fit2.all.bias, 
					amelia.res$fit1.av, amelia.res$fit1.av.bias, amelia.res$fit2.av, amelia.res$fit2.av.bias, amelia.res$time))

				norm.res <- .amelia(dgp.un, m=5, ncat=ncat, command="norm")			
				results <- rbind(results, c(itnum, dgp.un$type, N, ncat, npart, NMAR, 0, dgp.un$nmar, "NORM", im, 
					norm.res$mvmatch, norm.res$mvmatch.bias, norm.res$pr.rmse, norm.res$pr.bias, 
					norm.res$coef1, norm.res$coef1.bias, norm.res$coef1.mns, 
					norm.res$coef2, norm.res$coef2.bias, norm.res$coef2.mns,
					norm.res$fit1.all, norm.res$fit1.all.bias, norm.res$fit2.all, norm.res$fit2.all.bias, 
					norm.res$fit1.av, norm.res$fit1.av.bias, norm.res$fit2.av, norm.res$fit2.av.bias, norm.res$time))
					
				mcar.res <- .mi(dgp=dgp.un, m=5, chains=5, iter=30, est="MNL", ncat=ncat, usena=FALSE, mcar=TRUE)
				results <- rbind(results, c(itnum, dgp.un$type, N, ncat, npart, NMAR, 0, dgp.un$nmar, "MCAR", im, 
					mcar.res$mvmatch, mcar.res$mvmatch.bias, mcar.res$pr.rmse, mcar.res$pr.bias, 
					mcar.res$coef1, mcar.res$coef1.bias, mcar.res$coef1.mns, 
					mcar.res$coef2, mcar.res$coef2.bias, mcar.res$coef2.mns,
					mcar.res$fit1.all, mcar.res$fit1.all.bias, mcar.res$fit2.all, mcar.res$fit2.all.bias, 
					mcar.res$fit1.av, mcar.res$fit1.av.bias, mcar.res$fit2.av, mcar.res$fit2.av.bias, mcar.res$time))
					
				mnl.res <- .mi(dgp=dgp.un, m=5, chains=5, iter=30, est="MNL", ncat=ncat, usena=FALSE, mcar=FALSE)
				results <- rbind(results, c(itnum, dgp.un$type, N, ncat, npart, NMAR, 0, dgp.un$nmar, "MI: MNL", im, 
					mnl.res$mvmatch, mnl.res$mvmatch.bias, mnl.res$pr.rmse, mnl.res$pr.bias, 
					mnl.res$coef1, mnl.res$coef1.bias, mnl.res$coef1.mns, 
					mnl.res$coef2, mnl.res$coef2.bias, mnl.res$coef2.mns,
					mnl.res$fit1.all, mnl.res$fit1.all.bias, mnl.res$fit2.all, mnl.res$fit2.all.bias, 
					mnl.res$fit1.av, mnl.res$fit1.av.bias, mnl.res$fit2.av, mnl.res$fit2.av.bias, mnl.res$time))
					
				rnl.res <- .mi(dgp=dgp.un, m=5, chains=5, iter=30, est="RNL", ncat=ncat, usena=FALSE, mcar=FALSE)
				results <- rbind(results, c(itnum, dgp.un$type, N, ncat, npart, NMAR, 0, dgp.un$nmar, "MI: RNL", im, 
					rnl.res$mvmatch, rnl.res$mvmatch.bias, rnl.res$pr.rmse, rnl.res$pr.bias, 
					rnl.res$coef1, rnl.res$coef1.bias, rnl.res$coef1.mns, 
					rnl.res$coef2, rnl.res$coef2.bias, rnl.res$coef2.mns,
					rnl.res$fit1.all, rnl.res$fit1.all.bias, rnl.res$fit2.all, rnl.res$fit2.all.bias, 
					rnl.res$fit1.av, rnl.res$fit1.av.bias, rnl.res$fit2.av, rnl.res$fit2.av.bias, rnl.res$time))
			}
		}
	}
}
colnames(results) <- resnames
write.table(results, file=resfile, sep=",", col.names=TRUE, row.names=FALSE)

