###############################################################################################################
# Data analysis of COVID-19 published at: (article submitted waiting for publication)
# date of creation: 06/07/2021 (date in US format)
# R version: 4.0.3
# script name: functions.R
# aim: define functions for the file script.R of folder codes
# input: none
# output: none
# external sources: none 
###############################################################################################################


# FUNCTIONS ---------------------------------------------------------------------------------------------------

# extract legend from a ggplot
g_legend <- function(a.gplot){
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
  leg <- base::which(base::sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  base::return(legend)
}

# the code below was adapted from Supplemental Material of the article https://doi.org/10.1080/10618600.2012.681216
## Bootstrap code for REB/0 & REB/2
generic.group.boot.REB.0.2 <- function(y,X,alpha,beta,sigma.u,sigma.e,group,k=1000,verbose=F,stop=F) {
  N <- base::length(group)
  group.labels <- base::unique(group)
  H <- base::length(group.labels)
  P <- base::ncol(X)
  y.fit <- alpha + X%*%base::as.matrix(beta,ncol=1) 
  y.resid <- y-y.fit 
  y.resid.u <- base::rep(0,H)
  for(h in 1:H) y.resid.u[h] <- base::mean(y.resid[group==group.labels[h]])
      y.resid.e <- base::rep(0,N)
      for(h in 1:H) y.resid.e[group==group.labels[h]] <- y.resid[group==group.labels[h]]-y.resid.u[h]
      lambda <- (sigma.u/sigma.e)^2
      boot.mat.model.parameters <- base::matrix(0,nrow=k,ncol=P+4)
      ## REB/0 bootstrap loop
      for(b in 1:k) {
        y.b <- base::rep(0,N)
        donor.group.labels.b <- base::sample(x=group.labels,size=H,replace=T)
        y.resid.u.b <- base::sample(y.resid.u,size=H,replace=T)
        for(h in 1:H) {
          target.units <- (1:N)[group==group.labels[h]]
          donor.units <- (1:N)[group==donor.group.labels.b[h]]
          if(base::length(donor.units)>1) donating.units <- base::sample(x=donor.units,size=base::length(target.units),replace=T)
          else donating.units. <- base::rep(donor.units,base::length(target.units))
          y.b[target.units] <- y.fit[target.units]+y.resid.u.b[h]+y.resid.e[donating.units]
        }
        y.model.b <- nlme::lme(y.b~X,random = ~1 | group)
        var.comp.b <- base::as.double(nlme::VarCorr(y.model.b))[1:2]
        lambda.b <- var.comp.b[1]/var.comp.b[2]
        boot.mat.model.parameters[b,] <- base::c(y.model.b$coef$fixed,var.comp.b,lambda.b)
        if(verbose) base::print(base::c("bootstrap simulation ",b))
      }
  ## Post-bootstrap adjustments (REB/2)
  ## Rescaling
  boot.mat.model.adj.parameters <- boot.mat.model.parameters
  Sb <- base::log(boot.mat.model.adj.parameters[,(P+2):(P+3)])
  Mb <- base::apply(Sb,2,mean)
  CovSb <- stats::cov(Sb)
  SdSb <- base::sqrt(base::diag(CovSb))
  EW <- base::eigen(base::solve(CovSb),symmetric=T)
  Whalf <- EW$vectors%*%base::diag(base::sqrt(EW$values))
  Sm <- base::cbind(base::rep(Mb[1],k),base::rep(Mb[2],k))
  Sbmod <- (Sb-Sm)%*%Whalf
  Sbmod[,1] <- Sbmod[,1]*SdSb[1]
  Sbmod[,2] <- Sbmod[,2]*SdSb[2]
  boot.mat.model.adj.parameters[,(P+2):(P+3)] <- base::exp(Sm+Sbmod)
  ## Bias corrections
  input.effects <- base::c(alpha,beta,sigma.u^2,sigma.e^2,lambda)
  for (j in 1:(P+1)) boot.mat.model.adj.parameters[,j] <- boot.mat.model.adj.parameters[,j]+(input.effects[j]-base::mean(boot.mat.model.adj.parameters[,j]))
  for (j in (P+2):(P+4)) boot.mat.model.adj.parameters[,j] <- boot.mat.model.adj.parameters[,j]*(input.effects[j]/base::mean(boot.mat.model.adj.parameters[,j]))
  if(stop) base::browser()
  ## bootstrap.par is bootstrap distribution under REB/0
  ## bootstrap.adj.par is bootstrap distribution under REB/2
  base::list(bootstrap.par=boot.mat.model.parameters, bootstrap.adj.par=boot.mat.model.adj.parameters)
}