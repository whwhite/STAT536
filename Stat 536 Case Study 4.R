# Load data

emp <- read.table("~/STAT GRAD/Employee.txt",header=TRUE)
emp <- emp[,-1]
empnona <- emp[!is.na(emp$JobSat),]
empnona <- empnona[!is.na(empnona$WellBeing),]
empnona <- empnona[!is.na(empnona$JobPerf),]
mu0 <- apply(empnona,2,mean)
cov0 <- cov(empnona)

nreps <- 5000
betastorage <- matrix(0,nrow=nreps,ncol=6)
stderrstor <- matrix(0,nrow=nreps,ncol=6)


for(k in 1:nreps)
{
  emp2 <- emp
  for(i in 1:nrow(emp))
  {
    y <- emp[i,]
    if(sum(is.na(y))>0)
    {
      miss <- which(is.na(y))
      obs <- which(!is.na(y))
      mu1 <- mu0[miss]
      mu2 <- mu0[obs]
      if(length(miss)==1)
      {
        sig12 <- t(as.matrix(cov0[miss,obs]))
      }
      else
      {
        sig12 <- cov0[miss,obs]
      }
      sig1 <- cov0[miss,miss]
      sig2 <- cov0[obs,obs]
      sig21 <- t(sig12)
      mu1given2 <- mu1 + sig12%*%solve(sig2)%*%t(y[obs]-mu2)
      sig1given2 <- sig1 - sig12%*%solve(sig2)%*%t(sig12)
      draw <- mu1given2 + t(chol(sig1given2))%*%rnorm(length(miss))
      emp2[i,miss] <- t(draw)
    }
  }
  #print("update!")
  mu0 <- apply(emp2,2,mean)
  cov0 <- cov(emp2)
  mod <- lm(JobPerf~.,emp2)
  betas <- mod$coefficients
  stderr <- t(summary(mod)$coefficients[,2])
  betastorage[k,] <- betas
  stderrstor[k,] <- stderr
  emp2 <- emp
}

plot(betastorage[,1],type='l')
