retdata = read.csv('m-fac9003.csv')
t = dim(retdata)[1]

market = retdata[,14]
riskfree = retdata[,15]
retdata1 = retdata[,c(-14, -15)]
retdata1 = as.matrix(retdata1)
n = dim(retdata1)[2]
ones = rep(1,t)
X = cbind(ones,market)
b_hat = solve(t(X)%*%X)%*%t(X)%*%retdata1
E_hat = retdata1 - X%*%b_hat
diagD_hat = diag(t(E_hat)%*%E_hat)/(t-2)

#R-square
retvar = apply(retdata1,2,var) 
R_2 = 1 - diag(t(E_hat)%*%E_hat)/((t-1)*retvar)
res_std = sqrt(diagD_hat)
cov_factor = var(market)*t(b_hat)%*%b_hat + diag(diagD_hat); 
sd = sqrt(diag(cov_factor));
cor_factor = cov_factor/(sd%*%t(sd));
# sample variance and correlation matrix
cov_sample = cov(retdata1);
cor_sample = cor(retdata1);
# use factor covariance matrix to compute global minimum variance portfolio
one.vec = rep(1,13)
a = solve(cov_factor)%*%one.vec
b = t(one.vec)%*%a
mvp.w =a / as.numeric(b)
mvp.w
