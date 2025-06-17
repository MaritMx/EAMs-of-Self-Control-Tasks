##### Appendix B

# Initial model: Congruence effect on v and irrelevant stimulus effect on B/Z ----
ADmat <- matrix(c(-1/2,1/2),ncol=1,dimnames=list(NULL,"d")) # for LBA, RDM, LNR
Smat <- matrix(c(-1,1), nrow = 2, dimnames = list(NULL,"dif")) # for WDM, DDM

## LBA----

dLBA_both <- design(data=simon,matchfun=function(d)d$RS==d$lR,
                    functions = list(
                      CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                    formula=list(v~CI*lM,B~IS/lR,A~1,t0~1,sv~lM),
                    contrasts=list(lM=ADmat,lR=ADmat),
                    constants=c(sv=1,B_ISred=log(1)),model=LBA)



## RDM----
dRDM_both <- design(data=simonSmall,matchfun=function(d)d$RS==d$lR,
                    functions = list(
                      CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                    formula=list(v~CI*lM,B~IS/lR,A~1,t0~1,s~lM),
                    contrasts=list(lM=ADmat,lR=ADmat),
                    constants=c(s=1,B_ISred=log(1),A=log(0)),model=RDM)
## LNR----
dLNR_both <- design(data=simon,matchfun=function(d)d$RS==d$lR,
                    functions = list(
                      CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                    formula=list(m~CI*lM + IS*lR,s~lM,t0~1),
                    contrasts=list(lM=ADmat,lR=ADmat),  model=LNR)
## WDM----
dWDM_both <- design(data=simon,matchfun=function(d)d$RS==d$R,
                    functions = list(
                      CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                    formula=list(v~CI*RS, a~1, t0~1,  Z~0+IS),
                    contrasts = list(RS = Smat),
                    constants = c(v=0,v_CIincong=0),
                    model=DDM)
## DDM----
dDDM_both <- design(data=simon,matchfun=function(d)d$RS==d$R,
                    functions = list(
                      CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                    formula=list(v~CI*RS, a~1, t0~1,  Z~0+IS, sv~1, SZ~1, st0~1),
                    contrasts = list(RS = Smat),
                    constants = c(v=0,v_CIincong=0),
                    model=DDM)



# Only a congruence effect or irrelevant stimulus effect----
ADmat <- matrix(c(-1/2,1/2),ncol=1,dimnames=list(NULL,"d")) # for LBA, RDM, LNR
Smat <- matrix(c(-1,1), nrow = 2, dimnames = list(NULL,"dif")) # for WDM, DDM
## LBA----
dLBA_B <- design(data=simonSmall,matchfun=function(d)d$RS==d$lR,
                 functions = list(
                   CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                 formula=list(v~lM,B~IS/lR,A~1,t0~1,sv~lM),
                 contrasts=list(lM=ADmat,lR=ADmat),
                 constants=c(sv=1,B_ISred=log(1)),model=LBA)


dLBA_v <- design(data=simonSmall,matchfun=function(d)d$RS==d$lR,
                 functions = list(
                   CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                 formula=list(v~CI*lM,B~lR,A~1,t0~1,sv~lM),
                 contrasts=list(lM=ADmat,lR=ADmat),
                 constants=c(sv=1),model=LBA)
## RDM----
dRDM_v <- design(data=simonSmall,matchfun=function(d)d$RS==d$lR,
                 functions = list(
                   CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                 formula=list(v~CI*lM,B~lR,A~1,t0~1,s~lM),
                 contrasts=list(lM=ADmat,lR=ADmat),
                 constants=c(s=1,A=log(0)),model=RDM)


dRDM_B <- design(data=simonSmall,matchfun=function(d)d$RS==d$lR,
                 functions = list(
                   CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                 formula=list(v~lM,B~IS/lR,A~1,t0~1,s~lM),
                 contrasts=list(lM=ADmat,lR=ADmat),
                 constants=c(s=1,B_ISred=log(1),A=log(0)),model=RDM)
## LNR----
dLNR_IS <- design(data=simon,matchfun=function(d)d$RS==d$lR,
                  functions = list(
                    CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                  formula=list(m~lM + IS*lR,s~lM,t0~1),
                  contrasts=list(lM=ADmat,lR=ADmat),  model=LNR)


dLNR_CI <- design(data=simon,matchfun=function(d)d$RS==d$lR,
                  functions = list(
                    CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                  formula=list(m~CI*lM + lR,s~lM,t0~1),
                  contrasts=list(lM=ADmat,lR=ADmat),  model=LNR)
## WDM----
dWDM_v <- design(data=simon,matchfun=function(d)d$RS==d$R,
                 functions = list(
                   CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                 formula=list(v~CI*RS, a~1, t0~1,  Z~1),
                 contrasts = list(RS = Smat),
                 constants = c(v=0,v_CIincong=0),
                 model=DDM)


dWDM_z <- design(data=simon,matchfun=function(d)d$RS==d$R,
                 functions = list(
                   CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                 formula=list(v~RS, a~1, t0~1,  Z~0+IS),
                 contrasts = list(RS = Smat),
                 constants = c(v=0),
                 model=DDM)
## DDM----
dDDM_v <- design(data=simon,matchfun=function(d)d$RS==d$R,
                 functions = list(
                   CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                 formula=list(v~CI*RS, a~1, t0~1,  Z~1, sv~1, SZ~1, st0~1),
                 contrasts = list(RS = Smat),
                 constants = c(v=0,v_CIincong=0),
                 model=DDM)



dDDM_z <- design(data=simon,matchfun=function(d)d$RS==d$R,
                 functions = list(
                   CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                 formula=list(v~RS, a~1, t0~1,  Z~0+IS, sv~1, SZ~1, st0~1),
                 contrasts = list(RS = Smat),
                 constants = c(v=0),
                 model=DDM)



# Bounded and unbounded models with a different parameterization of a bias and congruence effect on B/Z and an sv effect for race models----
# For LBA and RDM
ADmat <- matrix(c(-1/2,1/2),ncol=1,dimnames=list(NULL,"d"))
# For LNR
ADmatLNR <- matrix(c(1/2,-1/2),ncol=1,dimnames=list(NULL,"d"))
# IS * lR, Bias and concurrency factors 
lrfun <- \(d)factor(paste0(substr(d$IS,1,1),toupper(substr(d$lR,1,1))),
                    levels=c("bB","bR","rB","rR"))
Bmat <- cbind(Cred=c(0,0,1,1),d=c(-1/2,1/2,-1/2,1/2),CI=c(-1/2,1/2,1/2,-1/2))


# DDM contrasts
Smat <- matrix(c(-1,1), nrow = 2, dimnames = list(NULL,"d"))
# RS * IS, Bias and concurrency factors 
lbfun <- \(d)factor(paste0(substr(d$RS,1,1),toupper(substr(d$IS,1,1))),
                    levels=c("bB","bR","rB","rR"))
Zmat <- cbind(CI=c(-1,0,0,1))

## LBA----
# Unbounded
dLBA_both <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                    functions = list(lr=lrfun,
                                     CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                    formula=list(v~CI*lM,B~lr,A~1,t0~1,sv~lM),
                    contrasts=list(lM=ADmat,lR=ADmat,lr=Bmat),
                    constants=c(sv=1,B_lrCred=log(1)),model=LBA)

dLBA_B <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                 functions = list(lr=lrfun,
                                  CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~lM,B~lr,A~1,t0~1,sv~lM),
                 contrasts=list(lM=ADmat,lR=ADmat,lr=Bmat),
                 constants=c(sv=1,B_lrCred=log(1)),model=LBA)

dLBA_v <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                 functions = list(
                   CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~CI*lM,B~lR,A~1,t0~1,sv~lM),
                 contrasts=list(lM=ADmat,lR=ADmat),
                 constants=c(sv=1),model=LBA)
# Bounded
dLBA_both <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                    functions = list(lr=lrfun,
                                     CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                    formula=list(v~CI*lM,B~lr,A~1,t0~1,sv~lM),
                    contrasts=list(lM=ADmat,lR=ADmat,lr=Bmat),
                    pre_transform = list(func = c(v_CIcong="exp","v_CIcong:lMd" = "exp",B_lrCI = "exp")),
                    constants=c(sv=1,B_lrCred=log(1)),model=LBA)

dLBA_B <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                 functions = list(lr=lrfun,
                                  CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~lM,B~lr,A~1,t0~1,sv~lM),
                 contrasts=list(lM=ADmat,lR=ADmat,lr=Bmat),
                 pre_transform = list(func = c(B_lrCI = "exp")),
                 constants=c(sv=1,B_lrCred=log(1)),model=LBA)

dLBA_v <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                 functions = list(
                   CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~CI*lM,B~lR,A~1,t0~1,sv~lM),
                 contrasts=list(lM=ADmat,lR=ADmat),
                 pre_transform = list(func = c(v_CIcong="exp","v_CIcong:lMd" = "exp")),
                 constants=c(sv=1),model=LBA)
## RDM----
# Unbounded
dRDM_both <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                    functions = list(lr=lrfun,
                                     CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                    formula=list(v~CI*lM,B~lr,t0~1,s~lM),
                    contrasts=list(lM=ADmat,lR=ADmat,lr=Bmat),
                    constants=c(s=1,B_lrCred=log(1)),model=RDM)

dRDM_B <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                 functions = list(lr=lrfun,
                                  CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~lM,B~lr,t0~1,s~lM),
                 contrasts=list(lM=ADmat,lR=ADmat,lr=Bmat),
                 constants=c(s=1,B_lrCred=log(1)),model=RDM)

dRDM_v <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                 functions = list(
                   CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~CI*lM,B~lR,t0~1,s~lM),
                 contrasts=list(lM=ADmat,lR=ADmat),
                 constants=c(s=1),model=RDM)
# Bounded

dRDM_both <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                    functions = list(lr=lrfun,
                                     CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                    formula=list(v~CI*lM,B~lr,t0~1,s~lM),
                    contrasts=list(lM=ADmat,lR=ADmat,lr=Bmat),
                    pre_transform = list(func = c(v_CIcong="exp","v_CIcong:lMd" = "exp",B_lrCI = "exp")),
                    constants=c(s=1,B_lrCred=log(1)),model=RDM)

dRDM_B <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                 functions = list(lr=lrfun,
                                  CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~lM,B~lr,t0~1,s~lM),
                 contrasts=list(lM=ADmat,lR=ADmat,lr=Bmat),
                 pre_transform = list(func = c(B_lrCI = "exp")),
                 constants=c(s=1,B_lrCred=log(1)),model=RDM)

dRDM_v <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                 functions = list(
                   CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~CI*lM,B~lR,t0~1,s~lM),
                 contrasts=list(lM=ADmat,lR=ADmat),
                 pre_transform = list(func = c(v_CIcong="exp","v_CIcong:lMd" = "exp")),
                 constants=c(s=1),model=RDM)

## LNR----
# Unbounded
dLNR_both <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                    functions = list(
                      CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                    formula=list(m~CI*lM,s~CI*lM,t0~1),
                    contrasts=list(m=list(lM=ADmat),s=list(lM=ADmatLNR)), model=LNR)

dLNR_m <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                 functions = list(
                   CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                 formula=list(m~CI*lM,s~lM,t0~1),
                 contrasts=list(m=list(lM=ADmat),s=list(lM=ADmatLNR)),  model=LNR)

dLNR_s <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                 functions = list(
                   CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                 formula=list(m~lM,s~CI*lM,t0~1),
                 contrasts=list(m=list(lM=ADmat),s=list(lM=ADmatLNR)), model=LNR)

# Bounded
dLNR_both <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                    functions = list(
                      CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                    formula=list(m~CI*lM,s~CI*lM,t0~1),
                    pre_transform = list(func = c(m_CIincong="exp","m_CIincong:lMd" = "exp")),
                    contrasts=list(m=list(lM=ADmat),s=list(lM=ADmatLNR)), model=LNR)

dLNR_m <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                 functions = list(
                   CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                 formula=list(m~CI*lM,s~lM,t0~1),
                 pre_transform = list(func = c(m_CIincong="exp","m_CIincong:lMd" = "exp")),
                 contrasts=list(m=list(lM=ADmat),s=list(lM=ADmatLNR)),  model=LNR)

dLNR_s <- design(data=dat,matchfun=function(d)d$RS==d$lR,
                 functions = list(
                   CI=function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))),
                 formula=list(m~lM,s~CI*lM,t0~1),
                 contrasts=list(m=list(lM=ADmat),s=list(lM=ADmatLNR)), model=LNR)


## WDM----
# Unbounded
dWDM_both <- design(data=dat,matchfun=function(d)d$RS==d$R,
                    functions = list(lb=lbfun,
                                     CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                    formula=list(v~CI*RS, a~1, t0~1,  Z~lb),
                    contrasts = list(RS = Smat,lb=Zmat),
                    constants = c(v=0,v_CIcong=0),
                    model=DDM)

dWDM_z <- design(data=dat,matchfun=function(d)d$RS==d$R,
                 functions = list(lb=lbfun,
                                  CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~RS, a~1, t0~1,  Z~lb),
                 contrasts = list(RS = Smat,lb=Zmat),
                 constants = c(v=0),
                 model=DDM)

dWDM_v <- design(data=dat,matchfun=function(d)d$RS==d$R,
                 functions = list(
                   CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~CI*RS, a~1, t0~1,  Z~1),
                 contrasts = list(RS = Smat),
                 constants = c(v=0,v_CIcong=0),
                 model=DDM)

# Bounded
dWDM_both <- design(data=dat,matchfun=function(d)d$RS==d$R,
                    functions = list(lb=lbfun,
                                     CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                    formula=list(v~CI*RS, a~1, t0~1,  Z~lb),
                    contrasts = list(RS = Smat,lb=Zmat),
                    constants = c(v=0,v_CIcong=0),
                    pre_transform = list(func = c("v_CIcong:RSd" = "exp",Z_lbCI = "exp")),
                    model=DDM)

dWDM_z <- design(data=dat,matchfun=function(d)d$RS==d$R,
                 functions = list(lb=lbfun,
                                  CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~RS, a~1, t0~1,  Z~lb),
                 contrasts = list(RS = Smat,lb=Zmat),
                 constants = c(v=0),
                 pre_transform = list(func = c(Z_lbCI = "exp")),
                 model=DDM)

dWDM_v <- design(data=dat,matchfun=function(d)d$RS==d$R,
                 functions = list(
                   CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~CI*RS, a~1, t0~1,  Z~1),
                 contrasts = list(RS = Smat),
                 constants = c(v=0,v_CIcong=0),
                 pre_transform = list(func = c("v_CIcong:RSd" = "exp")),
                 model=DDM)
## DDM----
# Unbounded
dDDM_both <- design(data=dat,matchfun=function(d)d$RS==d$R,
                    functions = list(lb=lbfun,
                                     CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                    formula=list(v~CI*RS, a~1, t0~1,  Z~lb, sv~1, SZ~1, st0~1),
                    contrasts = list(RS = Smat,lb=Zmat),
                    constants = c(v=0,v_CIcong=0),
                    model=DDM)

dDDM_z <- design(data=dat,matchfun=function(d)d$RS==d$R,
                 functions = list(lb=lbfun,
                                  CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~RS, a~1, t0~1,  Z~lb, sv~1, SZ~1, st0~1),
                 contrasts = list(RS = Smat,lb=Zmat),
                 constants = c(v=0),
                 model=DDM)

dDDM_v <- design(data=dat,matchfun=function(d)d$RS==d$R,
                 functions = list(
                   CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~CI*RS, a~1, t0~1,  Z~1, sv~1, SZ~1, st0~1),
                 contrasts = list(RS = Smat),
                 constants = c(v=0,v_CIcong=0),
                 model=DDM)
# Bounded
dDDM_both <- design(data=dat,matchfun=function(d)d$RS==d$R,
                    functions = list(lb=lbfun,
                                     CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                    formula=list(v~CI*RS, a~1, t0~1,  Z~lb, sv~1, SZ~1, st0~1),
                    contrasts = list(RS = Smat,lb=Zmat),
                    constants = c(v=0,v_CIcong=0),
                    pre_transform = list(func = c("v_CIcong:RSd" = "exp",Z_lbCI = "exp")),
                    model=DDM)

dDDM_z <- design(data=dat,matchfun=function(d)d$RS==d$R,
                 functions = list(lb=lbfun,
                                  CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~RS, a~1, t0~1,  Z~lb, sv~1, SZ~1, st0~1),
                 contrasts = list(RS = Smat,lb=Zmat),
                 constants = c(v=0),
                 pre_transform = list(func = c(Z_lbCI = "exp")),
                 model=DDM)

dDDM_v <- design(data=dat,matchfun=function(d)d$RS==d$R,
                 functions = list(
                   CI=function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))),
                 formula=list(v~CI*RS, a~1, t0~1,  Z~1, sv~1, SZ~1, st0~1),
                 contrasts = list(RS = Smat),
                 constants = c(v=0,v_CIcong=0),
                 pre_transform = list(func = c("v_CIcong:RSd" = "exp")),
                 model=DDM)




# Powerset of additional CI effect on sv(race models)/s, irrelevant stimulus on B,v and sv(race models)/s----
# For LBA, RDM, LNR
# Congruent vs. Incongruent trials, makes cong greater for positive
CIfun <- function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))

# Congruent vs. Incongruent trials, makes incong greater for positive
CIfunR <- function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))

# Function for irrelevant stimulus and accumulator
matchfun <- function(d)d$RS==d$lR

# Used with rates
ADmat <- matrix(c(-1/2,1/2),ncol=1,dimnames=list(NULL,"d"))
# Used with priming 
ADmatR <- matrix(c(1/2,-1/2),ncol=1,dimnames=list(NULL,"d")) 

# Function for irrelevant stimulus and accumulator
iMfun <- \(d)factor(d$IS==d$lR)


# For WDM, DDM
# Congruent vs. Incongruent trials, makes cong greater for positive
CIfun <- function(d)factor(d$RS==d$IS,labels = c("incong", "cong"))

# Reversed for LNR, makes incong greater for positive
CIfunR <- function(d)factor(d$RS!=d$IS,labels = c("cong", "incong"))

matchfun <- function(d)d$RS==d$lR

Smat <- matrix(c(-1,1), nrow = 2, dimnames = list(NULL,"d"))

# RS * IS, Bias and concurrency factors (positive expected in latter)
lbfun <- \(d)factor(paste0(substr(d$RS,1,1),toupper(substr(d$IS,1,1))),
                    levels=c("bB","bR","rB","rR"))
Zmat <- cbind(CI=c(-1,0,0,1))

## LBA----
dLBA_maximal <- design(data=dat,model=LBA,
                       matchfun=matchfun,functions = list(iM=iMfun,CI=CIfun,CIR=CIfunR),
                       formula=list(B~CIR+lR+iM,v~CI+lM+iM,sv~CIR+lM+iM,t0~1,A~1),
                       contrasts=list(B=list(lR=ADmat,iM=ADmatR),
                                      v=list(lM=ADmat,iM=ADmat),sv=list(lM=ADmatR,iM=ADmatR)),
                       pre_transform = list(func = c(B_iMd = "exp",v_iMd = "exp",sv_iMd = "exp",
                                                     v_CIcong = "exp")),
                       constants=c(sv=log(1)))

# emc <- make_emc(dat,dLBA_maximal)
# fit(emc,fileName="LBA_maximal.RData",cores_per_chain=3)


formulas=list(
  LBA_b = list(B~CIR+lR   ,v~CI+lM+iM,sv~CIR+lM+iM,t0~1,A~1), 
  LBA_v = list(B~CIR+lR+iM,v~CI+lM,   sv~CIR+lM+iM,t0~1,A~1),
  LBA_s = list(B~CIR+lR+iM,v~CI+lM+iM,sv~CIR+lM   ,t0~1,A~1),
  LBA_B = list(B~   lR+iM,v~CI+lM+iM,sv~CIR+lM+iM,t0~1,A~1),
  LBA_V = list(B~CIR+lR+iM,v~   lM+iM,sv~CIR+lM+iM,t0~1,A~1),
  LBA_S = list(B~CIR+lR+iM,v~CI+lM+iM,sv~   lM+iM,t0~1,A~1),
  LBA_bB = list(B~   lR  ,v~CI+lM+iM,sv~CIR+lM+iM,t0~1,A~1),
  LBA_vV = list(B~CIR+lR+iM,v~   lM,  sv~CIR+lM+iM,t0~1,A~1),
  LBA_sS = list(B~CIR+lR+iM,v~CI+lM+iM,sv~   lM   ,t0~1,A~1),
  LBAb = list(B~lR+iM,v~lM,sv~lM,t0~1,A~1),
  LBAv = list(B~lR,v~lM+iM,sv~lM,t0~1,A~1),
  LBAs = list(B~lR,v~lM,sv~lM+iM,t0~1,A~1),
  LBAB = list(B~CIR+lR,v~lM,sv~lM,t0~1,A~1),
  LBAV = list(B~lR,v~CI+lM,sv~lM,t0~1,A~1),
  LBAS = list(B~lR,v~lM,sv~CIR+lM,t0~1,A~1)
)

pre_transforms <- list(
  list(func = c(              v_iMd = "exp",sv_iMd = "exp",v_CIcong = "exp")),
  list(func = c(B_iMd = "exp",              sv_iMd = "exp",v_CIcong = "exp")),
  list(func = c(B_iMd = "exp",v_iMd = "exp",               v_CIcong = "exp")),
  list(func = c(B_iMd = "exp",v_iMd = "exp",sv_iMd = "exp",v_CIcong = "exp")),
  list(func = c(B_iMd = "exp",v_iMd = "exp",sv_iMd = "exp"                 )),
  list(func = c(B_iMd = "exp",v_iMd = "exp",sv_iMd = "exp",v_CIcong = "exp")),
  list(func = c(              v_iMd = "exp",sv_iMd = "exp",v_CIcong = "exp")),
  list(func = c(B_iMd = "exp",              sv_iMd = "exp"                 )),
  list(func = c(B_iMd = "exp",v_iMd = "exp",               v_CIcong = "exp")),
  list(func = c(B_iMd = "exp")),
  list(func = c(v_iMd = "exp")),
  list(func = c(sv_iMd = "exp")),
  NULL,
  list(func = c(v_CIcong = "exp")),
  NULL
)


for (i in 1:length(formulas)) {
  d <- design(data=dat,model=LBA,
              matchfun=matchfun,functions = list(iM=iMfun,CI=CIfun,CIR=CIfunR),
              formula=formulas[[i]],
              contrasts=list(B=list(lR=ADmat,iM=ADmatR),
                             v=list(lM=ADmat,iM=ADmat),sv=list(lM=ADmatR,iM=ADmatR)),
              pre_transform = pre_transforms[[i]],
              constants=c(sv=log(1)))
  
  # emc <- make_emc(dat,d)
  # fit(emc,fileName=paste0(names(formulas)[i],".RData"),cores_per_chain=6)
  
}

## RDM----
dRDM_maximal <- design(data=dat,model=RDM,
                       matchfun=matchfun,functions = list(iM=iMfun,CI=CIfun,CIR=CIfunR),
                       formula=list(B~CIR+lR+iM,v~CI+lM+iM,s~CIR+lM+iM,t0~1),
                       contrasts=list(B=list(lR=ADmat,iM=ADmatR),
                                      v=list(lM=ADmat,iM=ADmat),s=list(lM=ADmatR,iM=ADmatR)),
                       pre_transform = list(func = c(B_iMd = "exp",v_iMd = "exp",s_iMd = "exp",
                                                     v_CIcong = "exp")),
                       constants=c(s=log(1)))



formulas=list(
  RDM_b = list(B~CIR+lR   ,v~CI+lM+iM,s~CIR+lM+iM,t0~1),
  RDM_v = list(B~CIR+lR+iM,v~CI+lM,   s~CIR+lM+iM,t0~1),
  RDM_s = list(B~CIR+lR+iM,v~CI+lM+iM,s~CIR+lM   ,t0~1),
  RDM_B = list(B~   lR+iM,v~CI+lM+iM,s~CIR+lM+iM,t0~1),
  RDM_V = list(B~CIR+lR+iM,v~   lM+iM,s~CIR+lM+iM,t0~1),
  RDM_S = list(B~CIR+lR+iM,v~CI+lM+iM,s~   lM+iM,t0~1),
  RDM_bB = list(B~   lR  ,v~CI+lM+iM,s~CIR+lM+iM,t0~1),
  RDM_vV = list(B~CIR+lR+iM,v~   lM,  s~CIR+lM+iM,t0~1),
  RDM_sS = list(B~CIR+lR+iM,v~CI+lM+iM,s~   lM   ,t0~1),
  RDMb = list(B~lR+iM,v~lM,s~lM,t0~1),
  RDMv = list(B~lR,v~lM+iM,s~lM,t0~1),
  RDMs = list(B~lR,v~lM,s~lM+iM,t0~1),
  RDMB = list(B~CIR+lR,v~lM,s~lM,t0~1),
  RDMV = list(B~lR,v~CI+lM,s~lM,t0~1),
  RDMS = list(B~lR,v~lM,s~CIR+lM,t0~1)
)

pre_transforms <- list(
  list(func = c(              v_iMd = "exp",s_iMd = "exp",v_CIcong = "exp")),
  list(func = c(B_iMd = "exp",              s_iMd = "exp",v_CIcong = "exp")),
  list(func = c(B_iMd = "exp",v_iMd = "exp",               v_CIcong = "exp")),
  list(func = c(B_iMd = "exp",v_iMd = "exp",s_iMd = "exp",v_CIcong = "exp")),
  list(func = c(B_iMd = "exp",v_iMd = "exp",s_iMd = "exp"                 )),
  list(func = c(B_iMd = "exp",v_iMd = "exp",s_iMd = "exp",v_CIcong = "exp")),
  list(func = c(              v_iMd = "exp",s_iMd = "exp",v_CIcong = "exp")),
  list(func = c(B_iMd = "exp",              s_iMd = "exp"                 )),
  list(func = c(B_iMd = "exp",v_iMd = "exp",               v_CIcong = "exp")),
  list(func = c(B_iMd = "exp")),
  list(func = c(v_iMd = "exp")),
  list(func = c(s_iMd = "exp")),
  NULL,
  list(func = c(v_CIcong = "exp")),
  NULL
)


for (i in 1:length(formulas)) {
  d <- design(data=dat,model=RDM,
              matchfun=matchfun,functions = list(iM=iMfun,CI=CIfun,CIR=CIfunR),
              formula=formulas[[i]],
              contrasts=list(B=list(lR=ADmat,iM=ADmatR),
                             v=list(lM=ADmat,iM=ADmat),s=list(lM=ADmatR,iM=ADmatR)),
              pre_transform = pre_transforms[[i]],
              constants=c(s=log(1)))
  
  # emc <- make_emc(dat,d)
  # fit(emc,fileName=paste0(names(formulas)[i],".RData"),cores_per_chain=6)
  
}
## LNR----
dLNR_maximal <- design(data=dat, model=LNR,
                       matchfun=matchfun,functions = list(CI=CIfunR,iM=iMfun),
                       formula=list(m~CI+lM+iM+lR,s~CI+lM+iM,t0~1),
                       pre_transform = list(func = c(m_CIincong="exp", m_iMd = "exp")),
                       contrasts=list(m=list(lM=ADmatR,iM=ADmatR,lR=ADmat),s=list(lM=ADmatR,iM=ADmat)))


formulas=list(
  LNR_m =  list(m~CI+lM   +lR,s~CI+lM+iM,t0~1),
  LNR_s =  list(m~CI+lM+iM+lR,s~CI+lM   ,t0~1),
  LNR_M =  list(m~   lM+iM+lR,s~   lM+iM,t0~1),
  LNR_S =  list(m~CI+lM+iM+lR,s~   lM+iM,t0~1),
  LNR_mM = list(m~   lM+lR,   s~CI+lM+iM,t0~1),
  LNR_sS = list(m~CI+lM+iM+lR,s~   lM   ,t0~1),
  LNRm =   list(m~lM+lR+iM,s~lM,t0~1),
  LNRs =   list(m~lM+lR,s~lM+iM,t0~1),
  LNRM =   list(m~CI+lM+lR,s~lM,t0~1),
  LNRS =   list(m~CI+lM+lR,s~lM,t0~1)
)

pre_transforms <- list(
  list(func = c(m_CIincong="exp")),
  list(func = c(m_CIincong="exp", m_iMd = "exp")),
  list(func = c(                  m_iMd = "exp")),
  list(func = c(m_CIincong="exp", m_iMd = "exp")),
  NULL,
  list(func = c(m_CIincong="exp", m_iMd = "exp")),
  list(func = c(m_iMd = "exp")),
  NULL,
  list(func = c(m_CIincong="exp")),
  NULL
)


for (i in 1:length(formulas)) {
  d <- design(data=dat, model=LNR,
              matchfun=matchfun,functions = list(CI=CIfunR,iM=iMfun),
              formula=formulas[[i]],
              pre_transform = pre_transforms[[i]],
              contrasts=list(m=list(lM=ADmatR,iM=ADmatR,lR=ADmat),s=list(lM=ADmatR,iM=ADmat)))
  
  # emc <- make_emc(dat,d)
  # fit(emc,fileName=paste0(names(formulas)[i],".RData"),cores_per_chain=6)
  
}

## WDM----
dWDM_maximal <- design(data=dat,model=DDM,
                       matchfun=matchfun,functions = list(lb=lbfun,CI=CIfun),
                       formula=list(v~CI*RS, a~1, t0~1,  Z~lb, s~CI),
                       contrasts = list(RS = Smat,lb=Zmat),
                       pre_transform = list(func = c("v_CIcong:RSd" = "exp",Z_lbCI = "exp")),
                       constants = c(v_CIcong=0,s=log(1)))


formulas=list(
  WDM_v = list(v~   RS, a~1, t0~1,  Z~lb, s~CI),
  WDM_z = list(v~CI*RS, a~1, t0~1,  Z~1,  s~CI),
  WDM_s = list(v~CI*RS, a~1, t0~1,  Z~lb, s~1),
  WDMv = list(v~CI*RS, a~1, t0~1,  Z~1, s~1),
  WDMz = list(v~RS, a~1, t0~1,  Z~lb, s~1),
  WDMs = list(v~RS, a~1, t0~1,  Z~1, s~CI)
)

pre_transforms <- list(
  list(func = c(Z_lbCI = "exp")),
  list(func = c("v_CIcong:RSd" = "exp")),
  list(func = c("v_CIcong:RSd" = "exp",Z_lbCI = "exp")),
  list(func = c("v_CIcong:RSd" = "exp")),
  list(func = c(Z_lbCI = "exp")),
  NULL
)

constants <- list(
  c(s=log(1)),
  c(v_CIcong=0,s=log(1)),
  c(v_CIcong=0,s=log(1)),
  c(v_CIcong=0,s=log(1)),
  c(s=log(1)),
  c(s=log(1))
)

for (i in 1:length(formulas)) {
  d <- design(data=dat,model=DDM,
              matchfun=matchfun,functions = list(lb=lbfun,CI=CIfun),
              formula=formulas[[i]],
              contrasts = list(RS = Smat,lb=Zmat),
              pre_transform = pre_transforms[[i]],
              constants = constants[[i]])
  
  # emc <- make_emc(dat,d)
  # fit(emc,fileName=paste0(names(formulas)[i],".RData"),cores_per_chain=3)
  
}

## DDM----
dDDM_maximal_s <- design(data=dat,model=DDM,
                         matchfun=matchfun,functions = list(lb=lbfun,CI=CIfun),
                         formula=list(v~CI*RS, a~1, t0~1,  Z~lb, s~CI, sv~1, st0~1, SZ~1),
                         contrasts = list(RS = Smat,lb=Zmat),
                         pre_transform = list(func = c("v_CIcong:RSd" = "exp",Z_lbCI = "exp")),
                         constants = c(v_CIcong=0,s=log(1)))



formulas=list(
  DDM_v = list(v~   RS, a~1, t0~1,  Z~lb, s~CI, sv~1, st0~1, SZ~1),
  DDM_z = list(v~CI*RS, a~1, t0~1,  Z~1,  s~CI, sv~1, st0~1, SZ~1),
  DDM_s = list(v~CI*RS, a~1, t0~1,  Z~lb, s~1,  sv~1, st0~1, SZ~1),
  DDMv =   list(v~CI*RS, a~1, t0~1,  Z~1, s~1, sv~1, st0~1, SZ~1),
  DDMz = list(v~RS, a~1, t0~1,  Z~lb, s~1, sv~1, st0~1, SZ~1),
  DDMs = list(v~RS, a~1, t0~1,  Z~1, s~CI, sv~1, st0~1, SZ~1)
)

pre_transforms <- list(
  list(func = c(Z_lbCI = "exp")),
  list(func = c("v_CIcong:RSd" = "exp")),
  list(func = c("v_CIcong:RSd" = "exp",Z_lbCI = "exp")),
  list(func = c("v_CIcong:RSd" = "exp")),
  list(func = c(Z_lbCI = "exp")),
  NULL
)

constants <- list(
  c(s=log(1)),
  c(v_CIcong=0,s=log(1)),
  c(v_CIcong=0,s=log(1)),
  c(v_CIcong=0,s=log(1)),
  c(s=log(1)),
  c(s=log(1))
)

for (i in 1:length(formulas)) {
  d <- design(data=dat,model=DDM,
              matchfun=matchfun,functions = list(lb=lbfun,CI=CIfun),
              formula=formulas[[i]],
              contrasts = list(RS = Smat,lb=Zmat),
              pre_transform = pre_transforms[[i]],
              constants = constants[[i]])
  
  # emc <- make_emc(dat,d)
  # fit(emc,fileName=paste0(names(formulas)[i],".RData"),cores_per_chain=12)
  
}
