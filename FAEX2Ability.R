setwd("#Ability.txt所在文件夹,\需替换为\\")
ability <- as.matrix(read.table("Ability.txt"))

library(corrplot)
library(GPArotation)
corrplot(ability,order= "hclust")#粗浅的通过图来判断因子个数

library(psych)#提供比R自带的factanal更好的因子分析函数fa
cortest.bartlett(ability,n=556)
fa.parallel(ability,n.obs=556,fa="both",n.iter = 100,
            main = "Screen plots with parallel analysis")#一般选择折点的横坐标作为因子个数

NumFA=2
fa <- fa(ability,nfactors = 2,rotate="promax",fm="ml",scores="regression",residuals=TRUE)
fa
fa.varimax <- fa(ability,nfactors=2,rotate="varimax",fm="ml")
fa.varimax
fa.promax <- fa(ability,nfactors=2,rotate="promax",fm="pa")
fa.promax

sapply(1:NumFA, function(f) factanal(covmat=ability,n.obs=556, factors = f, method ="mle")$PVAL)

FAmle1=factanal(covmat=ability,n.obs=556, factors = 2, method ="mle", rotation = "promax")#only mle can be used in factanal
FAmle2= fa(ability, nfactors = 2,fm = "ml", rotate = "varimax", scores="regression",residuals=TRUE)
FAmle1
FAmle2

Score1 <- FAmle1$scores #no scoare is available if covmat is used

FAmle1$uniquenesses
(FAmle1.communality=1-FAmle1$uniquenesses)

URFAmle1=factanal(covmat=ability,n.obs=556,factors = 2, method ="mle", rotation = "none")#only mle can be used in factanal
#URFAmle2 <- fa(life, nfactors = 3,fm = "ml", rotate = "none", scores="regression")

plot(URFAmle1$loadings[,c(1:2)], pch = as.character(c(1:dim(ability)[2])),
     xlab = expression(paste(gamma,"1")), ylab = expression(paste(gamma,"2")),
     main = "First and second loadings",
     xlim = c(-1,1), ylim = c(-1,1))
points(FAmle1$loadings[,c(1:2)],
       pch = letters[c(1:dim(ability)[2])], col = "red")
abline(h = 0)
abline(v = 0)

library(semPlot)
semPaths(FAmle1,what="est",residuals=F,cut=0.4,posCol=c("white","darkgreen"),negCol=c("white","red"),nCharNodes = 4)
semPaths(fa,what="est",residuals=F,cut=0.4,posCol=c("white","darkgreen"),negCol=c("white","red"),nCharNodes = 4)
semPaths(fa.varimax,what="est",residuals=F,cut=0.4,posCol=c("white","darkgreen"),negCol=c("white","red"),nCharNodes = 4)
