###Rebel Funding And Terrorism###
###Sam Wilmer
###Merging Aggregated GTD with MasterCSV###
library(stats)
library(dplyr)
library(plm)
Comn<-read.csv(file = "Comn.csv", header = TRUE, sep = ",")
Data_to_link_to_Master<-read.csv(file = "Data to link to Master.csv", header = TRUE, sep = ",")
Comn$YEAR <- Comn$year
Comn$GROUP <- Comn$sideb
###Merge Data Sets on Group and Year Variables###
TerrFund <-merge(Comn, Data_to_link_to_Master, by =c("GROUP","YEAR"), all.x = TRUE)

###Reformatting Variables###

TerrFund$Territory <- ifelse(TerrFund$terrcont == "yes", 1, 0)
summary(TerrFund$Territory)

summary(TerrFund$external_exists)

TerrFund$rebpolwing

TerrFund$rebpol <- ifelse(TerrFund$rebpolwing =="explicit link"|TerrFund$rebpolwing =="alleged link"|TerrFund$rebpolwing =="acknowledged link", 1, 0)
summary(TerrFund$rebpol)

TerrFund <- TerrFund %>%
  mutate(nkill = ifelse(is.na(nkill),0,nkill))

TerrFund$incomp <- ifelse(TerrFund$incomp == 1, 1, 0)

###Lagged dependent variables###
TerrFund <- pdata.frame(TerrFund, index=c("GROUP","YEAR"))
table(index(TerrFund), useNA = "ifany")

###Negative Binomial###
install.packages("pglm")
library(pglm)

###Model For Number of Terrorist Attacks
Freq1 <- pglm(freq ~ nr_extortion + nr_smuggling + extortion + imf_gdpc +
                 imf_pop + Territory + external_exists + intensity + p_polity2 + wdi_taxipcgt + incomp + region + rebpol,
               TerrFund, family = negbin, model = "within", method="bfgs")

summary(Freq1)

Freq2 <- pglm(freq ~  nr_anystrategy + extortion + external_exists + imf_pop +
                wdi_taxipcgt + p_polity2 + Territory + intensity + incomp,
                      TerrFund, family = negbin, model = "within", method="bfgs")

summary(Freq2)

Freq3 <- pglm(freq ~  nr_extortion + nr_smuggling + extortion + external_exists + imf_pop +
                wdi_taxipcgt + p_polity2 + Territory + intensity + incomp,
              TerrFund, family = negbin, model = "within", method="bfgs")

summary(Freq3)

###Model For number killed in terrorist Attacks

Nkill1 <- Freq1 <- pglm(nkill ~ nr_extortion + nr_smuggling + extortion + imf_gdpc +
                          imf_pop + Territory + external_exists + intensity + p_polity2 + wdi_taxipcgt + incomp + region + rebpol,
                        TerrFund, family = negbin, model = "within", method="bfgs")
summary(Nkill1)


Nkill2 <- pglm(nkill ~ nr_anystrategy + extortion + external_exists + imf_pop +
                 wdi_taxipcgt + p_polity2 + Territory + intensity + incomp, 
              TerrFund, family = negbin, model = "within", method = "bfgs")

summary(Nkill2)

Nkill3 <- pglm(nkill ~ nr_extortion + nr_smuggling + extortion + external_exists + imf_pop +
                 wdi_taxipcgt + p_polity2 + Territory + intensity + incomp, 
               TerrFund, family = negbin, model = "within", method = "bfgs")

summary(Nkill3)



summary(TerrFund$nkill)

###Factor Analysis###
library(psych) 
library(car)
library(stargazer)
install.packages("mice")
library(mice)
install.packages("VIM")
library(VIM)

keeps <- c("nkill", "nr_extortion", "nr_smuggling", "extortion", "external_exists","imf_pop",
             "wdi_taxipcgt", "p_polity2", "Territory", "intensity", "incomp", "imf_gdpc", "rebpol")
TerrFund1= TerrFund[keeps]
###Scaling Data for EigenValues
scaled_TerrFund1 <- apply(TerrFund1,2, scale)###Center the measures###
head(scaled_TerrFund1)

Terr1Cov <- cov(scaled_TerrFund1, use = "na.or.complete")
Terr.eigen <- eigen(Terr1Cov)
str(Terr.eigen)

# Extract the loadings
phi <- Terr.eigen$vectors[,1:2]
phi <- -phi###Flip sign for more logical interpretation

row.names(phi) <- c("nkill", "nr_extortion", "nr_smuggling", "extortion", "external_exists","imf_pop",
                    "wdi_taxipcgt", "p_polity2", "Territory", "intensity", "incomp", "imf_gdpc", "rebpol")
colnames(phi) <- c("PC1", "PC2")
phi
# Calculate Principal Components scores
PC1 <- as.matrix(scaled_TerrFund1) %*% phi[,1]
PC2 <- as.matrix(scaled_TerrFund1) %*% phi[,2]

# Create data frame with Principal Components scores
PC <- data.frame(PC1, PC2)
head(PC)

###PVE:Proportion of Variation Explained###
PVE <- Terr.eigen$values / sum(Terr.eigen$values)
round(PVE, 2)

###PVE Plot###
PVEplot <- qplot(c(1:13), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
PVEplot
###Based on the Scree plot I use three factors for the factor analysis

###Factor Analysis###
install.packages("GPArotation")
library(GPArotation)
tempMusic <- mice(MusicClean,m=5,maxit=50,meth='pmm',seed=1337)
summary(tempMusic)

completedMusic <- complete(tempMusic,1)

?fa()
FA.none<-fa(TerrFund1, nfactors = 3, fm="ml", rotate = "none")
print(FA.none)

plot(FA.none$loadings[,1], FA.none$loadings[,3], xlim = c(-0.5, 1), ylim = c(-0.5, 1),
     xlab = "Factor 1", ylab = "Factor 3", main = "No Roation")
abline(h = 0, v = 0)



FA1<-fa(TerrFund1, nfactors = 3, fm="ml", rotate = "varimax")#The orthogonal rotation leads to a more simple structure, Maximum Likelihood estimation
print(FA1)

converted <- as.data.frame(unclass(FA1$loadings))
stargazer(converted, type = "html", out = "FA1.html")
plot(FA1$loadings[,1], FA1$loadings[,3], xlim = c(-0.5, 1), ylim = c(-0.5, 1),
     xlab = "Factor 1", ylab = "Factor 3", main = "Varimax Roation")
abline(h = 0, v = 0)

FA2<-fa(TerrFund1, nfactors = 3, fm="ml", rotate = "quartimax")#The orthogonal rotation leads to a more simple structure, Maximum Likelihood estimation
print(FA2)


plot(FA2$loadings[,1], FA2$loadings[,3], xlim = c(-0.5, 1), ylim = c(-0.5, 1),
     xlab = "Factor 1", ylab = "Factor 3", main = "Quartimax Roation")
abline(h = 0, v = 0)

FA3<-fa(TerrFund1, nfactors = 3, fm="ml", rotate = "Equamax")#The orthogonal rotation leads to a more simple structure, Maximum Likelihood estimation
print(FA3)


plot(FA3$loadings[,1], FA3$loadings[,3], xlim = c(-0.5, 1), ylim = c(-0.5, 1),
     xlab = "Factor 1", ylab = "Factor 3", main = "Equamax Roation")
abline(h = 0, v = 0)

pdf("Rotation Comparison.pdf",11,8.5)
par(mfrow=c(2,2))
par(mar=c(4,4,4,2))
plot(FA.none,labels=colnames(TerrFund1),title="None")
plot(FA1,labels=colnames(TerrFund1),title="Varimax")
plot(FA2,labels=colnames(TerrFund1),title="Quartimax")
plot(FA3,labels=colnames(TerrFund1),title="Equamax")
dev.off()



####Factor 1 are the democratic states that experience civil war, relatively low intensity and weaker groups.  Also these states are higher pop.
###Factor 2 are the very intense civil war years.  Higher levels of extortion and smuggling as well as more killed and conflict intensity.
###Factor 3 are civil wars where the state dominates: higher taxation, no territory relationship.
