setwd("C:\\Users\\jessi\\Documents\\Uni\\3. Mastersemester Statistik\\Praktikum")
library(openxlsx)
Raw_Data <- read.delim("ProphaneInputGroupsWithPepQuant.csv")
Sup_File <- read.xlsx("metadata_table.xlsx")
#SupFile <- read.csv("SupplementaryFile1.csv", sep = ";")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")



# Datenaufbereitung -------------------------------------------------------

## .dat Endung in der Sample ID entfernen
Sup_File$ID <- gsub("\\.dat$", "", Sup_File$ID)


length(unique(Raw_Data$Metaprotein.Number))
## Es gibt 45432 verschiedene Metaproteine im Datensatz

sum(is.na(Raw_Data))
## Es gibt keine NAs im Datensatz

sum(Raw_Data==0)
## Es gibt 42199893 Nullen im Datensatz (0 = In diesem Sample wurde 0 mal dieses Metaprotein gefunden)






## Zählen, wie viele verschiedene Metaproteine in den einzelnen Samples gefunden wurden
Count_Data <- data.frame(Sample = rep(NA, 960), Metaproteins_unique = rep(NA, 960))

for(i in 11:970){
  Count_Data[i - 10, 1] <- colnames(Raw_Data)[i]
  Count_Data[i - 10, 2] <- sum(Raw_Data[,i] != 0) ## != 0 bedeutet, es wurde mindestens einmal ein bestimmtes Metaprotein gefunden, sum zählt alle wo das TRUE ist
}

min(Count_Data$Metaproteins_unique)
max(Count_Data$Metaproteins_unique)






## Welche Metaproteine wurden in den Stichproben gefunden
Metaproteins_Found <- data.frame(Metaprotein_Number = Raw_Data[,1])

for(j in 1:960){
   Metaproteins_Found[,j+1] <- Metaproteins_Found$Metaprotein_Number %in% Raw_Data[which(Raw_Data[,j + 10] != 0), 1]
   ## Überprüft, welche Metaproteine gefunden wurden (Welche Metaproteine ungleich 0 sind) und codiert mit TRUE/FALSE
}

colnames(Metaproteins_Found)[2:961] <- colnames(Raw_Data[11:970])

sum(Metaproteins_Found$EXP.01_C.mgf) ## stimmt mit Count_Data überein


rows_all_true <- apply(Metaproteins_Found[,-1], 1, function(x) all(x == TRUE))
any(rows_all_true)
sum(rows_all_true) ## In wie vielen Zeilen sind alle Werte TRUE

rows_true <- rowSums(Metaproteins_Found[,-1]) ## In wie vielen Studien wurden die einzelnen Metaproteine gefunden
max(rows_true)
min(rows_true)
hist(rows_true)
sum(rows_true >= 480) ## Wie viele Metaproteine wurden in über der Hälfte der Studien gefunden 




# Subset Datensatz --------------------------------------------------------


## Subset vom Rohdatensatz mit den Daten, die zum Sup_File passen
## also behalte nur die Spalten (Samples), die auch im Sup_File vorkommen
## in der gleichen Reihenfolge wie in Sup_File

Sup_File$ID %in% Count_Data$Sample ## Sind die Sample IDs im Rohdatensatz enthalten?

Subset_Data <- cbind(Raw_Data[, 1:10], Raw_Data[, Sup_File$ID[Sup_File$ID %in% names(Raw_Data)]])

#Subset_Data <- cbind(Raw_Data[, 1:10], Raw_Data[, names(Raw_Data) %in% Sup_File$ID])



## Zählen, wie viele verschiedene Metaproteine in den einzelnen Samples gefunden wurden
Count_Data_Subset <- data.frame(Sample = rep(NA, 427), Metaproteins_unique = rep(NA, 427))

for(i in 11:437){
  Count_Data_Subset[i - 10, 1] <- colnames(Subset_Data)[i]
  Count_Data_Subset[i - 10, 2] <- sum(Subset_Data[,i] != 0) ## != 0 bedeutet, es wurde mindestens einmal ein bestimmtes Metaprotein gefunden, sum zählt alle wo das TRUE ist
}


## absolute Anzahl an gefundenen Metaproteinen pro Sample
Count_Data_Subset$Metaproteins_total <- as.numeric(colSums(Subset_Data[,11:437]))

## Zu Count_Data_Subset study, study2, disease, batch und condition (aus Sup_File) hinzufügen

Sup_File <- Sup_File[Sup_File$ID %in% Count_Data$Sample, ] ## Die IDs entfernen, die nicht vorkommen

Count_Data_Subset$study <- Sup_File$study
Count_Data_Subset$study2 <- Sup_File$study2
Count_Data_Subset$disease <- Sup_File$disease
Count_Data_Subset$condition <- Sup_File$condition
Count_Data_Subset$batch <- Sup_File$batch








## Welche Metaproteine wurden in den Stichproben gefunden
Metaproteins_Found_Subset <- data.frame(Metaprotein_Number = Subset_Data[,1])

for(j in 1:427){
  Metaproteins_Found_Subset[,j+1] <- Metaproteins_Found_Subset$Metaprotein_Number %in% Subset_Data[which(Subset_Data[,j + 10] != 0), 1]
  ## Überprüft, welche Metaproteine gefunden wurden (Welche Metaproteine ungleich 0 sind) und codiert mit TRUE/FALSE
}

colnames(Metaproteins_Found_Subset)[2:428] <- colnames(Subset_Data[11:437])


rows_all_true_subset <- apply(Metaproteins_Found_Subset[,-1], 1, function(x) all(x == TRUE))
any(rows_all_true_subset)
sum(rows_all_true_subset)

rows_true_subset <- rowSums(Metaproteins_Found_Subset[,-1]) ## In wie vielen Studien wurden die einzelnen Metaproteine gefunden
max(rows_true_subset)
min(rows_true_subset)
hist(rows_true_subset)
sum(rows_true_subset >= 214) ## Wie viele Metaproteine wurden in über der Hälfte der Studien gefunden

















# Paket MultiBaC ------------------------------------

BiocManager::install("MultiBaC")
browseVignettes("MultiBaC")
library("MultiBaC")

data("multiyeast")
## createMbac: generiert eine Listenobjekt, welches für weitere Mbac Funktionen benötigt wird
my_mbac <- createMbac(inputOmics = list(A.rna, A.gro, B.rna, B.ribo, C.rna, C.par),
                       batchFactor = c("A", "A", "B", "B", "C", "C"),
                       experimentalDesign = list("A" = c("Glu+", "Glu+",
                                                         "Glu+", "Glu-", "Glu-", "Glu-"),
                                                 "B" = c("Glu+", "Glu+", "Glu-", "Glu-"),
                                                 "C" = c("Glu+", "Glu+", "Glu-", "Glu-")),
                       omicNames = c("RNA", "GRO", "RNA", "RIBO", "RNA", "PAR"))

## batchEstPlot: This function uses linear models to estimate the batch effect magnitude using the common data
## across batches. It compares the result with theoretical distribution of diferrent levels of batch magnitude
batchEstPlot(my_mbac) 



## genModelList: This function performs PLS models for every batch. A PLS model is generated for each noncommon omic in each batch
my_mbac_2 <- genModelList(my_mbac, test.comp = NULL,
                           scale = FALSE, center = TRUE,
                           crossval = NULL,
                           showinfo = TRUE)

## genMissingOmics: This function generates for all the batches the omic data they had not originally. This is the previous
## step to apply ARSyNbac [1] correction
multiBatchDesign <- genMissingOmics(my_mbac_2)

## batchCorrection: Batch Correction mit ARSyNbac correction
my_finalwise_mbac <- batchCorrection(my_mbac_2,
                                     multiBatchDesign = multiBatchDesign,
                                     Interaction = FALSE,
                                     Variability = 0.9)


## MultiBaC: Batch Correction mit MultiBaC correction
my_final_mbac <- MultiBaC(my_mbac,
                           test.comp = NULL, scale = FALSE,
                           center = TRUE, crossval = NULL,
                           Variability = 0.90,
                           Interaction = TRUE ,
                           showplot = FALSE,
                           showinfo = FALSE)


plot(my_final_mbac) ## Enthält explained_varPlot und Q2_plot (MultiBaC)
plot(my_finalwise_mbac) ## Enthält explained_varPlot und Q2_plot (ARSyNbac)

plot_pca(my_mbac, typeP = "pca.org") ## pca plot for original data
plot_pca(my_final_mbac, typeP = "pca.cor") ## pca plot for corrected data (MultiBaC)
plot_pca(my_finalwise_mbac, typeP = "pca.cor") ## pca plot for corrected data (ARSyNbac)

inner_relPlot (my_final_mbac)
par(mfrow=c(1,1))

summary(my_mbac)





# Paket sva (ComBat, Leek et al. 2012) ---------------------------------------------------------------

BiocManager::install("sva")
BiocManager::install("bladderbatch")

library(sva)
library(bladderbatch)
data(bladderdata)
dat <- bladderEset[1:50,]
pheno = pData(dat)
edata = exprs(dat)
batch = pheno$batch
mod = model.matrix(~as.factor(cancer), data=pheno)
# parametric adjustment
combat_edata1 = ComBat(dat=edata, batch=batch, mod=NULL, par.prior=TRUE, prior.plots=FALSE)
# non-parametric adjustment, mean-only version
combat_edata2 = ComBat(dat=edata, batch=batch, mod=NULL, par.prior=FALSE, mean.only=TRUE)
# reference-batch version, with covariates
combat_edata3 = ComBat(dat=edata, batch=batch, mod=mod, par.prior=TRUE, ref.batch=3)

n.sv = num.sv(edata,mod,method="leek")
## empirical.controls: A function for estimating the probability that each gene is an empirical
## control
pcontrol <- empirical.controls(edata,mod,mod0=NULL,n.sv=n.sv,type="norm")


mod0 = model.matrix(~1,data=pheno)
## f.pvalue: A function for quickly calculating f statistic p-values for use in sva
pValues = f.pvalue(edata,mod,mod0)
qValues = p.adjust(pValues,method="BH")

## fstats: A function for quickly calculating f statistics for use in sva
fs <- fstats(edata, mod, mod0)

svobj = sva(edata,mod,mod0,n.sv=n.sv)

## sva.check : A function for post-hoc checking of an sva object to check for degenerate cases.
svacheckobj = sva.check(svobj,edata,mod,mod0)








# Paket limma (Ritchie et al. 2015) -------------------------------------------------------------

BiocManager::install("limma")






# Quantifizierung der Batch Effect Correction -----------------------------

## Silhouette Score
library("cluster")

## Für kmeans müssen Zeilen die batches sein
Example_Data <- t(cbind(A.gro, A.rna, B.ribo, B.rna, C.par, C.rna))
C_example <- kmeans(Example_Data, 6)
S_example <- silhouette(C_example$cluster, dist(Example_Data))
mean(S_example[, "sil_width"])


## Anzahl der gefundenen Metaproteine Clustern
C_example2 <- kmeans(Count_Data[,2], 4)
S_example2 <- silhouette(C_example2$cluster, dist(Count_Data[,2]))
mean(S_example2[, "sil_width"])


## PCA mit formula: MetaProteins = \mu + Studie + Krankheit
