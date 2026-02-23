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



# Zählen, wie viele verschiedene Metaproteine in den einzelnen Samples gefunden wurden
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


#ANOVA
summary(aov(Metaproteins_unique ~ condition + study, data = Count_Data_Subset))
summary(aov(Metaproteins_total ~ condition + study, data = Count_Data_Subset))







# Array

## 45432 Zeilen (Metaproteine), 7 Spalten,  427 Samples
Subset_Array <- array(NA, dim = c(45432, 7, 427))
dimnames(Subset_Array) <- list(NULL, c("Metaprotein.Number", "Metaproteins_Found", "study", "study2",
                                      "disease", "condition", "batch"), Sample = Count_Data_Subset$Sample)

Subset_Array[,1,] <- Subset_Data$Metaprotein.Number ## Überall die Metaproteinnummern einfügen

## Schleife über alle Samples
for(n in 1:427){
  Subset_Array[,2,n] <- Subset_Data[,n+10] ## In jedem Sample in der 2. Spalte die Anzahl einfügen, wie oft das jeweilige Metaprotein gefunden wurde
  Subset_Array[,3,n] <- rep(Count_Data_Subset[n,4], 45432) ## study
  Subset_Array[,4,n] <- rep(Count_Data_Subset[n,5], 45432) ## study2
  Subset_Array[,5,n] <- rep(Count_Data_Subset[n,6], 45432) ## disease
  Subset_Array[,6,n] <- rep(Count_Data_Subset[n,7], 45432) ## condition
  Subset_Array[,7,n] <- rep(Count_Data_Subset[n,8], 45432) ## batch
}

## Test: Subset_Array[,,"EXP.ABC.01.mgf"], Subset_Array[,,"EXP.P02_C.mgf"]




# Das Gleiche als Liste statt als Array

Subset_List <- vector("list", length = 427)
names(Subset_List) <- Count_Data_Subset$Sample ## Samples

## Schleife über alle Samples
for(n in seq_len(427)) {
  Subset_List[[n]] <- data.frame(
    Metaprotein.Number   <- Subset_Data$Metaprotein.Number,
    Metaproteins_Found   <- Subset_Data[, n + 10],
    study                <- rep(Count_Data_Subset[n, 4], 45432),
    study2               <- rep(Count_Data_Subset[n, 5], 45432),
    disease              <- rep(Count_Data_Subset[n, 6], 45432),
    condition            <- rep(Count_Data_Subset[n, 7], 45432),
    batch                <- rep(Count_Data_Subset[n, 8], 45432)
  )
}






# Gefundene Metaproteine pro Sample in eine Liste
Metaproteins_Found_List <- vector("list", length = 427)
names(Metaproteins_Found_List) <- Count_Data_Subset$Sample ## Samples
for(m in 1:427){
  Metaproteins_Found_List[[m]] <- Subset_List[[m]][which(Subset_List[[m]][,2] != 0), 1]
}









# Gefundene Metaproteine nach Studien aufteilen
table(sapply(Subset_List, function(x) x[,3]))
Study_List <- vector("list", length = 9)
names(Study_List) <- names(table(sapply(Subset_List, function(x) x[,3])))

studies <-  names(table(sapply(Subset_List, function(x) x[,3])))

Study_List <- lapply(studies, function(study_name) {
  ## Alle Samples nach Studien aufteilen (es reicht erstes Element der 3. Spalte, da in einem Sample immer der gleiche Studienname steht)
  samples_in_study <- Subset_List[sapply(Subset_List, function(x) x[1, 3] == study_name)]
  
  ## Gefundene Metaproteine aus diesen Samples (nur die behalten, die >0 sind)
  protein_lists <- lapply(samples_in_study, function(x) x$Metaprotein.Number[x$Metaproteins_Found != 0])
  
  ## Vereinigung der gefundenen Metaproteine über alle Samples der Studie
  all_proteins <- unique(unlist(protein_lists))
  
  return(all_proteins)
})

names(Study_List) <- studies

Study_List

common_proteins <- Reduce(intersect, Study_List) ## interset gibt Vektor von überschneidungen aus, Reduce wendet es auf die ganze Liste an
length(common_proteins) ## 709 Metaproteine überschneiden sich in allen Studien

# Discovery Studien
Study_List_Discovery <- list(Study_List$Henry, Study_List$`Thuy-Boun`, Study_List$Lehmann, Study_List$`Lloyd-Price`)
names(Study_List_Discovery) <- c("Henry", "Thuy-Boun", "Lehmann", "Lloyd-Price")
studies_discovery <- c("Henry", "Thuy-Boun", "Lehmann", "Lloyd-Price")
common_proteins_discovery <- Reduce(intersect, Study_List_Discovery)
length(common_proteins_discovery) ## 1838 Metaproteine überschneiden sich in allen Discovery Studien




# Venn Diagramme
#install.packages("VennDiagram")
library(VennDiagram)
venn.diagram(Study_List_Discovery, category.names = names(Study_List_Discovery),
             filename = NULL, alpha = 0.5, cat.cex = 1.2, cex = 1.2,
             fill = c("cadetblue","olivedrab3", "khaki1", "indianred"))







# Häufigkeit der gefundenen Metaproteine vergleichen

Study_Counts <- lapply(studies, function(study_name) {
  ## Alle Samples nach Studien aufteilen
  samples_in_study <- Subset_List[sapply(Subset_List, function(x) x[1, 3] == study_name)]
  
  ## Alle relevanten Zeilen aus diesen Samples zusammenführen
  df_study <- do.call(rbind,
                      lapply(samples_in_study,
                             function(x) data.frame(Metaprotein.Number = x$Metaprotein.Number,
                                                    Metaproteins_Found = x$Metaproteins_Found)))
  
  ## Nur Proteine behalten, die überhaupt gefunden wurden (>0)
  df_study <- df_study[df_study$Metaproteins_Found > 0, ]
  
  ## Gesamtanzahl pro Protein berechnen
  counts_per_protein <- aggregate(Metaproteins_Found ~ Metaprotein.Number,
                                  data = df_study,
                                  FUN = sum)
  
  return(counts_per_protein)
})

names(Study_Counts) <- studies

Study_Counts_Discovery <- list(Study_Counts$Henry, Study_Counts$`Thuy-Boun`, Study_Counts$Lehmann, Study_Counts$`Lloyd-Price`)
names(Study_Counts_Discovery) <- c("Henry", "Thuy-Boun", "Lehmann", "Lloyd-Price")
venn.diagram(lapply(Study_Counts_Discovery, function(x) x[,1]), category.names = names(Study_Counts_Discovery),
             filename = NULL, alpha = 0.5, cat.cex = 1.2, cex = 1.2,
             fill = c("cadetblue","olivedrab3", "khaki1", "indianred"))
## Venndiagramm stimmt mit Study_List_Discovery überein



## Wie oft wurden in den Studien einzelne Metaproteine gefunden 
par(mfrow = c(2,2))
boxplot(Study_Counts$`Lloyd-Price`[, 2], main = "Lloyd-Price")
boxplot(Study_Counts$Lehmann[, 2], main = "Lehmann")
boxplot(Study_Counts$`Thuy-Boun`[, 2], main = "Thuy-Boun")
boxplot(Study_Counts$Henry[, 2], main = "Henry")
par(mfrow = c(1,1))














# Paket sva (ComBat, Leek et al. 2012) ---------------------------------------------------------------

BiocManager::install("sva")

library(sva)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(patchwork)

Subset_List_Discovery <- Filter(function(x) { ## Filter: behält nur die Elemente der Liste, wo TRUE ist
  any(unique(x[,3]) %in% studies_discovery)
}, Subset_List)

# Zielstruktur für Combat:
# Zeilen = Metaproteine
# Spalten = Samples
# Werte = Häufigkeiten
# Vektor batch = Studienzuordnung pro Sample

sample_names <- names(Subset_List_Discovery) ## Namen der Samples aus der Liste
study_names <- sapply(Subset_List_Discovery, function(x) unique(x[,3])) ## vektor der angibt zu welcher Studie jedes Sample gehört
table(study_names)

Subset_List_Discovery <- imap(Subset_List_Discovery, ~ mutate(.x, Sample = .y)) ## Allen Dataframes in der Liste die Spalte Sample hinzufügen
df_long <- bind_rows(Subset_List_Discovery) ## Kombiniere alle Samples in einen langen Dataframe
names(df_long) <- c("Metaprotein.Number", "Metaproteins_Found", "study", "study2", "disease", "condition", "batch", "Sample")

## In breites Format umwandeln
matrix_df <- df_long |> 
  select("Metaprotein.Number", "Sample", "Metaproteins_Found") |> 
  pivot_wider(names_from = "Sample",
              values_from = "Metaproteins_Found",
              values_fill = list(Metaproteins_Found = NA)) |>
  as.data.frame()

## Metaprotein.Number als Zeilen des Dataframes umwandeln
rownames(matrix_df) <- matrix_df$Metaprotein.Number
matrix_df <- matrix_df[, -1]

## batch Vektor
batch <- study_names[match(colnames(matrix_df), names(study_names))]
table(batch) ## Wie viele Samples gibt es pro Studie


combat_data <- ComBat(as.matrix(matrix_df), batch = batch, mod = NULL, par.prior = TRUE, prior.plots = FALSE)


# PCA zur Kontrolle der Batch Korrektur
pca_before <- prcomp(t(matrix_df))
pca_after  <- prcomp(t(combat_data))

par(mfrow = c(1,1))
df_before <- data.frame(pca_before$x[,1:2], batch=batch)
g1 <- ggplot(df_before, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor ComBat")

df_after <- data.frame(pca_after$x[,1:2], batch=batch)
g2 <- ggplot(df_after, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach ComBat")

g1 + g2













# Quantifizierung der Batch Effect Correction -----------------------------

# Silhouette Score
library(cluster)

batch_labels <- as.factor(batch)

## before
coords_before <- pca_before$x

dist_matrix_before <- dist(coords_before)

sil_before <- silhouette(as.numeric(batch_labels), dist_matrix_before)

## after
coords_after <- pca_after$x

dist_matrix_after <- dist(coords_after)

sil_after <- silhouette(as.numeric(batch_labels), dist_matrix_after)

## Vergleich Silhouetten Score before und after
mean(sil_before[, "sil_width"])
mean(sil_after[, "sil_width"])










# Paket limma (Ritchie et al. 2015) -------------------------------------------------------------

BiocManager::install("limma")











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
