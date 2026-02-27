setwd("C:\\Users\\jessi\\Documents\\Uni\\3. Mastersemester Statistik\\Praktikum")
library(openxlsx)
Raw_Data <- read.delim("ProphaneInputGroupsWithPepQuant.csv")
Sup_File <- read.xlsx("metadata_table.xlsx")

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














# Liste mit Samples als Listeneinträge, Spalten sind jeweils die Informationen über die Metaproteine und Studienmerkmale

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



# Discovery Kohorte
studies_discovery <- c("Henry", "Thuy-Boun", "Lehmann", "Lloyd-Price")
Subset_List_Discovery <- Filter(function(x) { ## Filter(): behält nur die Elemente der Liste, wo TRUE ist
  any(unique(x[,3]) %in% studies_discovery)   ## also nur die Studien, die zur discovery Kohorte gehören
}, Subset_List)

remove_diseases <- c("lehmann_gca", "lehmann_ibs", "lehmann_ca") ## Diese Krankheiten gehören nicht zur discovery Kohorte
Subset_List_Discovery <- Subset_List_Discovery[!sapply(Subset_List_Discovery, function(df) unique(df[,4])) %in% remove_diseases]

table(sapply(Subset_List_Discovery, function(x) unique(x[, 4])))
table(sapply(Subset_List_Discovery, function(x) unique(x[, 5])))
table(sapply(Subset_List_Discovery, function(x) unique(x[, 3])))














# Gefundene Metaproteine und Häufigkeit nach Studien aufteilen

studies <-  names(table(sapply(Subset_List, function(x) x[,3])))

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




# Discovery
Study_Counts_Discovery <- lapply(studies_discovery, function(study_name) {
  ## Alle Samples nach Studien aufteilen
  samples_in_study <- Subset_List_Discovery[sapply(Subset_List_Discovery, function(x) x[1, 3] == study_name)]
  
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

names(Study_Counts_Discovery) <- studies_discovery

library(VennDiagram)
venn.diagram(lapply(Study_Counts_Discovery, function(x) x[,1]), category.names = names(Study_Counts_Discovery),
             filename = NULL, alpha = 0.5, cat.cex = 1.2, cex = 1.2,
             fill = c("cadetblue","olivedrab3", "khaki1", "indianred"))




## Wie oft wurden in den Studien einzelne Metaproteine gefunden 
par(mfrow = c(2,2))
boxplot(Study_Counts$`Lloyd-Price`[, 2], main = "Lloyd-Price")
boxplot(Study_Counts$Lehmann[, 2], main = "Lehmann")
boxplot(Study_Counts$`Thuy-Boun`[, 2], main = "Thuy-Boun")
boxplot(Study_Counts$Henry[, 2], main = "Henry")
par(mfrow = c(1,1))


# Auf Ausreißer testen
install.packages("outliers")
library(outliers)

grubbs.test(Study_Counts$`Lloyd-Price`[,2], type = 10)
grubbs.test(Study_Counts$Lehmann[,2], type = 10)
grubbs.test(Study_Counts$`Thuy-Boun`[,2], type = 10)
grubbs.test(Study_Counts$Henry[,2], type = 10)














# Paket sva (ComBat, Leek et al. 2012) ---------------------------------------------------------------

BiocManager::install("sva")

library(sva)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(patchwork)

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


# Normalisierter dataframe
csum <- as.numeric(colSums(matrix_df))
matrix_df_norm <- matrix_df / rep(csum, each = nrow(matrix_df))
combat_data_norm <- ComBat(as.matrix(matrix_df_norm), batch = batch, mod = NULL, par.prior = TRUE, prior.plots = FALSE)



# PCA zur Kontrolle der Batch Korrektur
pca_before <- prcomp(t(matrix_df))
pca_after_combat  <- prcomp(t(combat_data))

df_before <- data.frame(pca_before$x[,1:2], batch=batch)
g1_combat <- ggplot(df_before, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor ComBat")

df_after_combat <- data.frame(pca_after_combat$x[,1:2], batch=batch)
g2_combat <- ggplot(df_after_combat, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach ComBat")

g1_combat + g2_combat



## Normalisierte Daten
pca_before_norm <- prcomp(t(matrix_df_norm))
pca_after_combat_norm  <- prcomp(t(combat_data_norm))

df_before_norm <- data.frame(pca_before_norm$x[,1:2], batch=batch)
g1_combat_norm <- ggplot(df_before_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor ComBat (normalisiert)")

df_after_combat_norm <- data.frame(pca_after_combat_norm$x[,1:2], batch=batch)
g2_combat_norm <- ggplot(df_after_combat_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach ComBat (normalisiert)")

g1_combat_norm + g2_combat_norm




# Condition betrachten
study_condition <- sapply(Subset_List_Discovery, function(x) unique(x[, 6])) ## Vektor der angibt, ob das Sample control oder diseased ist

batch_condition <- study_condition[match(colnames(matrix_df), names(study_condition))]


pca_before <- prcomp(t(matrix_df))
pca_after_combat  <- prcomp(t(combat_data))

df_condition_before <- data.frame(pca_before$x[,1:2], condition=batch_condition)
g1_condition_combat <- ggplot(df_before, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Vor ComBat")

df_condition_after_combat <- data.frame(pca_after_combat$x[,1:2], condition=batch_condition)
g2_condition_combat <- ggplot(df_after_combat, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Nach ComBat")

g1_condition_combat + g2_condition_combat




# Disease betrachten
study_disease <- sapply(Subset_List_Discovery, function(x) unique(x[, 5]))

batch_disease <- study_disease[match(colnames(matrix_df), names(study_disease))]


pca_before <- prcomp(t(matrix_df))
pca_after_combat  <- prcomp(t(combat_data))

df_disease_before <- data.frame(pca_before$x[,1:2], condition=batch_disease)
g1_disease_combat <- ggplot(df_before, aes(PC1, PC2, color=batch_disease)) +
  geom_point() + ggtitle("Vor ComBat")

df_disease_after_combat <- data.frame(pca_after_combat$x[,1:2], condition=batch_disease)
g2_disease_combat <- ggplot(df_after_combat, aes(PC1, PC2, color=batch_disease)) +
  geom_point() + ggtitle("Nach ComBat")

g1_disease_combat + g2_disease_combat

















# Paket limma (Ritchie et al. 2015) -------------------------------------------------------------

BiocManager::install("limma")
library(limma)

limma_data <- removeBatchEffect(as.matrix(matrix_df), batch = batch)

# PCA zur Kontrolle der Batch Korrektur
pca_before <- prcomp(t(matrix_df))
pca_after_limma  <- prcomp(t(limma_data))

df_before <- data.frame(pca_before$x[,1:2], batch=batch)
g1_limma <- ggplot(df_before, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor limma")

df_after_limma <- data.frame(pca_after_limma$x[,1:2], batch=batch)
g2_limma <- ggplot(df_after_limma, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach limma")

g1_limma + g2_limma



# Normalisierte Daten
limma_data_norm <- removeBatchEffect(as.matrix(matrix_df_norm), batch = batch)


pca_before_norm <- prcomp(t(matrix_df_norm))
pca_after_limma_norm  <- prcomp(t(limma_data_norm))

df_before_norm <- data.frame(pca_before_norm$x[,1:2], batch=batch)
g1_limma_norm <- ggplot(df_before_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor limma (normalisiert)")

df_after_limma_norm <- data.frame(pca_after_limma_norm$x[,1:2], batch=batch)
g2_limma_norm <- ggplot(df_after_limma_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach limma (normalisiert)")

g1_limma_norm + g2_limma_norm




# Condition betrachten
pca_before <- prcomp(t(matrix_df))
pca_after_limma  <- prcomp(t(limma_data))

df_condition_before <- data.frame(pca_before$x[,1:2], condition=batch_condition)
g1_condition_limma <- ggplot(df_before, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Vor limma")

df_condition_after_limma <- data.frame(pca_after_limma$x[,1:2], condition=batch_condition)
g2_condition_limma <- ggplot(df_after_limma, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Nach limma")

g1_condition_limma + g2_condition_limma




















# Paket harmony (Korsunsky et al. 2019) -----------------------------------------------------------

install.packages("harmony")
library(harmony)


## Dateiformat für harmony: Samples als Zeilen, Metaproteine als Spalten -> matrix_df transponieren

harmony_data <- RunHarmony(data_mat = as.matrix(t(matrix_df)), meta_data = batch, vars_use = "batch")



pca_before <- prcomp(t(matrix_df))
pca_after_harmony  <- prcomp(harmony_data) ## prcomp erwartet Samples als Zeilen, was hier schon der Fall ist

df_before <- data.frame(pca_before$x[,1:2], condition=batch)
g1_harmony <- ggplot(df_before, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor harmony")

df_after_harmony <- data.frame(pca_after_harmony$x[,1:2], condition=batch)
g2_harmony <- ggplot(df_after_harmony, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach harmony")

g1_harmony + g2_harmony


## Alle 4 in eine Grafik
g1 <- ggplot(df_before, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor Korrektur")

(g1 | g2_combat) /
(g2_limma | g2_harmony)






# Normalisierte Daten
harmony_data_norm <- RunHarmony(data_mat = as.matrix(t(matrix_df_norm)), meta_data = batch, vars_use = "batch")


pca_before_norm <- prcomp(t(matrix_df_norm))
pca_after_harmony_norm  <- prcomp(harmony_data_norm) ## prcomp erwartet Samples als Zeilen, was hier schon der Fall ist

df_before_norm <- data.frame(pca_before_norm$x[,1:2], condition=batch)
g1_harmony_norm <- ggplot(df_before_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor harmony (normalisiert)")

df_after_harmony_norm <- data.frame(pca_after_harmony_norm$x[,1:2], condition=batch)
g2_harmony_norm <- ggplot(df_after_harmony_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach harmony (normalisiert)")

g1_harmony_norm + g2_harmony_norm
















# Quantifizierung der Batch Effect Correction -----------------------------

# Silhouette Score
library(cluster)

batch_labels <- as.factor(batch)

## before
coords_before <- pca_before$x

dist_matrix_before <- dist(coords_before)

sil_before <- silhouette(as.numeric(batch_labels), dist_matrix_before)


## after combat
coords_after_combat <- pca_after_combat$x

dist_matrix_after_combat <- dist(coords_after_combat)

sil_after_combat <- silhouette(as.numeric(batch_labels), dist_matrix_after_combat)



## after limma
coords_after_limma <- pca_after_limma$x

dist_matrix_after_limma <- dist(coords_after_limma)

sil_after_limma <- silhouette(as.numeric(batch_labels), dist_matrix_after_limma)



## after harmony
coords_after_harmony <- pca_after_harmony$x

dist_matrix_after_harmony <- dist(coords_after_harmony)

sil_after_harmony <- silhouette(as.numeric(batch_labels), dist_matrix_after_harmony)


## Vergleich average Silhouetten Score before und after
mean(sil_before[, "sil_width"])
mean(sil_after_combat[, "sil_width"])
mean(sil_after_limma[, "sil_width"])
mean(sil_after_harmony[, "sil_width"])




# Normalisierte Daten

## before
coords_before_norm <- pca_before_norm$x

dist_matrix_before_norm <- dist(coords_before_norm)

sil_before_norm <- silhouette(as.numeric(batch_labels), dist_matrix_before_norm)


## after combat
coords_after_combat_norm <- pca_after_combat_norm$x

dist_matrix_after_combat_norm <- dist(coords_after_combat_norm)

sil_after_combat_norm <- silhouette(as.numeric(batch_labels), dist_matrix_after_combat_norm)



## after limma
coords_after_limma_norm <- pca_after_limma_norm$x

dist_matrix_after_limma_norm <- dist(coords_after_limma_norm)

sil_after_limma_norm <- silhouette(as.numeric(batch_labels), dist_matrix_after_limma_norm)



## after harmony
coords_after_harmony_norm <- pca_after_harmony_norm$x

dist_matrix_after_harmony_norm <- dist(coords_after_harmony_norm)

sil_after_harmony_norm <- silhouette(as.numeric(batch_labels), dist_matrix_after_harmony_norm)


## Vergleich average Silhouetten Score before und after
mean(sil_before_norm[, "sil_width"])
mean(sil_after_combat_norm[, "sil_width"])
mean(sil_after_limma_norm[, "sil_width"])
mean(sil_after_harmony_norm[, "sil_width"])








# Condition betrachten 
batch_labels_condition <- as.factor(batch_condition)

## before
coords_before <- pca_before$x

dist_matrix_before <- dist(coords_before)

sil_condition_before <- silhouette(as.numeric(batch_labels_condition), dist_matrix_before)

## after combat
coords_after_combat <- pca_after_combat$x

dist_matrix_after_combat <- dist(coords_after_combat)

sil_condition_after_combat <- silhouette(as.numeric(batch_labels_condition), dist_matrix_after_combat)

## Vergleich Silhouetten Score before und after
mean(sil_condition_before[, "sil_width"])
mean(sil_condition_after_combat[, "sil_width"])


## after limma
coords_after_limma <- pca_after_limma$x

dist_matrix_after_limma <- dist(coords_after_limma)

sil_condition_after_limma <- silhouette(as.numeric(batch_labels_condition), dist_matrix_after_limma)


mean(sil_condition_before[, "sil_width"])
mean(sil_condition_after_limma[, "sil_width"])











# kBET
library(devtools)
install_github('theislab/kBET')
library(kBET)
set.seed(2)

k_bet <- kBET(as.matrix(matrix_df), batch = batch, plot = FALSE, n_repeat = 1000)
k_bet$summary

k_bet_combat <- kBET(combat_data, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_combat$summary

k_bet_limma <- kBET(limma_data, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_limma$summary

k_bet_harmony <- kBET(harmony_data, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_harmony$summary

















# Paket batchelor ---------------------------------------------------------

BiocManager::install("batchelor")
library(batchelor)

## Gefordetes Datenformat: Zeilen = Metaproteine, Spalten = Sample, Liste von Matrizen (jede Matrix ein Batch)

batches <- lapply(split(seq_along(batch), batch),
                  function(idx) as.matrix(matrix_df)[, idx])

mnn_data <- fastMNN(batches) 








# Paket MultiBaC (Ugidos et al. 2022) ------------------------------------

BiocManager::install("MultiBaC")
browseVignettes("MultiBaC")
library("MultiBaC")

mbac_data <- createMbac(inputOmics = as.matrix(matrix_df), batchFactor = batch_labels,
                        experimentalDesign = NULL, omicNames = Metaprotein.Number)




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






# Paket batchtma ----------------------------------------------------------

remotes::install_github("stopsack/batchtma")
library(batchtma)

adjust_batch(data = matrix_df, markers = as.matrix(matrix_df), batch = batch, method = simple)

