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

shared_metaproteins <- Reduce(intersect, lapply(Study_Counts_Discovery, function(x) x[,1])) ## geteilte Metaproteine

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



# Normalisierte Daten
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
g1_condition_combat <- ggplot(df_condition_before, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Vor ComBat")

df_condition_after_combat <- data.frame(pca_after_combat$x[,1:2], condition=batch_condition)
g2_condition_combat <- ggplot(df_condition_after_combat, aes(PC1, PC2, color=batch_condition)) +
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
g1_condition_limma <- ggplot(df_condition_before, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Vor limma")

df_condition_after_limma <- data.frame(pca_after_limma$x[,1:2], condition=batch_condition)
g2_condition_limma <- ggplot(df_condition_after_limma, aes(PC1, PC2, color=batch_condition)) +
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





## Normalisierte Daten
harmony_data_norm <- RunHarmony(data_mat = as.matrix(t(matrix_df_norm)), meta_data = batch, vars_use = "batch")

pca_before_norm <- prcomp(t(matrix_df_norm))
pca_after_harmony_norm  <- prcomp(harmony_data_norm)

df_before_norm <- data.frame(pca_before_norm$x[,1:2], batch=batch)
g1_harmony_norm <- ggplot(df_before_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor harmony (normalisiert)")

df_after_harmony_norm <- data.frame(pca_after_harmony_norm$x[,1:2], batch=batch)
g2_harmony_norm <- ggplot(df_after_harmony_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach harmony (normalisiert)")

g1_harmony_norm + g2_harmony_norm






# Condition betrachten
pca_before <- prcomp(t(matrix_df))
pca_after_harmony  <- prcomp(harmony_data) ## prcomp erwartet Samples als Zeilen, was hier schon der Fall ist

df_condition_before <- data.frame(pca_before$x[,1:2], condition=batch_condition)
g1_condition_harmony <- ggplot(df_condition_before, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Vor harmony")

df_condition_after_harmony <- data.frame(pca_after_harmony$x[,1:2], condition=batch_condition)
g2_condition_harmony <- ggplot(df_condition_after_harmony, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Nach harmony")



## Alle 4 in eine Grafik
g1_condition <- ggplot(df_condition_before, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Vor Korrektur")

(g1_condition | g2_condition_combat) /
(g2_condition_limma | g2_condition_harmony)




















# Paket MMUPHin -----------------------------------------------------------

#BiocManager::install("MMUPHin")
library(MMUPHin)
library(magrittr)
library(dplyr)
library(ggplot2)

## adjust_batch fordert feature matrix mit Samples als Spalten und Meta dataframe mit Samples als Zeilennamen
## und study als Spalte
studies_discovery <- c("Henry", "Thuy-Boun", "Lehmann", "Lloyd-Price")
Sup_File_Discovery <- Sup_File[Sup_File$study %in% studies_discovery,]
remove_diseases2 <- c("GCA", "IBS", "CA")
Sup_File_Discovery <- Sup_File_Discovery[!Sup_File_Discovery$disease %in% remove_diseases2,]
rownames(Sup_File_Discovery) <- Sup_File_Discovery$ID ## Samples sollen Zeilennamen sein
Sup_File_Discovery$ID <- NULL
all(colnames(matrix_df) == rownames(Sup_File_Discovery)) ## prüfen, ob Reihenfolge der Samples übereinstimmt



MMUPHin_data <- adjust_batch(feature_abd = matrix_df, batch = "study", data = Sup_File_Discovery)




pca_before <- prcomp(t(matrix_df))
pca_after_MMUPHin <- prcomp(t(MMUPHin_data$feature_abd_adj))

df_before <- data.frame(pca_before$x[,1:2], condition=batch)
g1_MMUPHin <- ggplot(df_before, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor MMUPHin")

df_after_MMUPHin <- data.frame(pca_after_MMUPHin$x[,1:2], condition=batch)
g2_MMUPHin <- ggplot(df_after_MMUPHin, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach MMUPHin")

g1_MMUPHin + g2_MMUPHin



g1
(g2_MMUPHin | g2_combat) /
  (g2_limma | g2_harmony)




## Normalisierte Daten
MMUPHin_data_norm <- adjust_batch(feature_abd = matrix_df_norm, batch = "study", data = Sup_File_Discovery)

pca_before_norm <- prcomp(t(matrix_df_norm))
pca_after_MMUPHin_norm  <- prcomp(t(MMUPHin_data_norm$feature_abd_adj))

df_before_norm <- data.frame(pca_before_norm$x[,1:2], batch=batch)
g1_MMUPHin_norm <- ggplot(df_before_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor MMUPHin (normalisiert)")

df_after_MMUPHin_norm <- data.frame(pca_after_MMUPHin_norm$x[,1:2], batch=batch)
g2_MMUPHin_norm <- ggplot(df_after_MMUPHin_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach MMUPHin (normalisiert)")

g1_MMUPHin_norm + g2_MMUPHin_norm


g1_norm <- ggplot(df_before_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor Korrektur (normalisiert)")


(g2_MMUPHin_norm | g2_combat_norm) /
  (g2_limma_norm | g2_harmony_norm)
g1_norm













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



## after MMUPHin
coords_after_MMUPHin <- pca_after_MMUPHin$x

dist_matrix_after_MMUPHin <- dist(coords_after_MMUPHin)

sil_after_MMUPHin <- silhouette(as.numeric(batch_labels), dist_matrix_after_MMUPHin)


## Vergleich average Silhouetten Score before und after
mean(sil_before[, "sil_width"])
mean(sil_after_combat[, "sil_width"])
mean(sil_after_limma[, "sil_width"])
mean(sil_after_harmony[, "sil_width"])
mean(sil_after_MMUPHin[, "sil_width"])












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



## after MMUPHin
coords_after_MMUPHin_norm <- pca_after_MMUPHin_norm$x

dist_matrix_after_MMUPHin_norm <- dist(coords_after_MMUPHin_norm)

sil_after_MMUPHin_norm <- silhouette(as.numeric(batch_labels), dist_matrix_after_MMUPHin_norm)


## Vergleich average Silhouetten Score before und after
mean(sil_before_norm[, "sil_width"])
mean(sil_after_combat_norm[, "sil_width"])
mean(sil_after_limma_norm[, "sil_width"])
mean(sil_after_harmony_norm[, "sil_width"])
mean(sil_after_MMUPHin_norm[, "sil_width"])












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

k_bet_MMUPHin <- kBET(MMUPHin_data$feature_abd_adj, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_MMUPHin$summary




# Normalisierte Daten
k_bet_norm <- kBET(as.matrix(matrix_df_norm), batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_norm$summary

k_bet_combat_norm <- kBET(combat_data_norm, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_combat_norm$summary

k_bet_limma_norm <- kBET(limma_data_norm, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_limma_norm$summary

k_bet_harmony_norm <- kBET(harmony_data_norm, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_harmony_norm$summary

k_bet_MMUPHin_norm <- kBET(MMUPHin_data_norm$feature_abd_adj, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_MMUPHin_norm$summary











# PERMANOVA
library(vegan)

## Erklärte Varianz durch die Studien
set.seed(2)
fit_adonis_before <- adonis2(dist_matrix_before ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_after_ComBat <- adonis2(dist_matrix_after_combat ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_after_limma <- adonis2(dist_matrix_after_limma ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_after_harmony <- adonis2(dist_matrix_after_harmony ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_after_MMUPHin <- adonis2(dist_matrix_after_MMUPHin ~ study, data = Sup_File_Discovery, method = "euclidean")
print(fit_adonis_before) 
print(fit_adonis_after_ComBat)
print(fit_adonis_after_limma)
print(fit_adonis_after_harmony)
print(fit_adonis_after_MMUPHin)



## Erklärte Varianz durch die Krankheit
set.seed(2)
fit_adonis_condition_before <- adonis2(dist_matrix_before ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_condition_after_ComBat <- adonis2(dist_matrix_after_combat ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_condition_after_limma <- adonis2(dist_matrix_after_limma ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_condition_after_harmony <- adonis2(dist_matrix_after_harmony ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_condition_after_MMUPHin <- adonis2(dist_matrix_after_MMUPHin ~ condition, data = Sup_File_Discovery, method = "euclidean")
print(fit_adonis_condition_before)
print(fit_adonis_condition_after_ComBat)
print(fit_adonis_condition_after_limma)
print(fit_adonis_condition_after_harmony)
print(fit_adonis_condition_after_MMUPHin)





# Normalisierte Daten

## Erklärte Varianz durch die Studien
set.seed(2)
fit_adonis_before_norm <- adonis2(dist_matrix_before_norm ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_after_ComBat_norm <- adonis2(dist_matrix_after_combat_norm ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_after_limma_norm <- adonis2(dist_matrix_after_limma_norm ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_after_harmony_norm <- adonis2(dist_matrix_after_harmony_norm ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_after_MMUPHin_norm <- adonis2(dist_matrix_after_MMUPHin_norm ~ study, data = Sup_File_Discovery, method = "euclidean")
print(fit_adonis_before_norm) 
print(fit_adonis_after_ComBat_norm)
print(fit_adonis_after_limma_norm)
print(fit_adonis_after_harmony_norm)
print(fit_adonis_after_MMUPHin_norm)



## Erklärte Varianz durch die Krankheit
set.seed(2)
fit_adonis_condition_before_norm <- adonis2(dist_matrix_before_norm ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_condition_after_ComBat_norm <- adonis2(dist_matrix_after_combat_norm ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_condition_after_limma_norm <- adonis2(dist_matrix_after_limma_norm ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_condition_after_harmony_norm <- adonis2(dist_matrix_after_harmony_norm ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_condition_after_MMUPHin_norm <- adonis2(dist_matrix_after_MMUPHin_norm ~ condition, data = Sup_File_Discovery, method = "euclidean")
print(fit_adonis_condition_before_norm)
print(fit_adonis_condition_after_ComBat_norm)
print(fit_adonis_condition_after_limma_norm)
print(fit_adonis_condition_after_harmony_norm)
print(fit_adonis_condition_after_MMUPHin_norm)
















# Biomarker Analyse -------------------------------------------------------

study_condition <- sapply(Subset_List_Discovery, function(x) unique(x[, 6]))
batch_condition <- study_condition[match(colnames(matrix_df), names(study_condition))]

# Erklärte Varianz der Krankheit vor der batch Korrektur
r2 <- apply(matrix_df, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20 <- names(r2)[r2 > 0.20]
proteins_over20 ## NAs können entstehen, wenn kein R^2 berechnet werden kann (zB wenn alle Werte von einem Protein gleich sind)
proteins_over20 <- proteins_over20[!is.na(proteins_over20)] ## NAs entfernen
table(proteins_over20)
Raw_Data$Protein.Accessions[Raw_Data$Metaprotein.Number==2024]
Raw_Data$Protein.Accessions[Raw_Data$Metaprotein.Number==22]
Raw_Data$Protein.Accessions[Raw_Data$Metaprotein.Number==33284]

## Wie oft wurden diese Metaproteine gefunden (in control vs. diseased)
matrix_df[which(rownames(matrix_df)==2024),]
tapply(as.numeric(matrix_df[which(rownames(matrix_df)==2024),]), batch_condition, mean)
tapply(as.numeric(matrix_df[which(rownames(matrix_df)==22),]), batch_condition, sum)
tapply(as.numeric(matrix_df[which(rownames(matrix_df)==33284),]), batch_condition, sum)











# Erklärte Varianz der Krankheit nach Combat
r2_ComBat <- apply(combat_data, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_ComBat <- names(r2_ComBat)[r2_ComBat > 0.2]
proteins_over20_ComBat <- proteins_over20_ComBat[!is.na(proteins_over20_ComBat)]
table(proteins_over20_ComBat)
length(table(proteins_over20_ComBat))

proteins_over20 %in% proteins_over20_ComBat ## Wurden die Metaproteine, die vorher als Biomarker identifiziert
                                            ## wurden, danach auch identifiziert

tapply(as.numeric(combat_data[which(rownames(combat_data)==2024),]), batch_condition, sum)
tapply(as.numeric(combat_data[which(rownames(combat_data)==22),]), batch_condition, sum)
tapply(as.numeric(combat_data[which(rownames(combat_data)==33284),]), batch_condition, sum)

## Wurde in diseased häufiger gefunden als in control
#tapply(as.numeric(combat_data[which(rownames(combat_data)==124),]), batch_condition, mean)
#tapply(as.numeric(combat_data[which(rownames(combat_data)==132),]), batch_condition, mean)
#tapply(as.numeric(combat_data[which(rownames(combat_data)==156),]), batch_condition, mean)
tapply(as.numeric(combat_data[which(rownames(combat_data)==1673),]), batch_condition, mean) ## sehr deutlich
#tapply(as.numeric(combat_data[which(rownames(combat_data)==1703),]), batch_condition, mean)
#tapply(as.numeric(combat_data[which(rownames(combat_data)==204),]), batch_condition, mean)
#tapply(as.numeric(combat_data[which(rownames(combat_data)==366),]), batch_condition, mean)
tapply(as.numeric(combat_data[which(rownames(combat_data)==37164),]), batch_condition, mean) ## sehr deutlich
#tapply(as.numeric(combat_data[which(rownames(combat_data)==400),]), batch_condition, mean)
tapply(as.numeric(combat_data[which(rownames(combat_data)==4293),]), batch_condition, mean) ## sehr deutlich
#tapply(as.numeric(combat_data[which(rownames(combat_data)==75),]), batch_condition, mean)
#tapply(as.numeric(combat_data[which(rownames(combat_data)==803),]), batch_condition, mean)
tapply(as.numeric(combat_data[which(rownames(combat_data)==9040),]), batch_condition, mean) ## sehr deutlich
tapply(as.numeric(combat_data[which(rownames(combat_data)==9510),]), batch_condition, mean) ## sehr deutlich



## Wurde in mehr diseased samples gefunden als in control samples
tapply(as.numeric(combat_data[which(rownames(combat_data)==9510),]!=0), batch_condition, sum) ## alle




## PCA plot der Biomarker
biomarker_combat <- as.numeric(proteins_over20_ComBat) ## oder nur die IBD Biomarker?

study_condition <- sapply(Subset_List_Discovery, function(x) unique(x[, 6])) ## Vektor der angibt, ob das Sample control oder diseased ist
batch_condition <- study_condition[match(colnames(matrix_df), names(study_condition))]
matrix_df_biomarker <- matrix_df[which(rownames(matrix_df) %in% biomarker_combat),] ## nur die Biomarker behalten
combat_data_biomarker <- combat_data[which(rownames(combat_data) %in% biomarker_combat),]


pca_biomarker_before <- prcomp(t(matrix_df_biomarker))
pca_biomarker_after_combat  <- prcomp(t(combat_data_biomarker))

df_biomarker_before <- data.frame(pca_biomarker_before$x[,1:2], condition=batch_condition)
g1_biomarker_combat <- ggplot(df_biomarker_before, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker vor ComBat")

df_biomarker_after_combat <- data.frame(pca_biomarker_after_combat$x[,1:2], condition=batch_condition)
g2_biomarker_combat <- ggplot(df_biomarker_after_combat, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker nach ComBat")

g1_biomarker_combat + g2_biomarker_combat











# Erklärte Varianz der Krankheit nach limma
r2_limma <- apply(limma_data, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_limma <- names(r2_limma)[r2_limma > 0.2]
proteins_over20_limma <- proteins_over20_limma[!is.na(proteins_over20_limma)]
table(proteins_over20_limma)
length(table(proteins_over20_limma))

proteins_over20 %in% proteins_over20_limma

## Wurde in diseased häufiger gefunden als in control
#tapply(as.numeric(limma_data[which(rownames(limma_data)==1078),]), batch_condition, mean)
tapply(as.numeric(limma_data[which(rownames(limma_data)==1673),]), batch_condition, mean) ## sehr deutlich
tapply(as.numeric(limma_data[which(rownames(limma_data)==37164),]), batch_condition, mean) ## sehr deutlich
tapply(as.numeric(limma_data[which(rownames(limma_data)==4293),]), batch_condition, mean) ## sehr deutlich
#tapply(as.numeric(limma_data[which(rownames(limma_data)==627),]), batch_condition, mean)
#tapply(as.numeric(limma_data[which(rownames(limma_data)==803),]), batch_condition, mean) ## deutlich
tapply(as.numeric(limma_data[which(rownames(limma_data)==9040),]), batch_condition, mean) ## sehr deutlich


## Wurde in mehr diseased samples gefunden als in control samples
tapply(as.numeric(limma_data[which(rownames(limma_data)==9040),]!=0), batch_condition, sum) ##alle



## PCA plot der Biomarker
biomarker_limma <- as.numeric(proteins_over20_limma)

matrix_df_biomarker_limma <- matrix_df[which(rownames(matrix_df) %in% biomarker_limma),] ## nur die Biomarker behalten
limma_data_biomarker <- limma_data[which(rownames(limma_data) %in% biomarker_limma),]


pca_biomarker_before <- prcomp(t(matrix_df_biomarker_limma))
pca_biomarker_after_limma  <- prcomp(t(limma_data_biomarker))

df_biomarker_before <- data.frame(pca_biomarker_before$x[,1:2], condition=batch_condition)
g1_biomarker_limma <- ggplot(df_biomarker_before, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker vor limma")

df_biomarker_after_limma <- data.frame(pca_biomarker_after_limma$x[,1:2], condition=batch_condition)
g2_biomarker_limma <- ggplot(df_biomarker_after_limma, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker nach limma")

g1_biomarker_limma + g2_biomarker_limma












# Erklärte Varianz der Krankheit nach harmony
r2_harmony <- apply(t(harmony_data), 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_harmony <- names(r2_harmony)[r2_harmony > 0.2]
proteins_over20_harmony <- proteins_over20_harmony[!is.na(proteins_over20_harmony)]
table(proteins_over20_harmony)
length(table(proteins_over20_harmony))

proteins_over20 %in% proteins_over20_harmony

## Wurde in diseased häufiger gefunden als in control
#tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==1032),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==1078),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==132),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==1712),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==2403),]), batch_condition, mean)
tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==37164),]), batch_condition, mean) ## sehr deutlich
tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==4293),]), batch_condition, mean) ## sehr deutlich
#tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==532),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==688),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==75),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==803),]), batch_condition, mean)
tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==9040),]), batch_condition, mean) ## sehr deutlich
#tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==998),]), batch_condition, mean)


## wurde bei combat und limma deutlich gefunden:
r2_harmony[which(names(r2_harmony) == 1673)] ## nur knapp unter der Biomarker Grenze


## Wurde in mehr diseased samples gefunden als in control samples
tapply(as.numeric(t(harmony_data)[which(rownames(t(harmony_data))==998),]!=0), batch_condition, sum) ## alle



## PCA plots der Biomarker
biomarker_harmony <- as.numeric(proteins_over20_harmony)

matrix_df_biomarker_harmony <- matrix_df[which(rownames(matrix_df) %in% biomarker_harmony),] ## nur die Biomarker behalten
harmony_data_biomarker <- harmony_data[, which(colnames(harmony_data) %in% biomarker_harmony)]


pca_biomarker_before <- prcomp(t(matrix_df_biomarker_harmony))
pca_biomarker_after_harmony  <- prcomp(harmony_data_biomarker)

df_biomarker_before <- data.frame(pca_biomarker_before$x[,1:2], condition=batch_condition)
g1_biomarker_harmony <- ggplot(df_biomarker_before, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker vor harmony")

df_biomarker_after_harmony <- data.frame(pca_biomarker_after_harmony$x[,1:2], condition=batch_condition)
g2_biomarker_harmony <- ggplot(df_biomarker_after_harmony, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker nach harmony")

g1_biomarker_harmony + g2_biomarker_harmony














# Erklärte Varianz der Krankheit nach MMUPHin
r2_MMUPHin <- apply(MMUPHin_data$feature_abd_adj, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_MMUPHin <- names(r2_MMUPHin)[r2_MMUPHin > 0.2]
proteins_over20_MMUPHin <- proteins_over20_MMUPHin[!is.na(proteins_over20_MMUPHin)]
table(proteins_over20_MMUPHin)
length(table(proteins_over20_MMUPHin))

proteins_over20 %in% proteins_over20_MMUPHin


## Wurde in diseased häufiger gefunden als in control
#tapply(as.numeric(MMUPHin_data$feature_abd_adj[which(rownames(MMUPHin_data$feature_abd_adj)==156),]), batch_condition, mean)

## Wurde in mehr diseased samples gefunden als in control samples
tapply(as.numeric(MMUPHin_data$feature_abd_adj[which(rownames(MMUPHin_data$feature_abd_adj)==156),]!=0), batch_condition, sum)
tapply(as.numeric(MMUPHin_data$feature_abd_adj[which(rownames(MMUPHin_data$feature_abd_adj)==998),]!=0), batch_condition, sum)
## alle außer 33284 und 627



## PCA plots der Biomarker
biomarker_MMUPHin <- as.numeric(proteins_over20_MMUPHin)

matrix_df_biomarker_MMUPHin <- matrix_df[which(rownames(matrix_df) %in% biomarker_MMUPHin),] ## nur die Biomarker behalten
MMUPHin_data_biomarker <- MMUPHin_data$feature_abd_adj[which(rownames(MMUPHin_data$feature_abd_adj) %in% biomarker_MMUPHin),]


pca_biomarker_before <- prcomp(t(matrix_df_biomarker_MMUPHin))
pca_biomarker_after_MMUPHin  <- prcomp(t(MMUPHin_data_biomarker))

df_biomarker_before <- data.frame(pca_biomarker_before$x[,1:2], condition=batch_condition)
g1_biomarker_MMUPHin <- ggplot(df_biomarker_before, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker vor MMUPHin")

df_biomarker_after_MMUPHin <- data.frame(pca_biomarker_after_MMUPHin$x[,1:2], condition=batch_condition)
g2_biomarker_MMUPHin <- ggplot(df_biomarker_after_MMUPHin, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker nach MMUPHin")

g1_biomarker_MMUPHin + g2_biomarker_MMUPHin



(g2_biomarker_MMUPHin + g2_biomarker_combat)/
  (g2_biomarker_limma + g2_biomarker_harmony)








biomarker <- list(ComBat = as.numeric(proteins_over20_ComBat), limma = as.numeric(proteins_over20_limma),
                  harmony = as.numeric(proteins_over20_harmony), MMUPHin = as.numeric(proteins_over20_MMUPHin))

venn.diagram(biomarker, category.names = names(biomarker),
             filename = NULL, alpha = 0.5, cat.cex = 1.2, cex = 1.2,
             fill = c("cadetblue","olivedrab3", "khaki1", "indianred"))











# Normalisierte Daten

# Erklärte Varianz der Krankheit vor der batch Korrektur
r2_norm <- apply(matrix_df_norm, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_norm <- names(r2_norm)[r2_norm > 0.20]
proteins_over20_norm <- proteins_over20_norm[!is.na(proteins_over20_norm)] ## NAs entfernen
table(proteins_over20_norm)
length(table(proteins_over20_norm))




# Erklärte Varianz der Krankheit nach Combat
r2_ComBat_norm <- apply(combat_data_norm, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_ComBat_norm <- names(r2_ComBat_norm)[r2_ComBat_norm > 0.2]
proteins_over20_ComBat_norm <- proteins_over20_ComBat_norm[!is.na(proteins_over20_ComBat_norm)]
table(proteins_over20_ComBat_norm)
length(table(proteins_over20_ComBat_norm))

tapply(as.numeric(combat_data_norm[which(rownames(combat_data_norm)==37164),]), batch_condition, mean)






# Erklärte Varianz der Krankheit nach limma
r2_limma_norm <- apply(limma_data_norm, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_limma_norm <- names(r2_limma_norm)[r2_limma_norm > 0.2]
proteins_over20_limma_norm <- proteins_over20_limma_norm[!is.na(proteins_over20_limma_norm)]
table(proteins_over20_limma_norm)
length(table(proteins_over20_limma_norm))

tapply(as.numeric(limma_data_norm[which(rownames(limma_data_norm)==37164),]), batch_condition, mean)





# Erklärte Varianz der Krankheit nach harmony
r2_harmony_norm <- apply(t(harmony_data_norm), 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_harmony_norm <- names(r2_harmony_norm)[r2_harmony_norm > 0.2]
proteins_over20_harmony_norm <- proteins_over20_harmony_norm[!is.na(proteins_over20_harmony_norm)]
table(proteins_over20_harmony_norm)
length(table(proteins_over20_harmony_norm))

tapply(as.numeric(t(harmony_data_norm)[which(rownames(t(harmony_data_norm))==1673),]), batch_condition, mean)





# Erklärte Varianz der Krankheit nach MMUPHin
r2_MMUPHin_norm <- apply(MMUPHin_data_norm$feature_abd_adj, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_MMUPHin_norm <- names(r2_MMUPHin_norm)[r2_MMUPHin_norm > 0.2]
proteins_over20_MMUPHin_norm <- proteins_over20_MMUPHin_norm[!is.na(proteins_over20_MMUPHin_norm)]
table(proteins_over20_MMUPHin_norm)
length(table(proteins_over20_MMUPHin_norm))

tapply(as.numeric(MMUPHin_data_norm$feature_abd_adj[which(rownames(MMUPHin_data_norm$feature_abd_adj)==2347),]), batch_condition, mean)

































# Gleiche Analyse mit 1620 shared Metaproteinen ---------------------------

shared_metaproteins <- Reduce(intersect, lapply(Study_Counts_Discovery, function(x) x[,1])) ## geteilte Metaproteine
Subset_List_Discovery_shared <- lapply(Subset_List_Discovery, function(x){
  x[x[[1]] %in% shared_metaproteins, ] ## nur Zeilen der geteilten Metaproteine behalten
})



## Daten für batch Korrektur vorbereiten
sample_names_shared <- names(Subset_List_Discovery_shared) ## Namen der Samples aus der Liste
study_names_shared <- sapply(Subset_List_Discovery_shared, function(x) unique(x[,3])) ## vektor der angibt zu welcher Studie jedes Sample gehört


Subset_List_Discovery_shared <- imap(Subset_List_Discovery_shared, ~ mutate(.x, Sample = .y)) ## Allen Dataframes in der Liste die Spalte Sample hinzufügen
df_long_shared <- bind_rows(Subset_List_Discovery_shared) ## Kombiniere alle Samples in einen langen Dataframe
names(df_long_shared) <- c("Metaprotein.Number", "Metaproteins_Found", "study", "study2", "disease", "condition", "batch", "Sample")

## In breites Format umwandeln
matrix_df_shared <- df_long_shared |> 
  select("Metaprotein.Number", "Sample", "Metaproteins_Found") |> 
  pivot_wider(names_from = "Sample",
              values_from = "Metaproteins_Found",
              values_fill = list(Metaproteins_Found = NA)) |>
  as.data.frame()

## Metaprotein.Number als Zeilen des Dataframes umwandeln
rownames(matrix_df_shared) <- matrix_df_shared$Metaprotein.Number
matrix_df_shared <- matrix_df_shared[, -1]




## ComBat
combat_data_shared <- ComBat(as.matrix(matrix_df_shared), batch = batch, mod = NULL, par.prior = TRUE, prior.plots = FALSE)

## limma
limma_data_shared <- removeBatchEffect(as.matrix(matrix_df_shared), batch = batch)

## harmony
harmony_data_shared <- RunHarmony(data_mat = as.matrix(t(matrix_df_shared)), meta_data = batch, vars_use = "batch")

## MMUPHin
MMUPHin_data_shared <- adjust_batch(feature_abd = matrix_df_shared, batch = "study", data = Sup_File_Discovery)



## PCA plots
pca_shared_before <- prcomp(t(matrix_df_shared))
pca_shared_after_combat  <- prcomp(t(combat_data_shared))

df_shared_after_combat <- data.frame(pca_shared_after_combat$x[,1:2], batch=batch)
g2_shared_combat <- ggplot(df_shared_after_combat, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach ComBat")



pca_shared_after_limma  <- prcomp(t(limma_data_shared))

df_shared_after_limma <- data.frame(pca_shared_after_limma$x[,1:2], batch=batch)
g2_shared_limma <- ggplot(df_shared_after_limma, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach limma")





pca_shared_after_harmony  <- prcomp(harmony_data_shared) ## prcomp erwartet Samples als Zeilen, was hier schon der Fall ist

df_shared_after_harmony <- data.frame(pca_shared_after_harmony$x[,1:2], condition=batch)
g2_shared_harmony <- ggplot(df_shared_after_harmony, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach harmony")





pca_shared_after_MMUPHin <- prcomp(t(MMUPHin_data_shared$feature_abd_adj))

df_shared_after_MMUPHin <- data.frame(pca_shared_after_MMUPHin$x[,1:2], condition=batch)
g2_shared_MMUPHin <- ggplot(df_shared_after_MMUPHin, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach MMUPHin")


df_shared_before <- data.frame(pca_shared_before$x[,1:2], batch=batch)
g1_shared <- ggplot(df_shared_before, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Vor Korrektur")




(g1_shared | g2_shared_combat) /
  (g2_shared_limma | g2_shared_harmony)


g1_shared
(g2_shared_MMUPHin | g2_shared_combat) /
  (g2_shared_limma | g2_shared_harmony)








# Condition

study_condition_shared <- sapply(Subset_List_Discovery_shared, function(x) unique(x[, 6])) ## Vektor der angibt, ob das Sample control oder diseased ist

batch_condition_shared <- study_condition_shared[match(colnames(matrix_df), names(study_condition_shared))]

## ComBat
pca_before_shared <- prcomp(t(matrix_df_shared))
pca_after_combat_shared  <- prcomp(t(combat_data_shared))

df_condition_before_shared <- data.frame(pca_before_shared$x[,1:2], condition=batch_condition_shared)
g1_condition_combat_shared <- ggplot(df_condition_before_shared, aes(PC1, PC2, color=batch_condition_shared)) +
  geom_point() + ggtitle("Vor ComBat")

df_condition_after_combat_shared <- data.frame(pca_after_combat_shared$x[,1:2], condition=batch_condition_shared)
g2_condition_combat_shared <- ggplot(df_condition_after_combat_shared, aes(PC1, PC2, color=batch_condition_shared)) +
  geom_point() + ggtitle("Nach ComBat")

g1_condition_combat_shared + g2_condition_combat_shared



## limma
pca_before_shared <- prcomp(t(matrix_df_shared))
pca_after_limma_shared  <- prcomp(t(limma_data_shared))

df_condition_before_shared <- data.frame(pca_before_shared$x[,1:2], condition=batch_condition_shared)
g1_condition_limma_shared <- ggplot(df_condition_before_shared, aes(PC1, PC2, color=batch_condition_shared)) +
  geom_point() + ggtitle("Vor limma (shared)")

df_condition_after_limma_shared <- data.frame(pca_after_limma_shared$x[,1:2], condition=batch_condition_shared)
g2_condition_limma_shared <- ggplot(df_condition_after_limma_shared, aes(PC1, PC2, color=batch_condition_shared)) +
  geom_point() + ggtitle("Nach limma")

g1_condition_limma_shared + g2_condition_limma_shared




## harmony
pca_before_shared <- prcomp(t(matrix_df_shared))
pca_after_harmony_shared  <- prcomp(harmony_data_shared)

df_condition_before_shared <- data.frame(pca_before_shared$x[,1:2], condition=batch_condition_shared)
g1_condition_harmony_shared <- ggplot(df_condition_before_shared, aes(PC1, PC2, color=batch_condition_shared)) +
  geom_point() + ggtitle("Vor harmony (shared)")

df_condition_after_harmony_shared <- data.frame(pca_after_harmony_shared$x[,1:2], condition=batch_condition_shared)
g2_condition_harmony_shared <- ggplot(df_condition_after_harmony_shared, aes(PC1, PC2, color=batch_condition_shared)) +
  geom_point() + ggtitle("Nach harmony")

g1_condition_harmony_shared + g2_condition_harmony_shared




## MMUPHin
pca_before_shared <- prcomp(t(matrix_df_shared))
pca_after_MMUPHin_shared  <- prcomp(t(MMUPHin_data_shared$feature_abd_adj))

df_condition_before_shared <- data.frame(pca_before_shared$x[,1:2], condition=batch_condition_shared)
g1_condition_MMUPHin_shared <- ggplot(df_condition_before_shared, aes(PC1, PC2, color=batch_condition_shared)) +
  geom_point() + ggtitle("Vor MMUPHin (shared)")

df_condition_after_MMUPHin_shared <- data.frame(pca_after_MMUPHin_shared$x[,1:2], condition=batch_condition_shared)
g2_condition_MMUPHin_shared <- ggplot(df_condition_after_MMUPHin_shared, aes(PC1, PC2, color=batch_condition_shared)) +
  geom_point() + ggtitle("Nach MMUPHin")

g1_condition_MMUPHin_shared + g2_condition_MMUPHin_shared



df_condition_before_shared <- data.frame(pca_before_shared$x[,1:2], condition=batch_condition)
g1_condition_shared <- ggplot(df_condition_before_shared, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Vor Korrektur")




g1_shared + g1_condition_shared

(g2_condition_MMUPHin_shared | g2_condition_combat_shared) /
  (g2_condition_limma_shared | g2_condition_harmony_shared)












# Normalisierte Daten

csum_shared <- as.numeric(colSums(matrix_df_shared))
matrix_df_shared_norm <- matrix_df_shared / rep(csum_shared, each = nrow(matrix_df_shared))

## ComBat
combat_data_shared_norm <- ComBat(as.matrix(matrix_df_shared_norm), batch = batch, mod = NULL, par.prior = TRUE, prior.plots = FALSE)

## limma
limma_data_shared_norm <- removeBatchEffect(as.matrix(matrix_df_shared_norm), batch = batch)

## harmony
harmony_data_shared_norm <- RunHarmony(data_mat = as.matrix(t(matrix_df_shared_norm)), meta_data = batch, vars_use = "batch")

## MMUPHin
MMUPHin_data_shared_norm <- adjust_batch(feature_abd = matrix_df_shared_norm, batch = "study", data = Sup_File_Discovery)



## ComBat
pca_before_shared_norm <- prcomp(t(matrix_df_shared_norm))
pca_after_combat_shared_norm  <- prcomp(t(combat_data_shared_norm))

df_before_shared_norm <- data.frame(pca_before_shared_norm$x[,1:2], condition=batch)
g1_shared_norm <- ggplot(df_before_shared_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach Normalisierung")

df_after_combat_shared_norm <- data.frame(pca_after_combat_shared_norm$x[,1:2], condition=batch)
g2_combat_shared_norm <- ggplot(df_after_combat_shared_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach ComBat")

## limma
pca_after_limma_shared_norm  <- prcomp(t(limma_data_shared_norm))

df_after_limma_shared_norm <- data.frame(pca_after_limma_shared_norm$x[,1:2], condition=batch)
g2_limma_shared_norm <- ggplot(df_after_limma_shared_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach limma")

## harmony
pca_after_harmony_shared_norm  <- prcomp(harmony_data_shared_norm)

df_after_harmony_shared_norm <- data.frame(pca_after_harmony_shared_norm$x[,1:2], condition=batch)
g2_harmony_shared_norm <- ggplot(df_after_harmony_shared_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach harmony")

## MMUPHin
pca_after_MMUPHin_shared_norm  <- prcomp(t(MMUPHin_data_shared_norm$feature_abd_adj))

df_after_MMUPHin_shared_norm <- data.frame(pca_after_MMUPHin_shared_norm$x[,1:2], condition=batch)
g2_MMUPHin_shared_norm <- ggplot(df_after_MMUPHin_shared_norm, aes(PC1, PC2, color=batch)) +
  geom_point() + ggtitle("Nach MMUPHin")



g1_shared_norm
(g2_MMUPHin_shared_norm | g2_combat_shared_norm) /
  (g2_limma_shared_norm | g2_harmony_shared_norm)







# Condition

## ComBat
pca_before_shared_norm <- prcomp(t(matrix_df_shared_norm))
pca_after_combat_shared_norm  <- prcomp(t(combat_data_shared_norm))

df_condition_before_shared_norm <- data.frame(pca_before_shared_norm$x[,1:2], condition=batch_condition)
g1_condition_shared_norm <- ggplot(df_before_shared_norm, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Nach Normalisierung")

d_conditionf_after_combat_shared_norm <- data.frame(pca_after_combat_shared_norm$x[,1:2], condition=batch_condition)
g2_condition_combat_shared_norm <- ggplot(df_after_combat_shared_norm, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Nach ComBat")

## limma
pca_after_limma_shared_norm  <- prcomp(t(limma_data_shared_norm))

df_condition_after_limma_shared_norm <- data.frame(pca_after_limma_shared_norm$x[,1:2], condition=batch_condition)
g2_condition_limma_shared_norm <- ggplot(df_after_limma_shared_norm, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Nach limma")

## harmony
pca_after_harmony_shared_norm  <- prcomp(harmony_data_shared_norm)

df_condition_after_harmony_shared_norm <- data.frame(pca_after_harmony_shared_norm$x[,1:2], condition=batch_condition)
g2_condition_harmony_shared_norm <- ggplot(df_after_harmony_shared_norm, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Nach harmony")

## MMUPHin
pca_after_MMUPHin_shared_norm  <- prcomp(t(MMUPHin_data_shared_norm$feature_abd_adj))

df_condition_after_MMUPHin_shared_norm <- data.frame(pca_after_MMUPHin_shared_norm$x[,1:2], condition=batch_condition)
g2_condition_MMUPHin_shared_norm <- ggplot(df_after_MMUPHin_shared_norm, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Nach MMUPHin")





(g1_shared + g1_condition_shared) /
(g1_shared_norm + g1_condition_shared_norm)

(g2_condition_MMUPHin_shared_norm | g2_condition_combat_shared_norm) /
  (g2_condition_limma_shared_norm | g2_condition_harmony_shared_norm)















## Quantifizierung

# Silhouette Score
library(cluster)

batch_labels <- as.factor(batch)

## before
coords_shared_before <- pca_shared_before$x

dist_matrix_shared_before <- dist(coords_shared_before)

sil_shared_before <- silhouette(as.numeric(batch_labels), dist_matrix_shared_before)


## after combat
coords_shared_after_combat <- pca_shared_after_combat$x

dist_matrix_shared_after_combat <- dist(coords_shared_after_combat)

sil_shared_after_combat <- silhouette(as.numeric(batch_labels), dist_matrix_shared_after_combat)



## after limma
coords_shared_after_limma <- pca_shared_after_limma$x

dist_matrix_shared_after_limma <- dist(coords_shared_after_limma)

sil_shared_after_limma <- silhouette(as.numeric(batch_labels), dist_matrix_shared_after_limma)



## after harmony
coords_shared_after_harmony <- pca_shared_after_harmony$x

dist_matrix_shared_after_harmony <- dist(coords_shared_after_harmony)

sil_shared_after_harmony <- silhouette(as.numeric(batch_labels), dist_matrix_shared_after_harmony)



## after MMUPHin
coords_shared_after_MMUPHin <- pca_shared_after_MMUPHin$x

dist_matrix_shared_after_MMUPHin <- dist(coords_shared_after_MMUPHin)

sil_shared_after_MMUPHin <- silhouette(as.numeric(batch_labels), dist_matrix_shared_after_MMUPHin)


## Vergleich average Silhouetten Score before und after
mean(sil_shared_before[, "sil_width"])
mean(sil_shared_after_combat[, "sil_width"])
mean(sil_shared_after_limma[, "sil_width"])
mean(sil_shared_after_harmony[, "sil_width"])
mean(sil_shared_after_MMUPHin[, "sil_width"])





# Normalisierte Daten

## before
coords_shared_before_norm <- pca_before_shared_norm$x

dist_matrix_shared_before_norm <- dist(coords_shared_before_norm)

sil_shared_before_norm <- silhouette(as.numeric(batch_labels), dist_matrix_shared_before_norm)


## after combat
coords_shared_after_combat_norm <- pca_after_combat_shared_norm$x

dist_matrix_shared_after_combat_norm <- dist(coords_shared_after_combat_norm)

sil_shared_after_combat_norm <- silhouette(as.numeric(batch_labels), dist_matrix_shared_after_combat_norm)



## after limma
coords_shared_after_limma_norm <- pca_after_limma_shared_norm$x

dist_matrix_shared_after_limma_norm <- dist(coords_shared_after_limma_norm)

sil_shared_after_limma_norm <- silhouette(as.numeric(batch_labels), dist_matrix_shared_after_limma_norm)



## after harmony
coords_shared_after_harmony_norm <- pca_after_harmony_shared_norm$x

dist_matrix_shared_after_harmony_norm <- dist(coords_shared_after_harmony_norm)

sil_shared_after_harmony_norm <- silhouette(as.numeric(batch_labels), dist_matrix_shared_after_harmony_norm)



## after MMUPHin
coords_shared_after_MMUPHin_norm <- pca_after_MMUPHin_shared_norm$x

dist_matrix_shared_after_MMUPHin_norm <- dist(coords_shared_after_MMUPHin_norm)

sil_shared_after_MMUPHin_norm <- silhouette(as.numeric(batch_labels), dist_matrix_shared_after_MMUPHin_norm)


## Vergleich average Silhouetten Score before und after
mean(sil_shared_before_norm[, "sil_width"])
mean(sil_shared_after_combat_norm[, "sil_width"])
mean(sil_shared_after_limma_norm[, "sil_width"])
mean(sil_shared_after_harmony_norm[, "sil_width"])
mean(sil_shared_after_MMUPHin_norm[, "sil_width"])





# kBET
library(kBET)
set.seed(2)

k_bet_shared <- kBET(as.matrix(matrix_df_shared), batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_shared$summary

k_bet_shared_combat <- kBET(combat_data_shared, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_shared_combat$summary

k_bet_shared_limma <- kBET(limma_data_shared, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_shared_limma$summary

k_bet_shared_harmony <- kBET(harmony_data_shared, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_shared_harmony$summary

k_bet_shared_MMUPHin <- kBET(MMUPHin_data_shared$feature_abd_adj, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_shared_MMUPHin$summary





# Normalisierte Daten

set.seed(2)

k_bet_shared_norm <- kBET(as.matrix(matrix_df_shared_norm), batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_shared_norm$summary

k_bet_shared_combat_norm <- kBET(combat_data_shared_norm, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_shared_combat_norm$summary

k_bet_shared_limma_norm <- kBET(limma_data_shared_norm, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_shared_limma_norm$summary

k_bet_shared_harmony_norm <- kBET(harmony_data_shared_norm, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_shared_harmony_norm$summary

k_bet_shared_MMUPHin_norm <- kBET(MMUPHin_data_shared_norm$feature_abd_adj, batch = batch, plot = FALSE, n_repeat = 1000)
k_bet_shared_MMUPHin_norm$summary








# PERMANOVA
library(vegan)

## Erklärte Varianz durch die Studien
set.seed(2)
fit_adonis_shared_before <- adonis2(dist_matrix_shared_before ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_after_ComBat <- adonis2(dist_matrix_shared_after_combat ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_after_limma <- adonis2(dist_matrix_shared_after_limma ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_after_harmony <- adonis2(dist_matrix_shared_after_harmony ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_after_MMUPHin <- adonis2(dist_matrix_shared_after_MMUPHin ~ study, data = Sup_File_Discovery, method = "euclidean")
print(fit_adonis_shared_before) 
print(fit_adonis_shared_after_ComBat)
print(fit_adonis_shared_after_limma)
print(fit_adonis_shared_after_harmony)
print(fit_adonis_shared_after_MMUPHin)



## Erklärte Varianz durch die Krankheit
set.seed(2)
fit_adonis_shared_condition_before <- adonis2(dist_matrix_shared_before ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_condition_after_ComBat <- adonis2(dist_matrix_shared_after_combat ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_condition_after_limma <- adonis2(dist_matrix_shared_after_limma ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_condition_after_harmony <- adonis2(dist_matrix_shared_after_harmony ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_condition_after_MMUPHin <- adonis2(dist_matrix_shared_after_MMUPHin ~ condition, data = Sup_File_Discovery, method = "euclidean")
print(fit_adonis_shared_condition_before)
print(fit_adonis_shared_condition_after_ComBat)
print(fit_adonis_shared_condition_after_limma)
print(fit_adonis_shared_condition_after_harmony)
print(fit_adonis_shared_condition_after_MMUPHin)








# Normalisierte Daten

## Erklärte Varianz durch die Studien
set.seed(2)
fit_adonis_shared_before_norm <- adonis2(dist_matrix_shared_before_norm ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_after_ComBat_norm <- adonis2(dist_matrix_shared_after_combat_norm ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_after_limma_norm <- adonis2(dist_matrix_shared_after_limma_norm ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_after_harmony_norm <- adonis2(dist_matrix_shared_after_harmony_norm ~ study, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_after_MMUPHin_norm <- adonis2(dist_matrix_shared_after_MMUPHin_norm ~ study, data = Sup_File_Discovery, method = "euclidean")
print(fit_adonis_shared_before_norm) 
print(fit_adonis_shared_after_ComBat_norm)
print(fit_adonis_shared_after_limma_norm)
print(fit_adonis_shared_after_harmony_norm)
print(fit_adonis_shared_after_MMUPHin_norm)



## Erklärte Varianz durch die Krankheit
set.seed(2)
fit_adonis_shared_condition_before_norm <- adonis2(dist_matrix_shared_before_norm ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_condition_after_ComBat_norm <- adonis2(dist_matrix_shared_after_combat_norm ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_condition_after_limma_norm <- adonis2(dist_matrix_shared_after_limma_norm ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_condition_after_harmony_norm <- adonis2(dist_matrix_shared_after_harmony_norm ~ condition, data = Sup_File_Discovery, method = "euclidean")
fit_adonis_shared_condition_after_MMUPHin_norm <- adonis2(dist_matrix_shared_after_MMUPHin_norm ~ condition, data = Sup_File_Discovery, method = "euclidean")
print(fit_adonis_shared_condition_before_norm)
print(fit_adonis_shared_condition_after_ComBat_norm)
print(fit_adonis_shared_condition_after_limma_norm)
print(fit_adonis_shared_condition_after_harmony_norm)
print(fit_adonis_shared_condition_after_MMUPHin_norm)




















# Biomarker Analyse

study_condition <- sapply(Subset_List_Discovery, function(x) unique(x[, 6]))
batch_condition <- study_condition[match(colnames(matrix_df), names(study_condition))]

# Erklärte Varianz der Krankheit vor der batch Korrektur
r2_shared <- apply(matrix_df_shared, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_shared <- names(r2_shared)[r2_shared > 0.20]
proteins_over20_shared ## NAs können entstehen, wenn kein R^2 berechnet werden kann (zB wenn alle Werte von einem Protein gleich sind)
proteins_over20_shared <- proteins_over20_shared[!is.na(proteins_over20_shared)] ## NAs entfernen
table(proteins_over20_shared)
Raw_Data$Protein.Accessions[Raw_Data$Metaprotein.Number==2024]
Raw_Data$Protein.Accessions[Raw_Data$Metaprotein.Number==22]
Raw_Data$Protein.Accessions[Raw_Data$Metaprotein.Number==33284]

tapply(as.numeric(matrix_df_shared[which(rownames(matrix_df_shared)==22),]), batch_condition, mean)




# Erklärte Varianz der Krankheit nach Combat
r2_shared_ComBat <- apply(combat_data_shared, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_shared_ComBat <- names(r2_shared_ComBat)[r2_shared_ComBat > 0.2]
proteins_over20_shared_ComBat <- proteins_over20_shared_ComBat[!is.na(proteins_over20_shared_ComBat)]
table(proteins_over20_shared_ComBat)
length(table(proteins_over20_shared_ComBat))

proteins_over20_shared %in% proteins_over20_shared_ComBat ## Wurden die Metaproteine, die vorher als Biomarker identifiziert
## wurden, danach auch identifiziert


## Wurde in diseased häufiger gefunden als in control
#tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==124),]), batch_condition, mean)
#tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==132),]), batch_condition, mean)
#tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==156),]), batch_condition, mean)
tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==1673),]), batch_condition, mean) ## sehr deutlich
#tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==1703),]), batch_condition, mean)
#tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==204),]), batch_condition, mean)
#tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==366),]), batch_condition, mean)
tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==37164),]), batch_condition, mean) ## sehr deutlich
#tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==400),]), batch_condition, mean)
tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==4293),]), batch_condition, mean) ## sehr deutlich
#tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==75),]), batch_condition, mean)
#tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==803),]), batch_condition, mean)
tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==9040),]), batch_condition, mean) ## sehr deutlich
tapply(as.numeric(combat_data_shared[which(rownames(combat_data_shared)==9510),]), batch_condition, mean) ## sehr deutlich





## PCA plot der Biomarker
biomarker_combat_shared <- as.numeric(proteins_over20_shared_ComBat) 

study_condition <- sapply(Subset_List_Discovery, function(x) unique(x[, 6])) ## Vektor der angibt, ob das Sample control oder diseased ist
batch_condition <- study_condition[match(colnames(matrix_df), names(study_condition))]
matrix_df_biomarker_shared <- matrix_df_shared[which(rownames(matrix_df_shared) %in% biomarker_combat_shared),] ## nur die Biomarker behalten
combat_data_biomarker_shared <- combat_data_shared[which(rownames(combat_data_shared) %in% biomarker_combat_shared),]


pca_biomarker_before_shared <- prcomp(t(matrix_df_biomarker_shared))
pca_biomarker_after_combat_shared  <- prcomp(t(combat_data_biomarker_shared))

df_biomarker_before_shared <- data.frame(pca_biomarker_before_shared$x[,1:2], condition=batch_condition)
g1_biomarker_combat_shared <- ggplot(df_biomarker_before_shared, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker vor ComBat (shared)")

df_biomarker_after_combat_shared <- data.frame(pca_biomarker_after_combat_shared$x[,1:2], condition=batch_condition)
g2_biomarker_combat_shared <- ggplot(df_biomarker_after_combat_shared, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker nach ComBat (shared)")

g1_biomarker_combat_shared + g2_biomarker_combat_shared












# Erklärte Varianz der Krankheit nach limma
r2_shared_limma <- apply(limma_data_shared, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_shared_limma <- names(r2_shared_limma)[r2_shared_limma > 0.2]
proteins_over20_shared_limma <- proteins_over20_shared_limma[!is.na(proteins_over20_shared_limma)]
table(proteins_over20_shared_limma)
length(table(proteins_over20_shared_limma))

proteins_over20_shared %in% proteins_over20_shared_limma


## Wurde in diseased häufiger gefunden als in control
#tapply(as.numeric(limma_data_shared[which(rownames(limma_data_shared)==1078),]), batch_condition, mean)
tapply(as.numeric(limma_data_shared[which(rownames(limma_data_shared)==1673),]), batch_condition, mean) ## sehr deutlich
tapply(as.numeric(limma_data_shared[which(rownames(limma_data_shared)==37164),]), batch_condition, mean) ## sehr deutlich
tapply(as.numeric(limma_data_shared[which(rownames(limma_data_shared)==4293),]), batch_condition, mean) ## sehr deutlich
#tapply(as.numeric(limma_data_shared[which(rownames(limma_data_shared)==627),]), batch_condition, mean)
#tapply(as.numeric(limma_data_shared[which(rownames(limma_data_shared)==803),]), batch_condition, mean) ## deutlich
tapply(as.numeric(limma_data_shared[which(rownames(limma_data_shared)==9040),]), batch_condition, mean) ## sehr deutlich
## Alle die bei limma als Biomarker gefunden wurden, wurden häufiger in diseased gefunden als in control




## PCA plot der Biomarker
biomarker_limma_shared <- as.numeric(proteins_over20_shared_limma) 

matrix_df_biomarker_shared <- matrix_df_shared[which(rownames(matrix_df_shared) %in% biomarker_limma_shared),] ## nur die Biomarker behalten
limma_data_biomarker_shared <- limma_data_shared[which(rownames(limma_data_shared) %in% biomarker_limma_shared),]


pca_biomarker_before_shared <- prcomp(t(matrix_df_biomarker_shared))
pca_biomarker_after_limma_shared  <- prcomp(t(limma_data_biomarker_shared))

df_biomarker_before_shared <- data.frame(pca_biomarker_before_shared$x[,1:2], condition=batch_condition)
g1_biomarker_limma_shared <- ggplot(df_biomarker_before_shared, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker vor limma (shared)")

df_biomarker_after_limma_shared <- data.frame(pca_biomarker_after_limma_shared$x[,1:2], condition=batch_condition)
g2_biomarker_limma_shared <- ggplot(df_biomarker_after_limma_shared, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker nach limma (shared)")

g1_biomarker_limma_shared + g2_biomarker_limma_shared














# Erklärte Varianz der Krankheit nach harmony
r2_shared_harmony <- apply(t(harmony_data_shared), 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_shared_harmony <- names(r2_shared_harmony)[r2_shared_harmony > 0.2]
proteins_over20_shared_harmony <- proteins_over20_shared_harmony[!is.na(proteins_over20_shared_harmony)]
table(proteins_over20_shared_harmony)
length(table(proteins_over20_shared_harmony))

proteins_over20_shared %in% proteins_over20_shared_harmony


## Wurde in diseased häufiger gefunden als in control
#tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==1032),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==1078),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==132),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==1712),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==2403),]), batch_condition, mean)
tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==37164),]), batch_condition, mean) ## sehr deutlich
tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==4293),]), batch_condition, mean) ## sehr deutlich
#tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==532),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==688),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==75),]), batch_condition, mean)
#tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==803),]), batch_condition, mean)
tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==9040),]), batch_condition, mean) ## sehr deutlich
#tapply(as.numeric(t(harmony_data_shared)[which(rownames(t(harmony_data_shared))==998),]), batch_condition, mean)






## PCA plot der Biomarker
biomarker_harmony_shared <- as.numeric(proteins_over20_shared_harmony) 

matrix_df_biomarker_shared <- matrix_df_shared[which(rownames(matrix_df_shared) %in% biomarker_harmony_shared),] ## nur die Biomarker behalten
harmony_data_biomarker_shared <- harmony_data_shared[, which(colnames(harmony_data_shared) %in% biomarker_harmony_shared)]


pca_biomarker_before_shared <- prcomp(t(matrix_df_biomarker_shared))
pca_biomarker_after_harmony_shared  <- prcomp(harmony_data_biomarker_shared)

df_biomarker_before_shared <- data.frame(pca_biomarker_before_shared$x[,1:2], condition=batch_condition)
g1_biomarker_harmony_shared <- ggplot(df_biomarker_before_shared, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker vor harmony (shared)")

df_biomarker_after_harmony_shared <- data.frame(pca_biomarker_after_harmony_shared$x[,1:2], condition=batch_condition)
g2_biomarker_harmony_shared <- ggplot(df_biomarker_after_harmony_shared, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker nach harmony (shared)")

g1_biomarker_harmony_shared + g2_biomarker_harmony_shared













# Erklärte Varianz der Krankheit nach MMUPHin
r2_shared_MMUPHin <- apply(MMUPHin_data_shared$feature_abd_adj, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_shared_MMUPHin <- names(r2_shared_MMUPHin)[r2_shared_MMUPHin > 0.2]
proteins_over20_shared_MMUPHin <- proteins_over20_shared_MMUPHin[!is.na(proteins_over20_shared_MMUPHin)]
table(proteins_over20_shared_MMUPHin)
length(table(proteins_over20_shared_MMUPHin))

proteins_over20_shared %in% proteins_over20_shared_MMUPHin


## Wurde in diseased häufiger gefunden als in control
#tapply(as.numeric(MMUPHin_data_shared$feature_abd_adj[which(rownames(MMUPHin_data_shared$feature_abd_adj)==156),]), batch_condition, mean)
## Alle die bei MMUPHin als Biomarker gefunden wurden, wurden häufiger in control gefunden als in diseased




## PCA plot der Biomarker
biomarker_MMUPHin_shared <- as.numeric(proteins_over20_shared_MMUPHin) 

matrix_df_biomarker_shared <- matrix_df_shared[which(rownames(matrix_df_shared) %in% biomarker_MMUPHin_shared),] ## nur die Biomarker behalten
MMUPHin_data_biomarker_shared <- MMUPHin_data_shared$feature_abd_adj[which(rownames(MMUPHin_data_shared$feature_abd_adj) %in% biomarker_MMUPHin_shared),]


pca_biomarker_before_shared <- prcomp(t(matrix_df_biomarker_shared))
pca_biomarker_after_MMUPHin_shared  <- prcomp(t(MMUPHin_data_biomarker_shared))

df_biomarker_before_shared <- data.frame(pca_biomarker_before_shared$x[,1:2], condition=batch_condition)
g1_biomarker_MMUPHin_shared <- ggplot(df_biomarker_before_shared, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker vor MMUPHin (shared)")

df_biomarker_after_MMUPHin_shared <- data.frame(pca_biomarker_after_MMUPHin_shared$x[,1:2], condition=batch_condition)
g2_biomarker_MMUPHin_shared <- ggplot(df_biomarker_after_MMUPHin_shared, aes(PC1, PC2, color=batch_condition)) +
  geom_point() + ggtitle("Biomarker nach MMUPHin (shared)")

g1_biomarker_MMUPHin_shared + g2_biomarker_MMUPHin_shared



(g2_biomarker_MMUPHin_shared + g2_biomarker_combat_shared)/
  (g2_biomarker_limma_shared + g2_biomarker_harmony_shared)












# Normalisierte Daten

# Erklärte Varianz der Krankheit vor der batch Korrektur
r2_shared_norm <- apply(matrix_df_shared_norm, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_shared_norm <- names(r2_shared_norm)[r2_shared_norm > 0.20]
proteins_over20_shared_norm <- proteins_over20_shared_norm[!is.na(proteins_over20_shared_norm)] ## NAs entfernen
table(proteins_over20_shared_norm)
length(table(proteins_over20_shared_norm))

tapply(as.numeric(matrix_df_shared_norm[which(rownames(matrix_df_shared_norm)==37164),]), batch_condition, mean)




# Erklärte Varianz der Krankheit nach Combat
r2_ComBat_shared_norm <- apply(combat_data_shared_norm, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_ComBat_shared_norm <- names(r2_ComBat_shared_norm)[r2_ComBat_shared_norm > 0.2]
proteins_over20_ComBat_shared_norm <- proteins_over20_ComBat_shared_norm[!is.na(proteins_over20_ComBat_shared_norm)]
table(proteins_over20_ComBat_shared_norm)
length(table(proteins_over20_ComBat_shared_norm))

tapply(as.numeric(combat_data_shared_norm[which(rownames(combat_data_shared_norm)==37164),]), batch_condition, mean)





# Erklärte Varianz der Krankheit nach limma
r2_limma_shared_norm <- apply(limma_data_shared_norm, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_limma_shared_norm <- names(r2_limma_shared_norm)[r2_limma_shared_norm > 0.2]
proteins_over20_limma_shared_norm <- proteins_over20_limma_shared_norm[!is.na(proteins_over20_limma_shared_norm)]
table(proteins_over20_limma_shared_norm)
length(table(proteins_over20_limma_shared_norm))

tapply(as.numeric(limma_data_shared_norm[which(rownames(limma_data_shared_norm)==37164),]), batch_condition, mean)





# Erklärte Varianz der Krankheit nach harmony
r2_harmony_shared_norm <- apply(t(harmony_data_shared_norm), 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_harmony_shared_norm <- names(r2_harmony_shared_norm)[r2_harmony_shared_norm > 0.2]
proteins_over20_harmony_shared_norm <- proteins_over20_harmony_shared_norm[!is.na(proteins_over20_harmony_shared_norm)]
table(proteins_over20_harmony_shared_norm)
length(table(proteins_over20_harmony_shared_norm))

tapply(as.numeric(t(harmony_data_shared_norm)[which(rownames(t(harmony_data_shared_norm))==37164),]), batch_condition, mean)





# Erklärte Varianz der Krankheit nach MMUPHin
r2_MMUPHin_shared_norm <- apply(MMUPHin_data_shared_norm$feature_abd_adj, 1, function(x) {
  model <- lm(x ~ batch_condition)
  summary(model)$r.squared
})

proteins_over20_MMUPHin_shared_norm <- names(r2_MMUPHin_shared_norm)[r2_MMUPHin_shared_norm > 0.2]
proteins_over20_MMUPHin_shared_norm <- proteins_over20_MMUPHin_shared_norm[!is.na(proteins_over20_MMUPHin_shared_norm)]
table(proteins_over20_MMUPHin_shared_norm)
length(table(proteins_over20_MMUPHin_shared_norm))

tapply(as.numeric(MMUPHin_data_shared_norm$feature_abd_adj[which(rownames(MMUPHin_data_shared_norm$feature_abd_adj)==2347),]), batch_condition, mean)




