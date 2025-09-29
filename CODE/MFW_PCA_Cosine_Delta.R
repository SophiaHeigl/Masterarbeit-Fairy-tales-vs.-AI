library(stylo)
getwd()
stylo()
path <- "C:/Users/Sophia/Downloads/MA/R_Studio/corpus"
dir.exists(path)
parent <- normalizePath(path, winslash = "/", mustWork = TRUE)
res <- stylo(
  gui = FALSE,
  corpus.dir = parent,
  analysis.type = "CA",
  distance.measure = "cosine",
  corpus.lang = "German",
  analyzed.features = "w",
  mfw.min = 1000, mfw.max = 1000, mfw.incr = 0,
  culling.min = 0, culling.max = 0, culling.incr = 0,
  encoding = "UTF-8",
  display.on.screen = FALSE
)
delta   <- as.matrix(res$distance.table)
samples <- rownames(delta)
classes <- sub("_.*$", "", basename(samples))
groups  <- unique(classes)
mean_between <- function(A,B,M,cls){ ia <- which(cls==A); ib <- which(cls==B); mean(M[ia,ib], na.rm=TRUE) }
pairs <- t(combn(groups, 2))
between_df <- data.frame(
  GroupA = pairs[,1],
  GroupB = pairs[,2],
  Cosine_Delta_Mean = apply(pairs, 1, \(ab) mean_between(ab[1], ab[2], delta, classes)),
  row.names = NULL
)
print(between_df)


delta <- as.matrix(res$distance.table)
out_dir <- file.path(parent, "_stylo_outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(delta, file.path(out_dir, "CosineDelta_distance_matrix.csv"), row.names = TRUE)
cat("Gespeichert unter: ", file.path(out_dir, "CosineDelta_distance_matrix.csv"), "\n")
write.csv(between_df, file.path(out_dir, "CosineDelta_your_table.csv"), row.names = FALSE)
write.table(between_df, file.path(out_dir, "CosineDelta_between_means.tsv"),
            sep = "\t", dec = ",", row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
