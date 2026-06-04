rm(list = ls())
graphics.off()

# ── Packages ──────────────────────────────────────────────────────────────────
library(minpack.lm)
library(modelr)
library(readxl)
library(cluster)
library(dplyr)
library(haven)
library(readr)
library(ggplot2)
library(tidyr)
library(scales)
library(xtable)
library(fclust)
library(RSRegress)   # install separately — not on CRAN

# ── Data ──────────────────────────────────────────────────────────────────────
Data1 <- read_excel("C:/Users/chias/Dropbox/Rank analysis (Cerqueti)/Dati/civili.xlsx",    na = "NA")
Data2 <- read_excel("C:/Users/chias/Dropbox/Rank analysis (Cerqueti)/Dati/econ.xlsx",      na = "NA")
Data3 <- read_excel("C:/Users/chias/Dropbox/Rank analysis (Cerqueti)/Dati/signorili.xlsx", na = "NA")
Data4 <- read_excel("C:/Users/chias/Dropbox/Rank analysis (Cerqueti)/Dati/vil.xlsx",       na = "NA")

# ── Shared plot objects (defined once) ────────────────────────────────────────
sem_labels <- paste0(rep(2004:2024, each = 2), "S", rep(1:2, 21))

cols_fixed <- c(
  "Exponential"     = "#0072B2",
  "Power law"       = "#009E73",
  "Universal"       = "black",
  "Zipf-Mandelbrot" = "#D55E00"
)
x_breaks <- seq(1, 42, by = 2)
x_labels <- as.character(2004:2024)

# ── Model fitting ─────────────────────────────────────────────────────────────
model_types  <- c("power", "zm", "exp", "universal")
model_labels <- c("Power", "Zipf", "Exp", "Universal")

.fit_grid <- function(Data, col_names) {
  n_sem <- length(col_names)
  rsq <- matrix(NA, 4, n_sem, dimnames = list(model_labels, col_names))
  aic <- matrix(NA, 4, n_sem, dimnames = list(model_labels, col_names))
  bic <- matrix(NA, 4, n_sem, dimnames = list(model_labels, col_names))
  for (i in seq_len(n_sem)) {
    col <- na.omit(Data[[i + 1]])
    for (j in seq_along(model_types)) {
      res <- tryCatch(
        RSregress(col, control = list(type = model_types[j])),
        error = function(e) NULL
      )
      if (!is.null(res)) {
        rsq[j, i] <- res$`R-square`
        aic[j, i] <- AIC(res$model)
        bic[j, i] <- BIC(res$model)
      }
    }
  }
  list(Rsq = rsq, AIC = aic, BIC = bic)
}

fit1 <- .fit_grid(Data1, names(Data1)[-1])
fit2 <- .fit_grid(Data2, names(Data2)[-1])
fit3 <- .fit_grid(Data3, names(Data3)[-1])
fit4 <- .fit_grid(Data4, names(Data4)[-1])

Rsq1 <- fit1$Rsq; AIC1 <- fit1$AIC; BIC1 <- fit1$BIC
Rsq2 <- fit2$Rsq; AIC2 <- fit2$AIC; BIC2 <- fit2$BIC
Rsq3 <- fit3$Rsq; AIC3 <- fit3$AIC; BIC3 <- fit3$BIC
Rsq4 <- fit4$Rsq; AIC4 <- fit4$AIC; BIC4 <- fit4$BIC

# ── Model comparison tables ───────────────────────────────────────────────────
R2_avg <- rbind(rowMeans(Rsq1), rowMeans(Rsq2), rowMeans(Rsq3), rowMeans(Rsq4))
rownames(R2_avg) <- c("Abitazioni civili", "Abitazioni economiche",
                      "Abitazioni signorili", "Ville e villini")
xtable(R2_avg, caption = "Average R2 for the alternative rank-size models", digits = 4)

delta_aic <- function(aic_mat) {
  d <- apply(aic_mat, 2, function(x) x - min(x, na.rm = TRUE))
  rowMeans(d, na.rm = TRUE)
}
dAIC_avg <- rbind(delta_aic(AIC1), delta_aic(AIC2),
                  delta_aic(AIC3), delta_aic(AIC4))
rownames(dAIC_avg) <- rownames(R2_avg)
colnames(dAIC_avg) <- model_labels
xtable(dAIC_avg, caption = "Average delta-AIC (0 = best model per semester)", digits = 2)

.frac_univ_wins <- function(aic_mat) {
  mean(apply(aic_mat, 2, function(x) which.min(x) == 4L), na.rm = TRUE)
}

.delta_summary <- function(aic_mat) {
  d <- apply(aic_mat, 2, function(x) x - min(x, na.rm = TRUE))
  rbind(
    Mean   = rowMeans(d, na.rm = TRUE),
    Median = apply(d, 1, median, na.rm = TRUE),
    Min    = apply(d, 1, min,    na.rm = TRUE),
    Max    = apply(d, 1, max,    na.rm = TRUE)
  )
}

frac_wins <- c(.frac_univ_wins(AIC1), .frac_univ_wins(AIC2),
               .frac_univ_wins(AIC3), .frac_univ_wins(AIC4))
names(frac_wins) <- rownames(R2_avg)
print(round(frac_wins, 3))

.akaike_weights <- function(aic_mat) {
  delta <- apply(aic_mat, 2, function(x) x - min(x, na.rm = TRUE))
  w_per_sem <- apply(delta, 2, function(d) {
    lw <- -d / 2
    lw <- lw - max(lw)
    w  <- exp(lw)
    w / sum(w)
  })
  rowMeans(w_per_sem, na.rm = TRUE)
}

AW <- rbind(.akaike_weights(AIC1), .akaike_weights(AIC2),
            .akaike_weights(AIC3), .akaike_weights(AIC4))
rownames(AW) <- rownames(R2_avg)
colnames(AW) <- model_labels
xtable(AW,
       caption = "Average Akaike weights (sum = 1 per semester)",
       digits  = 4)

# ── R² plots (base graphics, exploratory) ────────────────────────────────────
plot(Rsq1[1,], type = "o",
     main = "Rank-size law - R2 fit for Civil Dwellings OMI Prices",
     ylab = "R2", xaxt = "n", xlab = "Semester", ylim = c(0, 1),
     col = "green", pch = 16)
axis(1, at = 1:42, labels = 1:42, cex.axis = 0.9, las = 2)
lines(Rsq1[2,], type = "o", col = "red",   pch = 16)
lines(Rsq1[3,], type = "o", col = "blue",  pch = 16)
lines(Rsq1[4,], type = "o", col = "black", pch = 16)
legend("right",
       legend = c("Power law", "Zipf-Mandelbrot", "Exponential", "Universal"),
       col = c("green", "red", "blue", "black"), lty = 1, pch = 17, cex = 0.8)

# ── R² ggplot helper ──────────────────────────────────────────────────────────
.rsq_ggplot <- function(Rsq, ylim, ybreaks) {
  df <- as.data.frame(t(Rsq))
  names(df) <- c("Power law", "Zipf-Mandelbrot", "Exponential", "Universal")
  df$Semester <- seq_len(nrow(df))
  df_long <- pivot_longer(df, cols = -Semester, names_to = "Model", values_to = "R2")
  df_long$Model <- factor(df_long$Model,
                          levels = c("Universal", "Zipf-Mandelbrot",
                                     "Power law", "Exponential"))
  ggplot(df_long, aes(x = Semester, y = R2,
                      color = Model, linetype = Model, group = Model)) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.8) +
    scale_color_manual(values = cols_fixed) +
    scale_linetype_manual(values = c(
      "Exponential"     = "twodash",
      "Power law"       = "dashed",
      "Universal"       = "solid",
      "Zipf-Mandelbrot" = "dotdash"
    )) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels,
                       expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(limits = ylim, breaks = ybreaks,
                       expand = expansion(mult = c(0, 0.01))) +
    labs(x = "", y = expression(R^2), color = NULL, linetype = NULL) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x      = element_text(angle = 45, vjust = 1, hjust = 1,
                                      size = 11, face = "bold"),
      axis.text.y      = element_text(size = 12, face = "bold"),
      axis.title.y     = element_text(size = 13),
      panel.border     = element_blank(),
      axis.line        = element_line(linewidth = 0.7),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.25),
      panel.grid.major.x = element_blank(),
      legend.position  = "top",
      legend.direction = "horizontal",
      legend.text      = element_text(face = "bold", size = 12),
      plot.margin      = margin(10, 10, 8, 10)
    )
}

.rsq_ggplot(Rsq1, ylim = c(0.70, 1.005), ybreaks = seq(0.70, 1.0, by = 0.05))
ggsave("rsq1_civil.pdf",   width = 28, height = 15, units = "cm", device = cairo_pdf)

.rsq_ggplot(Rsq2, ylim = c(0.70, 1.005), ybreaks = seq(0.70, 1.0, by = 0.05))
ggsave("rsq2_ec.pdf",      width = 28, height = 15, units = "cm", device = cairo_pdf)

.rsq_ggplot(Rsq3, ylim = c(0.00, 1.005), ybreaks = seq(0.10, 1.0, by = 0.10))
ggsave("rsq3_highend.pdf", width = 28, height = 15, units = "cm", device = cairo_pdf)

.rsq_ggplot(Rsq4, ylim = c(0.70, 1.005), ybreaks = seq(0.70, 1.0, by = 0.05))
ggsave("rsq4_ville.pdf",   width = 28, height = 15, units = "cm", device = cairo_pdf)

# ── Rank-size fit plots for last semester (base graphics) ─────────────────────
.plot_fit <- function(Data, col_idx, title) {
  plot(sort(na.omit(unlist(Data[, col_idx])), TRUE),
       pch = 19, ylab = "Fitted values", xlab = "Rank", main = title)
  for (tp in list(list("power", "green"), list("zm", "red"),
                  list("exp", "blue"), list("universal", "orange"))) {
    m <- RSregress(as.data.frame(Data[, col_idx])[, 1],
                   control = list(type = tp[[1]]))
    lines(predict(m$model), col = tp[[2]], lwd = 3)
  }
  legend("topright",
         legend = c("Power law", "Zipf-Mandelbrot", "Exponential", "Universal"),
         col = c("green", "red", "blue", "orange"), lty = 1, cex = 0.8)
}

.plot_fit(Data1, 42, "Rank-size curves fit - Civil Dwellings (II semester 2024)")
.plot_fit(Data2, 42, "Rank-size curves fit - Economic Dwellings (II semester 2024)")
.plot_fit(Data3, 42, "Rank-size curves fit - Stately Dwellings (II semester 2024)")
.plot_fit(Data4, 42, "Rank-size curves fit - Villas & Cottages (II semester 2024)")

# ── Universal Law coefficient extraction ──────────────────────────────────────
.extract_coef <- function(Data) {
  Coef <- matrix(NA, 42, 5)
  for (i in 1:42) {
    dati_col <- na.omit(as.data.frame(Data[, i + 1])[, 1])
    res <- tryCatch(
      RSregress(dati_col, control = list(type = "universal"))$coefficients,
      error = function(e) rep(NA, 5)
    )
    Coef[i, ] <- res
  }
  colnames(Coef) <- c("A", "eta1", "teta1", "eta2", "teta2")
  rownames(Coef) <- colnames(Data)[2:43]
  Coef
}

Coef1 <- .extract_coef(Data1)
Coef2 <- .extract_coef(Data2)
Coef3 <- .extract_coef(Data3)
Coef4 <- .extract_coef(Data4)

xtable(Coef1, caption = "Estimated coefficients: Civil Dwellings",    label = "Civilian",  digits = 4)
xtable(Coef2, caption = "Estimated coefficients: Economic Dwellings", label = "Affordable", digits = 4)
xtable(Coef3, caption = "Estimated coefficients: Stately Dwellings",  label = "Stately",   digits = 4)
xtable(Coef4, caption = "Estimated coefficients: Villas & Cottages",  label = "Villas",    digits = 4)

# ── Fuzzy K-Medoids: seed-stable helpers ─────────────────────────────────────
# Average SIL.F over multiple seeds for robust k selection
.sil_seeds <- function(Cmat, m = 1.5, seeds = 1:10) {
  vapply(1:6, function(i) {
    mean(vapply(seeds, function(s) {
      set.seed(s)
      SIL.F(Cmat, FKM.med(Cmat, k = i + 1, m = m)$U)
    }, numeric(1)))
  }, numeric(1))
}

# Pick the FKM.med solution with highest SIL.F among candidate seeds
.best_fkm <- function(Cmat, k, m = 1.5, seeds = 1:10) {
  results <- lapply(seeds, function(s) { set.seed(s); FKM.med(Cmat, k = k, m = m) })
  sils    <- vapply(results, function(fc) SIL.F(Cmat, fc$U), numeric(1))
  results[[which.max(sils)]]
}

# ── K selection via SIL.F (K = 2..7, averaged across seeds 1:10) ─────────────
silo1 <- .sil_seeds(Coef1)
silo2 <- .sil_seeds(Coef2)
silo3 <- .sil_seeds(Coef3)
silo4 <- .sil_seeds(Coef4)

par(mar = c(4, 4, 4, 4), mfrow = c(2, 2))
plot(2:7, silo1, type = "b", xaxt = "n", main = "FS: Civil",    xlab = "Number of clusters", ylab = "FS"); axis(1, at = 2:7, las = 1)
plot(2:7, silo2, type = "b", xaxt = "n", main = "FS: Economic", xlab = "Number of clusters", ylab = "FS"); axis(1, at = 2:7, las = 1)
plot(2:7, silo3, type = "b", xaxt = "n", main = "FS: High-End", xlab = "Number of clusters", ylab = "FS"); axis(1, at = 2:7, las = 1)
plot(2:7, silo4, type = "b", xaxt = "n", main = "FS: Villas",   xlab = "Number of clusters", ylab = "FS"); axis(1, at = 2:7, las = 1)
par(mfrow = c(1, 1))

# Optimal clustering: k=2 forced for Civil/Economic/Stately (SIL.F selects k=3
# but C2 and C3 are near-identical non-crisis regimes — one genuine split suffices).
# Villas keeps data-driven k (genuine COVID structural break warrants 3 clusters).
algo1 <- .best_fkm(Coef1, k = 2)
algo2 <- .best_fkm(Coef2, k = 2)
algo3 <- .best_fkm(Coef3, k = 2)
algo4 <- .best_fkm(Coef4, k = which.max(silo4) + 1)

# Hard cluster assignment table (42 semesters x 4 segments)
clusteringRES <- matrix(NA, 42, 4)
clusteringRES[, 1] <- algo1$clus[, 1]
clusteringRES[, 2] <- algo2$clus[, 1]
clusteringRES[, 3] <- algo3$clus[, 1]
clusteringRES[, 4] <- algo4$clus[, 1]
rownames(clusteringRES) <- rownames(Coef1)
colnames(clusteringRES) <- c("Civil", "Economic", "High-End", "Villas")
clusteringRES
xtable(clusteringRES,
       caption = "Fuzzy K-Medoids cluster assignments by semester",
       label   = "tab:clustering")

# ── Fuzzy membership plots (one per segment) ──────────────────────────────────
.membership_plot <- function(algo, start_year = 2004, start_sem = 1) {
  U <- algo$U
  n <- nrow(U)
  K <- ncol(U)
  t <- seq_len(n)

  sem_index0 <- (start_sem - 1) + (t - 1)
  year   <- start_year + sem_index0 %/% 2
  sem    <- (sem_index0 %% 2) + 1
  sem_lb <- paste0(year, " S", sem)

  cluster_names <- paste0("Cluster ", seq_len(K))

  df_long <- data.frame(
    t       = rep(t, K),
    sem_lab = rep(sem_lb, K),
    cluster = factor(rep(cluster_names, each = n), levels = cluster_names),
    value   = as.vector(U)
  )

  df_end <- df_long[df_long$t == max(df_long$t), ]

  cols_k <- setNames(hue_pal()(K), cluster_names)
  cols_k["Cluster 1"] <- "black"
  if (K >= 2) cols_k["Cluster 2"] <- "#0072B2"
  if (K >= 3) cols_k["Cluster 3"] <- "#009E73"

  lts_k <- setNames(
    rep(c("solid", "longdash", "dotted", "dotdash", "twodash"), length.out = K),
    cluster_names
  )

  ggplot(df_long, aes(x = t, y = value, color = cluster, linetype = cluster)) +
    geom_vline(xintercept = which(sem == 2) + 0.5,
               linewidth = 0.3, color = "grey85") +
    geom_line(linewidth = 1.05, lineend = "round", linejoin = "round") +
    geom_point(size = 1.3, alpha = 0.7) +
    geom_text(data = df_end, aes(label = cluster),
              hjust = 0, nudge_x = 0.6, size = 3.5, show.legend = FALSE) +
    scale_color_manual(values = cols_k) +
    scale_linetype_manual(values = lts_k) +
    scale_x_continuous(breaks = t, labels = sem_lb,
                       expand = expansion(mult = c(0.01, 0.10))) +
    scale_y_continuous(breaks = pretty_breaks(n = 6),
                       expand = expansion(mult = c(0.03, 0.05))) +
    labs(x = "", y = "Membership") +
    coord_cartesian(xlim = c(1, n + 2), clip = "off") +
    theme_classic(base_size = 12) +
    theme(
      legend.position    = "bottom",
      legend.title       = element_blank(),
      axis.text.x        = element_text(size = 8.5, angle = 25,
                                        hjust = 0.5, vjust = 1, face = "bold"),
      axis.text.y        = element_text(size = 11, face = "bold"),
      axis.title         = element_text(size = 12),
      axis.line          = element_line(linewidth = 0.6),
      axis.ticks         = element_line(linewidth = 0.6),
      axis.ticks.length  = unit(2.2, "mm"),
      plot.margin        = margin(8, 28, 8, 8)
    )
}

.membership_plot(algo1); ggsave("clustercivil.pdf", width = 24, height = 17, units = "cm", device = cairo_pdf)
.membership_plot(algo2); ggsave("clusterec_.pdf",   width = 24, height = 17, units = "cm", device = cairo_pdf)
.membership_plot(algo3); ggsave("clustersig.pdf",   width = 24, height = 17, units = "cm", device = cairo_pdf)
.membership_plot(algo4); ggsave("clustervil.pdf",   width = 24, height = 17, units = "cm", device = cairo_pdf)

# ── Membership table (Civil + Economic) ──────────────────────────────────────
escape_latex <- function(x) {
  x <- as.character(x)
  x <- gsub("([&%$#])", "\\\\\\1", x)
  gsub("_", "\\\\_", x, fixed = TRUE)
}

U_civ  <- algo1$U
U_econ <- algo2$U
rownames(U_civ)  <- sem_labels
rownames(U_econ) <- sem_labels

tab_df <- data.frame(
  Semesters = escape_latex(sem_labels),
  Civil_C1  = round(U_civ[, 1], 3),
  Civil_C2  = round(U_civ[, 2], 3),
  Econ_C1   = round(U_econ[, 1], 3),
  Econ_C2   = round(U_econ[, 2], 3),
  row.names = NULL,
  check.names = FALSE
)

make_row    <- function(v) paste0(paste(v, collapse = " & "), " \\\\ \\hline")
body_lines  <- apply(tab_df, 1, make_row)
header_lines <- c(
  "      \\hline",
  "        \\textbf{Semesters} & \\multicolumn{2}{c}{\\textbf{Civil dwellings}} & \\multicolumn{2}{c}{\\textbf{Economic dwellings}} \\\\",
  "        \\cline{2-3} \\cline{4-5}",
  "         & \\textbf{Membership C1} & \\textbf{Membership C2} & \\textbf{Membership C1} & \\textbf{Membership C2} \\\\ \\hline"
)

tex_lines <- c(
  "\\begin{landscape}",
  "\\begin{table}[!ht]",
  "  \\centering",
  "  \\footnotesize",
  "  \\setlength{\\tabcolsep}{3pt}",
  "  \\renewcommand{\\arraystretch}{0.80}",
  "  \\begin{tabular}{lcccc}",
  "    \\multicolumn{5}{l}{\\small\\textbf{Table A?. Fuzzy cluster membership by semester}} \\\\[3pt]",
  header_lines,
  paste0("    ", body_lines),
  "  \\end{tabular}",
  "  \\vspace{2mm}",
  "  \\begin{tabular}{l}",
  "    \\multicolumn{1}{l}{\\textit{Note:} Membership values are fuzzy degrees of belonging. \\textit{Source:} Authors' calculations.} \\\\",
  "  \\end{tabular}",
  "\\end{table}",
  "\\end{landscape}"
)
writeLines(tex_lines, "table_membership_civil_econ.tex")

################################################################################
# Sub-period clustering
#   Period A: 2004S1 – 2013S2  (rows 1:20)
#   Period B: 2014S1 – 2024S2  (rows 21:42)
################################################################################

idx_A <- 1:20
idx_B <- 21:42

sem42    <- sem_labels
sem_A    <- sem42[idx_A]
sem_B    <- sem42[idx_B]

seg_list  <- list(Civil = Coef1, Economic = Coef2, Stately = Coef3, Villas = Coef4)
per_list  <- list(A = idx_A, B = idx_B)
per_label <- c(A = "2004S1-2013S2", B = "2014S1-2024S2")

.run_fkm <- function(Cmat, mset = 1.5, seeds = 1:10) {
  sil   <- .sil_seeds(Cmat, m = mset, seeds = seeds)
  k_opt <- which.max(sil) + 1
  fc    <- .best_fkm(Cmat, k = k_opt, m = mset, seeds = seeds)
  cat("k_opt =", k_opt, "\n")
  cat("Medoids:\n"); print(fc$H)
  fc
}

fc_sub <- list()
for (seg in names(seg_list)) {
  for (per in names(per_list)) {
    key  <- paste0(seg, "_", per)
    Cmat <- seg_list[[seg]][per_list[[per]], , drop = FALSE]
    cat("\n====", key, "====\n")
    fc_sub[[key]] <- .run_fkm(Cmat)
  }
}

# membership fuzzy (main period — used in regime tables)
fc_civ  <- .run_fkm(Coef1)
fc_econ <- .run_fkm(Coef2)
fc_sig  <- .run_fkm(Coef3)
fc_vil  <- .run_fkm(Coef4)

hard_civ  <- fc_civ$clus[, 1]
hard_econ <- fc_econ$clus[, 1]
hard_sig  <- fc_sig$clus[, 1]
hard_vil  <- fc_vil$clus[, 1]

# Regime transition table
.regime_table <- function(hard_vec, sem_labels, segment_name) {
  n      <- length(hard_vec)
  breaks <- c(0, which(diff(hard_vec) != 0), n)
  starts <- breaks[-length(breaks)] + 1
  ends   <- breaks[-1]
  data.frame(
    Segment  = segment_name,
    From     = sem_labels[starts],
    To       = sem_labels[ends],
    Cluster  = hard_vec[starts],
    Duration = ends - starts + 1,
    stringsAsFactors = FALSE
  )
}

trans_table <- rbind(
  .regime_table(hard_civ,  sem42, "Civil"),
  .regime_table(hard_econ, sem42, "Economic"),
  .regime_table(hard_sig,  sem42, "Stately"),
  .regime_table(hard_vil,  sem42, "Villas")
)
print(trans_table)

xtable(trans_table,
       caption = "Regime periods by housing segment (fuzzy k-medoids on Universal Law coefficients)",
       digits  = 0)

# Sub-period transition table
trans_sub <- do.call(rbind, lapply(names(fc_sub), function(key) {
  parts <- strsplit(key, "_")[[1]]
  seg   <- parts[1]; per <- parts[2]
  hard  <- fc_sub[[key]]$clus[, 1]
  sem   <- if (per == "A") sem_A else sem_B
  df    <- .regime_table(hard, sem, seg)
  df$Period <- per_label[per]
  df
}))
print(trans_sub)

xtable(trans_sub[, c("Period", "Segment", "From", "To", "Cluster", "Duration")],
       caption = "Regime periods by sub-period and housing segment",
       digits  = 0)

# ── Sub-period hard cluster plot ──────────────────────────────────────────────
seg_labels <- c(Civil    = "Civil dwellings",
                Economic = "Economic dwellings",
                Stately  = "Stately dwellings",
                Villas   = "Villas & cottages")

plot_sub <- do.call(rbind, lapply(names(fc_sub), function(key) {
  parts   <- strsplit(key, "_")[[1]]
  seg     <- parts[1]; per <- parts[2]
  hard    <- fc_sub[[key]]$clus[, 1]
  sem_idx <- if (per == "A") seq_along(sem_A) else (length(sem_A) + seq_along(sem_B))
  sem_lab <- if (per == "A") sem_A else sem_B
  data.frame(Segment = seg, Period = per, SemIdx = sem_idx,
             SemLabel = sem_lab, Cluster = factor(hard, levels = 1:3),
             stringsAsFactors = FALSE)
}))
plot_sub$Segment <- factor(plot_sub$Segment,
                           levels = c("Civil", "Economic", "Stately", "Villas"))

x_at  <- c(1, 5, 9, 13, 17, 20, 21, 25, 29, 33, 37, 42)
x_lab <- sem42[x_at]

ggplot(plot_sub, aes(x = SemIdx, y = as.integer(Cluster))) +
  geom_line(linewidth = 0.55, color = "grey35", lineend = "round") +
  geom_point(aes(shape = Cluster), size = 3.2, color = "black", stroke = 1.2) +
  geom_vline(xintercept = 20.5, linetype = "dashed",
             color = "grey55", linewidth = 0.55) +
  annotate("text", x = 10.5, y = 3.55, label = "2004-2013",
           size = 3.2, color = "grey40", fontface = "bold") +
  annotate("text", x = 31.5, y = 3.55, label = "2014-2024",
           size = 3.2, color = "grey40", fontface = "bold") +
  scale_shape_manual(name = NULL,
                     values = c("1" = 4, "2" = 16, "3" = 17),
                     labels = c("1" = "Cluster 1", "2" = "Cluster 2", "3" = "Cluster 3"),
                     drop = FALSE) +
  scale_x_continuous(breaks = x_at, labels = x_lab,
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(breaks = 1:3, labels = c("C1", "C2", "C3"), limits = c(0.6, 3.7)) +
  facet_wrap(~ Segment, ncol = 1, labeller = labeller(Segment = seg_labels)) +
  labs(x = "", y = "Cluster assignment") +
  theme_bw(base_size = 11) +
  theme(
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 11, hjust = 0),
    axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1,
                                      size = 7.5, face = "bold"),
    axis.text.y        = element_text(size = 10, face = "bold"),
    axis.title.y       = element_text(size = 11),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    legend.position    = "bottom",
    legend.text        = element_text(size = 10, face = "bold"),
    legend.key.size    = unit(5, "mm"),
    plot.margin        = margin(8, 12, 8, 8)
  )
ggsave("cluster_subperiod.pdf", width = 24, height = 22, units = "cm", device = cairo_pdf)

# ── Sub-period fuzzy membership plot ─────────────────────────────────────────
plot_fuzzy <- do.call(rbind, lapply(names(fc_sub), function(key) {
  parts <- strsplit(key, "_")[[1]]
  seg   <- parts[1]; per <- parts[2]
  fc    <- fc_sub[[key]]
  ord   <- order(fc$H[, "A"])
  U_ord <- fc$U[, ord, drop = FALSE]
  k     <- ncol(U_ord)
  sem_idx <- if (per == "A") seq_along(sem_A) else (length(sem_A) + seq_along(sem_B))
  sem_lab <- if (per == "A") sem_A else sem_B
  do.call(rbind, lapply(seq_len(k), function(j) {
    data.frame(Segment = seg, SemIdx = sem_idx, SemLabel = sem_lab,
               Cluster = paste0("C", j), Membership = U_ord[, j],
               stringsAsFactors = FALSE)
  }))
}))
plot_fuzzy$Segment <- factor(plot_fuzzy$Segment,
                             levels = c("Civil", "Economic", "Stately", "Villas"))
plot_fuzzy$Cluster <- factor(plot_fuzzy$Cluster, levels = c("C1", "C2", "C3"))

ggplot(plot_fuzzy, aes(x = SemIdx, y = Membership)) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "grey65", linewidth = 0.4) +
  geom_line(aes(group = SemIdx), color = "grey45", linewidth = 0.5, lineend = "round") +
  geom_point(aes(shape = Cluster, color = Cluster), size = 2.5) +
  geom_vline(xintercept = 20.5, linetype = "dashed", color = "grey55", linewidth = 0.55) +
  annotate("text", x = 10.5, y = 1.07, label = "2004-2013",
           size = 3.2, color = "grey40", fontface = "bold") +
  annotate("text", x = 31.5, y = 1.07, label = "2014-2024",
           size = 3.2, color = "grey40", fontface = "bold") +
  scale_color_manual(name = NULL,
                     values = c("C1" = "black", "C2" = "#0072B2", "C3" = "#D55E00"),
                     drop = FALSE) +
  scale_shape_manual(name = NULL,
                     values = c("C1" = 4, "C2" = 16, "C3" = 17),
                     drop = FALSE) +
  scale_x_continuous(breaks = seq_len(42), labels = sem42,
                     expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     limits = c(0, 1.12), expand = expansion(mult = c(0, 0))) +
  facet_wrap(~ Segment, ncol = 1, labeller = labeller(Segment = seg_labels)) +
  labs(x = "", y = "Fuzzy membership") +
  theme_bw(base_size = 11) +
  theme(
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 11, hjust = 0),
    axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1,
                                      size = 7.5, face = "bold"),
    axis.text.y        = element_text(size = 10, face = "bold"),
    axis.title.y       = element_text(size = 11),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    legend.position    = "bottom",
    legend.text        = element_text(size = 10, face = "bold"),
    legend.key.width   = unit(12, "mm"),
    plot.margin        = margin(8, 12, 8, 8)
  )
ggsave("cluster_fuzzy_subperiod.pdf", width = 24, height = 22, units = "cm", device = cairo_pdf)

# ── Centroid comparison A vs B ────────────────────────────────────────────────
for (seg in c("Civil", "Economic", "Stately", "Villas")) {
  cat("\n========================================\n")
  cat("Segment:", seg, "\n")
  for (per in c("A", "B")) {
    key <- paste0(seg, "_", per)
    H   <- fc_sub[[key]]$H
    ord <- order(H[, "A"])
    H   <- H[ord, , drop = FALSE]
    rownames(H) <- paste0("C", seq_len(nrow(H)))
    cat("\n  Period", per, "(", if (per == "A") "2004-2013" else "2014-2024", "):\n")
    print(round(H, 4))
  }
}

# ── Centroid coefficient table ────────────────────────────────────────────────
coef_table <- do.call(rbind, lapply(names(fc_sub), function(key) {
  parts   <- strsplit(key, "_")[[1]]
  seg     <- parts[1]; per <- parts[2]
  fc      <- fc_sub[[key]]
  ord     <- order(fc$H[, "A"])
  H_ord   <- fc$H[ord, , drop = FALSE]
  hard    <- apply(fc$U[, ord, drop = FALSE], 1, which.max)
  sem_lab <- if (per == "A") sem_A else sem_B
  do.call(rbind, lapply(seq_along(sem_lab), function(i) {
    cl <- hard[i]
    data.frame(Segment = seg, Period = per, Semester = sem_lab[i],
               Cluster = paste0("C", cl),
               A     = round(H_ord[cl, "A"],     4),
               eta1  = round(H_ord[cl, "eta1"],  4),
               teta1 = round(H_ord[cl, "teta1"], 4),
               eta2  = round(H_ord[cl, "eta2"],  4),
               teta2 = round(H_ord[cl, "teta2"], 4),
               stringsAsFactors = FALSE)
  }))
}))
coef_table$Segment <- factor(coef_table$Segment,
                             levels = c("Civil", "Economic", "Stately", "Villas"))
coef_table <- coef_table[order(coef_table$Segment, coef_table$Period,
                               coef_table$Semester), ]
print(coef_table, row.names = FALSE)
writexl::write_xlsx(coef_table, "cluster_coef_table.xlsx")
