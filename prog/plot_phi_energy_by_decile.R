# Run from the project root: Rscript prog/plot_phi_energy_by_decile.R
# Optional arguments: data directory, output PDF path.
args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1) args[1] else "data"
output <- if (length(args) >= 2) args[2] else "output/PHI_Energy_By_Decile_2050.pdf"
dump <- Sys.which("gdxdump")
if (!nzchar(dump)) {
  candidates <- Sys.glob("C:/GAMS/*/gdxdump.exe")
  if (!length(candidates)) stop("Add gdxdump to PATH.")
  dump <- candidates[1]
}
regions <- c("R17BRA", "R17JPN", "R17IND")
region_labels <- c("Brazil", "Japan", "India")
policies <- c("BaU", "Miti")
baseline <- setNames(c("SSP2_BaU_FixTrade_NoCC_No",
                       "SSP2_400C_2030CP_NoCC_No"), policies)
shock <- setNames(c("SSP2_BaU_OilRent500pFT_NoCC_No",
                    "SSP2_400C_2030CP_OilRent500p_NoCC_No"), policies)
keys <- c("R", "Ref", "Y", "DEC")

read_results <- function(path) {
  # Stage under an ASCII filename for older Windows versions of GAMS.
  staged <- tempfile(fileext = ".gdx")
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(c(staged, csv)))
  if (!file.copy(path, staged)) stop("Cannot read ", path)
  extract <- function(symbol) {
    status <- system2(dump, c(shQuote(staged), paste0("Symb=", symbol),
                              "Format=csv", paste0("Output=", shQuote(csv))))
    if (status != 0 || !file.exists(csv)) stop("gdxdump failed for ", symbol)
    d <- read.csv(csv, colClasses = "character")
    if ("I" %in% names(d)) d <- subset(d, I == "Energy")
    d <- subset(d, R %in% regions & Y == "2050" & DEC %in% c("Ave", "1", "10") &
                  Ref %in% c(baseline, shock))
    d[[symbol]] <- as.numeric(d$Val)
    stopifnot(nrow(d) == 36, !anyDuplicated(d[keys]), all(is.finite(d[[symbol]])))
    d[c(keys, symbol)]
  }
  Reduce(function(x, y) merge(x, y, by = keys),
         lapply(c("Consumption", "Expenditure", "Budget"), extract))
}

compare <- function(d, policy, decile, fixed = FALSE) {
  a <- d[d$Ref == shock[policy] & d$DEC == decile, ]
  b <- d[d$Ref == baseline[policy] & d$DEC == decile, ]
  a <- a[match(regions, a$R), ]
  b <- b[match(regions, b$R), ]
  stopifnot(all(a$R == b$R), all(b$Budget > 0),
            all(b$Consumption > 0), all(b$Expenditure > 0))
  if (fixed) stopifnot(all(a$Budget == b$Budget))
  data.frame(Consumption_before = b$Consumption, Consumption_after = a$Consumption,
             Expenditure_before = b$Expenditure, Expenditure_after = a$Expenditure,
             share = 100 * (a$Expenditure / a$Budget - b$Expenditure / b$Budget))
}
by_decile <- TRUE

draw_panel <- function(rows, policy, metric, limit) {
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 7.8), xaxs = "i", yaxs = "i")
  heading <- if (metric == "Consumption") "Energy consumption" else "Nominal Energy expenditure"
  show_share <- by_decile && metric == "Expenditure"
  before <- rows[[paste0(metric, "_before")]]
  after <- rows[[paste0(metric, "_after")]]
  stopifnot(all(before > 0), all(after >= 0), max(before, after) <= limit)
  y <- 6:1
  for (i in c(1, 2, 5, 6)) rect(0, y[i] - 0.5, 1, y[i] + 0.5, col = "#E9EEF7", border = NA)
  text(0.01, 7.55, paste(policy, heading, sep = " | "), adj = 0, font = 2, cex = 1.02)
  text(0.65, 6.8, "Baseline -> Shock", cex = 0.80)
  text(0.99, 6.8, if (show_share) "Change / Share (pp)" else "Change", adj = 1, cex = 0.76)
  scale <- function(value) 0.25 + 0.24 * value / limit
  segments(scale(before), y, scale(after), y, col = "#9DAAC0", lwd = 1.8)
  points(scale(before), y, pch = 16, col = "#7786A1", cex = 0.95)
  points(scale(after), y, pch = 16, col = "#416EBB", cex = 0.72)
  text(0.01, y, rows$label, adj = 0, cex = 0.79)
  format_value <- function(value) formatC(value, format = "f", digits = 1, big.mark = ",")
  text(0.65, y, paste(format_value(before), "->", format_value(after)), cex = 0.80)
  change <- sprintf("%+.2f%%", 100 * (after / before - 1))
  if (show_share) change <- paste(change, sprintf("%+.3f", rows$share), sep = " / ")
  text(0.99, y, change, adj = 1, font = 2, col = "#315A9F", cex = 0.78)
  text(scale(c(0, limit)), 0.22, c("0", format(limit, big.mark = ",", trim = TRUE)), cex = 0.70)
}

main <- function() {
  fixed <- read_results(file.path(data_dir, "DecileResults_region_10PO\u03b1.gdx"))
  if (!by_decile) full <- read_results(file.path(data_dir, "DecileResults_region_10_1118.gdx"))
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  pdf(output, width = 13, height = 9)
  on.exit(dev.off())
  par(mfrow = c(2, 2), oma = c(1, 1, 5, 1), mar = c(0.5, 1, 0.5, 1))
  for (policy in policies) {
    a <- if (by_decile) compare(fixed, policy, "1", TRUE) else compare(full, policy, "Ave")
    b <- compare(fixed, policy, if (by_decile) "10" else "Ave", TRUE)
    a$label <- paste(region_labels, if (by_decile) "D1" else "Standard")
    b$label <- paste(region_labels, if (by_decile) "D10" else "Fixed")
    rows <- rbind(a, b)[c(1, 4, 2, 5, 3, 6), ]
    draw_panel(rows, policy, "Consumption", if (by_decile) 3600 else 1800)
    draw_panel(rows, policy, "Expenditure", if (by_decile) 2500 else 1200)
  }
  title <- if (by_decile) "Energy consumption and expenditure by income decile" else
    "Energy consumption and expenditure by region"
  mtext(title, side = 3, outer = TRUE, line = 2.6, font = 2, cex = 1.25)
  mtext("2050 | Sustained shock | Gray: no shock | Blue: after shock",
        side = 3, outer = TRUE, line = 1, cex = 0.94)
}
main()
