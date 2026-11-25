# Run with Rscript. Requires gdxdump on PATH and data/analysis.gdx.
main <- function() {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  root <- normalizePath(file.path(dirname(script), ".."))
  scenarios <- c(
    BaU = "SSP2_BaU_FixTrade_NoCC_No",
    BaU_Shock2050 = "SSP2_BaU_OilRent500pFT2050_NoCC_No",
    BaU_Shock = "SSP2_BaU_OilRent500pFT_NoCC_No",
    Miti = "SSP2_400C_2030CP_NoCC_No",
    Miti_Shock2050 = "SSP2_400C_2030CP_OilRent500p2050_NoCC_No",
    Miti_Shock = "SSP2_400C_2030CP_OilRent500p_NoCC_No"
  )
  symbol <- "TPES_source"
  ids <- c("BRA", "JPN", "IND")
  labels <- c("Brazil", "Japan", "India")
  raw <- tempfile(fileext = ".csv")
  on.exit(unlink(raw), add = TRUE)
  dump <- Sys.which("gdxdump")
  if (!nzchar(dump)) stop("gdxdump was not found on PATH.")
  status <- system2(dump, c(shQuote(file.path(root, "data", "analysis.gdx")),
                            paste0("Symb=", symbol), "Format=csv",
                            paste0("Output=", shQuote(raw))))
  if (status != 0L) stop("GDX extraction failed.")
  d <- read.csv(raw)
  d <- subset(d, Y == 2050 & R %in% ids & SCENARIO %in% scenarios)
  output <- file.path(root, "output")
  dir.create(output, showWarnings = FALSE)
  values <- sapply(ids, function(region) {
    sapply(scenarios[c("BaU", "Miti")], function(scenario) {
      x <- subset(d, R == region & SCENARIO == scenario)
      total <- x$Val[x$ENE_F == "EB_TOT"]
      oil <- x$Val[x$ENE_F %in% c("EB_F_CRU", "EB_F_OIL")]
      stopifnot(length(total) == 1L, total > 0, length(oil) >= 1L)
      100 * sum(oil) / total
    })
  })
  png(file.path(output, "hub_oil_supply_share.png"), 1500, 540, res = 150)
  on.exit(dev.off(), add = TRUE)
  par(mfrow = c(1, 3), mar = c(4, 4, 2.7, 1))
  for (i in seq_along(ids)) {
    shares <- rev(values[, i])
    centers <- barplot(shares, horiz = TRUE, names.arg = c("Miti", "BaU"),
                       las = 1, col = c("#087F83", "#315D86"), border = NA,
                       xlim = c(0, 48), main = labels[i], xlab = "Oil share (%)")
    text(shares, centers, sprintf("%.1f%%", shares), pos = 4, cex = 0.9)
  }
}

main()
