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
  symbol <- "COM_ENE_s"
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
  d <- subset(d, C == "COM_P_P")
  stopifnot(all(with(d, table(R, SCENARIO)) > 0), length(unique(d$SCENARIO)) == 6L,
            length(unique(d$R)) == 3L, all(is.finite(d$Val)))
  scenarios <- scenarios[c("BaU", "BaU_Shock", "BaU_Shock2050",
                           "Miti", "Miti_Shock", "Miti_Shock2050")]
  values <- sapply(ids, function(region) {
    sapply(scenarios, function(scenario) {
      sum(d$Val[d$R == region & d$SCENARIO == scenario]) / 1000
    })
  })
  png(file.path(output, "hub_oil_product_consumption.png"), 2160, 720, res = 150)
  on.exit(dev.off(), add = TRUE)
  par(mfrow = c(1, 3), mar = c(4.3, 9, 3, 1))
  for (i in seq_along(ids)) {
    heights <- rev(values[, i])
    centers <- barplot(heights, horiz = TRUE, las = 1, border = NA,
                       names.arg = rev(names(scenarios)), cex.names = 0.8,
                       col = rev(rep(c("#315D86", "#087F83"), each = 3)),
                       xlim = c(0, max(values) * 1.23), main = labels[i],
                       xlab = "Oil product consumption (Mtoe/year), 2050")
    text(heights, centers, sprintf("%.1f", heights), pos = 4, cex = 0.85)
  }
}
main()
