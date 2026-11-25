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
  regions <- c(BRA = "Brazil", JPN = "Japan", IND = "India")
  years <- seq(2005, 2050, 5)
  raw <- tempfile(fileext = ".csv")
  on.exit(unlink(raw), add = TRUE)
  dump <- Sys.which("gdxdump")
  if (!nzchar(dump)) stop("gdxdump was not found on PATH.")
  status <- system2(dump, c(shQuote(file.path(root, "data", "analysis.gdx")),
                            "Symb=Domestic_Price_ctx", "Format=csv",
                            paste0("Output=", shQuote(raw))))
  if (status != 0L) stop("Domestic_Price_ctx extraction failed.")
  data <- read.csv(raw)
  prices <- subset(data, Dim4 == "COM_P_P" & SCENARIO %in% scenarios &
                     R %in% names(regions) & Y %in% years)
  counts <- with(prices, table(SCENARIO, R, Y))
  stopifnot(identical(dim(counts), c(6L, 3L, 10L)),
            all(counts == 1L), all(is.finite(prices$Val)))

  output <- file.path(root, "output")
  dir.create(output, showWarnings = FALSE)
  png(file.path(output, "COM_P_P_BRA_JPN_IND.png"),
      width = 2100, height = 1050, res = 175, bg = "white")
  on.exit(dev.off(), add = TRUE)
  colors <- rep(c("#53627A", "#AC5B00", "#007F86"), 2)
  styles <- rep(c(1, 2), each = 3)
  points <- c(16, 17, 15, 1, 2, 0)
  layout(rbind(1:3, rep(4, 3)), heights = c(6, 1.5))
  par(oma = c(0, 1, 4, 0), mar = c(3.2, 3.8, 2.5, 1),
      las = 1, mgp = c(2.4, 0.7, 0), tcl = -0.25)
  for (region in names(regions)) {
    plot(years, rep(NA_real_, length(years)), type = "n",
         ylim = c(0, 2.25), xlab = "Year", ylab = "", xaxt = "n",
         main = regions[[region]])
    abline(h = seq(0, 2, 0.5), col = "#E2E8EF")
    abline(v = 2030, col = "#B0BAC6", lty = 3)
    axis(1, at = c(2005, 2015, 2025, 2030, 2040, 2050), cex.axis = 0.83)
    for (i in seq_along(scenarios)) {
      series <- subset(prices, R == region & SCENARIO == scenarios[[i]])
      series <- series[order(series$Y), ]
      lines(series$Y, series$Val, type = "o", col = colors[i],
            lty = styles[i], pch = points[i], lwd = 2.2, cex = 0.7)
    }
  }
  par(mar = rep(0, 4))
  plot.new()
  legend("center", legend = names(scenarios), ncol = 2, bty = "n",
         col = colors, lty = styles, pch = points, lwd = 3,
         seg.len = 3, x.intersp = 1, y.intersp = 1.15, cex = 1.35)
  mtext("Domestic oil product prices (COM_P_P)", outer = TRUE,
        side = 3, line = 1.8, font = 2, cex = 1.25)
  mtext("Including carbon tax | thousand USD per toe", outer = TRUE,
        side = 3, line = 0.45, cex = 0.93, col = "#53627A")
  message("Plotted ", nrow(prices), " price observations.")
}

main()
