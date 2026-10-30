# Inputdata1005.gdx の pricechange から、WLD の Energy 相対価格を
# シナリオ別に描画する。
#
# 実行例（プロジェクトのルートで実行）:
#   Rscript prog/plot_pricechange_energy_wld_1005.R
#
# 出力:
#   output/pricechange_energy_WLD_1005.png

options(stringsAsFactors = FALSE)

# スクリプト自身の場所を基準にプロジェクトのルートを決める。
command_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command_args, value = TRUE)
if (length(file_arg) > 0L) {
  script_path <- normalizePath(
    sub("^--file=", "", file_arg[1L]),
    winslash = "/",
    mustWork = TRUE
  )
  project_dir <- dirname(dirname(script_path))
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

gdx_file <- file.path(project_dir, "data", "Inputdata1005.gdx")
output_dir <- file.path(project_dir, "output")
figure_file <- file.path(output_dir, "pricechange_energy_WLD_1005.png")
csv_file <- tempfile("pricechange_", fileext = ".csv")

if (!file.exists(gdx_file)) {
  stop("GDX file was not found: ", gdx_file)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(csv_file), add = TRUE)

# gdxdump を探索する。GAMS の bin フォルダを PATH に追加しておくと
# バージョンやインストール先が変わってもそのまま実行できる。
gdxdump <- Sys.which("gdxdump")
if (!nzchar(gdxdump) && .Platform$OS.type == "windows") {
  candidates <- c(
    "C:/GAMS/39/gdxdump.exe",
    Sys.glob("C:/GAMS/*/gdxdump.exe")
  )
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) > 0L) gdxdump <- candidates[1L]
}
if (!nzchar(gdxdump)) {
  stop("gdxdump was not found. Add the GAMS installation folder to PATH.")
}

# pricechange だけを一時CSVに書き出す。
status <- system2(
  command = gdxdump,
  args = c(
    shQuote(gdx_file),
    "Symb=pricechange",
    "Format=csv",
    paste0("Output=", shQuote(csv_file))
  )
)
if (!identical(status, 0L) || !file.exists(csv_file)) {
  stop("Failed to export pricechange from the GDX file (status: ", status, ").")
}

pricechange <- read.csv(
  csv_file,
  check.names = FALSE,
  colClasses = "character",
  strip.white = TRUE
)
stopifnot(identical(names(pricechange), c("Ref", "R", "Y", "I", "Val")))
names(pricechange) <- c("scenario", "region", "year", "item", "relative_price")
unlink(csv_file)
scenario_labels <- c(
  SSP2_BaU_FixTrade_NoCC_No = "Baseline",
  SSP2_BaU_OilRent150pFT_NoCC_No = "PRES\u00d71.5",
  SSP2_BaU_OilRent500pFT_NoCC_No = "PRES\u00d75.0"
)

# gdxdump の引用符付きCSVを環境依存なく扱うため、読込後に数値化する。
pricechange$year <- as.integer(pricechange$year)
pricechange$relative_price <- as.numeric(pricechange$relative_price)
if (anyNA(pricechange$year) || anyNA(pricechange$relative_price)) {
  stop("The year or value column exported by gdxdump could not be converted to numeric.")
}

plot_data <- pricechange[
  pricechange$region == "WLD" & pricechange$item == "Energy" &
    pricechange$scenario %in% names(scenario_labels),
  c("scenario", "year", "relative_price")
]
plot_data <- plot_data[order(plot_data$scenario, plot_data$year), ]

if (nrow(plot_data) == 0L) {
  stop("No observations matched region='WLD' and item='Energy'.")
}
if (anyDuplicated(plot_data[c("scenario", "year")])) {
  stop("Duplicate scenario-year observations were found.")
}

scenarios <- names(scenario_labels)
stopifnot(all(scenarios %in% plot_data$scenario))
years <- sort(unique(plot_data$year))
stopifnot(all(table(plot_data$scenario, plot_data$year) == 1L),
          all(is.finite(plot_data$relative_price)))
plot_data <- plot_data[order(match(plot_data$scenario, scenarios), plot_data$year), ]
plot_data$scenario_label <- unname(scenario_labels[plot_data$scenario])
write.csv(plot_data, file.path(output_dir, "pricechange_energy_WLD_1005.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
line_colors <- c("#333333", "#0072B2", "#D55E00")
line_types <- seq_along(scenarios)

grDevices::png(
  filename = figure_file,
  width = 1800,
  height = 1100,
  res = 180
)
old_par <- par(no.readonly = TRUE)

par(mar = c(5.2, 5.2, 4.0, 2.0))
plot(
  NA,
  xlim = range(years),
  ylim = range(plot_data$relative_price, finite = TRUE),
  xaxs = "i",
  yaxs = "r",
  xaxt = "n",
  xlab = "Year",
  ylab = "Relative price (2010 = 1)",
  main = "Energy relative price by scenario (WLD)",
  las = 1,
  bty = "l"
)
axis(1, at = years, labels = years)
abline(h = 1, col = "grey65", lty = 3, lwd = 1.5)
grid(nx = NA, ny = NULL, col = "grey90", lty = 1)

for (i in seq_along(scenarios)) {
  d <- plot_data[plot_data$scenario == scenarios[i], ]
  lines(
    d$year,
    d$relative_price,
    type = "o",
    col = line_colors[i],
    lty = line_types[i],
    lwd = 2.4,
    pch = 16,
    cex = 0.8
  )
}

legend(
  "topleft",
  legend = unname(scenario_labels),
  col = line_colors,
  lty = line_types,
  lwd = 2.4,
  pch = 16,
  bty = "n",
  cex = 1.0
)

# Positronなどの対話環境でも、ファイルへの書込みをここで確実に完了させる。
par(old_par)
closed_device <- grDevices::dev.off()

if (!file.exists(figure_file) || file.info(figure_file)$size == 0L) {
  stop("The PNG file was not written correctly: ", figure_file)
}

message("Saved: ", figure_file)

