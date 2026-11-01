# ============================================================
# METHODOLOGICAL FRAMEWORK FLOW DIAGRAM
#
# Layout
# ------
# Two panels read left -> right:
#   LEFT  panel : Data, Preprocessing, Software, WP1, WP2
#   RIGHT panel : WP3, WP4, WP5
# The canvas is drawn at the real Word page width (1 plot unit = 1 pt),
# so every font size below is the size you see on the page.
#   PAGE_W_IN = 9.5  -> A4 landscape, normal margins (text width 9.69 in)
# Insert the PNG at 100 %. On Letter landscape (text width 9.0 in),
# set the width to 9.0 in with the aspect ratio locked.
# ============================================================

library(ggplot2)
library(grid)

# ------------------------------------------------------------
# 1. SETTINGS  (all sizes in points on the final page)
# ------------------------------------------------------------
PAGE_W_IN <- 9.5            # figure width in inches
DPI       <- 300            # PNG resolution
FONT      <- "sans"         # maps to Arial on Windows

MARGIN    <- 5              # outer margin
GUTTER    <- 22             # space between the two panels
LH        <- 1.05           # line height (multiple of font size)
PADV      <- 3.6            # vertical padding inside boxes
PADH      <- 3.5            # horizontal padding inside boxes

ROW_GAP   <- 10             # gap between normal rows (room for arrows)
STACK_GAP <- 6              # gap between stacked inventory boxes
SEC_GAP   <- 14             # gap before a new work package
MAX_STRETCH <- 2.2          # left panel gaps may grow up to this factor

LINE_COL  <- "#555555"
LINE_W    <- 0.30           # mm
ARW <- arrow(length = unit(3.8, "pt"), angle = 28, type = "closed")

W <- PAGE_W_IN * 72         # canvas width in points

# ------------------------------------------------------------
# 2. BOX STYLES  (size = font size in pt on the page)
# ------------------------------------------------------------
STY <- list(
  root = list(fill="#263238", border="#263238", txt="white", size=12,  face="bold"),
  sec  = list(fill="#B85410", border="#B85410", txt="white", size=9,   face="bold"),
  l2   = list(fill="#E6B795", border="#B85410", txt="black", size=9,   face="bold"),
  l3   = list(fill="#F2DFD0", border="#D98A55", txt="black", size=8,   face="plain"),
  prim = list(fill="#0B5CA5", border="#0B5CA5", txt="white", size=8.5, face="bold"),
  cat  = list(fill="#9BBEDC", border="#1F6FB5", txt="black", size=8,   face="bold"),
  mid  = list(fill="#D3E2F1", border="#6FA2D1", txt="black", size=7.5, face="plain"),
  leaf = list(fill="#EFF4FA", border="#6FA2D1", txt="black", size=7.5, face="plain")
)

# ------------------------------------------------------------
# 3. PAGE GRID (x positions of the two panels)
# ------------------------------------------------------------
BW      <- (W - 2 * MARGIN - GUTTER) / 2          # panel width
PX      <- list(c(MARGIN, MARGIN + BW),            # panel 1 (left)
                c(W - MARGIN - BW, W - MARGIN))    # panel 2 (right)
COL_GAP <- 10
CW      <- (BW - 2 * COL_GAP) / 3                  # one of 3 columns
HALF    <- (BW - COL_GAP) / 2

col  <- function(b, i, j = i) c(PX[[b]][1] + (i - 1) * (CW + COL_GAP),
                                PX[[b]][1] + (j - 1) * (CW + COL_GAP) + CW)
half <- function(b, side) if (side == "L") c(PX[[b]][1], PX[[b]][1] + HALF) else
                                           c(PX[[b]][2] - HALF, PX[[b]][2])
ctr  <- function(b, w) c(mean(PX[[b]]) - w / 2, mean(PX[[b]]) + w / 2)

# ------------------------------------------------------------
# 4. BOXES   (row = vertical slot; boxes in one row share a height)
# ------------------------------------------------------------
B <- function(id, style, label, row = NA, xr, size = NULL) {
  s <- STY[[style]]
  data.frame(id = id, row = row, x1 = xr[1], x2 = xr[2], label = label,
             fill = s$fill, border = s$border, txt = s$txt,
             size = if (is.null(size)) s$size else size,
             face = s$face, stringsAsFactors = FALSE)
}

H_X <- c(PX[[1]][1], PX[[1]][1] + 192)             # inventory boxes
V_X <- c(PX[[1]][1] + 202, PX[[1]][2])             # validation box

boxes <- rbind(
  # ================= PANEL 1 (rows 1-10) =================
  B("root", "root", "Methodological\nFramework", 1, ctr(1, 150)),
  B("data", "sec",  "Data", 2, ctr(1, 150)),
  B("pre",  "l2",   "Data Preprocessing", 3, ctr(1, 220)),
  B("soft", "cat",
    "Software and Tools\nGoogle Earth Engine (GEE) | ArcGIS Pro | R | Python",
    4, ctr(1, 310)),

  # ---- WP1
  B("wp1", "sec", "WP1  Hazard Inventories", 5, ctr(1, 230)),
  B("h1", "mid",
    "Landslide inventory: Sentinel-2 / Landsat\nchange detection, SAR and visual interpretation",
    6, H_X),
  B("h2", "mid",
    "Flood extents: SAR inundation mapping,\nPDMA / NDMA / UNDP records",
    7, H_X),
  B("h3", "mid",
    "GLOF and glacial lakes (2000-2025): NDWI,\nMNDWI, NDSI, glacier change vs RGI 7.0",
    8, H_X),
  B("val", "cat",
    "Field validation and\ncommunity interviews:\nvalidated hazard\npresence / absence points",
    NA, V_X),

  # ---- WP2
  B("wp2", "sec", "WP2  Hydro-cryospheric Modelling", 9, ctr(1, 230)),
  B("base", "mid",
    "Glacier-enabled\nhydrological model,\ncalibrated and validated\nBaseline 1995-2024",
    10, col(1, 1)),
  B("fut", "mid",
    "Future projections\n2031-2060 and 2071-2100\nSSP2-4.5 and SSP5-8.5",
    10, col(1, 2)),
  B("out", "leaf",
    "Runoff composition,\nglacier and snow change,\ndynamic hazard drivers",
    10, col(1, 3)),

  # ================= PANEL 2 (rows 11-22) =================
  # ---- WP3
  B("wp3", "sec", "WP3  Single-hazard Susceptibility", 11, ctr(2, 230)),
  B("stat", "mid",
    "Static conditioning factors\n(same pixel size)\nelevation, slope, aspect, curvature,\nTWI, SPI, STI, lithology, soil,\nLULC, faults, PGA, NDVI",
    12, half(2, "L")),
  B("dyn", "mid",
    "Dynamic factors\nprecipitation, snowfall, snow-cover\nduration, glacier melt, temperature,\nsoil moisture (future values\nreplaced with WP2 output)",
    12, half(2, "R")),
  B("vif", "l3",
    "Multicollinearity check (VIF) and final factor selection",
    13, col(2, 1, 3)),
  B("lr",  "cat", "Logistic regression\n(baseline)", 14, col(2, 1)),
  B("rf",  "cat", "Random Forest", 14, col(2, 2)),
  B("xgb", "cat", "XGBoost",       14, col(2, 3)),
  B("cv",  "l3",
    "Nested spatio-temporal cross-validation and hyperparameter tuning",
    15, col(2, 1, 3)),
  B("perf", "leaf",
    "Performance: AUC, TSS,\nF1, Brier score,\ncalibration curves", 16, col(2, 1)),
  B("shap", "leaf",
    "SHAP explainability and\nuncertainty (ensemble,\nbootstrap)", 16, col(2, 2)),
  B("aoa", "leaf",
    "Area of applicability\nmask, time-based\nhold-out", 16, col(2, 3)),

  # ---- WP4
  B("wp4", "sec", "WP4  Multi-hazard Integration", 17, ctr(2, 230)),
  B("maps", "mid",
    "Susceptibility maps\nflood, landslide, GLOF\n(baseline and future)", 18, col(2, 1)),
  B("mat", "mid",
    "Interaction matrix and\ncascades (landslide dam,\nGLOF bank erosion)", 18, col(2, 2)),
  B("agg", "mid",
    "Aggregation methods\nprobabilistic,\nexpert-weighted\n(Bayesian network optional)",
    18, col(2, 3)),
  B("eval", "leaf",
    "Evaluation: success-rate and\nprediction-rate curves, hit rate\non withheld events",
    19, half(2, "L")),
  B("expo", "leaf",
    "Exposure overlay: population,\nbuildings, roads, bridges,\nirrigation, power, farmland",
    19, half(2, "R")),
  B("chg", "l2",
    "Change metrics (baseline vs future) and\nmulti-hazard hotspots by union council",
    20, col(2, 1, 3)),

  # ---- WP5
  B("wp5", "sec", "WP5  Validation and Outputs", 21, ctr(2, 230)),
  B("ws", "mid",
    "Two stakeholder workshops\nPDMA, PMD, WAPDA,\nNGOs, CBDRM committees", 22, col(2, 1)),
  B("atlas", "mid",
    "Web atlas (Google Earth\nEngine) and policy brief\n(Urdu / Khowar summaries)", 22, col(2, 2)),
  B("fin", "prim",
    "Multi-hazard risk\nassessment\nPapers 1-3, thesis", 22, col(2, 3))
)

# ------------------------------------------------------------
# 5. AUTOMATIC VERTICAL LAYOUT (each panel stacks independently)
# ------------------------------------------------------------
boxes$n <- vapply(strsplit(boxes$label, "\n", fixed = TRUE), length, 1L)
boxes$h <- boxes$n * boxes$size * LH + 2 * PADV

R     <- sort(unique(boxes$row[!is.na(boxes$row)]))
row_h <- vapply(R, function(r) max(boxes$h[boxes$row %in% r]), 1)
panel <- ifelse(R <= 10, 1, 2)

gaps <- rep(ROW_GAP, length(R))
gaps[R %in% c(1, 11)] <- 0                 # first row of each panel
gaps[R %in% c(7, 8)]  <- STACK_GAP         # inventory boxes sit close together
gaps[R %in% c(9)]     <- SEC_GAP           # WP2 starts

natural <- function(p) sum(row_h[panel == p]) + sum(gaps[panel == p])
target  <- max(natural(1), natural(2))

# stretch the shorter (left) panel's gaps so both panels end together
for (p in 1:2) {
  k <- panel == p
  if (sum(gaps[k]) > 0) {
    f <- min(MAX_STRETCH, (target - sum(row_h[k])) / sum(gaps[k]))
    gaps[k] <- gaps[k] * max(1, f)
  }
}

row_top <- numeric(length(R))
for (p in 1:2) {
  k <- which(panel == p)
  row_top[k] <- MARGIN + cumsum(gaps[k]) + c(0, cumsum(row_h[k])[-length(k)])
}

i <- !is.na(boxes$row)
boxes$y1[i] <- row_top[match(boxes$row[i], R)]
boxes$y2[i] <- boxes$y1[i] + row_h[match(boxes$row[i], R)]

# validation box: centred on the flood-extent box
k <- boxes$id == "val"; hc <- with(boxes[boxes$id == "h2", ], (y1 + y2) / 2)
boxes$y1[k] <- hc - boxes$h[k] / 2
boxes$y2[k] <- hc + boxes$h[k] / 2

boxes$cx <- (boxes$x1 + boxes$x2) / 2
boxes$cy <- (boxes$y1 + boxes$y2) / 2

H    <- max(boxes$y2) + MARGIN
H_IN <- H / 72
W_IN <- PAGE_W_IN

# ------------------------------------------------------------
# 6. TEXT-FIT CHECK (uses the real font metrics of your system)
# ------------------------------------------------------------
check_fit <- function(df) {
  tmp <- tempfile(fileext = ".png")
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(tmp, width = 4, height = 4, units = "in", res = 72)
  } else {
    png(tmp, width = 288, height = 288)
  }
  on.exit({ dev.off(); unlink(tmp) })
  bad <- 0
  for (k in seq_len(nrow(df))) {
    lines <- strsplit(df$label[k], "\n", fixed = TRUE)[[1]]
    gp <- gpar(fontsize = df$size[k], fontface = df$face[k], fontfamily = FONT)
    wid <- vapply(lines, function(s)
      convertWidth(grobWidth(textGrob(s, gp = gp)), "pt", valueOnly = TRUE), 1)
    room <- (df$x2[k] - df$x1[k]) - 2 * PADH
    if (any(wid > room)) {
      bad <- bad + 1
      message(sprintf("  TOO WIDE [%s]: '%s' (%.0f pt > %.0f pt)",
                      df$id[k], lines[which.max(wid)], max(wid), room))
    }
  }
  if (bad == 0) message("Text-fit check: every line fits inside its box.")
}
check_fit(boxes)

# ------------------------------------------------------------
# 7. CONNECTORS (built from box positions - nothing hard-coded)
# ------------------------------------------------------------
S <- function(x, y, xend, yend, arrow = FALSE)
  data.frame(x = x, y = y, xend = xend, yend = yend, arrow = arrow)
g <- function(ids) boxes[match(ids, boxes$id), ]

down  <- function(a, b) { A <- g(a); Bx <- g(b); S(A$cx, A$y2, A$cx, Bx$y1, TRUE) }
right <- function(a, b) { A <- g(a); Bx <- g(b); S(A$x2, A$cy, Bx$x1, A$cy, TRUE) }

# stub down from each source, horizontal bar, arrow(s) into target(s)
bracket <- function(from, to, to_x = NULL, frac = 0.4) {
  A <- g(from); Bx <- g(to)
  y0 <- max(A$y2); y1 <- min(Bx$y1); ym <- y0 + frac * (y1 - y0)
  tx <- if (is.null(to_x)) Bx$cx else to_x
  ty <- if (is.null(to_x)) Bx$y1 else rep(Bx$y1, length(tx))
  out <- lapply(seq_len(nrow(A)), function(k) S(A$cx[k], A$y2[k], A$cx[k], ym))
  xs <- c(A$cx, tx)
  if (diff(range(xs)) > 0.01) out <- c(out, list(S(min(xs), ym, max(xs), ym)))
  out <- c(out, lapply(seq_along(tx), function(k) S(tx[k], ym, tx[k], ty[k], TRUE)))
  do.call(rbind, out)
}

connectors <- rbind(
  # ---- panel 1: main sequence
  down("root", "data"), down("data", "pre"), down("pre", "soft"), down("soft", "wp1"),
  # WP1
  bracket("wp1", "h1"), down("h1", "h2"), down("h2", "h3"), right("h2", "val"),
  # WP2
  bracket("wp2", "base"), right("base", "fut"), right("fut", "out"),

  # ---- panel 2
  # WP3
  bracket("wp3", c("stat", "dyn")),
  bracket(c("stat", "dyn"), "vif", to_x = g(c("stat", "dyn"))$cx),
  bracket("vif", c("lr", "rf", "xgb")),
  bracket(c("lr", "rf", "xgb"), "cv"),
  bracket("cv", c("perf", "shap", "aoa")),
  # WP4
  bracket(c("perf", "shap", "aoa"), "wp4"),
  bracket("wp4", c("maps", "mat", "agg")),
  bracket(c("maps", "agg"), c("eval", "expo")),
  bracket(c("eval", "expo"), "chg"),
  # WP5
  down("chg", "wp5"),
  bracket("wp5", c("ws", "atlas", "fin"))
)

# light dashed divider between the two panels
divider <- data.frame(x = W / 2, xend = W / 2, y = MARGIN, yend = H - MARGIN)

# ------------------------------------------------------------
# 8. DRAW  (1 plot unit = 1 point on the page)
# ------------------------------------------------------------
p <- ggplot() +
  geom_segment(data = divider, aes(x = x, y = y, xend = xend, yend = yend),
               colour = "#C8C8C8", linewidth = 0.25, linetype = "dashed") +
  geom_segment(data = connectors[!connectors$arrow, ],
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = LINE_COL, linewidth = LINE_W, lineend = "butt") +
  geom_segment(data = connectors[connectors$arrow, ],
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = LINE_COL, linewidth = LINE_W, arrow = ARW,
               arrow.fill = LINE_COL, lineend = "butt") +
  geom_rect(data = boxes,
            aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2,
                fill = fill, colour = border),
            linewidth = LINE_W) +
  geom_text(data = boxes,
            aes(x = cx, y = cy, label = label, colour = txt,
                size = size / .pt, fontface = face),
            family = FONT, lineheight = LH, show.legend = FALSE) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_size_identity() +
  scale_x_continuous(limits = c(0, W), expand = c(0, 0)) +
  scale_y_reverse(limits = c(H, 0), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void(base_family = FONT) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(0, 0, 0, 0))

print(p)

# ------------------------------------------------------------
# 9. EXPORT
# ------------------------------------------------------------
png_dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"

ggsave("Methodological_Framework_Landscape.png", p,
       width = W_IN, height = H_IN, units = "in",
       dpi = DPI, device = png_dev, bg = "white")

# Optional vector copy (sharp at any zoom; use for print / journal upload)
try(ggsave("Methodological_Framework_Landscape.pdf", p,
           width = W_IN, height = H_IN, units = "in",
           device = grDevices::cairo_pdf, bg = "white"), silent = TRUE)

message(sprintf("Saved %.2f x %.2f in  (%d x %d px)  in: %s",
                W_IN, H_IN, round(W_IN * DPI), round(H_IN * DPI), getwd()))
