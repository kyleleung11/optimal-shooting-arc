library(jsonlite)

N_GAMES  <- 55
DATA_DIR <- "/Users/24kylel/Downloads/nba-movement-data-master/data"
csv_path <- "/Users/24kylel/Desktop/optimal shot arc project/shots_fixed (1).csv"

RIM_Y      <- 25.0
RIM_X      <- c(5.25, 88.75)
RIM_RADIUS <- 0.75
Z_BAND     <- c(9.5, 10.5)     # close enough to 10 feet considering noise of data

shots_df <- read.csv(csv_path)
colnames(shots_df) <- trimws(tolower(colnames(shots_df)))
covered_ids <- unique(shots_df$game_id[shots_df$shot_type == "3PT Field Goal"])
all_files  <- list.files(DATA_DIR, pattern = "[.]json$")
file_ids   <- as.numeric(sub("[.]json$", "", all_files))
game_files <- all_files[file_ids %in% covered_ids][seq_len(N_GAMES)]
game_files <- game_files[!is.na(game_files)]
cat("Pooling", length(game_files), "games\n")

# (left/right, depth) of a crossing point relative to the shot's aim line
make_factors <- function(cross_x, cross_y, rim_cx, rim_cy, d) {
  perp <- c(-d[2], d[1])
  v    <- c(cross_x - rim_cx, cross_y - rim_cy)
  c(lr = sum(v * perp), depth = sum(v * d) + RIM_RADIUS)
}

# quadratic-fit crossing: returns c(x, y) at exactly z = 10 or NULL
quad_crossing <- function(traj, k) {
  lo   <- max(1, k - 50)
  apex <- (lo - 1) + which.max(traj$z[lo:k])     # apex of the shot, before rim entry
  high <- traj$z > 10.5
  if (!high[apex]) return(NULL)
  a0 <- apex; while (a0 > 1 && high[a0 - 1]) a0 <- a0 - 1              # extend back
  a1 <- apex; while (a1 < nrow(traj) && high[a1 + 1]) a1 <- a1 + 1     # extend forward
  win <- a0:a1
  if (length(win) < 5) return(NULL)

  tt <- traj$wall[win] - traj$wall[k]            # seconds, centered at rim-entry frame
  z  <- traj$z[win]
  fit <- tryCatch(lm(z ~ tt + I(tt^2)), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  cf <- coef(fit); a <- cf[1]; b <- cf[2]; cc <- cf[3]
  if (is.na(cc) || cc >= 0) return(NULL)         # must open downward (gravity)

  roots <- polyroot(c(a - 10, b, cc))
  real  <- Re(roots)[abs(Im(roots)) < 1e-6]
  if (length(real) == 0) return(NULL)
  # descending crossing = the later root
  tstar <- max(real)
  gap <- tstar - max(tt)
  if (gap <= 0 || gap > 0.1) return(NULL)

  tt_all <- traj$wall - traj$wall[k]
  c(approx(tt_all, traj$x, xout = tstar, rule = 2, ties = mean)$y,
    approx(tt_all, traj$y, xout = tstar, rule = 2, ties = mean)$y)
}

process_game <- function(file_path) {
  json_data <- fromJSON(file_path, simplifyVector = FALSE)
  gid <- as.numeric(json_data$gameid)
  cs  <- shots_df[shots_df$game_id == gid & shots_df$shot_type == "3PT Field Goal", ]
  if (nrow(cs) == 0) return(NULL)

  rows <- data.frame()
  for (i in seq_len(nrow(cs))) {
    target_clock  <- cs$shot_time[i]; target_period <- cs$period[i]
    if (is.na(target_clock)) next

    found_event <- NULL
    for (event in json_data$events) {
      if (length(event$moments) < 10) next
      if (event$moments[[1]][[1]] != target_period) next
      clocks <- unlist(lapply(event$moments, function(m) m[[3]]))
      if (any(clocks <= (target_clock + 3) & clocks >= (target_clock - 3))) {
        found_event <- event; break
      }
    }
    if (is.null(found_event)) next

    ball_list <- lapply(found_event$moments, function(m) {
      clock <- m[[3]]
      if (clock <= (target_clock + 3) && clock >= (target_clock - 3)) {
        ball <- m[[6]][[1]]
        return(c(clock = clock, wall = m[[2]] / 1000,   # wall clock in seconds
                 x = ball[[3]], y = ball[[4]], z = ball[[5]]))
      }
      return(NULL)
    })
    ball_list <- ball_list[!sapply(ball_list, is.null)]
    traj <- as.data.frame(do.call(rbind, ball_list))
    if (nrow(traj) < 10) next

    # anchor frame k: the near-rim-height frame closest to a rim. The quadratic
    # uses it to know which rim, where to look for the apex, and where to center time.
    dL <- sqrt((traj$x - RIM_X[1])^2 + (traj$y - RIM_Y)^2)
    dR <- sqrt((traj$x - RIM_X[2])^2 + (traj$y - RIM_Y)^2)
    near <- which(traj$z > Z_BAND[1] & traj$z < Z_BAND[2])
    if (length(near) == 0) next
    k <- near[which.min(pmin(dL, dR)[near])]
    if (pmin(dL, dR)[k] > 2.0) next              # ball never really reached a rim

    rim_cx <- if (dL[k] < dR[k]) RIM_X[1] else RIM_X[2]
    rim_cy <- RIM_Y

    # aim direction: from the ball ~0.3 s before rim entry toward the rim center
    back <- max(1, k - 8)
    d <- c(rim_cx - traj$x[back], rim_cy - traj$y[back])
    if (sqrt(sum(d^2)) < 1e-6) next
    d <- d / sqrt(sum(d^2))

    qc <- quad_crossing(traj, k)
    if (is.null(qc)) next
    # checks if frame k is a real ball position near 10 ft
    if (sqrt((qc[1] - traj$x[k])^2 + (qc[2] - traj$y[k])^2) > 1.5) next
    quad <- make_factors(qc[1], qc[2], rim_cx, rim_cy, d)

    rows <- rbind(rows, data.frame(
      game = gid, make = cs$shot_made_flag[i],
      left_right = quad["lr"], depth_ft = quad["depth"]
    ))
  }
  rows
}

final_results <- data.frame()
for (f in game_files) {
  rg <- tryCatch(process_game(file.path(DATA_DIR, f)), error = function(e) NULL)
  if (!is.null(rg) && nrow(rg) > 0) final_results <- rbind(final_results, rg)
  cat(".")
}
# plotting
circle <- function(cx, cy, r, ...) {
  th <- seq(0, 2 * pi, length.out = 100); lines(cx + r * cos(th), cy + r * sin(th), ...)
}
XLIM <- c(-1.25, 1.25); YLIM <- c(-0.25, 2.0)

scatter_panel <- function(lr, dep, make, title) {
  cols <- ifelse(make == 1, "#b4042688", "#3b4cc088")
  plot(lr, dep, xlim = XLIM, ylim = YLIM, asp = 1, pch = 19, col = cols,
       xlab = "Left/Right distance (ft)", ylab = "Depth (ft)", main = title)
  circle(0, RIM_RADIUS, RIM_RADIUS, col = "#8b2b1a", lwd = 2)
  abline(h = 0, col = "gray60", lty = 3)
  legend("topright", legend = c("Make", "Miss"), pch = 19,
         col = c("#b40426", "#3b4cc0"), bty = "n")
}

heatmap_panel <- function(lr, dep, make, title, bin = 0.2, min_n = 3) {
  xb <- seq(XLIM[1], XLIM[2], by = bin); yb <- seq(YLIM[1], YLIM[2], by = bin)
  xi <- findInterval(lr,  xb, rightmost.closed = TRUE)
  yi <- findInterval(dep, yb, rightmost.closed = TRUE)
  rate <- matrix(NA_real_, length(xb) - 1, length(yb) - 1)
  ok <- xi >= 1 & xi < length(xb) & yi >= 1 & yi < length(yb)
  for (cxi in unique(xi[ok])) for (cyi in unique(yi[ok])) {
    sel <- ok & xi == cxi & yi == cyi
    if (sum(sel) >= min_n) rate[cxi, cyi] <- mean(make[sel])
  }
  ramp <- colorRampPalette(c("#3b4cc0", "white", "#b40426"))(100)
  xc <- (head(xb, -1) + tail(xb, -1)) / 2; yc <- (head(yb, -1) + tail(yb, -1)) / 2
  image(xc, yc, rate, zlim = c(0, 1), col = ramp, xlim = XLIM, ylim = YLIM, asp = 1,
        xlab = "Left/Right distance (ft)", ylab = "Depth (ft)", main = title)
  circle(0, RIM_RADIUS, RIM_RADIUS, col = "#8b2b1a", lwd = 2)
  lx <- 1.05; ly <- seq(0, 1, length.out = 100)
  for (j in 1:99) rect(lx, ly[j], lx + 0.12, ly[j + 1], col = ramp[j], border = NA)
  text(lx + 0.06, c(-0.05, 1.05), c("0", "1"), cex = 0.7)
}

if (nrow(final_results) > 0) {
  pdf("MakeMiss_Quadratic.pdf", width = 11, height = 5.5)
  par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))
  scatter_panel(final_results$left_right, final_results$depth_ft, final_results$make,
                "Make (red) / Miss (blue)")
  heatmap_panel(final_results$left_right, final_results$depth_ft, final_results$make,
                "Heatmap of makes and misses")
  mtext(sprintf("Scatterplot and Heatmap of makes and misses over 55 games",
                nrow(final_results), length(game_files)),
        outer = TRUE, cex = 1.3, font = 2, line = 0.5)
  dev.off()
}
