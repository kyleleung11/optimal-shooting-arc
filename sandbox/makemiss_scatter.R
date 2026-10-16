library(jsonlite)

file_path <- "/Users/24kylel/Downloads/nba-movement-data-master/data/0021500492.json"
csv_path  <- "/Users/24kylel/Desktop/optimal shot arc project/shots_fixed (1).csv"

json_data <- fromJSON(file_path, simplifyVector = FALSE)
shots_df  <- read.csv(csv_path)
colnames(shots_df) <- trimws(tolower(colnames(shots_df)))
game_id_numeric <- as.numeric(json_data$gameid)
current_shots <- shots_df[shots_df$game_id == game_id_numeric &
                          shots_df$shot_type == "3PT Field Goal", ]

RIM_Y  <- 25.0            # rim center y (half court width)
RIM_X  <- c(5.25, 88.75)  # the two rim centers (court length)
RIM_RADIUS <- 0.75        # ft (9 in)

final_results <- data.frame()

for (i in seq_len(nrow(current_shots))) {
  target_clock  <- current_shots$shot_time[i]
  target_period <- current_shots$period[i]
  if (is.na(target_clock)) next

  # ---- find the matching event (same quarter, clock within +/-3s) ----
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

  # ---- ball trajectory in the shot window ----
  ball_list <- lapply(found_event$moments, function(m) {
    clock <- m[[3]]
    if (clock <= (target_clock + 3) && clock >= (target_clock - 3)) {
      ball <- m[[6]][[1]]
      return(c(clock = clock, x = ball[[3]], y = ball[[4]], z = ball[[5]]))
    }
    return(NULL)
  })
  ball_list <- ball_list[!sapply(ball_list, is.null)]
  traj <- as.data.frame(do.call(rbind, ball_list))
  if (nrow(traj) < 10) next

  # ---- closest approach to a rim, at roughly rim height ----
  dL <- sqrt((traj$x - RIM_X[1])^2 + (traj$y - RIM_Y)^2)
  dR <- sqrt((traj$x - RIM_X[2])^2 + (traj$y - RIM_Y)^2)
  near <- which(traj$z > 9.5 & traj$z < 10.5)         # only frames near the rim plane
  if (length(near) == 0) next
  k <- near[ which.min(pmin(dL, dR)[near]) ]       # frame of closest approach
  if (pmin(dL, dR)[k] > 2.0) next                  # ball never really reached a rim -> skip

  rim_cx <- if (dL[k] < dR[k]) RIM_X[1] else RIM_X[2]
  rim_cy <- RIM_Y
  cross_x <- traj$x[k]; cross_y <- traj$y[k]

  # ---- decompose into left/right and depth using the ball's approach direction ----
  back <- max(1, k - 8)                            # ~0.3s before closest approach
  d    <- c(rim_cx - traj$x[back], rim_cy - traj$y[back]); d <- d / sqrt(sum(d^2))
  perp <- c(-d[2], d[1])
  v    <- c(cross_x - rim_cx, cross_y - rim_cy)
  left_right <- sum(v * perp)
  depth_ft   <- sum(v * d) + 0.75                  # front rim = 0, center = 0.75

  final_results <- rbind(final_results, data.frame(
    player     = current_shots$player_name[i],
    make       = current_shots$shot_made_flag[i],
    left_right = round(left_right, 2),
    depth_ft   = round(depth_ft, 2)
  ))
}

# ---- scatter: red = make, blue = miss, with the rim circle ----
if (nrow(final_results) > 0) {
  circle <- function(cx, cy, r, ...) {
    th <- seq(0, 2 * pi, length.out = 100)
    lines(cx + r * cos(th), cy + r * sin(th), ...)
  }

  pdf("MakeMiss_Toronto_Charlotte_1-1-16.pdf", width = 6.5, height = 6)
  par(oma = c(0, 0, 3, 0))

  cols <- ifelse(final_results$make == 1, "#b40426", "#3b4cc0")
  plot(final_results$left_right, final_results$depth_ft,
       xlim = c(-1.25, 1.25), ylim = c(-0.25, 2.0), asp = 1,
       pch = 19, col = cols,
       xlab = "Left/Right distance (ft)", ylab = "Depth (ft)",
       main = paste0("Make (red) / Miss (blue)  -  ", nrow(final_results), " shots"))
  circle(0, RIM_RADIUS, RIM_RADIUS, col = "#8b2b1a", lwd = 2)   # rim
  abline(h = 0, col = "gray60", lty = 3)                        # front of rim
  legend("topright", legend = c("Make", "Miss"), pch = 19,
         col = c("#b40426", "#3b4cc0"), bty = "n")
  mtext("Toronto_Charlotte_1-1-16", outer = TRUE, cex = 1.3, font = 2, line = 0.5)

  dev.off()
}

