
library(jsonlite)

file_path <- "~/Desktop/Matilda Rice/ARC_OPTIMIZATION/data/0021500523.json"
csv_path <- "~/Desktop/Matilda Rice/ARC_OPTIMIZATION/data/shots_fixed.csv"

json_data <- fromJSON(file_path, simplifyVector = FALSE)
shots_df <- read.csv(csv_path)
colnames(shots_df) <- trimws(tolower(colnames(shots_df)))

game_id_numeric <- as.numeric(json_data$gameid)

current_shots <- shots_df[
  shots_df$game_id == game_id_numeric &
    shots_df$shot_type == "3PT Field Goal",
]

gravity <- 32.174
rim_height <- 10.0
rim_y <- 25.0
rim_x <- c(5.25, 88.75)

final_results <- data.frame()

pdf("Significant Acceleration Peak Release.pdf", width = 8.5, height = 11)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))

acceleration_plots <- list()
velocity_plots <- list()


# Process shots

for (i in seq_len(nrow(current_shots))) {
  
  target_clock <- current_shots$shot_time[i]
  player_name <- current_shots$player_name[i]
  target_period <- current_shots$period[i]
  
  found_event <- NULL
  
  for (event in json_data$events) {
    if (length(event$moments) < 10) next
    if (event$moments[[1]][[1]] != target_period) next
    
    clocks <- unlist(lapply(event$moments, function(m) m[[3]]))
    
    if (any(clocks <= (target_clock + 3) &
            clocks >= (target_clock - 3))) {
      found_event <- event
      break
    }
  }
  
  if (is.null(found_event)) next
  
  ball_list <- lapply(found_event$moments, function(m) {
    clock <- m[[3]]
    
    if (clock <= target_clock + 3 &&
        clock >= target_clock - 3) {
      
      ball <- m[[6]][[1]]
      
      return(c(
        clock = clock,
        x = ball[[3]],
        y = ball[[4]],
        z = ball[[5]]
      ))
    }
    
    return(NULL)
  })
  
  ball_list <- ball_list[!sapply(ball_list, is.null)]
  if (length(ball_list) < 10) next
  
  traj <- as.data.frame(do.call(rbind, ball_list))
  if (nrow(traj) < 10) next
  
  
  # Rim detection
  
  rim_d <- pmin(
    sqrt((traj$x - rim_x[1])^2 + (traj$y - rim_y)^2),
    sqrt((traj$x - rim_x[2])^2 + (traj$y - rim_y)^2)
  )
  
  rim_hits <- which(
    rim_d < 2.0 &
      traj$z > 8.5 &
      traj$z < 12.0
  )
  
  if (length(rim_hits) == 0) next
  
  rim_idx <- rim_hits[1]
  rim_clock <- traj$clock[rim_idx]
  
  n <- nrow(traj)
  if (n < 7) next
  
  
  # Calculate three-frame forward vertical velocity
  
  traj$vz_smooth <- rep(NA_real_, n)
  
  for (j in 1:(n - 3)) {
    dt <- abs(traj$clock[j + 3] - traj$clock[j])
    if (!is.finite(dt) || dt <= 0) next
    
    traj$vz_smooth[j] <-
      (traj$z[j + 3] - traj$z[j]) / dt
  }
  
  
  # Calculate three-frame forward vertical acceleration
  
  traj$az <- rep(NA_real_, n)
  
  for (j in 1:(n - 6)) {
    dt <- abs(traj$clock[j + 3] - traj$clock[j])
    if (!is.finite(dt) || dt <= 0) next
    
    traj$az[j] <-
      (traj$vz_smooth[j + 3] - traj$vz_smooth[j]) / dt
  }
  
  acceleration_plots[[length(acceleration_plots) + 1]] <- list(
    player = player_name,
    clock = traj$clock,
    az = traj$az,
    rim_clock = rim_clock
  )
  
  velocity_plots[[length(velocity_plots) + 1]] <- list(
    player = player_name,
    clock = traj$clock,
    vz = traj$vz_smooth,
    rim_clock = rim_clock,
    release_clock = NA
  )
  
  
  # Release point detection
  
  # Search between 0.8 and 1.8 seconds before rim arrival
  win <- which(
    traj$clock <= (rim_clock + 1.8) &
      traj$clock >= (rim_clock + 1)
  )
  
  if (length(win) < 3) next
  
  # Identify positive acceleration peaks by comparing adjacent frames
  peak_candidates <- win[sapply(win, function(k) {
    if (k <= 1 || k >= nrow(traj)) return(FALSE)
    
    a <- traj$az[(k - 1):(k + 1)]
    
    all(is.finite(a)) &&
      a[2] > 0 &&
      a[2] > a[1] &&
      a[2] >= a[3]
  })]
  
  if (length(peak_candidates) == 0) next
  
  # Find the largest positive acceleration peak
  max_peak <- max(traj$az[peak_candidates])
  
  # Keep peaks at least 25% as large as the maximum peak
  min_peak_height <- 0.25 * max_peak
  
  significant_peaks <- peak_candidates[
    traj$az[peak_candidates] >= min_peak_height
  ]
  
  if (length(significant_peaks) == 0) next
  
  # Select the last significant acceleration peak before rim arrival
  release_idx <- significant_peaks[
    length(significant_peaks)
  ]
  
  release_point <- traj[release_idx, ]
  
  velocity_plots[[length(velocity_plots)]]$release_clock <-
    release_point$clock
  
  
  # Shot measurements
  
  post_release <- traj[release_idx:rim_idx, ]
  peak <- post_release[which.max(post_release$z), ]
  
  if (peak$z <= rim_height) next
  
  flight_time <- release_point$clock - rim_clock
  rim_clock_calc <- rim_clock
  
  if (flight_time < 0.55 || flight_time > 2.0) next
  
  keep <- which(
    traj$clock <= rim_clock + 3 &
      traj$clock >= rim_clock - 0.5
  )
  
  time <- rim_clock - traj$clock[keep]
  height <- traj$z[keep]
  velocity <- traj$vz_smooth[keep]
  acceleration <- traj$az[keep]
  
  
  # Plots
  
  plot(
    time, height,
    type = "o", pch = 16, cex = 1, lwd = 2,
    col = "#1f77b4",
    main = paste(
      "Shot:", player_name, "-",
      current_shots$shot_distance[i], "ft 3PT"
    ),
    xlab = "Seconds Relative to Rim Arrival",
    ylab = "Height (feet)",
    xlim = c(-2, 0.5),
    ylim = c(0, max(height, na.rm = TRUE) + 2)
  )
  
  abline(h = 10, col = "firebrick", lty = 2, lwd = 2)
  abline(v = 0, col = "orange", lty = 2, lwd = 2)
  abline(v = -flight_time, col = "gold", lty = 2, lwd = 2)
  
  points(
    -flight_time, release_point$z,
    col = "gold", pch = 16, cex = 1.5
  )
  
  plot(
    time, velocity,
    type = "o", pch = 16, cex = 1, lwd = 2,
    col = "darkgreen",
    main = paste("Vertical Velocity:", player_name),
    xlab = "Seconds Relative to Rim Arrival",
    ylab = "Vertical Velocity (ft/s)",
    xlim = c(-2, 0.5)
  )
  
  abline(h = 0, col = "firebrick", lty = 2, lwd = 2)
  abline(v = 0, col = "orange", lty = 2, lwd = 2)
  abline(v = -flight_time, col = "gold", lty = 2, lwd = 2)
  
  points(
    -flight_time, traj$vz_smooth[release_idx],
    col = "gold", pch = 16, cex = 1.5
  )
  
  if (length(keep) < 3 ||
      all(!is.finite(acceleration))) next
  
  plot(
    time, acceleration,
    type = "o", pch = 16, cex = 1, lwd = 2,
    col = "plum3",
    main = paste("Acceleration:", player_name),
    xlab = "Seconds Relative to Rim Arrival",
    ylab = "Vertical Acceleration (ft/s^2)",
    xlim = c(-2, 0.5)
  )
  
  abline(h = -gravity, col = "firebrick", lty = 2, lwd = 2)
  abline(v = 0, col = "orange", lty = 2, lwd = 2)
  abline(v = -flight_time, col = "gold", lty = 2, lwd = 2)
  
  points(
    -flight_time, traj$az[release_idx],
    col = "gold", pch = 16, cex = 1.5
  )
  
  
  # Save shot results
  
  shot_info <- data.frame(
    player = player_name,
    result = ifelse(
      current_shots$shot_made_flag[i] == 1,
      "Made", "Missed"
    ),
    release_h = round(release_point$z, 2),
    peak_h = round(peak$z, 2),
    flight_time = round(flight_time, 2),
    game_clock_at_rim = round(rim_clock_calc, 2)
  )
  
  final_results <- rbind(final_results, shot_info)
}

dev.off()

View(final_results)
