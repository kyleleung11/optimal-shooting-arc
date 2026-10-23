library(jsonlite)
# JSON Structure:
# json_data$gameid      <- the game ID (e.g. "0021500523")
# json_data$events      <- the list of all events in the game
# m[[1]]  <- quarter (1, 2, 3, or 4)
# m[[3]]  <- game clock in seconds (counts down, e.g. 342.8)
# m[[6]][[1]] <- ball;  ball[[3]]=x, ball[[4]]=y, ball[[5]]=z


#NEW CHANGES: Looking at an average over three frames for vertical velocity and 
#the same is also done for the vertical acceleration.


file_path <- "~/Desktop/Matilda Rice/ARC_OPTIMIZATION/data/0021500523.json"
csv_path  <- "~/Desktop/Matilda Rice/ARC_OPTIMIZATION/data/shots_fixed.csv"

json_data <- fromJSON(file_path, simplifyVector = FALSE)
shots_df  <- read.csv(csv_path)

colnames(shots_df) <- trimws(tolower(colnames(shots_df)))
game_id_numeric <- as.numeric(json_data$gameid)
current_shots <- shots_df[shots_df$game_id == game_id_numeric & 
                            shots_df$shot_type == "3PT Field Goal", ]

gravity    <- 32.174
rim_height <- 10.0
#Added rim x and y to mark center of hoops location on the court
rim_y      <- 25.0                
rim_x      <- c(5.25, 88.75)      

final_results <- data.frame()

pdf("Three frames forward.pdf", width = 8.5, height = 11)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))
acceleration_plots <- list()
velocity_plots     <- list()   # NEW: stores vertical velocity data for each shot
for (i in 1:nrow(current_shots)) {
  target_clock <- current_shots$shot_time[i]
  player_name  <- current_shots$player_name[i]
  #Find the period of the given shot
  target_period <- current_shots$period[i]  
  
  # Search all events in JSON for this specific game clock
  found_event <- NULL
  for (event in json_data$events) {
    if (length(event$moments) < 10) next
    # The quarter must match the game clock so Q1 10seconds is distinguished 
    # from Q3 10seconds etc.
    if (event$moments[[1]][[1]] != target_period) next
    clocks <- unlist(lapply(event$moments, function(m) m[[3]]))   
    if (any(clocks <= (target_clock + 3) & clocks >= (target_clock - 3))) {
      found_event <- event
      break
    }
  }
  
  if (is.null(found_event)) next
  
  # Extract Ball Trajectory
  ball_list <- lapply(found_event$moments, function(m) {
    clock <- m[[3]]
    #Changed down from 6 seconds to 3 for time range but this is just the first
    #gather of the data were not cutting out moments yet.
    if (clock <= (target_clock + 3) && clock >= (target_clock - 3)) {   
      ball <- m[[6]][[1]]
      return(c(clock = clock, x = ball[[3]], y = ball[[4]], z = ball[[5]]))
    }
    return(NULL)
  })
  
  ball_list <- ball_list[!sapply(ball_list, is.null)]
  traj <- as.data.frame(do.call(rbind, ball_list))
  if (nrow(traj) < 10) next
  
  # Start by finding when the ball contacts the rim first instead of finding
  # release point then deducting where the rim is.
  rim_d <- pmin(sqrt((traj$x - rim_x[1])^2 + (traj$y - rim_y)^2),
                sqrt((traj$x - rim_x[2])^2 + (traj$y - rim_y)^2))
  rim_hits <- which(rim_d < 2.0 & traj$z > 8.5 & traj$z < 12.0)
  if (length(rim_hits) == 0) next
  rim_idx   <- rim_hits[1]
  rim_clock <- traj$clock[rim_idx]
  
  # Number of frames
  n <- nrow(traj)
  
  # Three-frame forward velocity
  traj$vz_smooth <- rep(NA, n)
  
  for (j in 1:(n - 3)) {
    dt <- abs(traj$clock[j + 3] - traj$clock[j])
    traj$vz_smooth[j] <- (traj$z[j + 3] - traj$z[j]) / dt
  }
  
  # Three-frame forward acceleration
  traj$az <- rep(NA, n)
  
  for (j in 1:(n - 6)) {
    dt <- abs(traj$clock[j + 3] - traj$clock[j])
    traj$az[j] <- (traj$vz_smooth[j + 3] - traj$vz_smooth[j]) / dt
  }
  
  # Acceleration points
  acceleration_plots[[length(acceleration_plots) + 1]] <- list(
    player = player_name,
    clock = traj$clock,
    az = traj$az,
    rim_clock = rim_clock
  )
  
  # Velocity points
  velocity_plots[[length(velocity_plots) + 1]] <- list(
    player = player_name,
    clock = traj$clock,
    vz = traj$vz_smooth,
    rim_clock = rim_clock,
    release_clock = NA
  )
  
  # Now we search for the release only 2 seconds before rim contact and don't
  # look at anything else prior to 2 seconds.
  # The release must now hold for 3 consecutive frames (vz > 7 and z > 6) so a
  # single noisy frame can't trigger it. The release is the first frame of the
  # first run of 3 qualifying frames.
  win <- which(traj$clock <= (rim_clock + 2) & traj$clock >= rim_clock)
  n_consec <- 3
  release_candidates <- win[sapply(win, function(k) {
    run <- k:(k + n_consec - 1)
    all(run %in% win) && all(traj$vz[run] > 7 & traj$z[run] > 6)
  })]
  
  if (length(release_candidates) == 0) next
  release_idx   <- release_candidates[1]
  release_point <- traj[release_idx, ]
  
  # Record the release time on the velocity entry (NEW)
  velocity_plots[[length(velocity_plots)]]$release_clock <- release_point$clock
  
  # Peak between release and rim
  post_release    <- traj[release_idx:rim_idx, ]
  peak_idx_rel    <- which.max(post_release$z)
  peak            <- post_release[peak_idx_rel, ]
  
  if (peak$z <= rim_height) next
  
  # Measure the flight time don't just assume based on physics.
  flight_time    <- release_point$clock - rim_clock
  rim_clock_calc <- rim_clock
  #Check that the flight time makes sense
  if (flight_time < 0.55 || flight_time > 2.0) next 
  
  keep <- which(
    traj$clock <= rim_clock + 3 &
      traj$clock >= rim_clock - 0.5
  )
  time <- rim_clock - traj$clock[keep]
  height <- traj$z[keep]
  velocity <- traj$vz_smooth[keep]
  acceleration <- traj$az[keep]
  
#Plot the graphs
  # Shot arc
  plot(time, height,
       type = "o", pch = 16, cex = 1, lwd = 2,
       col = "#1f77b4",
       main = paste("Shot:", player_name, "-",
                    current_shots$shot_distance[i], "ft 3PT"),
       xlab = "Seconds Relative to Rim Arrival",
       ylab = "Height (feet)",
       xlim = c(-2, 0.5),
       ylim = c(0, max(height, na.rm = TRUE) + 2))
  
  abline(h = 10, col = "firebrick", lty = 2, lwd = 2)
  abline(v = 0, col = "orange", lty = 2, lwd = 2)
  abline(v = -flight_time, col = "gold", lty = 2, lwd = 2)
  
  points(-flight_time, release_point$z,
         col = "gold", pch = 16, cex = 1.5)
  
  #Velocity graphs
  
  plot(time, velocity,
       type = "o", pch = 16, cex = 1,
       col = "darkgreen", lwd = 2, 
       main = paste("Vertical Velocity:", player_name),
       xlab = "Seconds Relative to Rim Arrival",
       ylab = "Vertical Velocity (ft/s)",
       xlim = c(-2, 0.5))
  
  abline(h = 0, col = "firebrick", lty = 2, lwd = 2)
  abline(v = 0, col = "orange", lty = 2, lwd = 2)
  abline(v = rim_clock - release_point$clock, col = "gold", lty = 2, lwd = 2)
  points(-flight_time, traj$vz_smooth[release_idx],
         col = "gold", pch = 16, cex = 1.5)
  
  #Acceleration graphs
  
  if (length(keep) < 3 || all(!is.finite(acceleration))) next
  
  plot(time, acceleration,
       type = "o", pch = 16, cex = 1,
       col = "plum3", lwd = 2, 
       main = paste("Acceleration:", player_name),
       xlab = "Seconds Relative to Rim Arrival",
       ylab = "Vertical Acceleration (ft/s^2)",
       xlim = c(-2, 0.5))
  
  abline(h = -gravity, col = "firebrick", lty = 2, lwd = 2)
  abline(v = 0, col = "orange", lty = 2, lwd = 2)
  abline(v = rim_clock - release_point$clock, col = "gold", lty = 2, lwd= 2)
  points(-flight_time, traj$az[release_idx],
         col = "gold", pch = 16, cex = 1.5)
  
  
  #Save the graphs
  shot_info <- data.frame(
    player = player_name,
    result = ifelse(current_shots$shot_made_flag[i] == 1, "Made", "Missed"),
    release_h = round(release_point$z, 2),
    peak_h = round(peak$z, 2),
    flight_time = round(flight_time, 2),
    game_clock_at_rim = round(rim_clock_calc, 2)
  )
  final_results <- rbind(final_results, shot_info)
}



dev.off()
View(final_results)