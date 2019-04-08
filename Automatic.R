xy <- NULL

#change all coords
gps[(which(is.na(gps$Longitude))),]

xy <- data.frame(ID = 1:nrow(gps), X = gps$Latitude, Y = gps$Longitude)
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(xy, CRS("+proj=utm +zone=51 ellps=WGS84"))

gps$X <- as(res, "SpatialPoints")$X
gps$Y <- as(res, "SpatialPoints")$Y

#for (i in 1:38){
  for (j in 1:21){
game_one_gps <- gps[which(gps$GameID == 1),]
game_one_gps_player_two <- game_one_gps[which(game_one_gps$PlayerID == j),]

Zs <- c()

rows <- nrow(game_one_gps_player_two)
rows <- rows - 1

for (i in 1:rows){
  Zs[i] <- (mean(game_one_gps_player_two$AccelZ[i:i+1]) * (game_one_gps_player_two$X[i+1] - game_one_gps_player_two$X[i]) * (game_one_gps_player_two$Y[i+1] - game_one_gps_player_two$Y[i]))
}

Zs <- as.data.frame(Zs)
Zs <- rbind(0,Zs)
Zs <- Zs$Zs

game_one_gps_player_two$Height <- Zs

#plot_ly(game_one_gps_player_two, x= ~X, y= ~Y, z= ~Height, marker = list(size = 1))

#caclulcate force vector over time
rows <- nrow(game_one_gps_player_two); rows <- rows - 1
game_one_gps_player_two$f_x_2 <- c(game_one_gps_player_two$AccelX[1],game_one_gps_player_two$AccelX[1:rows])
game_one_gps_player_two$f_y_2 <- c(game_one_gps_player_two$AccelY[1],game_one_gps_player_two$AccelY[1:rows])
game_one_gps_player_two$f_z_2 <- c(game_one_gps_player_two$AccelZ[1],game_one_gps_player_two$AccelZ[1:rows])

# F <- 75 * sqrt((sum(sqrt((game_one_gps_player_two$f_x_2 - game_one_gps_player_two$AccelX)^2)))^2 + (sum(sqrt((game_one_gps_player_two$f_y_2 - game_one_gps_player_two$AccelY)^2)))^2 + (sum(sqrt((game_one_gps_player_two$f_z_2 - game_one_gps_player_two$Height)^2)))^2)

F <- 75 * sum(sqrt((game_one_gps_player_two$f_x_2 - game_one_gps_player_two$AccelX)^2 + (game_one_gps_player_two$f_y_2 - game_one_gps_player_two$AccelY)^2 + (game_one_gps_player_two$f_z_2 - game_one_gps_player_two$AccelZ)^2))

#calculating distance over the whole game
rows <- nrow(game_one_gps_player_two); rows <- rows - 1
game_one_gps_player_two$x_2 <- c(game_one_gps_player_two$X[1],game_one_gps_player_two$X[1:rows])
game_one_gps_player_two$y_2 <- c(game_one_gps_player_two$Y[1],game_one_gps_player_two$Y[1:rows])
game_one_gps_player_two$z_2 <- c(game_one_gps_player_two$Height[1],game_one_gps_player_two$Height[1:rows])

distance <- sum(sqrt((game_one_gps_player_two$x_2 - game_one_gps_player_two$X)^2 + (game_one_gps_player_two$y_2 - game_one_gps_player_two$Y)^2 + (game_one_gps_player_two$z_2 - game_one_gps_player_two$Height)^2))

E <- F * distance
full_matrix[i,j] <- E
  }
#}
