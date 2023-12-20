x <- list(w=1, score=c('B','O','B'), time=c(3, 60, 87))



data <- as.data.frame(matrix(nrow=0, ncol = 4))
colnames(data) <- c('Win', 'FCB', 'Oppo', 'Time')

one_game_data <- function(w, score, time){
  nrow <- nrow(data)
  for(i in 1:length(score)){
    #data from 1m ~ first score
    if(i == 1){
      for(t in 1:time[i]-1){
        data[nrow+t, 1] <- w
        data[nrow+t, 2:3] <- 0
        data[nrow+t, 4] <- t
      }
    }
    #data from last score to 90m
    if(i == length(score)){
      for(t in time[i]:90){
        if(score[i] == 'B'){
          data[nrow+t, 1] <- w
          data[nrow+t, 2] <- data[nrow+time[i]-1,2] + 1
          data[nrow+t, 3] <- data[nrow+time[i]-1,3]
          data[nrow+t, 4] <- t
        }else{
          data[nrow+t, 1] <- w
          data[nrow+t, 2] <- data[nrow+time[i]-1,2]
          data[nrow+t, 3] <- data[nrow+time[i]-1,3] + 1
          data[nrow+t, 4] <- t
        }
      }
    }
    #data at rest time
    if(i >= 1 && i < length(score)){
      if(score[i] == 'B'){
        for(t in time[i]:time[i+1]-1){
          data[nrow+t, 1] <- w
          data[nrow+t, 2] <- data[nrow+time[i]-1,2] + 1
          data[nrow+t, 3] <- data[nrow+time[i]-1,3]
          data[nrow+t, 4] <- t
        }
      }else if(score[i]=='O'){
        for(t in time[i]:time[i+1]-1){
          data[nrow+t, 1] <- w
          data[nrow+t, 2] <- data[nrow+time[i]-1,2]
          data[nrow+t, 3] <- data[nrow+time[i]-1,3] + 1
          data[nrow+t, 4] <- t
        } 
      }
    }
  }
  data
}