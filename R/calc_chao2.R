
calc_chao2 <- function(vec){
  q0 <- length(vec[vec > 0])
  q1 <- length(vec[vec == 1])
  q2 <- length(vec[vec == 2])
  if ((q1 - q2)^2 == (q1 + q2)^2) {
    ch = q0 + q1*(q1 - 1)/((q2 + 1)*2)
  } else {ch = q0 + q1^2/(q2*2)}
  return(ch)
}
