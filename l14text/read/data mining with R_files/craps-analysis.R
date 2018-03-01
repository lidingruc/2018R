# craps analysis (Hugh Murrell)

throwDice <- function(n=1) {         # here is recursion in action
  if (n==1) {
    return( c(sum(ceiling(6*runif(2)))) )
  } else {
    return( c(sum(ceiling(6*runif(2))),throwDice(n-1)) )
  }
}

play <- throwDice(200)

which(play == 7)
which(play == 45)[1]

crapsWin <- function(n=500) {
  s <- throwDice(n)
  if (s[1] == 7 || s[1] == 11) return(TRUE)
  p7 <- which(s == 7)[1]
  p11 <- which(s == 11)[1]
  p <- which(s == s[1])[2]
  if (anyNA(c(p7,p11,p))) {
    print("Undecided, retrying ...")
    return(crapsWin(n))
  }
  if (p < p7 && p < p11) return(TRUE)
  return(FALSE)
}

(crapsWinProb <- sum(sapply(rep(250,10000),crapsWin)) / 10000)


# ways ofthrowing a 7 is 16 , 61 , 25 , 52 , 34 , 43 so prob of 7 is 6/36
# ways of throwing 11 is 56 , 65 so prob of 11 is 2/36

# prob of 2 is 1/36
# prob of 3 is 2/36
# prob of 4 is 3/36
# prob of 5 is 4/36
# prob of 6 is 5/36

# prob of 8 is 5/36
# prob of 9 is 4/36
# prob of 10 is 3/36

# prob of 12 is 1/36

# prob of wining on first throw is 8/36

# if you throw a 2 on the first throw then the prob of wining 1/(8+1)
# if you throw a 3 on the first throw then the prob of wining 2/(8+ 2) etc etc

# so your chances of winning overall are:

(8/36)  + (1/36) * (1/9) + (2/36) * (2/10) + (3/36) * (3/11) +
  (4/36) * (4/12) + (5/36) * (5/13) + (5/36) * (5/13) + (4/36) * (4/12) +
  (3/36) * (3/11) + (1/36) * (1/9)