library(xtermStyle)

dhondt <- function() {
  ## Find total votes for each party
  party.votes.pc <- colSums(votes)/sum(colSums(votes))*100
  print(party.votes.pc)
  ## Set number of seats per party to zero
  party.seats <- party.votes.pc*0
  for (i in 1:nrow(votes)) {
    winner <- which.max(party.votes.pc/(party.seats + 1))
    party.seats[winner] <- party.seats[winner] + 1
  }
  return(data.frame(party.seats=party.seats, party.votes.pc=party.votes.pc, seats.per.vote.pc=party.seats/party.votes.pc))
}

remainders <- function() {
  votes[is.na(votes)] <- 0
  ## Calculate number of votes for each party
  party.votes.pc <- colSums(votes)/sum(colSums(votes))*100
  print(party.votes.pc)
  ## Set number of seats per party to zero
  party.seats <- party.votes.pc*0
  ## Percentatges in constituencies
  votes.pc <- votes/rowSums(votes)*100
  ## Number of consituencies
  ncons <- nrow(votes)
  nparties <- ncol(votes)
  ## Party names
  party.names <- colnames(votes)
  
  ## Elect our members

  results <- NULL
  for (i in 1:ncons) {
    cons.names <- rownames(votes.pc)
    party.seats.pc <- party.seats/ncons*100
    underrepresentation <- party.votes.pc/(party.seats.pc + 1/ncons)
    barplot(log(underrepresentation))
    adjusted.votes <- votes.pc*matrix(underrepresentation, ncons, nparties, byrow=TRUE)
    max.ind <- which(adjusted.votes == max(adjusted.votes), arr.ind=TRUE)
    winner.ind <- max.ind[1,"col"]
    winner <- party.names[winner.ind]
    cons.ind <- max.ind[1,"row"]
    cons <- cons.names[cons.ind]
    party.seats[winner] <- party.seats[winner] + 1
    newres <- cbind(Constituency=cons, Winner=winner)
    votes.pc <- votes.pc[-cons.ind,]
    print(newres)
    results <- rbind(results, newres)
  }
  return(list(party.seats=party.seats,
              party.votes.pc=party.votes.pc,
              results=results,
              underrepresentation=underrepresentation))
}

system1 <- function() {
  res <- NULL
  round <- 1
  unelected <- 1:nrow(votes)
  votes[is.na(votes)] <- 0
  for(round in 1:10) {
    votespc <- votes/rowSums(votes)*100
    threshold <- rowSums(votes)
    simple.maj <- which(apply(votes/threshold > 0.5, 1, any))

    message(paste("Round", round))
    votes.simple.maj <- votes[simple.maj,]
    threshold.simple.maj <- threshold[simple.maj]
    winner.ind <- unlist(apply(votes[simple.maj,], 1, which.max))
    newres <- cbind(const=names(simple.maj),
                    winner=colnames(votes.simple.maj)[winner.ind])
    print(newres)
    for(i in 1:nrow(votes.simple.maj)) {
      votes.simple.maj[i, winner.ind[i]] <- votes.simple.maj[i, winner.ind[i]] - threshold.simple.maj[i]
                                        # votes.simple.maj[i, winner.ind[i]] <- 0
    }
    wasted.votes <- colSums(votes.simple.maj)
    remaining.votes <- votes[-simple.maj,]
    remaining.votes <- remaining.votes + matrix(wasted.votes/nrow(remaining.votes), nrow(votes), ncol(votes), byrow=TRUE)
    votes <- remaining.votes
    message(paste(nrow(votes), "constituencies remaining"))
  }

  res <- rbind(res, cbind(newres, round=round))
               
}

national.transferable.vote <- function(votes) {
  winners <- rep(NA, nrow(votes))
  names(winners) <- rownames(votes)

  votes0 <- votes
  round <- 0

  ## while(round < 2) {
  while(any(is.na(winners))) {
    
    round <- round + 1
    votes.pc <- votes/rowSums(votes)*100

    message("Round", round, "determining winners")
    simple.maj.consts <- which(apply(votes.pc > 50, 1, any) & is.na(winners))
    if (length(simple.maj.consts) == 0) {
      apply(votes.pc[is.na(winners),], 1, max)
      simple.maj.consts <- which(rownames(votes) == names(sort(apply(res$votes.pc[is.na(winners),], 1, max), decreasing=TRUE)[1]))
      print(simple.maj.consts)
      message("No simple majority winners")
    }
    votes.to.dist <- votes[1,]*0
    for (const in simple.maj.consts) {
      message(rownames(votes)[const])
      ## Find winner
      winners[const] <- names(which.max(votes[const,]))
      message(paste(winners[const], "WIN  with", votes[const,winners[const]],
                    "of", sum(votes[const,]), "votes cast (",
                    votes[const,winners[const]]/sum(votes[const,])*100, "%)"))
      
      vtd <- votes[const,]
      vtd[winners[const]] <- vtd[winners[const]] - sum(votes[const,])/2
      vtd[1,vtd[1,]<0] <- 0
      message("Votes to distribute")
      print(vtd[1, vtd[1,]!=0])

      votes.to.dist <- votes.to.dist + vtd
    }
    message(paste("Round", round, "distributing votes"))
    for (party in colnames(votes)) {
      ## Find constituencies to distribute to
      p.consts <- which((votes[,party] != 0) & is.na(winners))
      if ((length(p.consts) > 0) & votes.to.dist[1,party] != 0) {
        message(paste("Distributing", votes.to.dist[1,party], "votes for", party, "to", length(p.consts), "Constituencies"))
        votes[p.consts,party] <-  votes[p.consts,party] + votes.to.dist[1,party]/length(p.consts)
      }
    }
  }
  return(list(votes.pc=votes.pc, winners=winners))
}



booster <- function(votes) {
  votes.orig = votes
  party.votes <- colSums(votes)
  fptp.orig <- colnames(votes)[unlist(apply(votes, 1, which.max))]
  while(TRUE) {
    fptp <- colnames(votes)[unlist(apply(votes, 1, which.max))]
    seats = table(fptp)
    print(seats)
    votes.per.seat = sapply(names(seats), function(x) {party.votes[x]/seats[x]})
    names(votes.per.seat) <- names(seats)
    print(votes.per.seat)
    votes.per.seat.norm = votes.per.seat/(sum(party.votes)/sum(seats))
    print(votes.per.seat.norm)
    for (party in names(seats)) {
      votes[,party] = votes[,party] * (1 + (votes.per.seat.norm[party] - 1)/1000)
    }
    if (all(votes.per.seat.norm < 1.3) & all(votes.per.seat.norm > 0.7))
      break
    }
    print(fptp.orig)
    print(fptp)
    return(cbind(votes, fptp.orig, fptp))
}

shifter <- function(votes) {
  V <- votes
  ## Remaining vote matrix
  R <- (votes > 0)*1
  X <- V # Initial shifted vote matrix
  M <- nrow(X) # Number of constituencies
  N <- ncol(X) # Number of parties
  
  ## Loop

  while(TRUE) {
    ## Compute constraint forces
    A <- rbind(cbind(diag(rowSums(R)), R - matrix(1, nrow(R), ncol(R))),
               cbind(t(R), diag(colSums(R))))
    lambda.mu <- solve(A, c(rowSums(X), colSums(X)))
    lambda <- lambda.mu[1:M]
    mu <- lambda.mu[(M+1):(M+N)]
    ## Speed
    S <- R * (X - lambda %*% rbind(rep(1, N)) - cbind(rep(1, M)) %*% mu)
    ## print(S)
    if (isTRUE(all.equal(S, matrix(0, nrow(S), ncol(S))))) {
      message("Converged")
      break
    }
    ## Time to elimination
    TAU <- -X / S
    t.elim <- min(na.omit(TAU[TAU>0]))
    ind.elim <- which(TAU == t.elim, arr.ind=TRUE)
    ## Update votes
    X1 <- X + S*t.elim
    if (max(abs(colSums(V) - colSums(X1)))>1) {
      warning("Discrepancy in party totals")
      break
    }
    X <- X1
    
    for (i in 1:nrow(ind.elim)){
      X[ind.elim[i,1], ind.elim[i,2]] <- 0

      ## Update remaining matrix
      R[ind.elim[i,1], ind.elim[i,2]] <- 0

    
      message(paste(style(colnames(R)[ind.elim[i,2]], "in", rownames(R)[ind.elim[i,1]], "eliminated;", font="bold"), sum(R), "candidates left"))
      if (is.na(colnames(R)[ind.elim[i,2]]) | is.na(rownames(R)[ind.elim[i,1]])) {
        print(ind.elim)
      }
      if (sum(R[ind.elim[i,1],]) == 1) {
       message(style(paste(colnames(R)[which(R[ind.elim[i,1],] == 1)], "ELECTED in", rownames(R)[ind.elim[i,1]]), fg="red", font="bold"))
      }
     # message("Biggest discrepency in party totals:", max(abs(colSums(V) - colSums(X))))
    }  
  }
  ## Determine winners
  Winds <- apply(X, 1, which.max)
  W <- R*0
  for(i in 1:M) {
    W[i, Winds[i]] <- 1
  }
  return(list(X=X, W=W, S=S))
}


## Given a remain matrix, generate the constraint matrix
constraint.matrix <- function(R) {
  A <- rbind(cbind(diag(rowSums(R)), R - matrix(1, nrow(R), ncol(R))),
               cbind(t(R), diag(colSums(R))))
}

## Given a remain matrix, generate the matrix that elimiates element
## i,j of the constraint matrix
eliminator.matrix <- function(R, i, j) {
  # Num parties left in each const
  P <- rowSums(R)
  M <- nrow(R)
  # Num consts left for each party
  C <- colSums(R)
  N <- ncol(R)
  ## Initialise as diag matrix
  E <- diag(1, M + N)
  E[i,i] <- 1/(P[i] - 1)*(P[i] + 1/(C[j] - 1))
  E[M+j,i] <- 1/(C[j] - 1)
  E[M+(1:N)[-j],i] <- -C[j]/(C[j] - 1)/(P[i] - 1)/C[(1:N)[-j]]

  E[i,M+j] <- C[j]/(C[j] - 1)/(P[i] - 1)
  E[M+j,M+j] <- C[j]/(C[j] - 1)
  E[M+(1:N)[-j],M+j] <- -C[j]/(C[j] - 1)/(P[i] - 1)/C[(1:N)[-j]]
  return(E)
}

eg <- rbind("Smoketown"=cbind(Progressives=48, Regressives=90, Obsessives=12),
                 "Verdant Valley"=cbind(72, 60, 12))
rownames(eg) <- c("Smoketown", "Verdant Valley")
#out = booster(votes)
#table(out[,c("fptp", "fptp.orig")])
R <- (eg>0) * 1
A <- constraint.matrix(R)
R1 <- R
R1[1, 1] <- 0
A1 <- constraint.matrix(R1)
A1.it <- A %*% solve(eliminator.matrix(R, 1, 1))
print(all(A1.it == A1))

R2 <- R1
R2[2, 3] <- 0
A2 <- constraint.matrix(R2)
A2.it <- A1 %*% solve(eliminator.matrix(R1, 2, 3))
print(all(A2.it == A2))
print(solve(A2, A1))

## ans <- shifter(votes)
## shifter.winners <- cbind(rownames(ans$W), colnames(ans$W)[unlist(apply(ans$W, 1, which.max))])
## colnames(shifter.winners) <- c("Const.", "Winner")
## write.csv(shifter.winners, "lagrange-shifter-winners-2010.csv", row.names=FALSE)

