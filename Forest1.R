library(igraph)

################################################
# Total neighbours
################################################
tot_nei <- function(g, nodes){
  unique(unlist(neighbors(g, nodes)))
}

################################################
# Fire cluster (proper BFS)
################################################
Fire <- function(g, x, seed){

  if (x[seed] == 0) return(integer(0))

  burned <- seed
  frontier <- seed

  while(length(frontier) > 0){
    ne <- tot_nei(g, frontier)
    new <- ne[x[ne] == 1 & !(ne %in% burned)]

    if(length(new) == 0) break

    burned <- unique(c(burned, new))
    frontier <- new
  }

  return(burned)
}

################################################
# Forest Fire SOC
################################################
Forest <- function(n, t, p = 0.01, f = 1e-4){

  g <- make_lattice(length = n, dim = 2)
  nod <- gorder(g)

  score <- rep(0, nod)   # 0 = empty, 1 = tree

  fire_sizes <- c()
  occ <- c()

  for(i in 1:t){

    # Tree growth (slow driving)
    grow <- which(runif(nod) < p)
    score[grow] <- 1

    # Lightning (very rare)
    if(runif(1) < f){
      seed <- sample(1:nod, 1)
      fi <- Fire(g, score, seed)

      if(length(fi) > 0){
        score[fi] <- 0
        fire_sizes <- c(fire_sizes, length(fi))
      }
    }

    occ <- c(occ, sum(score))
  }

  # Occupancy plot
  plot(occ, type="l", col="blue", lwd=2,
       xlab="Time", ylab="Tree occupancy",
       main="Forest Fire SOC")

  return(list(occupancy = occ,
              fire_sizes = fire_sizes))
}

################################################
# Run
################################################

res <- Forest(50, 5000, p=0.01, f=1e-4)

# Fire size distribution
hist(res$fire_sizes, breaks=50, prob=TRUE,
     main="Fire size distribution",
     xlab="Fire size")
