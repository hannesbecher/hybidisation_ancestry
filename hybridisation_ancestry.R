# Ancestral block simulation


# Two genomes per individual.
# Constant population size

# Gametes are made from diploids. They turn into the new genomes.
# Each adult has two genomes. With random mating (not excluding rare
# selfing) 2N gametes can be combined into the new individuals. If there is
# (partial) selfing, then both of an idividual's genomes (ie gametes) come from
# the same parent.

# Initialise ####
# Initialise list of genomes
#   There are two types, 0 and 1
N <- 50
genomes <- lapply(rep(c(0,1), each=N), function(x){
#genomes <- lapply(rep(c(0,1), 50), function(x){
  list(x, vector(mode = "numeric"))
}
)
parents <- matrix(1:(2*N), 2, N)

parent_fitness <- rep(1, N)

ancAtPos <- function(gnm, pos){
  if(sum(gnm[[2]]<pos)%%2 == 0) gnm[[1]]
  else abs(gnm[[1]] - 1)
}

# A function to make gametes from a parent
#   Need to check ancestry at point of rec, not currently implemented!
gamete <- function(parent){
  co <- runif(1)
  ids <- sample(c(1,2), 2)
  g1 <- genomes[[parents[ids[1], parent] ]]
  g2 <- genomes[[parents[ids[2], parent] ]]
  anc1 <- ancAtPos(g1, co)
  anc2 <- ancAtPos(g2, co)
  if(anc1 == anc2){
    gam <- list(g1[[1]], c(g1[[2]][g1[[2]]<co], g2[[2]][g2[[2]]>=co]))
    # print("no new junct")
  } else {
    gam <- list(g1[[1]], c(g1[[2]][g1[[2]]<co], co, g2[[2]][g2[[2]]>=co]))
    # print("new junction!")
  }
  return(gam)
}

# choose parents of next generarion
choose_parents <- function(on=N, sr=0.5){
  sapply(1:on, function(x) {
    p1 <- sample(on, 1)
    if(sample(2,1,prob = c(1-sr, sr))==1) {
      p2 <- sample(on, 1)
      # print(x)
    } else p2 <- p1
    c(p1, p2)
  })
}
genomes_new <- list()

gen <- 0
generation <- function(times=1){
  for(i in 1:times){
    if(gen%%100 == 0) print(gen)
    parentsc <- choose_parents(sr = 0.0)
    parentsc  
    for(i in 1:N){
      genomes_new[[i*2-1]] <- gamete(parentsc[1, i])
      genomes_new[[i*2]] <- gamete(parentsc[2, i])
    }
    genomes <<- genomes_new
    gen <<- gen + 1
  }
}
# run ####
#generation(500)
generation(5)
# gen
# plot(sapply(seq(0,1,.01), function(x) ancAtPos(genomes[[1]], x)),
#      xlab="Genome position", ylab="Ancestry", pch=19)



#genomes[[1]]

# junctions per genome
# sapply(genomes, function(x) length(x[[2]]))
# sapply(genomes, function(x) x[[1]])

# plot(rowSums(sapply(genomes, function(y) {
#   sapply(seq(0,1,.001), function(x) ancAtPos(y, x))
# }
# )))

plot(rowSums(sapply(genomes, function(y) {
  sapply(seq(0,1,.01), function(x) ancAtPos(y, x))
}
)), xlab="position on chromosome", ylab="allele frequency", ylim=c(0,2*N))

