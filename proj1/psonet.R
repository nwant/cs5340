install.packages('pso')
install.packages('plyr')
install.packages('sigmoid')
library("pso")
library("plyr")
library("sigmoid")

# create the training data for a XOR gate
xor.output <- matrix(c(0, 1, 1, 0), ncol=1)
xor.input <- matrix(c(0,0,1,1,0,1,0,1), ncol=2)

# configuration for neural network and pso
config <- c(
  n.inputs = 2,   # the number of inputs in the ANN
  n.hiddens = 2,  # the number of neurons for the hidden layer 
  n.outputs = 1,  # the number of neurons in the output layer
  n.h.layers = 2, # the number of layers in the hidden layer
  pso.n.pop = 2,  # the size of the population in pso
  pso.w = .74,    # the inertia for the pso optimization
  pso.c1 = 2.5,   # constant 1 for pso optimization
  pso.iter = 2,   # the number of iterations to run pso
  w.min = 0,      # the min value for any weight
  w.max = 1,      # the max value for any weight
  v.min = -1,     # the min value for any velocity
  v.max = 1       # the max value for any velocity
)

#=============
# fitness
# -------------------------------------------------------------
# 
# 
# inputs:
#   X - a matrix of the training data inputs
#   W - a list of weight matrices. is expected to have structure as provided below
#   T - a matrix of the expected output training data
#   config - named vector that contains the following members:
#     n.inputs - the number of inputs in the ANN
#     n.hiddens - the number of nuerons for each hidden layer
#     n.outputs - the number of nuerons in the output layer
#     n.h.layers - number of hidden layers in the ANN
#     act.func - the activation function for each neuron
# 
#  W vector structure: 
#   assume n.i = number of inputs, n.h = number of neurons/hidden layer, n.o = number of neurons/output layer
#     and n.l = number of hidden layers.
#   W will be a vector with matrices of the following size:
#   c([n.i x n.h] [n.h x n.h] ... [n.h x n.h] [n.h x n.o])
#   where the number of [n.h x n.h] matrixes in the vector will be equal to n.l - 1.
#   note, if there is only 1 hidden layer, there will be no [n.h x n.h] matrixes in the W vector
#  
#  Returns: a matrix with the fitness for each training data in X
# ---------------------------
fitness <- function(X, W, T, config) {
  n.i <- config[["n.inputs"]] # number of inputs
  n.h <- config[["n.hiddens"]] # number of nuerons/hidden layer
  n.o <- config[["n.outputs"]] # number of nuerons in outer layer
  n.l <- config[["n.h.layers"]] # number of hidden layers
  
  # get the output for hidden layer 1
  net <- X %*% W[[1]] 
  out <- sigmoid(net)
  
  # determine the output for the rest of the hidden layers
  h <- n.l - 1
  if (h > 0) {
    for (i in 2:h) {
      net <- out %*% W[[i]]
      out <- sigmoid(net)
    }
  }
  
  # determine the output of 
  net <- out %*% W[[length(W)]] # get the last element
  out <- sigmoid(net)
  
  # determine the error for each output using least squared
  e <- .5*(T - out)^2
  
  # return the function, which is the average of the error for each training data set.
  return(mean(e))
}

init.matrix.list <- function(config, lb, ub) {
  n.i <- config[["n.inputs"]] # number of inputs
  n.h <- config[["n.hiddens"]] # number of nuerons/hidden layer
  n.o <- config[["n.outputs"]] # number of nuerons in outer layer
  n.l <- config[["n.h.layers"]] # number of hidden layers
  
  # randomly generate weights for the input layer to hidden layer
  l <- list()
  l[[1]] <- matrix(runif(n.i*n.h, lb, ub), ncol=n.h)
  
  h <- n.l - 1
  if (h > 0) {
    for (i in 1:h) {
      l[[length(l) + 1]] <- matrix(runif(n.h*n.h, lb, ub), ncol=n.h)
    }
  }
  l[[length(l) + 1]] <- matrix(runif(n.h*n.o, lb, ub), ncol=n.o)
  
  return(l)
}

pso <- function(X, T, config) {
  # initialize initial population of weights and associated vectors
  n <- config[["pso.n.pop"]] # the size of the population
  pop <- list()            # the populations of weights
  V <- list()              # list of the velocities for each member of the population
  p.best <- list()         # list of the personal best weight configuration for each member of the population
  p.best.fitness <- list() # list of the fitness value for each personal best weight configuration for each mem of the pop
  g.best <- NULL           # the global best weight configuration for all members of the population
  g.best.fitness <- NULL   # the fitness value for the global best weight configuation for all members of the population
  update.init.vars <- function() { 
    i <- length(pop)
    pop[[i]] <- init.weights(config)
    V[[i]] <- init.vectors(config)
    p.best[[i]] <- pop[[i]]
    p.best.fitness[[i]] <- fitness(X, pop[[i]], T)
    
    # is this the global best of the population?
    if (is.null(g.best) || p.best.fitness[[i]] < g.best.fitness) {
      g.best <- pop[[i]]
      g.best.fitness <- fitness(X, g.best, T)
    }
  }
  rep(update.init.vars, n)
  
  update.pop <- function() {
    # determine the random values
    rand <- runif(n*2, 0, 1) # generate 2 random numbers between 0 and 1 for each member of the population
    # calculate the fitness value
    if ()
  }
  
  
}

init.weights <- function(config) {
  return(init.matrix.list(config, lb=config[["w.min"]], ub=config[["w.max"]]))
}

init.vectors <- function(config) {
  return(init.matrix.list(config, lb=config[["v.min"]], ub=config[["v.max"]]))
}








