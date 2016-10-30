#!/usr/bin/env Rscript
# Nathaniel Want (nwqk6)
# CS5340-E01 Fall 2016
# Programming Assignment 1
# November 4, 2016

install.packages('pso')
install.packages('plyr')
install.packages('sigmoid')
library("pso")
library("plyr")
library("sigmoid")

# create the training data for a XOR gate
xor.output <- matrix(c(0, 1, 1, 0), ncol=1)
xor.input <- matrix(c(0,0,1,1,0,1,0,1), ncol=2)


#################### CONFIGURATION ###################################
# configuration for neural network and pso
config <- c(
  n.inputs = 2,      # the number of inputs in the ANN
  n.hiddens = 2,     # the number of neurons for the hidden layer 
  n.outputs = 1,     # the number of neurons in the output layer
  n.h.layers = 2,    # the number of layers in the hidden layer
  pso.n.pop = 2,     # the size of the population in pso
  pso.w = .74,       # the inertia for the pso optimization
  pso.c1 = 2.5,      # the acceleration factor related to gbest for pso
  pso.c2 = 2.5,      # the acceleration factor related to pbest for pso
  pso.iter = 100,    # the number of iterations to run pso
  w.min = 0,         # the min value for any weight
  w.max = 1,         # the max value for any weight
  v.min = -1,        # the min value for any velocity
  v.max = 1          # the max value for any velocity
)
############################################################################

#==============================================================
# fitness
# -------------------------------------------------------------
# get the average fitness value for all fitnesses for each training data
# 
# inputs:
#   X - a matrix of the training data inputs
#   W - a list of weight matrices. is expected to have structure as provided below
#   T - a matrix of the expected output training data
#   config - a configuation named list
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
# -------------------------------------------------------------
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

#==============================================================
# init.matrix.list
# -------------------------------------------------------------
# generate a list of matrices with randomly generated numbers for 
# each connection in theANN, each of which is within the provided 
# upper and lower bounds 
# 
# inputs:
#   config - a configuation named list
#   lb -- the lower bound for each element of each matrix
#   ub -- the upper bound for each element of each matrix
# 
#  Returns: the list of matrices
# -------------------------------------------------------------
init.matrix.list <- function(config, lb, ub) {
  n.i <- config[["n.inputs"]] # number of inputs
  n.h <- config[["n.hiddens"]] # number of nuerons/hidden layer
  n.o <- config[["n.outputs"]] # number of nuerons in outer layer
  n.l <- config[["n.h.layers"]] # number of hidden layers
  
  # randomly generate numbers for the input layer to hidden layer
  l <- list()
  l[[1]] <- matrix(runif(n.i*n.h, lb, ub), ncol=n.h)
  
  # randomly generate numbers for each layer of neurons in the hidden layer
  h <- n.l - 1
  if (h > 0) {
    for (i in 1:h) {
      l[[length(l) + 1]] <- matrix(runif(n.h*n.h, lb, ub), ncol=n.h)
    }
  }
  
  # randomly generate numbers for the hidden layer to outer layer
  l[[length(l) + 1]] <- matrix(runif(n.h*n.o, lb, ub), ncol=n.o)
  
  return(l)
}

#==============================================================
# init.weights
# -------------------------------------------------------------
# generate a list of matrices with randomly generated numbers for 
# the weights for each connection in theANN
# 
# inputs:
#   config - a configuation named list
# 
#  Returns: the weight configuration
# ---------------------------
init.weights <- function(config) {
  return(init.matrix.list(config, lb=config[["w.min"]], ub=config[["w.max"]]))
}

#==============================================================
# init.velocities
# -------------------------------------------------------------
# generate a list of matrices with randomly generated numbers for 
# the velocity for each connection in theANN
# 
# inputs:
#   config - a configuation named list
# 
#  Returns: the velocity configuration
# ---------------------------
init.velocities <- function(config) {
  return(init.matrix.list(config, lb=config[["v.min"]], ub=config[["v.max"]]))
}

#==============================================================
# pso
# -------------------------------------------------------------
# use pso algorithm to train an ANN, as defined in a config named list,
# to determine the best weight values for the ANN after some number
# of iterations, as defined by the provided config named list
# 
# inputs:
#   X -- a matrix of the training data inputs
#   T -- a matrix of the training data outputs
#   config - a configuation named list
# 
#  Returns: the best weight values 
# -------------------------------------------------------------
pso <- function(X, T, config) {
  # initialize initial population of weights and associated vectors
  n <- config[["pso.n.pop"]] # the size of the population
  pop <- list()              # the populations of weights
  V <- list()                # list of the velocities for each member of the population
  p.best <- list()           # list of the personal best weight configuration for each member of the population
  p.best.fitness <- list()   # list of the fitness value for each personal best weight configuration for each mem of the pop
  g.best <- NULL             # the global best weight configuration for all members of the population
  g.best.fitness <- NULL     # the fitness value for the global best weight configuation for all members of the population
  iter = config[["pso.iter"]]# the number of iterations to update the population 
  
  # initialize the population and velocities for each member of the population
  for (i in 1:n) { 
    pop[[i]] <- init.weights(config)
    V[[i]] <- init.velocities(config)
    p.best[[i]] <- NULL
    p.best.fitness[[i]] <- NULL
  }
  
  for (j in 1:iter) { # do for each iteration of the 
    for (i in 1:n) { # do for each member of the population
      # calculate the fitness value
      if (length(p.best) < i) {
        p.best[[i]] <- pop[[i]]
        p.best.fitness[[i]] <- fitness(X, pop[[i]], T, config)
      } else {
        # is this the new personal best for this member?
        f <- fitness(X, pop[[i]], T, config)
        if (f < p.best.fitness[[i]]) {
          p.best[[i]] <- pop[[i]]
          p.best.fitness[[i]] <- f
        }
      }
      
      # is the personal best for this member the global best for the swarm?
      if (is.null(g.best) || p.best.fitness[[i]] < g.best.fitness) {
        g.best <- pop[[i]]
        g.best.fitness <- fitness(X, g.best, T, config)
      }
      
      # update the new velocity and position for this member.
      V[[i]] <- pso.update.velocity(V[[i]], p.best[[i]], g.best, pop[[i]], config)
      pop[[i]] <- pso.update.position(pop[[i]], V[[i]], config)
    }
  }
  
  return(g.best)
}

#==============================================================
# pso.update.velocity
# -------------------------------------------------------------
# update the velocity for a population member in a swarm
# 
# inputs:
#   V -- the velocity matrices for this member
#   p.best -- the personal best weight configuration for this member
#   g.best -- the global best weight configuration for the swarm
#   pos -- the current weight configuation (the position) for this member
#   config -- a configuation named list
#
#  Returns: the updated velocity matrices for this member
# -------------------------------------------------------------
pso.update.velocity <- function(V, p.best, g.best, pos, config) {
  w = config[["pso.w"]]      # the interia constant for pso
  c1 = config[["pso.c1"]]    # the acceleration factor related to gbest
  c2 = config[["pso.c2"]]    # the acceleration factor related to pbest
  lb = config[["v.min"]]     # the lower bound for any velocity
  ub = config[["v.max"]]     # the upper bound for any velocity
  
  # generate 2 random numbers (rand1 and rand2) between 0 and 1
  rand <- runif(2, 0, 1)
  # update all of the velocities using the pso velocity update function
  # clip the velocities if their updated values go out of the upper/lower bounds
  for(i in 1:length(V))
    V[[i]] <- w * V[[i]] + (c1*rand[[1]]) * (p.best[[i]] - pos[[i]]) + (c2*rand[[2]]) * (g.best[[i]] - pos[[i]])
    V[[i]][V[[i]] < lb] <- lb
    V[[i]][V[[i]] > ub] <- ub
  return(V)
}

#==============================================================
# pso.update.position
# -------------------------------------------------------------
# update the position for a population member in a swarm
# 
# inputs:
#   pos -- the current weight configuation (the position) for this member
#   V -- the velocity matrices for this member
#   config -- a configuation named list
#
#  Returns: the updated weight configuration (position) for this member
# -------------------------------------------------------------
pso.update.position <- function(pos, V, config) {
  lb = config[["w.min"]] # the lower bound for any weight
  ub = config[["w.max"]] # the upper bound for any weight
  
  # update all of the weights using the pso position update function
  # clip the weights if their updated values go out of the upper/lower bounds
  for (i in 1:length(pos)) {
    pos[[i]] = pos[[i]] + V[[i]]
    pos[[i]][pos[[i]] < lb] <- lb
    pos[[i]][pos[[i]] > ub] <- ub
  }
  return(pos)
}












