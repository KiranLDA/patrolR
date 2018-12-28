#' Find optimal sites to be patrolled within budget
#'
#' @description this function optimises patrols at a single site
#'
#' @param sites list of sites to patrol
#' @param benefit the benefit of patrolling those sites
#' @param cost cost incurred from patrolling those sotes
#' @param budget available budget
#' @param chromosome initial values for the genetic algorithm
#' @param iter number of iterations for genetic algorithm, default 100
#' @param popSize population size of genetic algortihm, default 200
#' @param mutationChance population size of genetic algortihm, default 0.01
#' @param elitism default set to true T
#'
#' @return genetic algorithm output from genalg package with best solution
#'
#' @examples
#' data(patrol_info)
#' sites =rownames(patrol_info)
#' benefit = patrol_info[,"Total number of birds"] * patrol_info[,"Total number of species"]* patrol_info[,"Average number of disturbances per count"]
#' cost = patrol_info[,"Caloundra patrol cost"]
#' budget <- 2000
#'
#' #find best sites to patrol within budget
#' solutions = staticPATROL(sites, benefit, cost, budget)
#'
#' # Print optimal solution
#' cat(summary(solutions))
#'
#' # All solutions
#' solutions
#'
#'
#' @export
staticPATROL<- function(sites, benefit, cost, budget,
                        chromosome = rep_len(1, length(sites)),
                        iter = 100, popSize = 200,
                        mutationChance = 0.01,
                        elitism = T){

  # for testing
  # data(patrol_info)
  # sites =rownames(patrol_info)
  # benefit = patrol_info[,"Total number of birds"] * patrol_info[,"Total number of species"]* patrol_info[,"Average number of disturbances per count"]
  # cost = patrol_info[,"Caloundra patrol cost"]
  # budget <- 2000
  # chromosome = rep_len(1, length(sites))

  dataset <- data.frame(sites =sites,
                        benefit = benefit,
                        cost = cost)

  # chromosome = c(1, 1, 1, 1, 1, 1, 1,1,1,1)
  # dataset[chromosome == 1, ]
  # cat(chromosome %*% dataset$benefit)
  evalFunc <- function(x) {
    current_solution_benefit <- x %*% dataset$benefit
    current_solution_cost <- x %*% dataset$cost

    if (current_solution_cost > budget)
      return(0) else return(-current_solution_benefit)
  }


  GAmodel <- rbga.bin(size = length(sites), popSize = popSize, iters = iter, mutationChance = mutationChance,
                      elitism = elitism, evalFunc = evalFunc)
  # print(cat(summary(GAmodel)))
  #
  # results = summary(GAmodel)
  # solution = c(1, 1, 1, 1, 1, 0, 1)
  # dataset[solution == 1, ]
  # # solution vs available
  # cat(paste(solution %*% dataset$benefit, "/", sum(dataset$benefit)))
  return(GAmodel)
}

