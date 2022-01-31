library(dplyr)

sim_prmt <- expand.grid(
  p = c(5, 10, 50, 100, 200),
  # n_train = c(500),
  # n_test = c(10000),
  dis = c("gaussian", "binomial")
) %>% filter(p==100)
   # data.frame()

# A wrapper function to set up each job
start.sim <- function(
  p,
  n_train, n_test, dis
) {
  # names
  # ntrain.name <- paste0("ntr_", n_train)
  # ntest.name <- paste0("nte_", n_test)
  p.name <- paste0("p_", p)
  dis.name <- paste0("dis_", dis)
  
  # job.name <- paste("bgam", dis.name, p.name,
  #                   # ntrain.name,
  #                   # ntest.name, 
  #                   sep="-")
  
  job.name <- paste("bgam", dis.name, p.name,
                    # ntrain.name,
                    # ntest.name, 
                    sep="-")
  
  job.flag <- paste0("--job-name=",job.name)
  
  err.flag <- paste0("--error=",job.name,".err")
  
  out.flag <- paste0("--output=",job.name,".out")
  
  arg.flag <- paste0("--export=",
                     # ,"ntrain=", n_train, ",",
                     #"ntest=", n_test, ",",
                     "p=", p,",",
                     "dist=", dis )
  
  system(
    paste("sbatch", job.flag, err.flag, out.flag, arg.flag,"~/GitHub/Manuscript-BHAM/Simulation/Code/bgam_sim.job")
  )
}

# Delete Previous Log Files
# unlink("/data/user/boyiguo1/bgam/log/", recursive = TRUE)
# unlink("/data/user/boyiguo1/bgam/sim_res/main/", recursive = TRUE)



for(i in 1:NROW(sim_prmt)){
  do.call(start.sim, sim_prmt[i,])
}