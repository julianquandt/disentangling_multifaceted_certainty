# optionally install packages ---------------------------------------------

if(!exists("init_scripts_executed")){
  
  init_scripts_executed = TRUE
  
  this_dir_name <- function () # https://stackoverflow.com/a/1816487
  {
    frame_files <- lapply(sys.frames(), function(x) x$ofile)
    frame_files <- Filter(Negate(is.null), frame_files)
    file_name <- frame_files[[length(frame_files)]]
    dir_name <- dirname(file_name)
    return(dir_name)
    
  }
  
  # handle install requests
  prompt_input <- function(){
    resp <- readline("(y = install, n = cancel)")
    
    if(!(resp == "y" | resp == "n")) {
      cat("yes or no answer needed (y = install / n = cancel)\n")
      prompt_input()
    }
    
    return(resp)
  }
  
  # install or load packages
  opt_install <- function(nps, ask = T){
    for (ap in nps){
      ip <- as.data.frame(installed.packages())
      ip <- rownames(ip)
      
      if(ap %in% ip){
        require(ap, character.only = T)
      }
      else{
        if(ask == T){
          cat(paste0("package ", ap, " not installed. Install now?\n"))
          resp <- prompt_input()
          if(resp == "n"){
            cat(paste0("package ", ap, " will not be installed.\n"))
          }else if(resp == "y"){
            install.packages(ap)
            require(ap, character.only = T)
          }
        }
        else {
          install.packages(as.character(ap))
          require(ap, character.only = T)
        }
      }
    }
  }
  
  
  # opt_install(c("rstan", "flextable", "officer", "plyr", "ggplot2", "brms", "afex",
  #               "prereg", "gridExtra", "loo", "osfr", "ggbeeswarm",
  #               "stringr", "HDInterval", "grid", "future", "cmdstanr", "listenv", 
  #               "parallel", "doParallel", "foreach", "doRNG", "papaja", "patchwork", "cowplot", "emmeans"))

  
  require(rstan)
  require(flextable)
  require(officer)
  require(plyr)
  require(ggplot2)
  require(brms)
  require(afex)
  require(prereg)
  require(gridExtra)
  require(loo)
  require(osfr)
  require(ggbeeswarm)
  require(stringr)
  require(HDInterval)
  require(grid)
  require(future)
  require(cmdstanr)
  require(listenv)
  require(parallel)
  require(doParallel)
  require(foreach)
  require(doRNG)
  require(papaja)
  require(patchwork)
  require(cowplot)
  require(emmeans)
  require(devtools)
  require(here)
  
  # misc --------------------------------------------------------------------
  
  
  # function to inverse %in% operator
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  n_cores <- 8
  
  # set standard contrasts to sum-to-zero and polynomial
  options(contrasts = c("contr.sum","contr.poly"))
  
  # set locale to english.
  Sys.setenv(LANG = "en")
  
}




