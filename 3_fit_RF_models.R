source("0_import_libraries.R")
results.all <- read.csv("Results/Results_I_N_scenarios.csv")    # read in R0, relative contribution of each transmission pathway for 1000 parameter sets under 27 scenarios

# reformat the parameter sets to combine with results.all
k.value <- c(0.05, 0.15, 0.5)
par.set <- NULL
for(k.sce in k.value)
{
  scaled <- read.csv(paste("Parameters/LHS_pars_scaled_k", k.sce, ".csv", sep = ""))
  
  for(I.sce in 1:3)
    for(N.sce in 1:3)
    {
      scaled$I <- eval(parse(text = paste("scaled$I", I.sce, sep = "")))
      
      scaled$Nlh <- eval(parse(text = paste("scaled$Nlh", N.sce, sep = "")))
      scaled$Nnh <- eval(parse(text = paste("scaled$Nnh", N.sce, sep = "")))
      scaled$Nah <- eval(parse(text = paste("scaled$Nah", N.sce, sep = "")))
      
      scaled$Cll <- eval(parse(text = paste("scaled$Cll", N.sce, sep = "")))
      scaled$Cnl <- eval(parse(text = paste("scaled$Cnl", N.sce, sep = "")))
      scaled$Cal <- eval(parse(text = paste("scaled$Cal", N.sce, sep = "")))
      scaled$Cln <- eval(parse(text = paste("scaled$Cln", N.sce, sep = "")))
      scaled$Cnn <- eval(parse(text = paste("scaled$Cnn", N.sce, sep = "")))
      scaled$Can <- eval(parse(text = paste("scaled$Can", N.sce, sep = "")))
      scaled$Cla <- eval(parse(text = paste("scaled$Cla", N.sce, sep = "")))
      scaled$Cna <- eval(parse(text = paste("scaled$Cna", N.sce, sep = "")))
      scaled$Caa <- eval(parse(text = paste("scaled$Caa", N.sce, sep = "")))
      
      par.set.current <- scaled %>%
        select(E:Ra) %>%
        mutate(I.sce = I.sce,
               N.sce = N.sce,
               K.sce = k.sce) %>%
        mutate(ID = 1:n())
      
      par.set <- rbind(par.set, par.set.current)
    }
}



# combine results with the parameter sets, and classify the relative contributions to seven patterns. See definitions of the patterns in S2 Text
results.all.withpar <- results.all %>%
  mutate(ID = rep(1:1000, 27)) %>%
  left_join(par.set) %>%
  mutate(pattern = case_when(
    vertical > 0.6 ~ "transovarial",
    nonsystemic > 0.6 ~ "nonsystemic",
    systemic > 0.6 ~ "systemic",
    vertical < 0.2 & systemic < 0.6 & nonsystemic < 0.6 ~ "systemic + nonsystemic",
    vertical < 0.6 & systemic < 0.2 & nonsystemic < 0.6 ~ "transovarial + nonsystemic",
    vertical < 0.6 & systemic < 0.6 & nonsystemic < 0.2 ~ "transovarial + systemic",
    vertical < 0.6 & vertical > 0.2 & systemic < 0.6 & systemic > 0.2 & nonsystemic < 0.6 & nonsystemic > 0.2 ~ "all"
  ))



# train RF models and get variable importance; 5 outcomes and 9 scenarios
Outcome <- c("R0", "systemic", "nonsystemic", "vertical", "pattern")
for(out.i in 1:5)   # loop over 5 outcomes
{
  rf.imp <- NULL
  rf.pdp <- NULL
  
  for(k.sce in k.value)
    for(i.sce in 1:3)
      for(n.sce in 1:3)
      {
        cat(out.i, k.sce, i.sce, n.sce, "\n")
        
        current.dat <- results.all.withpar %>%
          filter(K.sce == k.sce, I.sce == i.sce, N.sce == n.sce) %>%
          mutate(pattern = factor(pattern)) %>%
          select(E:Ra, Outcome[out.i]) 
        
        if(Outcome[out.i] != "pattern")
        {
          task <- makeRegrTask(data = current.dat, target = Outcome[out.i])
        } else {
          task <- makeClassifTask(data = current.dat, target = "pattern")
        }
        rand_frst <-  tuneRanger(task)
        rand_all_result <- ranger(as.formula(paste(Outcome[out.i], "~.")), data=current.dat, importance = "permutation", mtry = rand_frst$recommended.pars$mtry, min.node.size = rand_frst$recommended.pars$min.node.size, sample.fraction = rand_frst$recommended.pars$sample.fraction)
        
        if(out.i != 5)
        {
          for(par.i in 1:length(par.names))
          {
            current.pdp <- partial(rand_all_result, par.names[par.i]) %>%
              rename(par = par.names[par.i]) %>%
              mutate(K.sce = k.sce, I.sce = i.sce, N.sce = n.sce,
                     outcome = Outcome[out.i],
                     parname = par.names[par.i])
            rf.pdp <- rbind(rf.pdp, current.pdp)
          }
        }
        
        
        # record the importance from 100 random draws of 80% of the training data
        for(i in 1:100)
        {
          rand_result <- ranger(as.formula(paste(Outcome[out.i], "~.")), data=current.dat[sample(1:1000, 800, replace = TRUE),], importance = "permutation", mtry = rand_frst$recommended.pars$mtry, min.node.size = rand_frst$recommended.pars$min.node.size, sample.fraction = rand_frst$recommended.pars$sample.fraction)
          rf.imp <- rbind(rf.imp, 
                          data.frame(variable = names(rand_result$variable.importance), importance =rand_result$variable.importance, ID = i, K.sce = k.sce, I.sce = i.sce, N.sce = n.sce))
        }
      }
  write.csv(rf.imp, paste("Results/RF_imp_", Outcome[out.i], ".csv", sep = ""), row.names = FALSE)
  write.csv(rf.pdp, paste("Results/RF_pdp_", Outcome[out.i], ".csv", sep = ""), row.names = FALSE)
}



