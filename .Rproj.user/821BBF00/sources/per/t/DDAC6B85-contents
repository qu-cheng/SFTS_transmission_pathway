source("0_import_libraries.R")


results.all <- sens.mat.all <- elas.mat.all <- sens.mat.kij.all <- elas.mat.kij.all <- list()  # record result, results.all for R0, sens.mat.all and elas.mat.all for the sensitivity and elasticity values for the parameters, sens.mat.kij.all and elas.mat.kij.all for sensitivity and elasticity values for element kij in the next-generation matrix



# WARNING: the following code takes a long time (~ 18 mins on a MacBook Air with M3 chip, without parallelization using parallelization can reduce the computational time)
k.value <- c(0.05, 0.15, 0.5)   # loop over different tick aggregation pattern scenarios
for(k.sce in k.value)
{
  scaled <- read.csv(paste("Parameters/LHS_pars_scaled_k", k.sce, ".csv", sep = ""))
  n.size <- nrow(scaled) # sample size
  
  for(I.sce in 1:3)    # loop over different duration of viremia period scenarios
    for(N.sce in 1:3)    # loop over different tick abundance scenarios
    {
      results <- matrix(NA, n.size, 6)
      colnames(results) <- c("vertical", "nonsystemic", "systemic", "R0","R0_vertical", "R0_novertical")   # for recording relative contribution from each transmission pathway (the first 3 columns) and the R0 value, R0 value for only vertical transmisison, and R0 value without vertical transmission
      
      sens.mat <- elas.mat <- matrix(NA, n.size, n.par)
      colnames(sens.mat) <- colnames(elas.mat) <- par.names
      
      sens.mat.kij <- elas.mat.kij <- matrix(NA, n.size, 25)  # sens for kij element, first column, then row
      
      # get the values for the current scenario
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
      
      
      for(i in 1:nrow(scaled))
      {
        cat(I.sce, N.sce, k.sce, i, "\n")
        
        current.par <- scaled[i,]
        attach(current.par)
        
        # estimate each element of the next generation matrix
        K.mat <- matrix(0, 5, 5)
        
        K.mat[1,1] <- Sl*Sn*Sa*E*Ra
        K.mat[1,2] <- Sn*Sa*E*Ra
        K.mat[1,3] <- Sa*E*Ra
        K.mat[1,4] <- E*Ra
        
        K.mat[2,1] <- (Sl*Cll + Sl*Sn*Cln + Sl*Sn*Sa*Cla)*theta*Cs*Hcn
        K.mat[2,2] <- (Sn*Cln + Sn*Sa*Cla)*theta*Cs*Hcn
        K.mat[2,3] <- (Sa*Cla)*theta*Cs*Hcn
        
        K.mat[3,1] <- (Sl*Cnl + Sl*Sn*Cnn + Sl*Sn*Sa*Cna)*theta*Cs*Hcn
        K.mat[3,2] <- (Sn*Cnn + Sn*Sa*Cna)*theta*Cs*Hcn
        K.mat[3,3] <- (Sa*Cna)*theta*Cs*Hcn
        
        K.mat[4,1] <- (Sl*Cal + Sl*Sn*Can + Sl*Sn*Sa*Caa)*theta*Cs*Hcn
        K.mat[4,2] <- (Sn*Can + Sn*Sa*Caa)*theta*Cs*Hcn
        K.mat[4,3] <- (Sa*Caa)*theta*Cs*Hcn
        
        K.mat[2,5] <- Pl*I*Nlh/Dl
        K.mat[3,5] <- Pn*I*Nnh/Dn
        K.mat[4,5] <- Pa*I*Nah/Da
        
        K.mat[5,1] <- (Sl*Ql + Sl*Sn*Qn + Sl*Sn*Sa*Qa)*Hcs
        K.mat[5,2] <- (Sn*Qn + Sn*Sa*Qa)*Hcs
        K.mat[5,3] <- (Sa*Qa)*Hcs
        
        R0 <- Re(eigen(K.mat)$values[1])    # estimate R0 as the largest eigen value of the next-generation of the matrix
        
        K.mat.vertical <- matrix(0, 5, 5)
        K.mat.vertical[1,1:4] <- K.mat[1, 1:4]
        R0.vertical <- Re(eigen(K.mat.vertical)$values[1])
        
        K.mat.novertical <- K.mat
        K.mat.novertical[1, 1:4] <- 0
        R0.novertical <- Re(eigen(K.mat.novertical)$values[1])
        
        E.mat <- matrix(0, 5, 5)
        for(ei in 1:5)
          for(ej in 1:5)
          {
            # estimate the elasticity of each element through a numerical way: perturb each element by a very small amount, then estimate the change in R0 and estimate elasticity
            K.mat.pert <- K.mat
            
            delta.kij <- K.mat[ei, ej]*0.001
            K.mat.pert[ei, ej] <- K.mat[ei, ej] + delta.kij
            
            delta.R0 <- Re(eigen(K.mat.pert)$values[1]) - R0
            
            E.mat[ei, ej] <- K.mat[ei, ej]/R0 * delta.R0/delta.kij
          }
        E.mat[is.na(E.mat)] <- 0
        
        results[i,] <- c(vertical = sum(E.mat[1, 1:4]),
                         nonsystemic = sum(E.mat[2:4, 1:3]),
                         systemic = sum(E.mat[5,1:3], E.mat[2:4,5]),
                         R0 = R0,
                         R0.vertical = R0.vertical,
                         R0.novertical = R0.novertical)
        
        # for sensitivity analysis by ij
        elas.mat.kij[i,] <- as.numeric(E.mat)
        detach(current.par)
        
        # for sensitivity analysis
        # perturb a parameter instead of a matrix element kij
        for(par.i in 1:31)
        {
          new.par <- current.par
          
          new.par[par.i] <- current.par[par.i] * 1.001
          
          attach(new.par)
          
          K.mat.new <- matrix(0, 5, 5)
          
          K.mat.new[1,1] <- Sl*Sn*Sa*E*Ra
          K.mat.new[1,2] <- Sn*Sa*E*Ra
          K.mat.new[1,3] <- Sa*E*Ra
          K.mat.new[1,4] <- E*Ra
          
          K.mat.new[2,1] <- (Sl*Cll + Sl*Sn*Cln + Sl*Sn*Sa*Cla)*theta*Cs*Hcn
          K.mat.new[2,2] <- (Sn*Cln + Sn*Sa*Cla)*theta*Cs*Hcn
          K.mat.new[2,3] <- (Sa*Cla)*theta*Cs*Hcn
          
          K.mat.new[3,1] <- (Sl*Cnl + Sl*Sn*Cnn + Sl*Sn*Sa*Cna)*theta*Cs*Hcn
          K.mat.new[3,2] <- (Sn*Cnn + Sn*Sa*Cna)*theta*Cs*Hcn
          K.mat.new[3,3] <- (Sa*Cna)*theta*Cs*Hcn
          
          K.mat.new[4,1] <- (Sl*Cal + Sl*Sn*Can + Sl*Sn*Sa*Caa)*theta*Cs*Hcn
          K.mat.new[4,2] <- (Sn*Can + Sn*Sa*Caa)*theta*Cs*Hcn
          K.mat.new[4,3] <- (Sa*Caa)*theta*Cs*Hcn
          
          K.mat.new[2,5] <- Pl*I*Nlh/Dl
          K.mat.new[3,5] <- Pn*I*Nnh/Dn
          K.mat.new[4,5] <- Pa*I*Nah/Da
          
          K.mat.new[5,1] <- (Sl*Ql + Sl*Sn*Qn + Sl*Sn*Sa*Qa)*Hcs
          K.mat.new[5,2] <- (Sn*Qn + Sn*Sa*Qa)*Hcs
          K.mat.new[5,3] <- (Sa*Qa)*Hcs
          
          R0.new <- Re(eigen(K.mat.new)$values[1])
          
          sens.mat[i, par.i] <- (R0.new - R0)/ as.numeric(current.par[par.i] * 0.001) 
          elas.mat[i, par.i] <- (R0.new - R0)/R0/0.001
          
          detach(new.par)
        }
        
      }
      
      # add the current scenario ID to the current results
      results <- results %>%
        as.data.frame() %>%
        mutate(I.sce = I.sce, N.sce = N.sce, K.sce = k.sce)
      
      sens.mat <- sens.mat  %>%
        as.data.frame() %>%
        mutate(I.sce = I.sce, N.sce = N.sce, K.sce = k.sce)
      
      elas.mat <- elas.mat %>%
        as.data.frame() %>%
        mutate(I.sce = I.sce, N.sce = N.sce, K.sce = k.sce)
      
      sens.mat.kij <- sens.mat.kij %>%
        as.data.frame() %>%
        mutate(I.sce = I.sce, N.sce = N.sce, K.sce = k.sce)
      
      elas.mat.kij <- elas.mat.kij %>%
        as.data.frame() %>%
        mutate(I.sce = I.sce, N.sce = N.sce, K.sce = k.sce)
      
      # attach the current results to all results
      results.all <- rbind(results.all, results)
      sens.mat.all <- rbind(sens.mat.all, sens.mat)
      elas.mat.all <- rbind(elas.mat.all, elas.mat)
      sens.mat.kij.all <- rbind(sens.mat.kij.all, sens.mat.kij)
      elas.mat.kij.all <- rbind(elas.mat.kij.all, elas.mat.kij)
    }
}

write.csv(results.all, "Results/Results_I_N_scenarios.csv", row.names = FALSE)
write.csv(sens.mat.all, "Results/sens_mat_I_N_scenarios.csv", row.names = FALSE)
write.csv(elas.mat.all, "Results/elas_mat_I_N_scenarios.csv", row.names = FALSE)
write.csv(sens.mat.kij.all, "Results/sens_mat_kij_I_N_scenarios.csv", row.names = FALSE)
write.csv(elas.mat.kij.all, "Results/elas_mat_kij_I_N_scenarios.csv", row.names = FALSE)
