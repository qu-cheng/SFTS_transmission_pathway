source("0_import_libraries.R")

n.size <- 1000  # sample size = 1000

#  sampling from LHS
unscaled <- randomLHS(n.size, 31)
colnames(unscaled) <- c("E", "Sl", "Sn", "Sa",
                        "Cll", "Cnl", "Cal",
                        "Cln", "Cnn", "Can",
                        "Cla", "Cna", "Caa",
                        "Cs",
                        "Nlh", "Nnh", "Nah",
                        "Dl","Dn", "Da",
                        "Hcn", "Hcs",
                        "I", "theta", "Pl", "Pn", "Pa", "Ql", "Qn", "Qa", "Ra")


# use the LHS sample as the cumulative density to convert the uniform-distributed LHS samples to predefined distributions
scaled <- unscaled


#================= parameters that DO NOT VARY with scenarios ================
scaled[, "E"] <- qtruncnorm(unscaled[,"E"], 0, Inf, 1400, 612)

scaled[, "Sl"] <- qlnorm(unscaled[,"Sl"], meanlog = -3.1, sdlog = 0.4)
scaled[, "Sn"] <- qlnorm(unscaled[,"Sn"], meanlog = -2.4, sdlog = 0.4)
scaled[, "Sa"] <- qlnorm(unscaled[,"Sa"], meanlog = -2.4, sdlog = 0.4)

scaled[, "Cs"] <- 0.1 + unscaled[, "Cs"]*0.4

scaled[, "Dl"] <- qtruncnorm(unscaled[,"Dl"], 0, Inf, 4.1, 0.5)
scaled[, "Dn"] <- qtruncnorm(unscaled[,"Dn"], 0, Inf, 3.6, 0.9)
scaled[, "Da"] <- qtruncnorm(unscaled[,"Da"], 0, Inf, 5.7, 1)

scaled[, "Hcn"] <- 0.1 + unscaled[, "Hcn"]*0.8
scaled[, "Hcs"] <- 0.1 + unscaled[, "Hcs"]*0.8

scaled[, "theta"] <- 0.9347151 + unscaled[, "theta"]*0.0652849

scaled[, "Pl"] <- qtruncnorm(unscaled[,"Pl"], 0, Inf, 14/120, 0.03)
scaled[, "Pn"] <- qtruncnorm(unscaled[,"Pn"], 0, Inf, 12/30, 0.0894)  
scaled[, "Pa"] <- qtruncnorm(unscaled[,"Pa"], 0, Inf, 6/13, 0.1382642)

scaled[, "Ql"] <- qlnormTrunc(unscaled[,"Ql"], meanlog = -1.478410, sdlog = 0.77, min = 0, max = 1)   # assumed to be the same as Qn
scaled[, "Qn"] <- qlnormTrunc(unscaled[,"Qn"], meanlog = -1.478410, sdlog = 0.77, min = 0, max = 1)
scaled[, "Qa"] <- qlnormTrunc(unscaled[,"Qa"], meanlog = -0.6753073, sdlog = 0.41, min = 0, max = 1)

scaled[, "Ra"] <- 0.0343 + unscaled[, "Ra"]*0.9657








#================= parameters that VARY with scenarios ================
scaled <- as.data.frame(scaled)
scaled$I1 <- scaled$I2 <- scaled$I3 <- NA

scaled[, "I1"] <- 1 + scaled[, "I"]*4   # Short, 1-5 days
scaled[, "I2"] <- 5 + scaled[, "I"]*4   # Medium, 5-9 days
scaled[, "I3"] <- 9 + scaled[, "I"]*3   # Long, 9-12 days

# For the Ns and Cs
scaled$Nlh1 <- scaled$Nlh2 <- scaled$Nlh3 <- NA   # Low, total abundance of 0.01~ 2, Liaoning
scaled$Nnh1 <- scaled$Nnh2 <- scaled$Nnh3 <- NA   # Medium, total abundance of 2~ 5, Shandong
scaled$Nah1 <- scaled$Nah2 <- scaled$Nah3 <- NA   # High, total abundance of 5~12, Hubei

# the ratio of L, N, and A is c(0.785, 0.168, 0.047)
scaled[, "Nlh1"] <- (0.01 + (2-0.01) * scaled[, "Nlh"])*0.785   
scaled[, "Nnh1"] <- (0.01 + (2-0.01) * scaled[, "Nnh"])*0.168  
scaled[, "Nah1"] <- (0.01 + (2-0.01) * scaled[, "Nah"])*0.047   

scaled[, "Nlh2"] <- (2 + 3 * scaled[, "Nlh"])*0.785   
scaled[, "Nnh2"] <- (2 + 3 * scaled[, "Nnh"])*0.168   
scaled[, "Nah2"] <- (2 + 3 * scaled[, "Nah"])*0.047  

scaled[, "Nlh3"] <- (5 + 7 * scaled[, "Nlh"])*0.785   
scaled[, "Nnh3"] <- (5 + 7 * scaled[, "Nnh"])*0.168    
scaled[, "Nah3"] <- (5 + 7 * scaled[, "Nah"])*0.047  

scaled$Cll1 <- scaled$Cll2 <- scaled$Cll3 <- NA
scaled$Cnl1 <- scaled$Cnl2 <- scaled$Cnl3 <- NA
scaled$Cal1 <- scaled$Cal2 <- scaled$Cal3 <- NA

scaled$Cln1 <- scaled$Cln2 <- scaled$Cln3 <- NA
scaled$Cnn1 <- scaled$Cnn2 <- scaled$Cnn3 <- NA
scaled$Can1 <- scaled$Can2 <- scaled$Can3 <- NA

scaled$Cla1 <- scaled$Cla2 <- scaled$Cla3 <- NA
scaled$Cna1 <- scaled$Cna2 <- scaled$Cna3 <- NA
scaled$Caa1 <- scaled$Caa2 <- scaled$Caa3 <- NA

# to get the Cs under different scenarios
for(k in c(0.05, 0.15, 0.5))
{
  for(i in 1:nrow(scaled))
  {
      print(i)
      # scenario: low tick burden
      current.dat <- data.frame(L = rnbinom(100000, mu =  scaled[i, "Nlh1"], size = k),
                                N = rnbinom(100000, mu =  scaled[i, "Nnh1"], size = k),
                                A = rnbinom(100000, mu =  scaled[i, "Nah1"], size = k)) %>%
        mutate(LL = ifelse(L == 0, NA, L-1),
               NL = ifelse(L == 0, NA, N/L),
               AL = ifelse(L == 0, NA, A/L),
               LN = ifelse(N == 0, NA, L/N),
               NN = ifelse(N == 0, NA, N-1),
               AN = ifelse(N == 0, NA, A/N),
               LA = ifelse(A == 0, NA, L/A),
               nA = ifelse(A == 0, NA, N/A),
               AA = ifelse(A == 0, NA, A-1))

      Cofeedings <- colMeans(current.dat, na.rm = TRUE)
      scaled[i, c("Cll1", "Cnl1", "Cal1", "Cln1", "Cnn1", "Can1", "Cla1", "Cna1", "Caa1")] <- Cofeedings[4:12]

      # scenario 2
      current.dat <- data.frame(L = rnbinom(100000, mu =  scaled[i, "Nlh2"], size = k),
                                N = rnbinom(100000, mu =  scaled[i, "Nnh2"], size = k),
                                A = rnbinom(100000, mu =  scaled[i, "Nah2"], size = k)) %>%
        mutate(LL = ifelse(L == 0, NA, L-1),
               NL = ifelse(L == 0, NA, N/L),
               AL = ifelse(L == 0, NA, A/L),
               LN = ifelse(N == 0, NA, L/N),
               NN = ifelse(N == 0, NA, N-1),
               AN = ifelse(N == 0, NA, A/N),
               LA = ifelse(A == 0, NA, L/A),
               nA = ifelse(A == 0, NA, N/A),
               AA = ifelse(A == 0, NA, A-1))

      Cofeedings <- colMeans(current.dat, na.rm = TRUE)
      scaled[i, c("Cll2", "Cnl2", "Cal2", "Cln2", "Cnn2", "Can2", "Cla2", "Cna2", "Caa2")] <- Cofeedings[4:12]

      # scenario 3
      current.dat <- data.frame(L = rnbinom(100000, mu =  scaled[i, "Nlh3"], size = k),
                                N = rnbinom(100000, mu =  scaled[i, "Nnh3"], size = k),
                                A = rnbinom(100000, mu =  scaled[i, "Nah3"], size = k)) %>%
        mutate(LL = ifelse(L == 0, NA, L-1),
               NL = ifelse(L == 0, NA, N/L),
               AL = ifelse(L == 0, NA, A/L),
               LN = ifelse(N == 0, NA, L/N),
               NN = ifelse(N == 0, NA, N-1),
               AN = ifelse(N == 0, NA, A/N),
               LA = ifelse(A == 0, NA, L/A),
               nA = ifelse(A == 0, NA, N/A),
               AA = ifelse(A == 0, NA, A-1))

      Cofeedings <- colMeans(current.dat, na.rm = TRUE)
      scaled[i, c("Cll3", "Cnl3", "Cal3", "Cln3", "Cnn3", "Can3", "Cla3", "Cna3", "Caa3")] <- Cofeedings[4:12]
  }
  
    write.csv(scaled, paste("Parameters/LHS_pars_scaled_k", k, ".csv", sep = ""), row.names = FALSE)
}