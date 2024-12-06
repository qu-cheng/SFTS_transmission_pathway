library(lhs)
library(tidyverse)
library(Ternary)
library(cowplot)
library(ranger)
library(ggsci)
library(ggtern)
library(randomForest)
library(ranger)
library(truncnorm)
library(EnvStats)
library(tuneRanger)
library(pdp)
library(paletteer)

set.seed(20240710)

k.value <- c(0.05, 0.15, 0.5)
n.par <- 31   # 31 parameters
par.names <-  c("E", "Sl", "Sn", "Sa",
                "Cll", "Cnl", "Cal",
                "Cln", "Cnn", "Can",
                "Cla", "Cna", "Caa",
                "Cs",
                "Nlh", "Nnh", "Nah",
                "Dl","Dn", "Da",
                "Hcn", "Hcs",
                "I", "theta", "Pl", "Pn", "Pa", "Ql", "Qn", "Qa", "Ra")

lab <- c("Sl" = expression(S[l]),
         "Sn" = expression(S[n]),
         "Sa" = expression(S[a]),
         "Cll" = expression(C[ll]),
         "Cnl" = expression(C[nl]),
         "Cal" = expression(C[al]),
         "Cln" = expression(C[ln]),
         "Cnn" = expression(C[nn]),
         "Can" = expression(C[an]),
         "Cla" = expression(C[la]),
         "Cna" = expression(C[na]),
         "Caa" = expression(C[aa]),
         "Nlh" = expression(N[lh]),
         "Nnh" = expression(N[nh]),
         "Nah" = expression(N[ah]),
         "Dl" = expression(D[l]),
         "Dn" = expression(D[n]),
         "Da" = expression(D[a]),
         "Hcn" = expression(H[cn]),
         "Hcs" = expression(H[cs]),
         "theta" = expression(theta),
         "Pl" = expression(P[l]),
         "Pn" = expression(P[n]),
         "Pa" = expression(P[a]),
         "Ql" = expression(Q[l]),
         "Qn" = expression(Q[n]),
         "Qa" = expression(Q[a]),
         "Ra" = expression(R[a]),
         "Cs" = expression(C[s])
)


make_labelstring <- function(mypanels) {
  mylabels <- sapply(mypanels, 
                     function(x) paste({LETTERS[which(mypanels == x)]}, "*')'~", x, sep = ""))
  
  return(mylabels)
}




make_labelstring_eij <- function(mypanels) {
  mylabels <- sapply(mypanels, 
                     function(x) paste({LETTERS[which(mypanels == x)]}, ") ", x, sep = ""))
  
  return(mylabels)
}
