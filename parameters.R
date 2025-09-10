#parameters
make_par = function(scenario){
    if (scenario=='1'){  #cure fraction 0.2
    #incidence
    alpha = c(2, -1, 0, 1, 0)
    #latency
    beta = c(1, 0, -1, 0)
    lambdaC = 0.05
    M_fixed = 10
  }
  
  
  if (scenario=='2'){  #cure fraction 0.2
    #incidence
    alpha = c(2, -1, 0, 1, 0)
    #latency
    beta = c(1, 0, -1, 0)
    lambdaC = 0.05
    M_fixed = 10
  }
  
  
  if (scenario=='3'){  #cure fraction 0.4
    #incidence
    alpha = c(0.65, -1, 0, 1, 0)
    #latency
    beta = c(1, 0, -1, 0)
    lambdaC = 0.07
    M_fixed = 10
  }
  
  
  if (scenario=='4'){  #cure fraction 0.4
    #incidence
    alpha = c(0.65, -1, 0, 1, 0)
    #latency
    beta = c(1, 0, -1, 0)
    lambdaC = 0.07
    M_fixed = 10
  }
  
  
  if (scenario=='5'){  #cure fraction 0.2
    #incidence
    alpha = c(2, -1, 0, 1, 0)
    #latency
    beta = c(1, 0, -1, 0)
    lambdaC = 0.05
    M_fixed = 10
  }
  
  
  if (scenario=='6'){  #cure fraction 0.2
    #incidence
    alpha = c(2, -1, 0, 1, 0)
    #latency
    beta = c(1, 0, -1, 0)
    lambdaC = 0.05
    M_fixed = 10
  }
  
  
  if (scenario=='7'){  #cure fraction 0.4
    #incidence
    alpha = c(0.65, -1, 0, 1, 0)
    #latency
    beta = c(1, 0, -1, 0)
    lambdaC = 0.07
    M_fixed = 10
  }
  
  
  if (scenario=='8'){  #cure fraction 0.4
    #incidence
    alpha = c(0.65, -1, 0, 1, 0)
    #latency
    beta = c(1, 0, -1, 0)
    lambdaC = 0.07
    M_fixed = 10
  }


  if (scenario=='9'){  #cure fraction 0.2
    #incidence
    alpha = c(2, -1, 0, 1, 0)
    #latency
    beta = c(1, 0, -1, 0)
    lambdaC = 0.05
    M_fixed = 10
  }
  
  
  if (scenario=='10'){  #cure fraction 0.2
    #incidence
    alpha = c(2, -1, 0, 1, 0)
    #latency
    beta = c(1, 0, -1, 0)
    lambdaC = 0.05
    M_fixed = 10
  }
  
  
  if (scenario=='11'){  #cure fraction 0.4
    #incidence
    alpha = c(0.65, -1, 0, 1, 0)
    #latency
    beta = c(1, 0, -1, 0)
    lambdaC = 0.07
    M_fixed = 10
  }
  
  
  if (scenario=='12'){  #cure fraction 0.4
    #incidence
    alpha = c(0.65, -1, 0, 1, 0)
    #latency
    beta = c(1, 0, -1, 0)
    lambdaC = 0.07
    M_fixed = 10
  }
  
  return(list(alpha = alpha, beta = beta, lambdaC = lambdaC, M_fixed = M_fixed))
}
