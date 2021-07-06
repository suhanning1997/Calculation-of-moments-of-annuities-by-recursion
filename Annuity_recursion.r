mu_1 <- (1/1.05)*0.15 + (1/1.06)*0.6 + (1/1.07)*0.25
mu_2 <- (1/1.05^2)*0.15 + (1/1.06^2)*0.6 + (1/1.07^2)*0.25
mu_3 <- (1/1.05^3)*0.15 + (1/1.06^3)*0.6 + (1/1.07^3)*0.25

moment_calculator <- function (n, r) {
  if (r == 1) 
  {
    if (n == 1) {
      return( mu_1 )
    } else {
      return (mu_1 * (1 + moment_calculator(n - 1, r)))
    }
  }
  else if (r == 2)
  {
    if (n == 1) {
      return( mu_2 )
    } else {
      return (mu_2 * (1 + 2 * moment_calculator(n - 1, 1) + moment_calculator(n - 1, r)))
    }
  }
  else if (r == 3)
  {
    if (n == 1) {
      return( mu_3 )
    } else {
      return (mu_3 * (1 + 3 * moment_calculator(n - 1, 1) + 3 * moment_calculator(n - 1, 2) + moment_calculator(n - 1, r)))
    }
  }
  else 
  {
    print("higher moment functionality not yet available.")
  }
}

df <- data.frame(n = c(1:20),
                 First_Moment = c(rep(NA, 20)),
                 Second_Moment = c(rep(NA, 20)),
                 Third_Moment = c(rep(NA, 20)))

for (r in 2:4) {
  for (n in 1:20){
    df[[r]][n] = moment_calculator(n, r - 1)
  }
}


library(stargazer)
stargazer(df, summary=FALSE)
                 