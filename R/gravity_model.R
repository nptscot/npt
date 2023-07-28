gravity_model = function(beta, d, m, n) {
  m * n * exp(-beta * d / 1000)
} 