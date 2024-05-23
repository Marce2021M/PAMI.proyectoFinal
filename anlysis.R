# Ajustar las distribuciones utilizando optimización
ajustar_distribucion <- function(logverosimilitud, start_params) {
  fit <- optim(start_params, logverosimilitud, method = "SANN")
  return(fit)
}

# Ajustar las distribuciones
for(i in 1:1000){
(params_weibull <- optim(fn = logverosimilitud_weibull,
method = "L-BFGS-B", lower = c(0, 0), upper = c(10, 10), par = c(1000, 1000))
}

(params_gamma <- optim(fn = logverosimilitud_gamma, 
method = "SANN", lower = c(0, 0), upper = c(10, 10), par = c(1, 1)))
(params_lognormal <- optim(fn = logverosimilitud_lognormal,
method = "SANN", lower = c(0, 0), upper = c(10, 10), par = c(1, 1)))
(params_pareto <- optim(fn = logverosimilitud_pareto,
method = "SANN", lower = c(0, 0), upper = c(10, 10), par = c(1, 1)))


# Inicialización de la lista para guardar resultados
resultados <- vector("list", 1000)

# Bucle para probar diferentes puntos de inicio
for(i in 1:100000) {
  # Generar puntos de inicio aleatorios
  par_ini <- runif(2, min = 0, max = 1000)
  
  # Realizar la optimización
  # Intentar realizar la optimización y manejar errores
  optim_result <- try(optim(par = par_ini, fn = logverosimilitud_weibull,
                            method = "L-BFGS-B", lower = c(0, 0), upper = c(1000, 1000)), silent = TRUE)
  
  # Verificar si la optimización fue exitosa
  if (inherits(optim_result, "try-error")) {
    # Si hubo un error, continuar con la siguiente iteración
    next
  }
  # Guardar los resultados
  resultados[[i]] <- list(
    par_ini = par_ini,
    par_opt = optim_result$par,
    convergencia = optim_result$convergence,
    valor = optim_result$value
  )
}

# Convertir la lista de resultados a un data frame para su análisis
resultados_df <- do.call(rbind, lapply(resultados, as.data.frame))

# Mostrar los resultados
print(resultados_df)
