#############################################################
#										#
#	MODELO DE COMPETENCIA ENTRE ESPECIES EN ISLAS		#
#										#
#	Rodrigo Rodríguez Granjel, Pau Colom Montojo		#
#										#
#############################################################



##### ENUNCIADO DEL MODELO #####

# En una isla (sistema cerrado) cohabitan dos especies de plantas arbustivas con ciclo de vida anual y productoras de frutos carnosos.
# La temperatura anual, en términos de máximas y mínimas absolutas, es un factor clave para la producción de frutos. En el 100% de su productividad,
# la especie 1 produce 20 frutos en un rango de temperatura de 15ºC a 28ºC. Mientras, la especie 2 produce igualmente 20 frutos, pero en un rango de
# 8ºC a 28ºC. Cuando la temperatura máxima absoluta es mayor que el rango óptimo de cada especie, se produce una disminución de 10% en la producción
# de frutos por cada grado de aumento. Cuando la temperatura mínima es menor que el rango óptimo de cada especie, se produce una disminución de 5% en
# la producción de frutos por cada grado de disminución.
# La isla tiene un tamaño de 100 ha, cada planta necesita una superficie de 1 m2 (parcela) y por lo tanto puede albergar un máximo de 1 millón de arbustos.
# La forma normal de dispersión de los frutos de ambas especies es de tipo barócora, solamente actuando la gravedad, y por tanto los frutos solamente
# pueden alcanzar la propia parcela (en el 50% de los casos) o cualquiera de las parcelas colindantes (en el otro 50%). Sin embargo, esta isla puede
# albergar una población de un pájaro frugívoro que se alimenta de los frutos de ambas especies de arbustos, indistintamente, y actúa como agente dispersor
# a más larga distancia. Cuando el pájaro se alimenta ingiere una cantidad variable de frutos del mismo arbusto que oscila entre 1 y 10, y a continuación
# defeca sus semillas en una parcela situada entre 0 (hay la posibilidad de que no se mueva del arbusto y defeque en el mismo sitio de alimentación) y
# 25 parcelas de distancia (1/4 de la longitud total de la isla), con igual probabilidad para todas ellas.
#Tras todos los eventos de dispersión, en cada parcela hay tres posibilidades:
# - que no crezca nada –: 100% si no hay semillas en la parcela, 20% con una semilla y una reducción del 1% con cada semilla a mayores, hasta probabilidad
#   0 de que no haya nada con la presencia de 20 semillas, independientemente de la especie;
# - que crezca la especie 1 –: en función del número de semillas presentes; a igual número de semillas, la especie 1 tiene un 5% más de probabilidades de
#   establecerse que la especie 2;
# - que crezca la especie 2 –: en función del número de semillas presentes.
# Así, la especie arbustiva número 1 es mejor competidora a igualdad de condiciones, mientras que la especie 2 es más robusta resistiendo rangos más
# amplios de temperatura (resiste mejor el frío).
# Lo que se pretende con este modelo es ver cómo varía la población de dos plantas que compiten entre ellas por el espacio y que están limitadas por
# la temperatura, en distintos supuestos (temperaturas bajas –de 0 a 8ºC las mínimas y de 16 a 24ºC las máximas–, templadas –de 10 a 16ºC las mínimas
# y de 24 a 30ºC las máximas–, y altas – de 16 a 24ºC las mínimas y de 29 a 37ºC las máximas–, y cada una en presencia o ausencia de la población de
# pájaros frugívoros).




### ¡INFORMACIÓN E INSTRUCCIONES!
# ------ La parte A consiste en el modelo completo, sobre el cual se puede observar cómo funciona el programa
# ------ La parte B consiste en la obtención de datos: NO SE RECOMIENDA EJECUTAR, tarda alrededor de 24 horas en finalizar.
# ------ La parte C consiste en estadística descriptiva realizada sobre los datos obtenidos, ejecutable.
# ------ La parte D consiste en comentarios adicionales y bibliografía relacionada con la temática del programa.





##########################								####################################################################################
##########################  PARTE A : MODELO COMPLETO				####################################################################################
##########################								####################################################################################


## VARIABLES A MODIFICAR

## en general

tmax <- 100				# tiempo de simulación

## específicamente

# rangos de temperatura anuales
lower_mintemp <- 10		# temperatura mínima anual más baja posible
higher_mintemp <- 16		# temperatura mínima anual más alta posible
lower_maxtemp <- 24		# temperatura máxima anual más baja posible
higher_maxtemp <- 30		# temperatura máxima anual más alta posible

# tamaño de la isla (dimensiones de la matriz)
xdim <- 100				# dimensión x
ydim <- 100				# dimensión y

# producción de bayas
max_berries <- 20			# máxima producción de bayas por individuo
dcold <- 1				# descenso en la producción de bayas por cada grado que descienda la Tª por debajo del rango mínimo de la espcie
dheat <- 2				# aumento en la producción de bayas por cada grado que ascienda la Tª por encima del rango máximo de la especie

# resistencia de las especies a la temperatura (rangos normales)
rangemax_Sp1 <- 28		# límite superior del rango normal de Tª (especie 1)
rangemin_Sp1 <- 15		# límite inferior del rango normal de Tª (especie 1)
rangemax_Sp2 <- 28		# límite superior del rango normal de Tª (especie 2)
rangemin_Sp2 <- 8			# límite inferior del rango normal de Tª (especie 2)

# probabilidad de que un fruto caiga en su propia parcela y no en las vecinas; de 0 (nunca) a 1 (siempre)
p_prop <- 5/10

# probabilidad de actuación del pájaro; de 0 (nunca actúa) a 1 (siempre actúa)
p_bird <- 1/20


## INICIALIZACIÓN

# para comenzar, se define una isla con las dos especies distribuidas de forma aleatoria
library(lattice)			# páquete gráfico lattice - permite: levelplot()
library(graphics)			# permite legend()
ISLA <- matrix(0, nrow=xdim, ncol=ydim)
for (i in 1:(round(2*xdim/5))){
	for (j in 1:ydim){
		ISLA[i,j] <- 1
	}
}
for (i in ((round(3*xdim/5))):xdim){
	for (j in 1:ydim){
		ISLA[i,j] <- 2
	}
}

## FUNCIONES NECESARIAS

##-1-## Producción de cada especie el año i

Production_Sp <- function(max_berries, dcold, dheat, rangemax_Sp, rangemin_Sp, temp_min, temp_max){
	if(temp_max < rangemax_Sp){
		heatloss_Sp <- 0		# evita pérdida de producción de frutos cuando la temp. máx. del año está dentro del rango de la especie
	} else {
		heatloss_Sp <- dheat * (temp_max - rangemax_Sp)		# descenso causado por el calor (cuando proceda)
	}
	if(temp_min > rangemin_Sp){
		coldloss_Sp <- 0		# evita pérdida de producción de frutos cuando la temp. mín. del año está dentro del rango de la especie
	} else {
		coldloss_Sp <- dcold * (rangemin_Sp - temp_min)		# descenso causado por el frío (cuando proceda)
	}
	fruits_Sp <- round(max_berries - heatloss_Sp - coldloss_Sp)		# fórmula para calcular la producción anual de frutos
	if (fruits_Sp < 0){
		fruits_Sp <- 0		# evitar que la producción sea negativa
	}
	return (fruits_Sp)
} # fin de Production_Sp()


##-2-## Separación de las especies en dos submatrices

Submatrix_Sp <- function(Sp, ISLA, P_Sp1, P_Sp2, xdim, ydim){
	if (Sp == "Sp1"){		# si se introduce "Sp1" en la función
		SP1 <- matrix(0, nrow=xdim, ncol=ydim)		# genera una submatriz para la Sp1
		for (a in 1:xdim){
			for (b in 1:ydim){
				if (ISLA[a, b] == 1){
					SP1[a, b] <- P_Sp1		# donde está la Sp1 (valor = 1) asigna P_Sp1
				} else {
					SP1[a, b] <- 0
				}
			}
		}
		return(SP1)
	}
	if (Sp == "Sp2"){		# si se introduce "Sp2" en la función
		SP2 <- matrix(0, nrow=xdim, ncol=ydim)		# genera una submatriz para la Sp2
		for (a in 1:xdim){
			for (b in 1:ydim){
				if (ISLA[a, b] == 2){
					SP2[a, b] <- P_Sp2		# donde está la Sp2 (valor = 2) asigna P_Sp2
				} else {
					SP2[a, b] <- 0
				}
			}
		}
		return(SP2)
	}
} # fin de Submatrix_Sp()


##-3-## Repartición de las bayas por las diferentes parcelas

Spread <- function(X, p_prop, xdim, ydim){
	Y <- matrix(0, nrow=xdim, ncol=ydim)
	p_peri <- (1-p_prop)/8
	# Posiciones centrales
	for (i in 2:(ydim-1)){
		for (j in 2:(xdim-1)){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							Y[i-1, j-1] <- Y[i-1, j-1] + 1
						} else {
							if (azar == 2){
								Y[i, j-1] <- Y[i, j-1] + 1
							} else {
								if (azar == 3){
									Y[i+1, j-1] <- Y[i+1, j-1] + 1
								} else {
									if (azar == 4){
										Y[i-1, j] <- Y[i-1, j] + 1
									} else {
										if (azar == 6){
											Y[i+1, j] <- Y[i+1, j] + 1
										} else {
											if (azar == 7){
												Y[i-1, j+1] <- Y[i-1, j+1] + 1
											} else {
												if (azar == 8){
													Y[i, j+1] <- Y[i, j+1] + 1
												} else {
													if (azar == 9){
														Y[i+1, j+1] <- Y[i+1, j+1] + 1
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	# Margen superior
	for (i in 1:1){
		for (j in 2:(xdim-1)){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							# agua
						} else {
							if (azar == 2){
								Y[i, j-1] <- Y[i, j-1] + 1
							} else {
								if (azar == 3){
									Y[i+1, j-1] <- Y[i+1, j-1] + 1
								} else {
									if (azar == 4){
										# agua
									} else {
										if (azar == 6){
											Y[i+1, j] <- Y[i+1, j] + 1
										} else {
											if (azar == 7){
												# agua
											} else {
												if (azar == 8){
													Y[i, j+1] <- Y[i, j+1] + 1
												} else {
													if (azar == 9){
														Y[i+1, j+1] <- Y[i+1, j+1] + 1
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	# Margen inferior
	for (i in ydim:ydim){
		for (j in 2:(xdim-1)){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							Y[i-1, j-1] <- Y[i-1, j-1] + 1
						} else {
							if (azar == 2){
								Y[i, j-1] <- Y[i, j-1] + 1
							} else {
								if (azar == 3){
									# agua
								} else {
									if (azar == 4){
										Y[i-1, j] <- Y[i-1, j] + 1
									} else {
										if (azar == 6){
											# agua
										} else {
											if (azar == 7){
												Y[i-1, j+1] <- Y[i-1, j+1] + 1
											} else {
												if (azar == 8){
													Y[i, j+1] <- Y[i, j+1] + 1
												} else {
													if (azar == 9){
														# agua
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	# Margen izquierdo
	for (i in 2:(ydim-1)){
		for (j in 1:1){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							# agua
						} else {
							if (azar == 2){
								# agua
							} else {
								if (azar == 3){
									# agua
								} else {
									if (azar == 4){
										Y[i-1, j] <- Y[i-1, j] + 1
									} else {
										if (azar == 6){
											Y[i+1, j] <- Y[i+1, j] + 1
										} else {
											if (azar == 7){
												Y[i-1, j+1] <- Y[i-1, j+1] + 1
											} else {
												if (azar == 8){
													Y[i, j+1] <- Y[i, j+1] + 1
												} else {
													if (azar == 9){
														Y[i+1, j+1] <- Y[i+1, j+1] + 1
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	# Margen derecho
	for (i in 2:(ydim-1)){
		for (j in xdim:xdim){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							Y[i-1, j-1] <- Y[i-1, j-1] + 1
						} else {
							if (azar == 2){
								Y[i, j-1] <- Y[i, j-1] + 1
							} else {
								if (azar == 3){
									Y[i+1, j-1] <- Y[i+1, j-1] + 1
								} else {
									if (azar == 4){
										Y[i-1, j] <- Y[i-1, j] + 1
									} else {
										if (azar == 6){
											Y[i+1, j] <- Y[i+1, j] + 1
										} else {
											if (azar == 7){
												# agua
											} else {
												if (azar == 8){
													# agua
												} else {
													if (azar == 9){
														# agua
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	# Esquina izquierda superior
	for (i in 1:1){
		for (j in 1:1){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							# agua
						} else {
							if (azar == 2){
								# agua
							} else {
								if (azar == 3){
									# agua
								} else {
									if (azar == 4){
										# agua
									} else {
										if (azar == 6){
											Y[i+1, j] <- Y[i+1, j] + 1
										} else {
											if (azar == 7){
												# agua
											} else {
												if (azar == 8){
													Y[i, j+1] <- Y[i, j+1] + 1
												} else {
													if (azar == 9){
														Y[i+1, j+1] <- Y[i+1, j+1] + 1
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}

	# Esquina izquierda inferior
	for (i in ydim:ydim){
		for (j in 1:1){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							# agua
						} else {
							if (azar == 2){
								# agua
							} else {
								if (azar == 3){
									# agua
								} else {
									if (azar == 4){
										Y[i-1, j] <- Y[i-1, j] + 1
									} else {
										if (azar == 6){
											# agua
										} else {
											if (azar == 7){
												Y[i-1, j+1] <- Y[i-1, j+1] + 1
											} else {
												if (azar == 8){
													Y[i, j+1] <- Y[i, j+1] + 1
												} else {
													if (azar == 9){
														# agua
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	# Esquina derecha superior
	for (i in 1:1){
		for (j in xdim:xdim){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							# agua
						} else {
							if (azar == 2){
								Y[i, j-1] <- Y[i, j-1] + 1
							} else {
								if (azar == 3){
									Y[i+1, j-1] <- Y[i+1, j-1] + 1
								} else {
									if (azar == 4){
										# agua
									} else {
										if (azar == 6){
											Y[i+1, j] <- Y[i+1, j] + 1
										} else {
											if (azar == 7){
												# agua
											} else {
												if (azar == 8){
													# agua
												} else {
													if (azar == 9){
														# agua
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}

	# Esquina derecha inferior
	for (i in ydim:ydim){
		for (j in xdim:xdim){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							Y[i-1, j-1] <- Y[i-1, j-1] + 1
						} else {
							if (azar == 2){
								Y[i, j-1] <- Y[i, j-1] + 1
							} else {
								if (azar == 3){
									# agua
								} else {
									if (azar == 4){
										Y[i-1, j] <- Y[i-1, j] + 1
									} else {
										if (azar == 6){
											# agua
										} else {
											if (azar == 7){
												# agua
											} else {
												if (azar == 8){
													# agua
												} else {
													if (azar == 9){
														# agua
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	return(Y)
} #fin de Spread()


##-4-## Efecto de dispersión debida a pájaros en la isla

Birds <- function(X, p_bird, xdim, ydim){

	for (i in 1:ydim){
		for (j in 1:xdim){
			if (X[i, j] > 0){		# si la celda tiene frutos...

				# aleatorio con prob. 1/40 de que el pájaro se alimente en esa celda
				paj_si <- rbinom(1, 1, p=p_bird)

				if (paj_si == 1){
					# el pájaro puede comer un máximo de 10 frutos
					if (X[i, j] <= 10){
						paj_frutos <- round(runif(1, 1, X[i, j]))
						} else {
							paj_frutos <- round(runif(1, min=1, max=10))
						}

					# celda actual menos los frutos que se comió el pájaro
					X[i, j] <- X[i, j] - paj_frutos

					# se genera aleatoriamente una coordenada i para dejar los frutos (cuidándose de evitar caer fuera de la isla)
					if (i < (ydim/4)){
						ni_aleat <- round(runif(1, -i, +i))
					} else {
						if ((ydim-i) < (ydim/4)){
							ni_aleat <- round(runif(1, -(ydim-i), +(ydim-i)))
						} else {
							ni_aleat <- round(runif(1, -(ydim/4), +(ydim/4)))
						}
					}

					# se genera aleatoriamente una coordenada j para dejar los frutos (cuidándose de evitar caer fuera de la isla)
					if (j < (xdim/4)){
						nj_aleat <- round(runif(1, -j, +j))
					} else {
						if ((xdim-j) < (xdim/4)){
							nj_aleat <- round(runif(1, -(xdim-j), +(xdim-j)))
						} else {
							nj_aleat <- round(runif(1, -(xdim/4), +(xdim/4)))
						}
					}
					# los pájaros dispersan un máximo de 1/4 de la dimensión de la isla

					# se depositan los frutos en la celda seleccionada aleatoriamente
					X[i + ni_aleat, j + nj_aleat] <- X[i + ni_aleat, j + nj_aleat] + paj_frutos
				}
			}
		} # fin de j
	} # fin de i
	return (X)
} # fin de Birds()


## CUERPO DEL PROGRAMA

# primer plot con la isla inicial
levelplot(ISLA, xlab = "", ylab = "", main="Isla", col.regions= c("white", "white", "white", "white", "white", "white", "white", "green", "green", "green", "green", "green", "green", "green", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))

N_Sp1 <- rep(0, tmax)		# vector para guardar la población de la Sp1 a lo largo del tiempo
N_Sp2 <- rep(0, tmax)		# vector para guardar la población de la Sp2 a lo largo del tiempo

for (i in 1:tmax){		# bucle de duración tmax; 1 paso del tiempo representa 1 año
	# temperatura mínima y máxima anual aleatorias dentro del rango anteriormente definido
	temp_min <- round(runif(1, lower_mintemp, higher_mintemp))
	temp_max <- round(runif(1, lower_maxtemp, higher_maxtemp))
	
	# producción anual de cada especie
	P_Sp1 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp1, rangemin_Sp1, temp_min, temp_max)
	P_Sp2 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp2, rangemin_Sp2, temp_min, temp_max)

	# separación en dos submatrices
	SP1 <- Submatrix_Sp("Sp1", ISLA, P_Sp1, P_Sp2, xdim, ydim)
	SP2 <- Submatrix_Sp("Sp2", ISLA, P_Sp1, P_Sp2, xdim, ydim)
	
	# repartición de los frutos
	SpreadSP1_sin_paj <- Spread(SP1, p_prop, xdim, ydim)
	SpreadSP2_sin_paj <- Spread(SP2, p_prop, xdim, ydim)

	# efecto de los pájaros
	SpreadSP1 <- Birds(SpreadSP1_sin_paj, p_bird, xdim, ydim)
	SpreadSP2 <- Birds(SpreadSP2_sin_paj, p_bird, xdim, ydim)
	Sum_SEEDS <- SpreadSP1 + SpreadSP2
	
	# cálculo de la nueva matriz poblacional
	ISLA <- matrix(0, nrow=xdim, ncol=ydim)
	aux_Sp1 <- 0	# variable de apoyo para contar el número de individuos de la especie 1
	aux_Sp2 <- 0	# variable de apoyo para contar el número de individuos de la especie 2

	for (a in 1:xdim){
		for (b in 1:ydim){
			# cuando no hay semillas de ninguna planta, no se establece nada
			if (Sum_SEEDS[a, b] == 0){
				prob_nothing <- 1		# probabilidad de no establecimiento
				prob_Sp1 <- 0		# probabilidad de establecimiento de la especie 1
				prob_Sp2 <- 0		# probabilidad de establecimiento de la especie 2
			} else {
				# cuando hay una semilla (independientemente de la especie), la probabilidad de nada es 0.2
				# y la probabilidad de dicha especie es 0.8
				if (Sum_SEEDS[a, b] == 1){
					prob_nothing <- 0.2
					if (SpreadSP1[a, b] == 1 && SpreadSP2[a, b] == 0){
						prob_Sp1 <- 1 - prob_nothing
						prob_Sp2 <- 0
					} else {
						if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] == 1){
							prob_Sp1 <- 0
							prob_Sp2 <- 1 - prob_nothing
						}
					}
				} else {
					# cuando hay más de una semilla:
					if (Sum_SEEDS[a, b] > 1){
						prob_nothing <- 0.2 - ((Sum_SEEDS[a, b] - 1) * 0.01)	# la probabilidad de nada es 0.2 menos 0.01 por el número de semillas...
						# si la probabilidad de nada es cero o menor:
						if (prob_nothing <= 0){
							prob_nothing <- 0		# se fija la probabilidad de nada en cero
							# cuando las semillas son solamente de una especie, la probabilidad es 1
							if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
								prob_Sp1 <- 1
								prob_Sp2 <- 0
							} else {
								if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
									prob_Sp1 <- 0
									prob_Sp2 <- 1
								} else {
									# cuando son de ambas especies a la vez, se calcula según las siguientes fórmulas
									if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										prob_Sp2 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
									}
								}
							}
						} else { # cuando la probabilidad de nada es mayor que cero:
							# si solo hay semillas de una especie, la probabilidad es 1 menos la probabilidad de nada
							if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
								prob_Sp1 <- 1 - prob_nothing
								prob_Sp2 <- 0
							} else {
								if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
									prob_Sp1 <- 0
									prob_Sp2 <- 1 - prob_nothing
								} else {
									# cuando hay de las dos especies, se calcula como se muestra a continuación:
									if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										prob_Sp2 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
									}
								}
							}							
						}
					}
				}
			}
			# evita posibles descompensaciones en la probabilidad
			if (prob_Sp1 > 1 || prob_Sp2 < 0){
				prob_Sp1 <- 1
				prob_Sp2 <- 0
			}
			# calcula un número aleatorio (0, 1 o 2) en función de las probabilidades anteriormente calculadas
			aleat <- sample(c(0, 1, 2), size=1, prob=c(prob_nothing, prob_Sp1, prob_Sp2))
			# si sale 0 no se establece nada, si sale 1 se establece la especie 1 y si sale 2 se establece la especie 2
			if (aleat == 1){
				ISLA[a, b] <- 1
				aux_Sp1 <- aux_Sp1 + 1		# se aprovecha para el recuento del número de individuos de la especie 1
			} else {
				if (aleat == 2){
					ISLA[a, b] <- 2
					aux_Sp2 <- aux_Sp2 + 1		# se aprovecha para el recuento del número de individuos de la especie 2
				} else {
					if (aleat == 0){
						ISLA[a, b] <- 0
					}
				}
			}
		}
	}

	# se guardan las poblaciones de ambas especies
	N_Sp1[i] <- aux_Sp1
	N_Sp2[i] <- aux_Sp2
	
	# gráfico anual de la isla
	if (N_Sp1[i] > 0 && N_Sp2[i] > 0){
		graph <- levelplot(ISLA, xlab = "", ylab = "", main="Isla", col.regions= c("white", "white", "white", "white", "white", "white", "white", "green", "green", "green", "green", "green", "green", "green", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))
		print(graph)
	} else {
		if (N_Sp1[i] == 0 && N_Sp2[i] > 0){
			graph <- levelplot(ISLA, xlab = "", ylab = "", main="Isla", col.regions= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))
			print(graph)
		} else {
			if (N_Sp1[i] > 0 && N_Sp2[i] == 0){
				graph <- levelplot(ISLA, xlab = "", ylab = "", main="Isla", col.regions= c("white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "white", "green", "green", "green", "green", "green", "green", "green", "green", "green", "green", "green"))
				print(graph)
			}
		}
	}

	
	# ¿extinción? guarda el año de extinción si se produce
	if ((N_Sp1[i] == 0 && N_Sp1[i-1] != 0) || (N_Sp2[i] == 0 && N_Sp2[i-1] != 0)){
		ext <- "si"
		t_ext <- i
		}

} # fin de i

# en caso de extinción:
if (N_Sp1[tmax] == 0){
	cat("La especie 1 se extinguió en el año ", t_ext, " \n")
} else {
	if (N_Sp2[tmax] == 0){
		cat("La especie 2 se extinguió en el año", t_ext, " \n")
	}
}

## IMPRIMIR UN PLOT CON LA VARIACIÓN DE LA POBLACIÓN DE AMBAS ESPECIES A LO LARGO DEL TIEMPO
xyplot(N_Sp1[1:tmax]+N_Sp2[1:tmax] ~ 1:tmax, xlab="tiempo (años)", ylab="Población (individuos)", auto.key = TRUE)





##########################								####################################################################################
##########################  PARTE B : OBTENCIÓN DE DATOS			####################################################################################
##########################								####################################################################################


# Paquete gráfico
library(lattice)

## FUNCIONES NECESARIAS (se vuelven a escribir las funciones para que la parte B funcione independientemente de la parte A)

##-1-## Producción de cada especie el año i

Production_Sp <- function(max_berries, dcold, dheat, rangemax_Sp, rangemin_Sp, temp_min, temp_max){
	if(temp_max < rangemax_Sp){
		heatloss_Sp <- 0		# evita pérdida de producción de frutos cuando la temp. máx. del año está dentro del rango de la especie
	} else {
		heatloss_Sp <- dheat * (temp_max - rangemax_Sp)		# descenso causado por el calor (cuando proceda)
	}
	if(temp_min > rangemin_Sp){
		coldloss_Sp <- 0		# evita pérdida de producción de frutos cuando la temp. mín. del año está dentro del rango de la especie
	} else {
		coldloss_Sp <- dcold * (rangemin_Sp - temp_min)		# descenso causado por el frío (cuando proceda)
	}
	fruits_Sp <- round(max_berries - heatloss_Sp - coldloss_Sp)		# fórmula para calcular la producción anual de frutos
	if (fruits_Sp < 0){
		fruits_Sp <- 0		# evitar que la producción sea negativa
	}
	return (fruits_Sp)
} # fin de Production_Sp()


##-2-## Separación de las especies en dos submatrices

Submatrix_Sp <- function(Sp, ISLA, P_Sp1, P_Sp2, xdim, ydim){
	if (Sp == "Sp1"){		# si se introduce "Sp1" en la función
		SP1 <- matrix(0, nrow=xdim, ncol=ydim)		# genera una submatriz para la Sp1
		for (a in 1:xdim){
			for (b in 1:ydim){
				if (ISLA[a, b] == 1){
					SP1[a, b] <- P_Sp1		# donde está la Sp1 (valor = 1) asigna P_Sp1
				} else {
					SP1[a, b] <- 0
				}
			}
		}
		return(SP1)
	}
	if (Sp == "Sp2"){		# si se introduce "Sp2" en la función
		SP2 <- matrix(0, nrow=xdim, ncol=ydim)		# genera una submatriz para la Sp2
		for (a in 1:xdim){
			for (b in 1:ydim){
				if (ISLA[a, b] == 2){
					SP2[a, b] <- P_Sp2		# donde está la Sp2 (valor = 2) asigna P_Sp2
				} else {
					SP2[a, b] <- 0
				}
			}
		}
		return(SP2)
	}
} # fin de Submatrix_Sp()


##-3-## Repartición de las bayas por las diferentes parcelas

Spread <- function(X, p_prop, xdim, ydim){
	Y <- matrix(0, nrow=xdim, ncol=ydim)
	p_peri <- (1-p_prop)/8
	# Posiciones centrales
	for (i in 2:(ydim-1)){
		for (j in 2:(xdim-1)){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							Y[i-1, j-1] <- Y[i-1, j-1] + 1
						} else {
							if (azar == 2){
								Y[i, j-1] <- Y[i, j-1] + 1
							} else {
								if (azar == 3){
									Y[i+1, j-1] <- Y[i+1, j-1] + 1
								} else {
									if (azar == 4){
										Y[i-1, j] <- Y[i-1, j] + 1
									} else {
										if (azar == 6){
											Y[i+1, j] <- Y[i+1, j] + 1
										} else {
											if (azar == 7){
												Y[i-1, j+1] <- Y[i-1, j+1] + 1
											} else {
												if (azar == 8){
													Y[i, j+1] <- Y[i, j+1] + 1
												} else {
													if (azar == 9){
														Y[i+1, j+1] <- Y[i+1, j+1] + 1
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	# Margen superior
	for (i in 1:1){
		for (j in 2:(xdim-1)){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							# agua
						} else {
							if (azar == 2){
								Y[i, j-1] <- Y[i, j-1] + 1
							} else {
								if (azar == 3){
									Y[i+1, j-1] <- Y[i+1, j-1] + 1
								} else {
									if (azar == 4){
										# agua
									} else {
										if (azar == 6){
											Y[i+1, j] <- Y[i+1, j] + 1
										} else {
											if (azar == 7){
												# agua
											} else {
												if (azar == 8){
													Y[i, j+1] <- Y[i, j+1] + 1
												} else {
													if (azar == 9){
														Y[i+1, j+1] <- Y[i+1, j+1] + 1
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	# Margen inferior
	for (i in ydim:ydim){
		for (j in 2:(xdim-1)){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							Y[i-1, j-1] <- Y[i-1, j-1] + 1
						} else {
							if (azar == 2){
								Y[i, j-1] <- Y[i, j-1] + 1
							} else {
								if (azar == 3){
									# agua
								} else {
									if (azar == 4){
										Y[i-1, j] <- Y[i-1, j] + 1
									} else {
										if (azar == 6){
											# agua
										} else {
											if (azar == 7){
												Y[i-1, j+1] <- Y[i-1, j+1] + 1
											} else {
												if (azar == 8){
													Y[i, j+1] <- Y[i, j+1] + 1
												} else {
													if (azar == 9){
														# agua
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	# Margen izquierdo
	for (i in 2:(ydim-1)){
		for (j in 1:1){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							# agua
						} else {
							if (azar == 2){
								# agua
							} else {
								if (azar == 3){
									# agua
								} else {
									if (azar == 4){
										Y[i-1, j] <- Y[i-1, j] + 1
									} else {
										if (azar == 6){
											Y[i+1, j] <- Y[i+1, j] + 1
										} else {
											if (azar == 7){
												Y[i-1, j+1] <- Y[i-1, j+1] + 1
											} else {
												if (azar == 8){
													Y[i, j+1] <- Y[i, j+1] + 1
												} else {
													if (azar == 9){
														Y[i+1, j+1] <- Y[i+1, j+1] + 1
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	# Margen derecho
	for (i in 2:(ydim-1)){
		for (j in xdim:xdim){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							Y[i-1, j-1] <- Y[i-1, j-1] + 1
						} else {
							if (azar == 2){
								Y[i, j-1] <- Y[i, j-1] + 1
							} else {
								if (azar == 3){
									Y[i+1, j-1] <- Y[i+1, j-1] + 1
								} else {
									if (azar == 4){
										Y[i-1, j] <- Y[i-1, j] + 1
									} else {
										if (azar == 6){
											Y[i+1, j] <- Y[i+1, j] + 1
										} else {
											if (azar == 7){
												# agua
											} else {
												if (azar == 8){
													# agua
												} else {
													if (azar == 9){
														# agua
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	# Esquina izquierda superior
	for (i in 1:1){
		for (j in 1:1){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							# agua
						} else {
							if (azar == 2){
								# agua
							} else {
								if (azar == 3){
									# agua
								} else {
									if (azar == 4){
										# agua
									} else {
										if (azar == 6){
											Y[i+1, j] <- Y[i+1, j] + 1
										} else {
											if (azar == 7){
												# agua
											} else {
												if (azar == 8){
													Y[i, j+1] <- Y[i, j+1] + 1
												} else {
													if (azar == 9){
														Y[i+1, j+1] <- Y[i+1, j+1] + 1
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}

	# Esquina izquierda inferior
	for (i in ydim:ydim){
		for (j in 1:1){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							# agua
						} else {
							if (azar == 2){
								# agua
							} else {
								if (azar == 3){
									# agua
								} else {
									if (azar == 4){
										Y[i-1, j] <- Y[i-1, j] + 1
									} else {
										if (azar == 6){
											# agua
										} else {
											if (azar == 7){
												Y[i-1, j+1] <- Y[i-1, j+1] + 1
											} else {
												if (azar == 8){
													Y[i, j+1] <- Y[i, j+1] + 1
												} else {
													if (azar == 9){
														# agua
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	
	# Esquina derecha superior
	for (i in 1:1){
		for (j in xdim:xdim){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							# agua
						} else {
							if (azar == 2){
								Y[i, j-1] <- Y[i, j-1] + 1
							} else {
								if (azar == 3){
									Y[i+1, j-1] <- Y[i+1, j-1] + 1
								} else {
									if (azar == 4){
										# agua
									} else {
										if (azar == 6){
											Y[i+1, j] <- Y[i+1, j] + 1
										} else {
											if (azar == 7){
												# agua
											} else {
												if (azar == 8){
													# agua
												} else {
													if (azar == 9){
														# agua
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}

	# Esquina derecha inferior
	for (i in ydim:ydim){
		for (j in xdim:xdim){
			if (X[i,j] > 0){
				for (k in 1:X[i,j]){
					azar <- sample(c(1, 2, 3, 4, 5, 6, 7, 8, 9), size=1, prob=c(p_peri, p_peri, p_peri, p_peri, p_prop, p_peri, p_peri, p_peri, p_peri))
					if (azar == 5){
						Y[i, j] <- Y[i, j] + 1
					} else {
						if (azar == 1){
							Y[i-1, j-1] <- Y[i-1, j-1] + 1
						} else {
							if (azar == 2){
								Y[i, j-1] <- Y[i, j-1] + 1
							} else {
								if (azar == 3){
									# agua
								} else {
									if (azar == 4){
										Y[i-1, j] <- Y[i-1, j] + 1
									} else {
										if (azar == 6){
											# agua
										} else {
											if (azar == 7){
												# agua
											} else {
												if (azar == 8){
													# agua
												} else {
													if (azar == 9){
														# agua
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	return(Y)
} #fin de Spread()


##-4-## Efecto de dispersión debida a pájaros en la isla

Birds <- function(X, p_bird, xdim, ydim){

	for (i in 1:ydim){
		for (j in 1:xdim){
			if (X[i, j] > 0){		# si la celda tiene frutos...

				# aleatorio con prob. 1/40 de que el pájaro se alimente en esa celda
				paj_si <- rbinom(1, 1, p=p_bird)

				if (paj_si == 1){
					# el pájaro puede comer un máximo de 10 frutos
					if (X[i, j] <= 10){
						paj_frutos <- round(runif(1, 1, X[i, j]))
						} else {
							paj_frutos <- round(runif(1, min=1, max=10))
						}

					# celda actual menos los frutos que se comió el pájaro
					X[i, j] <- X[i, j] - paj_frutos

					# se genera aleatoriamente una coordenada i para dejar los frutos (cuidándose de evitar caer fuera de la isla)
					if (i < (ydim/4)){
						ni_aleat <- round(runif(1, -i, +i))
					} else {
						if ((ydim-i) < (ydim/4)){
							ni_aleat <- round(runif(1, -(ydim-i), +(ydim-i)))
						} else {
							ni_aleat <- round(runif(1, -(ydim/4), +(ydim/4)))
						}
					}

					# se genera aleatoriamente una coordenada j para dejar los frutos (cuidándose de evitar caer fuera de la isla)
					if (j < (xdim/4)){
						nj_aleat <- round(runif(1, -j, +j))
					} else {
						if ((xdim-j) < (xdim/4)){
							nj_aleat <- round(runif(1, -(xdim-j), +(xdim-j)))
						} else {
							nj_aleat <- round(runif(1, -(xdim/4), +(xdim/4)))
						}
					}
					# los pájaros dispersan un máximo de 1/4 de la dimensión de la isla

					# se depositan los frutos en la celda seleccionada aleatoriamente
					X[i + ni_aleat, j + nj_aleat] <- X[i + ni_aleat, j + nj_aleat] + paj_frutos
				}
			}
		} # fin de j
	} # fin de i
	return (X)
} # fin de Birds()


#####

# Variables para guardar las salidas del programa

supuesto <- NULL
tiempo <- NULL
pajaro <- NULL
cat_temp <- NULL
tempmin_m <- NULL
tempmin_sd <- NULL
tempmax_m <- NULL
tempmax_sd <- NULL
pobl_max <- NULL
Sp1_inicial <- NULL
Sp2_inicial <- NULL
Sp1_media <- NULL
Sp2_media <- NULL
Sp1_var <- NULL
Sp2_var <- NULL
Sp1_sd <- NULL
Sp2_sd <- NULL
Sp1_final <- NULL
Sp2_final <- NULL
extincion <- NULL
t_extincion <- NULL


### NÚMERO DE RÉPLICAS DE CADA SUPUESTO ###

replicas <- 100


# Supuestos:

####################################################################
##### SUPUESTO 1 #####  tiempo: 100, pájaro: no, temp: templada   ##
####################################################################

sup <- "supuesto1"
time <- 100
paj <- "no"
temper <- "templada"

## VARIABLES MODIFICABLES

## en general

tmax <- 100				# tiempo de simulación

## específicamente

# rangos de temperatura anuales
lower_mintemp <- 10		# temperatura mínima anual más baja posible
higher_mintemp <- 16		# temperatura mínima anual más alta posible
lower_maxtemp <- 24		# temperatura máxima anual más baja posible
higher_maxtemp <- 30		# temperatura máxima anual más alta posible

# tamaño de la isla (dimensiones de la matriz)
xdim <- 100				# dimensión x
ydim <- 100				# dimensión y

# producción de bayas
max_berries <- 20			# máxima producción de bayas por individuo
dcold <- 1				# descenso en la producción de bayas por cada grado que descienda la Tª por debajo del rango mínimo de la espcie
dheat <- 2				# aumento en la producción de bayas por cada grado que ascienda la Tª por encima del rango máximo de la especie

# resistencia de las especies a la temperatura (rangos normales)
rangemax_Sp1 <- 28		# límite superior del rango normal de Tª (especie 1)
rangemin_Sp1 <- 15		# límite inferior del rango normal de Tª (especie 1)
rangemax_Sp2 <- 28		# límite superior del rango normal de Tª (especie 2)
rangemin_Sp2 <- 8			# límite inferior del rango normal de Tª (especie 2)

# probabilidad de que un fruto caiga en su propia parcela y no en las vecinas; de 0 (nunca) a 1 (siempre)
p_prop <- 5/10

# probabilidad de actuación del pájaro; de 0 (nunca actúa) a 1 (siempre actúa)
p_bird <- 0/20


## BUCLE DE RÉPLICAS

for (z in 1:replicas){

	ext <- "no"
	t_ext <- NA
	
	## INICIALIZACIÓN

	# para comenzar, se define una isla con las dos especies distribuidas de forma aleatoria

	ISLA <- matrix(0, nrow=xdim, ncol=ydim)
	n_Sp1 <- 0
	n_Sp2 <- 0
	for (i in 1:(round(2*xdim/5))){
		for (j in 1:ydim){
			ISLA[i,j] <- 1
			n_Sp1 <- n_Sp1 + 1
		}
	}
	for (i in ((round(3*xdim/5))+1):xdim){
		for (j in 1:ydim){
			ISLA[i,j] <- 2
			n_Sp2 <- n_Sp2 + 1
		}
	}

	tempmin_tmax <- rep(0, tmax)
	tempmax_tmax <- rep(0, tmax)
	N_Sp1 <- rep(0, tmax)		# vector para guardar la población de la Sp1 a lo largo del tiempo
	N_Sp2 <- rep(0, tmax)		# vector para guardar la población de la Sp2 a lo largo del tiempo

	for (i in 1:tmax){		# bucle de duración tmax; 1 paso del tiempo representa 1 año
		# temperatura mínima y máxima anual
		temp_min <- round(runif(1, lower_mintemp, higher_mintemp))
		temp_max <- round(runif(1, lower_maxtemp, higher_maxtemp))
		tempmin_tmax[i] <- temp_min
		tempmax_tmax[i] <- temp_max


		# producción anual de cada especie
		P_Sp1 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp1, rangemin_Sp1, temp_min, temp_max)
		P_Sp2 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp2, rangemin_Sp2, temp_min, temp_max)

		# separación en dos submatrices
		SP1 <- Submatrix_Sp("Sp1", ISLA, P_Sp1, P_Sp2, xdim, ydim)
		SP2 <- Submatrix_Sp("Sp2", ISLA, P_Sp1, P_Sp2, xdim, ydim)
	
		# repartición de los frutos
		SpreadSP1_sin_paj <- Spread(SP1, p_prop, xdim, ydim)
		SpreadSP2_sin_paj <- Spread(SP2, p_prop, xdim, ydim)
	
		# efecto de los pájaros
		SpreadSP1 <- Birds(SpreadSP1_sin_paj, p_bird, xdim, ydim)
		SpreadSP2 <- Birds(SpreadSP2_sin_paj, p_bird, xdim, ydim)
		Sum_SEEDS <- SpreadSP1 + SpreadSP2
	
		# cálculo de la nueva matriz poblacional
		ISLA <- matrix(0, nrow=xdim, ncol=ydim)		
		aux_Sp1 <- 0
		aux_Sp2 <- 0
		for (a in 1:xdim){
			for (b in 1:ydim){
				# cuando no hay semillas de ninguna planta, no se establece nada
				if (Sum_SEEDS[a, b] == 0){
					prob_nothing <- 1		# probabilidad de no establecimiento
					prob_Sp1 <- 0		# probabilidad de establecimiento de la especie 1
					prob_Sp2 <- 0		# probabilidad de establecimiento de la especie 2
				} else {
					# cuando hay una semilla (independientemente de la especie), la probabilidad de nada es 0.2
					# y la probabilidad de dicha especie es 0.8
					if (Sum_SEEDS[a, b] == 1){
						prob_nothing <- 0.2
						if (SpreadSP1[a, b] == 1 && SpreadSP2[a, b] == 0){
							prob_Sp1 <- 1 - prob_nothing
							prob_Sp2 <- 0
						} else {
							if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] == 1){
								prob_Sp1 <- 0
								prob_Sp2 <- 1 - prob_nothing
							}
						}
					} else {
						# cuando hay más de una semilla:
						if (Sum_SEEDS[a, b] > 1){
							prob_nothing <- 0.2 - ((Sum_SEEDS[a, b] - 1) * 0.01)	# la probabilidad de nada es 0.2 menos 0.01 por el número de semillas...
							# si la probabilidad de nada es cero o menor:
							if (prob_nothing <= 0){
								prob_nothing <- 0		# se fija la probabilidad de nada en cero
								# cuando las semillas son solamente de una especie, la probabilidad es 1
								if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
									prob_Sp1 <- 1
									prob_Sp2 <- 0
								} else {
									if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- 0
										prob_Sp2 <- 1
									} else {
										# cuando son de ambas especies a la vez, se calcula según las siguientes fórmulas
										if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
											prob_Sp1 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
											prob_Sp2 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										}
									}
								}
							} else { # cuando la probabilidad de nada es mayor que cero:
								# si solo hay semillas de una especie, la probabilidad es 1 menos la probabilidad de nada
								if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
									prob_Sp1 <- 1 - prob_nothing
									prob_Sp2 <- 0
								} else {
									if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- 0
										prob_Sp2 <- 1 - prob_nothing
									} else {
										# cuando hay de las dos especies, se calcula como se muestra a continuación:
										if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
											prob_Sp1 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
											prob_Sp2 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										}
									}
								}							
							}
						}
					}
				}
				if (prob_Sp1 > 1 || prob_Sp2 < 0){
					prob_Sp1 <- 1
					prob_Sp2 <- 0
				}
				aleat <- sample(c(0, 1, 2), size=1, prob=c(prob_nothing, prob_Sp1, prob_Sp2))
				if (aleat == 1){
					ISLA[a, b] <- 1
					aux_Sp1 <- aux_Sp1 + 1
				} else {
					if (aleat == 2){
						ISLA[a, b] <- 2
						aux_Sp2 <- aux_Sp2 + 1
					} else {
						if (aleat == 0){
							ISLA[a, b] <- 0
						}
					}
				}
			}
		}
		N_Sp1[i] <- aux_Sp1
		N_Sp2[i] <- aux_Sp2

		# estimar extinción
		if ((N_Sp1[i] == 0 && N_Sp1[i-1] != 0) || (N_Sp2[i] == 0 && N_Sp2[i-1] != 0)){
			ext <- "si"
			t_ext <- i
		}

	} # fin de i


	supuesto <- c(supuesto, sup)				##########
	tiempo <- c(tiempo, time)				##########
	pajaro <- c(pajaro, paj)				##########
	cat_temp <- c(cat_temp, temper)			##########

	Sp1_inicial <- c(Sp1_inicial, n_Sp1)		##########
	Sp2_inicial <- c(Sp2_inicial, n_Sp2)		##########

	tempmin_m <- c(tempmin_m, mean(tempmin_tmax))	##########
	tempmin_sd <- c(tempmin_sd, sd(tempmin_tmax))	##########
	tempmax_m <- c(tempmax_m, mean(tempmax_tmax))	##########
	tempmax_sd <- c(tempmax_sd, sd(tempmax_tmax))	##########
	
	pobl_max <- c(pobl_max, xdim*ydim)			##########

	Sp1_media <- c(Sp1_media, mean(N_Sp1))		##########
	Sp1_var <- c(Sp1_var, var(N_Sp1))			##########
	Sp1_sd <- c(Sp1_sd, sd(N_Sp1))			##########
	Sp2_media <- c(Sp2_media, mean(N_Sp2))		##########
	Sp2_var <- c(Sp2_var, var(N_Sp2))			##########
	Sp2_sd <- c(Sp2_sd, sd(N_Sp2))			##########
	Sp1_final <- c(Sp1_final, N_Sp1[tmax])		##########
	Sp2_final <- c(Sp2_final, N_Sp2[tmax])		##########

	extincion <- c(extincion, ext)			##########
	t_extincion <- c(t_extincion, t_ext)		##########


} # fin de z, fin de supuesto

# PLOT para saber el progreso del programa:
MATRIZ <- matrix(0, 5, 7)
MATRIZ[2,2] <- 1
MATRIZ[3,2] <- 1
MATRIZ[4,2] <- 1
MATRIZ[3,3] <- 1
MATRIZ[3,4] <- 1
MATRIZ[3,5] <- 1
MATRIZ[3,6] <- 1
MATRIZ[2,6] <- 1
levelplot(MATRIZ, xlab = "", ylab = "", main="", col.regions= c("white", "white", "white", "white", "white", "white", "white", "green", "green", "green", "green", "green", "green", "green", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))

# guarda en un fichero nuevo los datos obtenidos hasta el momento, por si hubiese algún problema:
ARJEPLOG1 <- data.frame(supuesto, tiempo, pajaro, cat_temp, Sp1_inicial, Sp2_inicial, tempmin_m, tempmin_sd, tempmax_m, tempmax_sd, pobl_max, Sp1_media, Sp1_var, Sp1_sd, Sp2_media, Sp2_var, Sp2_sd, Sp1_final, Sp2_final, extincion, t_extincion)
write.table(ARJEPLOG1, file="ARJEPLOG1.txt")


####################################################################
##### SUPUESTO 2 #####  tiempo: 100, pájaro: no, temp: baja      ##
####################################################################

sup <- "supuesto2"
time <- 100
paj <- "no"
temper <- "baja"

## VARIABLES MODIFICABLES

## en general

tmax <- 100				# tiempo de simulación

## específicamente

# rangos de temperatura anuales
lower_mintemp <- 0		# temperatura mínima anual más baja posible
higher_mintemp <- 8		# temperatura mínima anual más alta posible
lower_maxtemp <- 16		# temperatura máxima anual más baja posible
higher_maxtemp <- 24		# temperatura máxima anual más alta posible

# tamaño de la isla (dimensiones de la matriz)
xdim <- 100				# dimensión x
ydim <- 100				# dimensión y

# producción de bayas
max_berries <- 20			# máxima producción de bayas por individuo
dcold <- 1				# descenso en la producción de bayas por cada grado que descienda la Tª por debajo del rango mínimo de la espcie
dheat <- 2				# aumento en la producción de bayas por cada grado que ascienda la Tª por encima del rango máximo de la especie

# resistencia de las especies a la temperatura (rangos normales)
rangemax_Sp1 <- 28		# límite superior del rango normal de Tª (especie 1)
rangemin_Sp1 <- 15		# límite inferior del rango normal de Tª (especie 1)
rangemax_Sp2 <- 28		# límite superior del rango normal de Tª (especie 2)
rangemin_Sp2 <- 8			# límite inferior del rango normal de Tª (especie 2)

# probabilidad de que un fruto caiga en su propia parcela y no en las vecinas; de 0 (nunca) a 1 (siempre)
p_prop <- 5/10

# probabilidad de actuación del pájaro; de 0 (nunca actúa) a 1 (siempre actúa)
p_bird <- 0/20


## BUCLE DE RÉPLICAS

for (z in 1:replicas){

	ext <- "no"
	t_ext <- NA
	
	## INICIALIZACIÓN

	# para comenzar, se define una isla con las dos especies distribuidas de forma aleatoria

	ISLA <- matrix(0, nrow=xdim, ncol=ydim)
	n_Sp1 <- 0
	n_Sp2 <- 0
	for (i in 1:(round(2*xdim/5))){
		for (j in 1:ydim){
			ISLA[i,j] <- 1
			n_Sp1 <- n_Sp1 + 1
		}
	}
	for (i in ((round(3*xdim/5))+1):xdim){
		for (j in 1:ydim){
			ISLA[i,j] <- 2
			n_Sp2 <- n_Sp2 + 1
		}
	}

	tempmin_tmax <- rep(0, tmax)
	tempmax_tmax <- rep(0, tmax)
	N_Sp1 <- rep(0, tmax)		# vector para guardar la población de la Sp1 a lo largo del tiempo
	N_Sp2 <- rep(0, tmax)		# vector para guardar la población de la Sp2 a lo largo del tiempo

	for (i in 1:tmax){		# bucle de duración tmax; 1 paso del tiempo representa 1 año
		# temperatura mínima y máxima anual
		temp_min <- round(runif(1, lower_mintemp, higher_mintemp))
		temp_max <- round(runif(1, lower_maxtemp, higher_maxtemp))
		tempmin_tmax[i] <- temp_min
		tempmax_tmax[i] <- temp_max


		# producción anual de cada especie
		P_Sp1 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp1, rangemin_Sp1, temp_min, temp_max)
		P_Sp2 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp2, rangemin_Sp2, temp_min, temp_max)

		# separación en dos submatrices
		SP1 <- Submatrix_Sp("Sp1", ISLA, P_Sp1, P_Sp2, xdim, ydim)
		SP2 <- Submatrix_Sp("Sp2", ISLA, P_Sp1, P_Sp2, xdim, ydim)
	
		# repartición de los frutos
		SpreadSP1_sin_paj <- Spread(SP1, p_prop, xdim, ydim)
		SpreadSP2_sin_paj <- Spread(SP2, p_prop, xdim, ydim)
	
		# efecto de los pájaros
		SpreadSP1 <- Birds(SpreadSP1_sin_paj, p_bird, xdim, ydim)
		SpreadSP2 <- Birds(SpreadSP2_sin_paj, p_bird, xdim, ydim)
		Sum_SEEDS <- SpreadSP1 + SpreadSP2
	
		# cálculo de la nueva matriz poblacional
		ISLA <- matrix(0, nrow=xdim, ncol=ydim)		
		aux_Sp1 <- 0
		aux_Sp2 <- 0
		for (a in 1:xdim){
			for (b in 1:ydim){
				# cuando no hay semillas de ninguna planta, no se establece nada
				if (Sum_SEEDS[a, b] == 0){
					prob_nothing <- 1		# probabilidad de no establecimiento
					prob_Sp1 <- 0		# probabilidad de establecimiento de la especie 1
					prob_Sp2 <- 0		# probabilidad de establecimiento de la especie 2
				} else {
					# cuando hay una semilla (independientemente de la especie), la probabilidad de nada es 0.2
					# y la probabilidad de dicha especie es 0.8
					if (Sum_SEEDS[a, b] == 1){
						prob_nothing <- 0.2
						if (SpreadSP1[a, b] == 1 && SpreadSP2[a, b] == 0){
							prob_Sp1 <- 1 - prob_nothing
							prob_Sp2 <- 0
						} else {
							if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] == 1){
								prob_Sp1 <- 0
								prob_Sp2 <- 1 - prob_nothing
							}
						}
					} else {
						# cuando hay más de una semilla:
						if (Sum_SEEDS[a, b] > 1){
							prob_nothing <- 0.2 - ((Sum_SEEDS[a, b] - 1) * 0.01)	# la probabilidad de nada es 0.2 menos 0.01 por el número de semillas...
							# si la probabilidad de nada es cero o menor:
							if (prob_nothing <= 0){
								prob_nothing <- 0		# se fija la probabilidad de nada en cero
								# cuando las semillas son solamente de una especie, la probabilidad es 1
								if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
									prob_Sp1 <- 1
									prob_Sp2 <- 0
								} else {
									if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- 0
										prob_Sp2 <- 1
									} else {
										# cuando son de ambas especies a la vez, se calcula según las siguientes fórmulas
										if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
											prob_Sp1 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
											prob_Sp2 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										}
									}
								}
							} else { # cuando la probabilidad de nada es mayor que cero:
								# si solo hay semillas de una especie, la probabilidad es 1 menos la probabilidad de nada
								if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
									prob_Sp1 <- 1 - prob_nothing
									prob_Sp2 <- 0
								} else {
									if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- 0
										prob_Sp2 <- 1 - prob_nothing
									} else {
										# cuando hay de las dos especies, se calcula como se muestra a continuación:
										if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
											prob_Sp1 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
											prob_Sp2 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										}
									}
								}							
							}
						}
					}
				}
				if (prob_Sp1 > 1 || prob_Sp2 < 0){
					prob_Sp1 <- 1
					prob_Sp2 <- 0
				}
				aleat <- sample(c(0, 1, 2), size=1, prob=c(prob_nothing, prob_Sp1, prob_Sp2))
				if (aleat == 1){
					ISLA[a, b] <- 1
					aux_Sp1 <- aux_Sp1 + 1
				} else {
					if (aleat == 2){
						ISLA[a, b] <- 2
						aux_Sp2 <- aux_Sp2 + 1
					} else {
						if (aleat == 0){
							ISLA[a, b] <- 0
						}
					}
				}
			}
		}
		N_Sp1[i] <- aux_Sp1
		N_Sp2[i] <- aux_Sp2

		# estimar extinción
		if ((N_Sp1[i] == 0 && N_Sp1[i-1] != 0) || (N_Sp2[i] == 0 && N_Sp2[i-1] != 0)){
			ext <- "si"
			t_ext <- i
		}

	} # fin de i


	supuesto <- c(supuesto, sup)				##########
	tiempo <- c(tiempo, time)				##########
	pajaro <- c(pajaro, paj)				##########
	cat_temp <- c(cat_temp, temper)			##########

	Sp1_inicial <- c(Sp1_inicial, n_Sp1)		##########
	Sp2_inicial <- c(Sp2_inicial, n_Sp2)		##########

	tempmin_m <- c(tempmin_m, mean(tempmin_tmax))	##########
	tempmin_sd <- c(tempmin_sd, sd(tempmin_tmax))	##########
	tempmax_m <- c(tempmax_m, mean(tempmax_tmax))	##########
	tempmax_sd <- c(tempmax_sd, sd(tempmax_tmax))	##########
	
	pobl_max <- c(pobl_max, xdim*ydim)			##########

	Sp1_media <- c(Sp1_media, mean(N_Sp1))		##########
	Sp1_var <- c(Sp1_var, var(N_Sp1))			##########
	Sp1_sd <- c(Sp1_sd, sd(N_Sp1))			##########
	Sp2_media <- c(Sp2_media, mean(N_Sp2))		##########
	Sp2_var <- c(Sp2_var, var(N_Sp2))			##########
	Sp2_sd <- c(Sp2_sd, sd(N_Sp2))			##########
	Sp1_final <- c(Sp1_final, N_Sp1[tmax])		##########
	Sp2_final <- c(Sp2_final, N_Sp2[tmax])		##########

	extincion <- c(extincion, ext)			##########
	t_extincion <- c(t_extincion, t_ext)		##########


} # fin de z, fin de supuesto

# PLOT para saber el progreso del programa:
MATRIZ <- matrix(0, 5, 7)
MATRIZ[2,2] <- 1
MATRIZ[2,3] <- 1
MATRIZ[2,4] <- 1
MATRIZ[3,2] <- 1
MATRIZ[3,4] <- 1
MATRIZ[4,2] <- 1
MATRIZ[4,4] <- 1
MATRIZ[4,5] <- 1
MATRIZ[2,6] <- 1
MATRIZ[3,6] <- 1
MATRIZ[4,6] <- 1
levelplot(MATRIZ, xlab = "", ylab = "", main="", col.regions= c("white", "white", "white", "white", "white", "white", "white", "green", "green", "green", "green", "green", "green", "green", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))

# guarda en un fichero nuevo los datos obtenidos hasta el momento, por si hubiese algún problema:
ARJEPLOG2 <- data.frame(supuesto, tiempo, pajaro, cat_temp, Sp1_inicial, Sp2_inicial, tempmin_m, tempmin_sd, tempmax_m, tempmax_sd, pobl_max, Sp1_media, Sp1_var, Sp1_sd, Sp2_media, Sp2_var, Sp2_sd, Sp1_final, Sp2_final, extincion, t_extincion)
write.table(ARJEPLOG2, file="ARJEPLOG2.txt")


####################################################################
##### SUPUESTO 3 #####  tiempo: 100, pájaro: no, temp: alta       ##
####################################################################

sup <- "supuesto3"
time <- 100
paj <- "no"
temper <- "alta"

## VARIABLES MODIFICABLES

## en general

tmax <- 100				# tiempo de simulación

## específicamente

# rangos de temperatura anuales
lower_mintemp <- 16		# temperatura mínima anual más baja posible
higher_mintemp <- 24		# temperatura mínima anual más alta posible
lower_maxtemp <- 29		# temperatura máxima anual más baja posible
higher_maxtemp <- 37		# temperatura máxima anual más alta posible

# tamaño de la isla (dimensiones de la matriz)
xdim <- 100				# dimensión x
ydim <- 100				# dimensión y

# producción de bayas
max_berries <- 20			# máxima producción de bayas por individuo
dcold <- 1				# descenso en la producción de bayas por cada grado que descienda la Tª por debajo del rango mínimo de la espcie
dheat <- 2				# aumento en la producción de bayas por cada grado que ascienda la Tª por encima del rango máximo de la especie

# resistencia de las especies a la temperatura (rangos normales)
rangemax_Sp1 <- 28		# límite superior del rango normal de Tª (especie 1)
rangemin_Sp1 <- 15		# límite inferior del rango normal de Tª (especie 1)
rangemax_Sp2 <- 28		# límite superior del rango normal de Tª (especie 2)
rangemin_Sp2 <- 8			# límite inferior del rango normal de Tª (especie 2)

# probabilidad de que un fruto caiga en su propia parcela y no en las vecinas; de 0 (nunca) a 1 (siempre)
p_prop <- 5/10

# probabilidad de actuación del pájaro; de 0 (nunca actúa) a 1 (siempre actúa)
p_bird <- 0/20


## BUCLE DE RÉPLICAS

for (z in 1:replicas){

	ext <- "no"
	t_ext <- NA
	
	## INICIALIZACIÓN

	# para comenzar, se define una isla con las dos especies distribuidas de forma aleatoria

	ISLA <- matrix(0, nrow=xdim, ncol=ydim)
	n_Sp1 <- 0
	n_Sp2 <- 0
	for (i in 1:(round(2*xdim/5))){
		for (j in 1:ydim){
			ISLA[i,j] <- 1
			n_Sp1 <- n_Sp1 + 1
		}
	}
	for (i in ((round(3*xdim/5))+1):xdim){
		for (j in 1:ydim){
			ISLA[i,j] <- 2
			n_Sp2 <- n_Sp2 + 1
		}
	}

	tempmin_tmax <- rep(0, tmax)
	tempmax_tmax <- rep(0, tmax)
	N_Sp1 <- rep(0, tmax)		# vector para guardar la población de la Sp1 a lo largo del tiempo
	N_Sp2 <- rep(0, tmax)		# vector para guardar la población de la Sp2 a lo largo del tiempo

	for (i in 1:tmax){		# bucle de duración tmax; 1 paso del tiempo representa 1 año
		# temperatura mínima y máxima anual
		temp_min <- round(runif(1, lower_mintemp, higher_mintemp))
		temp_max <- round(runif(1, lower_maxtemp, higher_maxtemp))
		tempmin_tmax[i] <- temp_min
		tempmax_tmax[i] <- temp_max


		# producción anual de cada especie
		P_Sp1 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp1, rangemin_Sp1, temp_min, temp_max)
		P_Sp2 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp2, rangemin_Sp2, temp_min, temp_max)

		# separación en dos submatrices
		SP1 <- Submatrix_Sp("Sp1", ISLA, P_Sp1, P_Sp2, xdim, ydim)
		SP2 <- Submatrix_Sp("Sp2", ISLA, P_Sp1, P_Sp2, xdim, ydim)
	
		# repartición de los frutos
		SpreadSP1_sin_paj <- Spread(SP1, p_prop, xdim, ydim)
		SpreadSP2_sin_paj <- Spread(SP2, p_prop, xdim, ydim)
	
		# efecto de los pájaros
		SpreadSP1 <- Birds(SpreadSP1_sin_paj, p_bird, xdim, ydim)
		SpreadSP2 <- Birds(SpreadSP2_sin_paj, p_bird, xdim, ydim)
		Sum_SEEDS <- SpreadSP1 + SpreadSP2
	
		# cálculo de la nueva matriz poblacional
		ISLA <- matrix(0, nrow=xdim, ncol=ydim)		
		aux_Sp1 <- 0
		aux_Sp2 <- 0
		for (a in 1:xdim){
			for (b in 1:ydim){
				# cuando no hay semillas de ninguna planta, no se establece nada
				if (Sum_SEEDS[a, b] == 0){
					prob_nothing <- 1		# probabilidad de no establecimiento
					prob_Sp1 <- 0		# probabilidad de establecimiento de la especie 1
					prob_Sp2 <- 0		# probabilidad de establecimiento de la especie 2
				} else {
					# cuando hay una semilla (independientemente de la especie), la probabilidad de nada es 0.2
					# y la probabilidad de dicha especie es 0.8
					if (Sum_SEEDS[a, b] == 1){
						prob_nothing <- 0.2
						if (SpreadSP1[a, b] == 1 && SpreadSP2[a, b] == 0){
							prob_Sp1 <- 1 - prob_nothing
							prob_Sp2 <- 0
						} else {
							if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] == 1){
								prob_Sp1 <- 0
								prob_Sp2 <- 1 - prob_nothing
							}
						}
					} else {
						# cuando hay más de una semilla:
						if (Sum_SEEDS[a, b] > 1){
							prob_nothing <- 0.2 - ((Sum_SEEDS[a, b] - 1) * 0.01)	# la probabilidad de nada es 0.2 menos 0.01 por el número de semillas...
							# si la probabilidad de nada es cero o menor:
							if (prob_nothing <= 0){
								prob_nothing <- 0		# se fija la probabilidad de nada en cero
								# cuando las semillas son solamente de una especie, la probabilidad es 1
								if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
									prob_Sp1 <- 1
									prob_Sp2 <- 0
								} else {
									if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- 0
										prob_Sp2 <- 1
									} else {
										# cuando son de ambas especies a la vez, se calcula según las siguientes fórmulas
										if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
											prob_Sp1 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
											prob_Sp2 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										}
									}
								}
							} else { # cuando la probabilidad de nada es mayor que cero:
								# si solo hay semillas de una especie, la probabilidad es 1 menos la probabilidad de nada
								if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
									prob_Sp1 <- 1 - prob_nothing
									prob_Sp2 <- 0
								} else {
									if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- 0
										prob_Sp2 <- 1 - prob_nothing
									} else {
										# cuando hay de las dos especies, se calcula como se muestra a continuación:
										if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
											prob_Sp1 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
											prob_Sp2 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										}
									}
								}							
							}
						}
					}
				}
				if (prob_Sp1 > 1 || prob_Sp2 < 0){
					prob_Sp1 <- 1
					prob_Sp2 <- 0
				}
				aleat <- sample(c(0, 1, 2), size=1, prob=c(prob_nothing, prob_Sp1, prob_Sp2))
				if (aleat == 1){
					ISLA[a, b] <- 1
					aux_Sp1 <- aux_Sp1 + 1
				} else {
					if (aleat == 2){
						ISLA[a, b] <- 2
						aux_Sp2 <- aux_Sp2 + 1
					} else {
						if (aleat == 0){
							ISLA[a, b] <- 0
						}
					}
				}
			}
		}
		N_Sp1[i] <- aux_Sp1
		N_Sp2[i] <- aux_Sp2

		# estimar extinción
		if ((N_Sp1[i] == 0 && N_Sp1[i-1] != 0) || (N_Sp2[i] == 0 && N_Sp2[i-1] != 0)){
			ext <- "si"
			t_ext <- i
		}

	} # fin de i


	supuesto <- c(supuesto, sup)				##########
	tiempo <- c(tiempo, time)				##########
	pajaro <- c(pajaro, paj)				##########
	cat_temp <- c(cat_temp, temper)			##########

	Sp1_inicial <- c(Sp1_inicial, n_Sp1)		##########
	Sp2_inicial <- c(Sp2_inicial, n_Sp2)		##########

	tempmin_m <- c(tempmin_m, mean(tempmin_tmax))	##########
	tempmin_sd <- c(tempmin_sd, sd(tempmin_tmax))	##########
	tempmax_m <- c(tempmax_m, mean(tempmax_tmax))	##########
	tempmax_sd <- c(tempmax_sd, sd(tempmax_tmax))	##########
	
	pobl_max <- c(pobl_max, xdim*ydim)			##########

	Sp1_media <- c(Sp1_media, mean(N_Sp1))		##########
	Sp1_var <- c(Sp1_var, var(N_Sp1))			##########
	Sp1_sd <- c(Sp1_sd, sd(N_Sp1))			##########
	Sp2_media <- c(Sp2_media, mean(N_Sp2))		##########
	Sp2_var <- c(Sp2_var, var(N_Sp2))			##########
	Sp2_sd <- c(Sp2_sd, sd(N_Sp2))			##########
	Sp1_final <- c(Sp1_final, N_Sp1[tmax])		##########
	Sp2_final <- c(Sp2_final, N_Sp2[tmax])		##########

	extincion <- c(extincion, ext)			##########
	t_extincion <- c(t_extincion, t_ext)		##########


} # fin de z, fin de supuesto

# PLOT para saber el progreso del programa:
MATRIZ <- matrix(0, 5, 7)
MATRIZ[2,2] <- 1
MATRIZ[3,2] <- 1
MATRIZ[3,4] <- 1
MATRIZ[4,2] <- 1
MATRIZ[4,3] <- 1
MATRIZ[4,4] <- 1
MATRIZ[4,5] <- 1
MATRIZ[2,6] <- 1
MATRIZ[3,6] <- 1
MATRIZ[4,6] <- 1
levelplot(MATRIZ, xlab = "", ylab = "", main="", col.regions= c("white", "white", "white", "white", "white", "white", "white", "green", "green", "green", "green", "green", "green", "green", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))

# guarda en un fichero nuevo los datos obtenidos hasta el momento, por si hubiese algún problema:
ARJEPLOG3 <- data.frame(supuesto, tiempo, pajaro, cat_temp, Sp1_inicial, Sp2_inicial, tempmin_m, tempmin_sd, tempmax_m, tempmax_sd, pobl_max, Sp1_media, Sp1_var, Sp1_sd, Sp2_media, Sp2_var, Sp2_sd, Sp1_final, Sp2_final, extincion, t_extincion)
write.table(ARJEPLOG3, file="ARJEPLOG3.txt")


####################################################################
##### SUPUESTO 4 #####  tiempo: 100, pájaro: si, temp: templada   ##
####################################################################

sup <- "supuesto4"
time <- 100
paj <- "si"
temper <- "templada"

## VARIABLES MODIFICABLES

## en general

tmax <- 100				# tiempo de simulación

## específicamente

# rangos de temperatura anuales
lower_mintemp <- 10		# temperatura mínima anual más baja posible
higher_mintemp <- 16		# temperatura mínima anual más alta posible
lower_maxtemp <- 24		# temperatura máxima anual más baja posible
higher_maxtemp <- 30		# temperatura máxima anual más alta posible

# tamaño de la isla (dimensiones de la matriz)
xdim <- 100				# dimensión x
ydim <- 100				# dimensión y

# producción de bayas
max_berries <- 20			# máxima producción de bayas por individuo
dcold <- 1				# descenso en la producción de bayas por cada grado que descienda la Tª por debajo del rango mínimo de la espcie
dheat <- 2				# aumento en la producción de bayas por cada grado que ascienda la Tª por encima del rango máximo de la especie

# resistencia de las especies a la temperatura (rangos normales)
rangemax_Sp1 <- 28		# límite superior del rango normal de Tª (especie 1)
rangemin_Sp1 <- 15		# límite inferior del rango normal de Tª (especie 1)
rangemax_Sp2 <- 28		# límite superior del rango normal de Tª (especie 2)
rangemin_Sp2 <- 8			# límite inferior del rango normal de Tª (especie 2)

# probabilidad de que un fruto caiga en su propia parcela y no en las vecinas; de 0 (nunca) a 1 (siempre)
p_prop <- 5/10

# probabilidad de actuación del pájaro; de 0 (nunca actúa) a 1 (siempre actúa)
p_bird <- 1/20


## BUCLE DE RÉPLICAS

for (z in 1:replicas){

	ext <- "no"
	t_ext <- NA
	
	## INICIALIZACIÓN

	# para comenzar, se define una isla con las dos especies distribuidas de forma aleatoria

	ISLA <- matrix(0, nrow=xdim, ncol=ydim)
	n_Sp1 <- 0
	n_Sp2 <- 0
	for (i in 1:(round(2*xdim/5))){
		for (j in 1:ydim){
			ISLA[i,j] <- 1
			n_Sp1 <- n_Sp1 + 1
		}
	}
	for (i in ((round(3*xdim/5))+1):xdim){
		for (j in 1:ydim){
			ISLA[i,j] <- 2
			n_Sp2 <- n_Sp2 + 1
		}
	}

	tempmin_tmax <- rep(0, tmax)
	tempmax_tmax <- rep(0, tmax)
	N_Sp1 <- rep(0, tmax)		# vector para guardar la población de la Sp1 a lo largo del tiempo
	N_Sp2 <- rep(0, tmax)		# vector para guardar la población de la Sp2 a lo largo del tiempo

	for (i in 1:tmax){		# bucle de duración tmax; 1 paso del tiempo representa 1 año
		# temperatura mínima y máxima anual
		temp_min <- round(runif(1, lower_mintemp, higher_mintemp))
		temp_max <- round(runif(1, lower_maxtemp, higher_maxtemp))
		tempmin_tmax[i] <- temp_min
		tempmax_tmax[i] <- temp_max


		# producción anual de cada especie
		P_Sp1 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp1, rangemin_Sp1, temp_min, temp_max)
		P_Sp2 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp2, rangemin_Sp2, temp_min, temp_max)

		# separación en dos submatrices
		SP1 <- Submatrix_Sp("Sp1", ISLA, P_Sp1, P_Sp2, xdim, ydim)
		SP2 <- Submatrix_Sp("Sp2", ISLA, P_Sp1, P_Sp2, xdim, ydim)
	
		# repartición de los frutos
		SpreadSP1_sin_paj <- Spread(SP1, p_prop, xdim, ydim)
		SpreadSP2_sin_paj <- Spread(SP2, p_prop, xdim, ydim)
	
		# efecto de los pájaros
		SpreadSP1 <- Birds(SpreadSP1_sin_paj, p_bird, xdim, ydim)
		SpreadSP2 <- Birds(SpreadSP2_sin_paj, p_bird, xdim, ydim)
		Sum_SEEDS <- SpreadSP1 + SpreadSP2
	
		# cálculo de la nueva matriz poblacional
		ISLA <- matrix(0, nrow=xdim, ncol=ydim)		
		aux_Sp1 <- 0
		aux_Sp2 <- 0
		for (a in 1:xdim){
			for (b in 1:ydim){
				# cuando no hay semillas de ninguna planta, no se establece nada
				if (Sum_SEEDS[a, b] == 0){
					prob_nothing <- 1		# probabilidad de no establecimiento
					prob_Sp1 <- 0		# probabilidad de establecimiento de la especie 1
					prob_Sp2 <- 0		# probabilidad de establecimiento de la especie 2
				} else {
					# cuando hay una semilla (independientemente de la especie), la probabilidad de nada es 0.2
					# y la probabilidad de dicha especie es 0.8
					if (Sum_SEEDS[a, b] == 1){
						prob_nothing <- 0.2
						if (SpreadSP1[a, b] == 1 && SpreadSP2[a, b] == 0){
							prob_Sp1 <- 1 - prob_nothing
							prob_Sp2 <- 0
						} else {
							if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] == 1){
								prob_Sp1 <- 0
								prob_Sp2 <- 1 - prob_nothing
							}
						}
					} else {
						# cuando hay más de una semilla:
						if (Sum_SEEDS[a, b] > 1){
							prob_nothing <- 0.2 - ((Sum_SEEDS[a, b] - 1) * 0.01)	# la probabilidad de nada es 0.2 menos 0.01 por el número de semillas...
							# si la probabilidad de nada es cero o menor:
							if (prob_nothing <= 0){
								prob_nothing <- 0		# se fija la probabilidad de nada en cero
								# cuando las semillas son solamente de una especie, la probabilidad es 1
								if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
									prob_Sp1 <- 1
									prob_Sp2 <- 0
								} else {
									if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- 0
										prob_Sp2 <- 1
									} else {
										# cuando son de ambas especies a la vez, se calcula según las siguientes fórmulas
										if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
											prob_Sp1 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
											prob_Sp2 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										}
									}
								}
							} else { # cuando la probabilidad de nada es mayor que cero:
								# si solo hay semillas de una especie, la probabilidad es 1 menos la probabilidad de nada
								if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
									prob_Sp1 <- 1 - prob_nothing
									prob_Sp2 <- 0
								} else {
									if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- 0
										prob_Sp2 <- 1 - prob_nothing
									} else {
										# cuando hay de las dos especies, se calcula como se muestra a continuación:
										if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
											prob_Sp1 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
											prob_Sp2 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										}
									}
								}							
							}
						}
					}
				}
				if (prob_Sp1 > 1 || prob_Sp2 < 0){
					prob_Sp1 <- 1
					prob_Sp2 <- 0
				}
				aleat <- sample(c(0, 1, 2), size=1, prob=c(prob_nothing, prob_Sp1, prob_Sp2))
				if (aleat == 1){
					ISLA[a, b] <- 1
					aux_Sp1 <- aux_Sp1 + 1
				} else {
					if (aleat == 2){
						ISLA[a, b] <- 2
						aux_Sp2 <- aux_Sp2 + 1
					} else {
						if (aleat == 0){
							ISLA[a, b] <- 0
						}
					}
				}
			}
		}
		N_Sp1[i] <- aux_Sp1
		N_Sp2[i] <- aux_Sp2

		# estimar extinción
		if ((N_Sp1[i] == 0 && N_Sp1[i-1] != 0) || (N_Sp2[i] == 0 && N_Sp2[i-1] != 0)){
			ext <- "si"
			t_ext <- i
		}

	} # fin de i


	supuesto <- c(supuesto, sup)				##########
	tiempo <- c(tiempo, time)				##########
	pajaro <- c(pajaro, paj)				##########
	cat_temp <- c(cat_temp, temper)			##########

	Sp1_inicial <- c(Sp1_inicial, n_Sp1)		##########
	Sp2_inicial <- c(Sp2_inicial, n_Sp2)		##########

	tempmin_m <- c(tempmin_m, mean(tempmin_tmax))	##########
	tempmin_sd <- c(tempmin_sd, sd(tempmin_tmax))	##########
	tempmax_m <- c(tempmax_m, mean(tempmax_tmax))	##########
	tempmax_sd <- c(tempmax_sd, sd(tempmax_tmax))	##########
	
	pobl_max <- c(pobl_max, xdim*ydim)			##########

	Sp1_media <- c(Sp1_media, mean(N_Sp1))		##########
	Sp1_var <- c(Sp1_var, var(N_Sp1))			##########
	Sp1_sd <- c(Sp1_sd, sd(N_Sp1))			##########
	Sp2_media <- c(Sp2_media, mean(N_Sp2))		##########
	Sp2_var <- c(Sp2_var, var(N_Sp2))			##########
	Sp2_sd <- c(Sp2_sd, sd(N_Sp2))			##########
	Sp1_final <- c(Sp1_final, N_Sp1[tmax])		##########
	Sp2_final <- c(Sp2_final, N_Sp2[tmax])		##########

	extincion <- c(extincion, ext)			##########
	t_extincion <- c(t_extincion, t_ext)		##########


} # fin de z, fin de supuesto

# PLOT para saber el progreso del programa:
MATRIZ <- matrix(0, 5, 7)
MATRIZ[2,4] <- 1
MATRIZ[3,4] <- 1
MATRIZ[4,2] <- 1
MATRIZ[4,3] <- 1
MATRIZ[4,4] <- 1
MATRIZ[2,5] <- 1
MATRIZ[4,5] <- 1
MATRIZ[2,6] <- 1
MATRIZ[4,6] <- 1
levelplot(MATRIZ, xlab = "", ylab = "", main="", col.regions= c("white", "white", "white", "white", "white", "white", "white", "green", "green", "green", "green", "green", "green", "green", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))

# guarda en un fichero nuevo los datos obtenidos hasta el momento, por si hubiese algún problema:
ARJEPLOG4 <- data.frame(supuesto, tiempo, pajaro, cat_temp, Sp1_inicial, Sp2_inicial, tempmin_m, tempmin_sd, tempmax_m, tempmax_sd, pobl_max, Sp1_media, Sp1_var, Sp1_sd, Sp2_media, Sp2_var, Sp2_sd, Sp1_final, Sp2_final, extincion, t_extincion)
write.table(ARJEPLOG4, file="ARJEPLOG4.txt")


####################################################################
##### SUPUESTO 5 #####  tiempo: 100, pájaro: si, temp: baja       ##
####################################################################

sup <- "supuesto5"
time <- 100
paj <- "si"
temper <- "baja"

## VARIABLES MODIFICABLES

## en general

tmax <- 100				# tiempo de simulación

## específicamente

# rangos de temperatura anuales
lower_mintemp <- 0		# temperatura mínima anual más baja posible
higher_mintemp <- 8		# temperatura mínima anual más alta posible
lower_maxtemp <- 16		# temperatura máxima anual más baja posible
higher_maxtemp <- 24		# temperatura máxima anual más alta posible

# tamaño de la isla (dimensiones de la matriz)
xdim <- 100				# dimensión x
ydim <- 100				# dimensión y

# producción de bayas
max_berries <- 20			# máxima producción de bayas por individuo
dcold <- 1				# descenso en la producción de bayas por cada grado que descienda la Tª por debajo del rango mínimo de la espcie
dheat <- 2				# aumento en la producción de bayas por cada grado que ascienda la Tª por encima del rango máximo de la especie

# resistencia de las especies a la temperatura (rangos normales)
rangemax_Sp1 <- 28		# límite superior del rango normal de Tª (especie 1)
rangemin_Sp1 <- 15		# límite inferior del rango normal de Tª (especie 1)
rangemax_Sp2 <- 28		# límite superior del rango normal de Tª (especie 2)
rangemin_Sp2 <- 8			# límite inferior del rango normal de Tª (especie 2)

# probabilidad de que un fruto caiga en su propia parcela y no en las vecinas; de 0 (nunca) a 1 (siempre)
p_prop <- 5/10

# probabilidad de actuación del pájaro; de 0 (nunca actúa) a 1 (siempre actúa)
p_bird <- 1/20


## BUCLE DE RÉPLICAS

for (z in 1:replicas){

	ext <- "no"
	t_ext <- NA
	
	## INICIALIZACIÓN

	# para comenzar, se define una isla con las dos especies distribuidas de forma aleatoria

	ISLA <- matrix(0, nrow=xdim, ncol=ydim)
	n_Sp1 <- 0
	n_Sp2 <- 0
	for (i in 1:(round(2*xdim/5))){
		for (j in 1:ydim){
			ISLA[i,j] <- 1
			n_Sp1 <- n_Sp1 + 1
		}
	}
	for (i in ((round(3*xdim/5))+1):xdim){
		for (j in 1:ydim){
			ISLA[i,j] <- 2
			n_Sp2 <- n_Sp2 + 1
		}
	}

	tempmin_tmax <- rep(0, tmax)
	tempmax_tmax <- rep(0, tmax)
	N_Sp1 <- rep(0, tmax)		# vector para guardar la población de la Sp1 a lo largo del tiempo
	N_Sp2 <- rep(0, tmax)		# vector para guardar la población de la Sp2 a lo largo del tiempo

	for (i in 1:tmax){		# bucle de duración tmax; 1 paso del tiempo representa 1 año
		# temperatura mínima y máxima anual
		temp_min <- round(runif(1, lower_mintemp, higher_mintemp))
		temp_max <- round(runif(1, lower_maxtemp, higher_maxtemp))
		tempmin_tmax[i] <- temp_min
		tempmax_tmax[i] <- temp_max


		# producción anual de cada especie
		P_Sp1 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp1, rangemin_Sp1, temp_min, temp_max)
		P_Sp2 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp2, rangemin_Sp2, temp_min, temp_max)

		# separación en dos submatrices
		SP1 <- Submatrix_Sp("Sp1", ISLA, P_Sp1, P_Sp2, xdim, ydim)
		SP2 <- Submatrix_Sp("Sp2", ISLA, P_Sp1, P_Sp2, xdim, ydim)
	
		# repartición de los frutos
		SpreadSP1_sin_paj <- Spread(SP1, p_prop, xdim, ydim)
		SpreadSP2_sin_paj <- Spread(SP2, p_prop, xdim, ydim)
	
		# efecto de los pájaros
		SpreadSP1 <- Birds(SpreadSP1_sin_paj, p_bird, xdim, ydim)
		SpreadSP2 <- Birds(SpreadSP2_sin_paj, p_bird, xdim, ydim)
		Sum_SEEDS <- SpreadSP1 + SpreadSP2
	
		# cálculo de la nueva matriz poblacional
		ISLA <- matrix(0, nrow=xdim, ncol=ydim)		
		aux_Sp1 <- 0
		aux_Sp2 <- 0
		for (a in 1:xdim){
			for (b in 1:ydim){
				# cuando no hay semillas de ninguna planta, no se establece nada
				if (Sum_SEEDS[a, b] == 0){
					prob_nothing <- 1		# probabilidad de no establecimiento
					prob_Sp1 <- 0		# probabilidad de establecimiento de la especie 1
					prob_Sp2 <- 0		# probabilidad de establecimiento de la especie 2
				} else {
					# cuando hay una semilla (independientemente de la especie), la probabilidad de nada es 0.2
					# y la probabilidad de dicha especie es 0.8
					if (Sum_SEEDS[a, b] == 1){
						prob_nothing <- 0.2
						if (SpreadSP1[a, b] == 1 && SpreadSP2[a, b] == 0){
							prob_Sp1 <- 1 - prob_nothing
							prob_Sp2 <- 0
						} else {
							if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] == 1){
								prob_Sp1 <- 0
								prob_Sp2 <- 1 - prob_nothing
							}
						}
					} else {
						# cuando hay más de una semilla:
						if (Sum_SEEDS[a, b] > 1){
							prob_nothing <- 0.2 - ((Sum_SEEDS[a, b] - 1) * 0.01)	# la probabilidad de nada es 0.2 menos 0.01 por el número de semillas...
							# si la probabilidad de nada es cero o menor:
							if (prob_nothing <= 0){
								prob_nothing <- 0		# se fija la probabilidad de nada en cero
								# cuando las semillas son solamente de una especie, la probabilidad es 1
								if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
									prob_Sp1 <- 1
									prob_Sp2 <- 0
								} else {
									if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- 0
										prob_Sp2 <- 1
									} else {
										# cuando son de ambas especies a la vez, se calcula según las siguientes fórmulas
										if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
											prob_Sp1 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
											prob_Sp2 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										}
									}
								}
							} else { # cuando la probabilidad de nada es mayor que cero:
								# si solo hay semillas de una especie, la probabilidad es 1 menos la probabilidad de nada
								if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
									prob_Sp1 <- 1 - prob_nothing
									prob_Sp2 <- 0
								} else {
									if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- 0
										prob_Sp2 <- 1 - prob_nothing
									} else {
										# cuando hay de las dos especies, se calcula como se muestra a continuación:
										if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
											prob_Sp1 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
											prob_Sp2 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										}
									}
								}							
							}
						}
					}
				}
				if (prob_Sp1 > 1 || prob_Sp2 < 0){
					prob_Sp1 <- 1
					prob_Sp2 <- 0
				}
				aleat <- sample(c(0, 1, 2), size=1, prob=c(prob_nothing, prob_Sp1, prob_Sp2))
				if (aleat == 1){
					ISLA[a, b] <- 1
					aux_Sp1 <- aux_Sp1 + 1
				} else {
					if (aleat == 2){
						ISLA[a, b] <- 2
						aux_Sp2 <- aux_Sp2 + 1
					} else {
						if (aleat == 0){
							ISLA[a, b] <- 0
						}
					}
				}
			}
		}
		N_Sp1[i] <- aux_Sp1
		N_Sp2[i] <- aux_Sp2

		# estimar extinción
		if ((N_Sp1[i] == 0 && N_Sp1[i-1] != 0) || (N_Sp2[i] == 0 && N_Sp2[i-1] != 0)){
			ext <- "si"
			t_ext <- i
		}

	} # fin de i


	supuesto <- c(supuesto, sup)				##########
	tiempo <- c(tiempo, time)				##########
	pajaro <- c(pajaro, paj)				##########
	cat_temp <- c(cat_temp, temper)			##########

	Sp1_inicial <- c(Sp1_inicial, n_Sp1)		##########
	Sp2_inicial <- c(Sp2_inicial, n_Sp2)		##########

	tempmin_m <- c(tempmin_m, mean(tempmin_tmax))	##########
	tempmin_sd <- c(tempmin_sd, sd(tempmin_tmax))	##########
	tempmax_m <- c(tempmax_m, mean(tempmax_tmax))	##########
	tempmax_sd <- c(tempmax_sd, sd(tempmax_tmax))	##########
	
	pobl_max <- c(pobl_max, xdim*ydim)			##########

	Sp1_media <- c(Sp1_media, mean(N_Sp1))		##########
	Sp1_var <- c(Sp1_var, var(N_Sp1))			##########
	Sp1_sd <- c(Sp1_sd, sd(N_Sp1))			##########
	Sp2_media <- c(Sp2_media, mean(N_Sp2))		##########
	Sp2_var <- c(Sp2_var, var(N_Sp2))			##########
	Sp2_sd <- c(Sp2_sd, sd(N_Sp2))			##########
	Sp1_final <- c(Sp1_final, N_Sp1[tmax])		##########
	Sp2_final <- c(Sp2_final, N_Sp2[tmax])		##########

	extincion <- c(extincion, ext)			##########
	t_extincion <- c(t_extincion, t_ext)		##########


} # fin de z, fin de supuesto

# PLOT para saber el progreso del programa:
MATRIZ <- matrix(0, 5, 7)
MATRIZ[2,2] <- 1
MATRIZ[2,4] <- 1
MATRIZ[3,2] <- 1
MATRIZ[3,4] <- 1
MATRIZ[4,2] <- 1
MATRIZ[4,3] <- 1
MATRIZ[4,4] <- 1
MATRIZ[2,5] <- 1
MATRIZ[2,6] <- 1
MATRIZ[3,6] <- 1
MATRIZ[4,6] <- 1
levelplot(MATRIZ, xlab = "", ylab = "", main="", col.regions= c("white", "white", "white", "white", "white", "white", "white", "green", "green", "green", "green", "green", "green", "green", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))

# guarda en un fichero nuevo los datos obtenidos hasta el momento, por si hubiese algún problema:
ARJEPLOG5 <- data.frame(supuesto, tiempo, pajaro, cat_temp, Sp1_inicial, Sp2_inicial, tempmin_m, tempmin_sd, tempmax_m, tempmax_sd, pobl_max, Sp1_media, Sp1_var, Sp1_sd, Sp2_media, Sp2_var, Sp2_sd, Sp1_final, Sp2_final, extincion, t_extincion)
write.table(ARJEPLOG5, file="ARJEPLOG5.txt")


####################################################################
##### SUPUESTO 6 #####  tiempo: 100, pájaro: si, temp: alta       ##
####################################################################

sup <- "supuesto6"
time <- 100
paj <- "si"
temper <- "alta"

## VARIABLES MODIFICABLES

## en general

tmax <- 100				# tiempo de simulación

## específicamente

# rangos de temperatura anuales
lower_mintemp <- 16		# temperatura mínima anual más baja posible
higher_mintemp <- 24		# temperatura mínima anual más alta posible
lower_maxtemp <- 29		# temperatura máxima anual más baja posible
higher_maxtemp <- 37		# temperatura máxima anual más alta posible

# tamaño de la isla (dimensiones de la matriz)
xdim <- 100				# dimensión x
ydim <- 100				# dimensión y

# producción de bayas
max_berries <- 20			# máxima producción de bayas por individuo
dcold <- 1				# descenso en la producción de bayas por cada grado que descienda la Tª por debajo del rango mínimo de la espcie
dheat <- 2				# aumento en la producción de bayas por cada grado que ascienda la Tª por encima del rango máximo de la especie

# resistencia de las especies a la temperatura (rangos normales)
rangemax_Sp1 <- 28		# límite superior del rango normal de Tª (especie 1)
rangemin_Sp1 <- 15		# límite inferior del rango normal de Tª (especie 1)
rangemax_Sp2 <- 28		# límite superior del rango normal de Tª (especie 2)
rangemin_Sp2 <- 8			# límite inferior del rango normal de Tª (especie 2)

# probabilidad de que un fruto caiga en su propia parcela y no en las vecinas; de 0 (nunca) a 1 (siempre)
p_prop <- 5/10

# probabilidad de actuación del pájaro; de 0 (nunca actúa) a 1 (siempre actúa)
p_bird <- 1/20


## BUCLE DE RÉPLICAS

for (z in 1:replicas){

	ext <- "no"
	t_ext <- NA
	
	## INICIALIZACIÓN

	# para comenzar, se define una isla con las dos especies distribuidas de forma aleatoria

	ISLA <- matrix(0, nrow=xdim, ncol=ydim)
	n_Sp1 <- 0
	n_Sp2 <- 0
	for (i in 1:(round(2*xdim/5))){
		for (j in 1:ydim){
			ISLA[i,j] <- 1
			n_Sp1 <- n_Sp1 + 1
		}
	}
	for (i in ((round(3*xdim/5))+1):xdim){
		for (j in 1:ydim){
			ISLA[i,j] <- 2
			n_Sp2 <- n_Sp2 + 1
		}
	}

	tempmin_tmax <- rep(0, tmax)
	tempmax_tmax <- rep(0, tmax)
	N_Sp1 <- rep(0, tmax)		# vector para guardar la población de la Sp1 a lo largo del tiempo
	N_Sp2 <- rep(0, tmax)		# vector para guardar la población de la Sp2 a lo largo del tiempo

	for (i in 1:tmax){		# bucle de duración tmax; 1 paso del tiempo representa 1 año
		# temperatura mínima y máxima anual
		temp_min <- round(runif(1, lower_mintemp, higher_mintemp))
		temp_max <- round(runif(1, lower_maxtemp, higher_maxtemp))
		tempmin_tmax[i] <- temp_min
		tempmax_tmax[i] <- temp_max


		# producción anual de cada especie
		P_Sp1 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp1, rangemin_Sp1, temp_min, temp_max)
		P_Sp2 <- Production_Sp(max_berries, dcold, dheat, rangemax_Sp2, rangemin_Sp2, temp_min, temp_max)

		# separación en dos submatrices
		SP1 <- Submatrix_Sp("Sp1", ISLA, P_Sp1, P_Sp2, xdim, ydim)
		SP2 <- Submatrix_Sp("Sp2", ISLA, P_Sp1, P_Sp2, xdim, ydim)
	
		# repartición de los frutos
		SpreadSP1_sin_paj <- Spread(SP1, p_prop, xdim, ydim)
		SpreadSP2_sin_paj <- Spread(SP2, p_prop, xdim, ydim)
	
		# efecto de los pájaros
		SpreadSP1 <- Birds(SpreadSP1_sin_paj, p_bird, xdim, ydim)
		SpreadSP2 <- Birds(SpreadSP2_sin_paj, p_bird, xdim, ydim)
		Sum_SEEDS <- SpreadSP1 + SpreadSP2
	
		# cálculo de la nueva matriz poblacional
		ISLA <- matrix(0, nrow=xdim, ncol=ydim)		
		aux_Sp1 <- 0
		aux_Sp2 <- 0
		for (a in 1:xdim){
			for (b in 1:ydim){
				# cuando no hay semillas de ninguna planta, no se establece nada
				if (Sum_SEEDS[a, b] == 0){
					prob_nothing <- 1		# probabilidad de no establecimiento
					prob_Sp1 <- 0		# probabilidad de establecimiento de la especie 1
					prob_Sp2 <- 0		# probabilidad de establecimiento de la especie 2
				} else {
					# cuando hay una semilla (independientemente de la especie), la probabilidad de nada es 0.2
					# y la probabilidad de dicha especie es 0.8
					if (Sum_SEEDS[a, b] == 1){
						prob_nothing <- 0.2
						if (SpreadSP1[a, b] == 1 && SpreadSP2[a, b] == 0){
							prob_Sp1 <- 1 - prob_nothing
							prob_Sp2 <- 0
						} else {
							if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] == 1){
								prob_Sp1 <- 0
								prob_Sp2 <- 1 - prob_nothing
							}
						}
					} else {
						# cuando hay más de una semilla:
						if (Sum_SEEDS[a, b] > 1){
							prob_nothing <- 0.2 - ((Sum_SEEDS[a, b] - 1) * 0.01)	# la probabilidad de nada es 0.2 menos 0.01 por el número de semillas...
							# si la probabilidad de nada es cero o menor:
							if (prob_nothing <= 0){
								prob_nothing <- 0		# se fija la probabilidad de nada en cero
								# cuando las semillas son solamente de una especie, la probabilidad es 1
								if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
									prob_Sp1 <- 1
									prob_Sp2 <- 0
								} else {
									if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- 0
										prob_Sp2 <- 1
									} else {
										# cuando son de ambas especies a la vez, se calcula según las siguientes fórmulas
										if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
											prob_Sp1 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
											prob_Sp2 <- ((1 / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - (((1 / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										}
									}
								}
							} else { # cuando la probabilidad de nada es mayor que cero:
								# si solo hay semillas de una especie, la probabilidad es 1 menos la probabilidad de nada
								if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] == 0){
									prob_Sp1 <- 1 - prob_nothing
									prob_Sp2 <- 0
								} else {
									if (SpreadSP1[a, b] == 0 && SpreadSP2[a, b] != 0){
										prob_Sp1 <- 0
										prob_Sp2 <- 1 - prob_nothing
									} else {
										# cuando hay de las dos especies, se calcula como se muestra a continuación:
										if (SpreadSP1[a, b] != 0 && SpreadSP2[a, b] != 0){
											prob_Sp1 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP1[a, b]) + ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
											prob_Sp2 <- (((1 - prob_nothing) / Sum_SEEDS[a, b]) * SpreadSP2[a, b]) - ((((1 - prob_nothing) / Sum_SEEDS[a, b]) * Sum_SEEDS[a, b]) * 0.025)
										}
									}
								}							
							}
						}
					}
				}
				if (prob_Sp1 > 1 || prob_Sp2 < 0){
					prob_Sp1 <- 1
					prob_Sp2 <- 0
				}
				aleat <- sample(c(0, 1, 2), size=1, prob=c(prob_nothing, prob_Sp1, prob_Sp2))
				if (aleat == 1){
					ISLA[a, b] <- 1
					aux_Sp1 <- aux_Sp1 + 1
				} else {
					if (aleat == 2){
						ISLA[a, b] <- 2
						aux_Sp2 <- aux_Sp2 + 1
					} else {
						if (aleat == 0){
							ISLA[a, b] <- 0
						}
					}
				}
			}
		}
		N_Sp1[i] <- aux_Sp1
		N_Sp2[i] <- aux_Sp2

		# estimar extinción
		if ((N_Sp1[i] == 0 && N_Sp1[i-1] != 0) || (N_Sp2[i] == 0 && N_Sp2[i-1] != 0)){
			ext <- "si"
			t_ext <- i
		}

	} # fin de i


	supuesto <- c(supuesto, sup)				##########
	tiempo <- c(tiempo, time)				##########
	pajaro <- c(pajaro, paj)				##########
	cat_temp <- c(cat_temp, temper)			##########

	Sp1_inicial <- c(Sp1_inicial, n_Sp1)		##########
	Sp2_inicial <- c(Sp2_inicial, n_Sp2)		##########

	tempmin_m <- c(tempmin_m, mean(tempmin_tmax))	##########
	tempmin_sd <- c(tempmin_sd, sd(tempmin_tmax))	##########
	tempmax_m <- c(tempmax_m, mean(tempmax_tmax))	##########
	tempmax_sd <- c(tempmax_sd, sd(tempmax_tmax))	##########
	
	pobl_max <- c(pobl_max, xdim*ydim)			##########

	Sp1_media <- c(Sp1_media, mean(N_Sp1))		##########
	Sp1_var <- c(Sp1_var, var(N_Sp1))			##########
	Sp1_sd <- c(Sp1_sd, sd(N_Sp1))			##########
	Sp2_media <- c(Sp2_media, mean(N_Sp2))		##########
	Sp2_var <- c(Sp2_var, var(N_Sp2))			##########
	Sp2_sd <- c(Sp2_sd, sd(N_Sp2))			##########
	Sp1_final <- c(Sp1_final, N_Sp1[tmax])		##########
	Sp2_final <- c(Sp2_final, N_Sp2[tmax])		##########

	extincion <- c(extincion, ext)			##########
	t_extincion <- c(t_extincion, t_ext)		##########


} # fin de z, fin de supuesto

# PLOT para saber el progreso del programa:
MATRIZ <- matrix(0, 5, 7)
MATRIZ[2,2] <- 1
MATRIZ[2,3] <- 1
MATRIZ[2,4] <- 1
MATRIZ[3,2] <- 1
MATRIZ[3,4] <- 1
MATRIZ[4,2] <- 1
MATRIZ[4,3] <- 1
MATRIZ[4,4] <- 1
MATRIZ[2,5] <- 1
MATRIZ[2,6] <- 1
MATRIZ[3,6] <- 1
MATRIZ[4,6] <- 1
levelplot(MATRIZ, xlab = "", ylab = "", main="", col.regions= c("white", "white", "white", "white", "white", "white", "white", "green", "green", "green", "green", "green", "green", "green", "blue", "blue", "blue", "blue", "blue", "blue", "blue"))


### SALIDA DE DATOS COMPLETA

# guarda en un fichero nuevo los datos obtenidos hasta el momento, por si hubiese algún problema:
ARJEPLOG <- data.frame(supuesto, tiempo, pajaro, cat_temp, Sp1_inicial, Sp2_inicial, tempmin_m, tempmin_sd, tempmax_m, tempmax_sd, pobl_max, Sp1_media, Sp1_var, Sp1_sd, Sp2_media, Sp2_var, Sp2_sd, Sp1_final, Sp2_final, extincion, t_extincion)
write.table(ARJEPLOG, file="ARJEPLOG.txt")





##########################								####################################################################################
##########################  PARTE C : ESTADÍSTICA DESCRIPTIVA		####################################################################################
##########################								####################################################################################

library(graphics)

##### DATOS PARA LOS BOXPLOT (obtenidos de la parte B de este programa) #####

## DATOS T_TEMPLADA
Tiempo <- rep(100, 200)
Paj1 <- rep("no", 100)
Paj2 <- rep("si", 100)
Pajaro <- c(Paj1, Paj2)
Sp1 <- c(5397, 5280, 5496, 5407, 4895, 5133, 5073, 5072, 4884, 4783, 5091, 4984, 4751, 5098, 5276, 5301, 4835, 5226, 5195, 5559, 5372, 5267, 5079, 5250, 5231, 5266, 5236, 5102, 5246, 4777, 4995, 5266, 5173, 5143, 4912, 5244, 5176, 5160, 4965, 5268, 5563, 5323, 5205, 4784, 5218, 5561, 5184, 5148, 5275, 5165, 5132, 5216, 5143, 5090, 4981, 4986, 4981, 5383, 4604, 5239, 5097, 5167, 5125, 4829, 4892, 4858, 5154, 4991, 5404, 4989, 5362, 5258, 5215, 5134, 4750, 5273, 5373, 5570, 5021, 5004, 5002, 5330, 4832, 5500, 5038, 5179, 4948, 5126, 5151, 5290, 4911, 5151, 4972, 5682, 4909, 4796, 4880, 4991, 5115, 5157, 6533, 5781, 6973, 7217, 6473, 6758, 6221, 6723, 5811, 7361, 5698, 6031, 6948, 6595, 6486, 7150, 6428, 6153, 7042, 6297, 6452, 5734, 5999, 6364, 5525, 5637, 5829, 7510, 5769, 5837, 6965, 7403, 6300, 7561, 6582, 6136, 6138, 5816, 5258, 6911, 6025, 6268, 6360, 6869, 6967, 7021, 7009, 4796, 6973, 6152, 6386, 6357, 6458, 6883, 6323, 6906, 5653, 5596, 5394, 6595, 6173, 5865, 5552, 6417, 6630, 6703, 5724, 6303, 6413, 6136, 6495, 6188, 6842, 5527, 6159, 5873, 5489, 6258, 6018, 5598, 7156, 6042, 6482, 6823, 6751, 6219, 5706, 6265, 6723, 6509, 5353, 6546, 5047, 5995, 5866, 6645, 5987, 5851, 6289, 6422)
Sp2 <- c(4155, 4410, 4210, 4177, 4616, 4460, 4491, 4551, 4321, 4350, 4516, 4549, 4838, 4492, 4352, 4354, 4727, 4358, 4417, 4171, 4327, 4380, 4361, 4429, 4527, 4371, 4352, 4638, 4426, 4503, 4545, 4417, 4481, 4281, 4623, 4450, 4155, 4587, 4520, 4337, 4152, 4320, 4269, 4403, 4431, 4009, 4502, 4356, 4209, 4563, 4310, 4514, 4431, 4491, 4473, 4602, 4534, 4348, 4790, 4439, 4637, 4292, 4597, 4658, 4719, 4670, 4446, 4697, 4274, 4398, 4346, 4454, 4530, 4492, 4847, 4437, 4365, 4058, 4508, 4553, 4691, 4392, 4498, 4211, 4649, 4305, 4767, 4523, 4365, 4409, 4556, 4616, 4394, 4059, 4619, 4607, 4546, 4621, 4297, 4413, 2973, 3868, 2689, 2460, 3171, 2950, 3438, 2818, 3547, 2184, 4015, 3667, 2595, 2943, 3033, 2322, 3033, 3262, 2665, 3344, 2913, 3913, 3427, 3186, 3902, 3883, 3850, 2141, 3800, 3876, 2452, 2232, 3257, 2136, 3102, 3481, 3562, 3467, 4335, 2706, 3566, 3063, 3163, 2790, 2605, 2415, 2530, 4754, 2578, 3580, 3245, 3298, 3076, 2752, 2848, 2790, 3691, 4111, 3908, 2920, 3420, 3759, 3923, 3042, 2808, 2962, 3616, 3356, 3234, 3495, 2774, 3317, 2846, 3559, 3264, 3634, 4181, 3458, 3446, 3644, 2488, 3558, 2994, 2708, 2936, 3190, 3572, 3435, 2978, 2974, 4105, 3164, 4430, 3492, 3812, 2876, 3579, 3805, 3334, 3124)

T_Templada <- data.frame(Tiempo, Pajaro, Sp1, Sp2)


## DATOS T_BAJA
BAJA <- read.table(file="D:/BAJA.txt", header=TRUE)
Tiempo <- rep(100, 200)
Paj1 <- rep("no", 100)
Paj2 <- rep("si", 100)
Pajaro <- c(Paj1, Paj2)
Sp1 <- c(1809, 1896, 1531, 2062, 1698, 1728, 1899, 1861, 1985, 2058, 1986, 1951, 1967, 1885, 1862, 1906, 1870, 1725, 1927, 2006, 1865, 1683, 2017, 1753, 1890, 1879, 2059, 1872, 2070, 1919, 2049, 1769, 2014, 1684, 1804, 1830, 1958, 2167, 1904, 1931, 1570, 1712, 1851, 1636, 1707, 1992, 2059, 1989, 1748, 1837, 1764, 1903, 1705, 1956, 1787, 1636, 1845, 1919, 1834, 1792, 1768, 1705, 1936, 1863, 1795, 1922, 1791, 1642, 1970, 1825, 1991, 1835, 1921, 1913, 1878, 2099, 1861, 1923, 1837, 1881, 1917, 1729, 1991, 1966, 1880, 1703, 1967, 2092, 2029, 2145, 2042, 1652, 1822, 1835, 1846, 1936, 1862, 1889, 2108, 1885, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Sp2 <- c(7580, 7216, 7535, 7373, 7524, 7410, 7541, 7238, 7444, 7265, 7178, 7324, 7528, 7285, 7583, 7593, 7263, 7606, 7572, 7486, 7474, 7569, 7497, 7371, 7402, 7447, 7153, 7729, 7053, 7394, 7198, 7439, 7475, 7828, 7497, 7588, 7564, 7283, 7323, 7439, 7720, 7408, 7269, 7526, 7477, 7456, 7277, 7146, 7516, 7420, 7523, 7426, 7395, 7556, 7500, 7739, 7490, 7309, 7664, 7356, 7313, 7383, 7543, 7632, 7733, 7616, 7713, 7708, 7299, 7411, 7362, 7521, 7369, 7517, 7649, 7389, 7750, 7339, 7569, 7662, 7392, 7274, 7318, 7559, 7194, 7492, 7368, 7409, 7451, 7249, 7501, 7667, 7577, 7510, 7643, 7390, 7538, 7274, 7337, 7526, 9152, 9376, 9485, 9179, 9622, 9389, 9300, 9541, 9345, 9515, 9479, 9431, 9451, 9717, 9427, 9630, 9544, 9351, 9675, 9638, 9470, 9293, 9671, 9277, 9299, 9375, 9307, 9552, 9570, 9434, 9695, 9222, 9670, 9440, 9548, 9393, 9175, 9473, 9447, 9559, 9510, 9460, 9601, 9633, 9599, 9531, 9721, 9516, 9260, 9278, 9431, 9265, 9444, 9291, 9453, 9447, 9519, 9390, 9685, 9327, 9290, 9444, 9330, 9578, 9543, 9560, 9351, 9310, 9608, 9626, 9480, 9531, 9548, 9446, 9341, 9527, 9377, 9367, 9250, 9415, 9413, 9322, 9402, 9537, 9465, 9383, 9515, 9318, 9237, 9630, 9494, 9486, 9345, 9430, 9667, 9560, 9425, 9553, 9297, 9442)

T_Baja <- data.frame(Tiempo, Pajaro, Sp1, Sp2)


## DATOS T_ALTA
Tiempo <- rep(100, 200)
Paj1 <- rep("no", 100)
Paj2 <- rep("si", 100)
Pajaro <- c(Paj1, Paj2)
Sp1 <- c(5106, 5526, 5861, 4935, 5137, 5349, 4979, 4953, 5237, 5736, 5200, 5343, 5559, 4112, 5310, 4946, 4779, 5449, 5480, 5214, 3900, 4813, 4975, 4953, 5572, 5391, 5277, 5014, 5234, 5110, 5248, 5462, 5534, 5275, 4996, 4645, 5609, 5369, 5663, 5508, 5225, 5851, 4275, 4348, 5497, 5790, 5249, 5138, 5670, 5256, 5517, 5160, 5649, 5856, 5353, 5192, 5056, 5801, 4411, 5506, 5026, 5519, 5614, 5345, 4694, 5266, 5310, 4797, 5292, 5478, 5502, 5584, 5467, 5156, 5021, 4330, 5766, 5448, 5725, 4901, 5610, 5317, 5496, 5712, 4728, 5536, 4580, 5033, 5818, 5459, 4998, 5414, 5907, 5027, 5276, 5170, 5364, 5463, 5039, 5711, 8195, 8160, 9021, 8548, 8348, 8595, 9042, 8777, 9109, 8918, 8877, 8431, 7844, 9004, 9109, 7525, 8953, 7938, 8808, 8429, 8475, 8250, 8776, 8969, 6627, 8036, 8099, 8495, 8775, 7912, 8968, 7655, 8579, 7635, 7702, 8596, 8723, 7622, 8110, 8453, 8529, 7807, 8889, 8885, 7560, 9223, 7977, 8010, 8911, 8908, 6717, 8293, 8466, 9027, 8972, 9146, 8766, 8214, 8003, 8800, 6569, 7398, 8588, 8012, 8151, 8798, 6191, 7671, 8925, 7760, 9031, 8287, 8473, 8482, 8509, 8576, 8415, 7689, 8457, 8582, 9127, 9018, 8900, 8627, 8654, 8005, 8698, 8891, 8600, 9074, 8671, 8415, 8275, 8334, 8801, 8432, 8661, 8778, 8039, 8604)
Sp2 <- c(3397, 3602, 3730, 3402, 3391, 3527, 3373, 3331, 3271, 3476, 3306, 3447, 3341, 2660, 3172, 3160, 3319, 3548, 3208, 3217, 2671, 2956, 3149, 2939, 3494, 3406, 3507, 3396, 3377, 3183, 3375, 3851, 3288, 3451, 3293, 2860, 3555, 3337, 3669, 3324, 3338, 3525, 2928, 2775, 3487, 3737, 3328, 3223, 3342, 3286, 3385, 3461, 3491, 3424, 3479, 3302, 3524, 3393, 2659, 3338, 3493, 3647, 3857, 3705, 3178, 3133, 3346, 3323, 3158, 3501, 3565, 3520, 3207, 3134, 3211, 2614, 3558, 3594, 3551, 3146, 3438, 3281, 3318, 3436, 3212, 3409, 2628, 3275, 3536, 3352, 3248, 3550, 3602, 2997, 3287, 3301, 3612, 3625, 3271, 3545, 378, 285, 266, 439, 320, 582, 318, 369, 286, 335, 227, 209, 384, 429, 136, 305, 395, 293, 365, 194, 256, 271, 290, 257, 236, 291, 275, 338, 365, 247, 200, 295, 443, 318, 273, 316, 414, 348, 410, 226, 369, 167, 393, 475, 251, 268, 384, 380, 255, 290, 306, 396, 132, 348, 139, 270, 491, 313, 528, 536, 299, 552, 251, 436, 437, 337, 287, 195, 418, 300, 325, 322, 340, 293, 286, 376, 349, 386, 207, 428, 261, 317, 316, 250, 304, 245, 288, 185, 405, 261, 364, 498, 336, 277, 267, 223, 329, 270, 365, 385)

T_Alta <- data.frame(Tiempo, Pajaro, Sp1, Sp2)


##### BOXPLOT #####

### Temperaturas templadas ###
# Sp1:
boxplot(T_Templada$Sp1 ~ T_Templada$Pajaro, ylim=c(0, 10000), xlab = "Pájaro", ylab = "Población (individuos)", main = "Especie 1  -  Tª templada")
# Sp2:
boxplot(T_Templada$Sp2 ~ T_Templada$Pajaro, ylim=c(0, 10000), xlab = "Pájaro", ylab = "Población (individuos)", main = "Especie 2  -  Tª templada")

### Temparaturas bajas ###
# Sp1:
boxplot(T_Baja$Sp1 ~ T_Baja$Pajaro, ylim=c(0, 10000), xlab = "Pájaro", ylab = "Población (individuos)", main = "Especie 1  -  Tª baja")
# Sp2:
boxplot(T_Baja$Sp2 ~ T_Baja$Pajaro, ylim=c(0, 10000), xlab = "Pájaro", ylab = "Población (individuos)", main = "Especie 2  -  Tª baja")

### Temperaturas altas ###
# Sp1:
boxplot(T_Alta$Sp1 ~ T_Alta$Pajaro, ylim=c(0, 10000), xlab = "Pájaro", ylab = "Población (individuos)", main = "Especie 1  -  Tª alta")
# Sp2:
boxplot(T_Alta$Sp2 ~ T_Alta$Pajaro, ylim=c(0, 10000), xlab = "Pájaro", ylab = "Población (individuos)", main = "Especie 2  -  Tª alta")





##########################								####################################################################################
##########################  PARTE D : COMENTARIOS Y BIBLIOGRAFÍA		####################################################################################
##########################								####################################################################################

# Este programa describe un caso que no se corresponde enteramente con la realidad, salvo en algunos de sus detalles.
# Las dos especies de plantas arbustivas utilizadas están inspiradas en:
##### especie 1: "Vaccinium myrtilus",
##### especie 2: "Vaccinium vitis-idaea",
# que conviven junto a otras especies arbustivas (p. ej. "Empretrum hermaphroditum" o "Linnaea borealis") y leñosas
# (p. ej. "Picea abies", "Pinus sylvestris" y "Betula pubescens") en los bosques boreales europeos, y concretamente
# en islas en lagos al norte de Suecia (p. ej., lagos Hornavan y Uddjaure, en la localidad de Arjeplog).


# Véanse las siguientes referencias para una caracterización general de dichos los ecosistemas insulares boreales:

### Wardle, D.A., Zackrisson, O., Hörnberg, G. y Gallet, C. (1997). The influence of island area on ecosystem properties. Science, 277: 1296-1299.

# Interesante descripción de las especies arbustivas:
### Nilsson, M.-C. y Wardle, D.A. (2005). Understory vegetation as a forest ecosystem driver: evidence from the northern Swedish boreal forest. Front. Ecol. Env., 3(8): 421-428.

