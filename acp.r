t <- matrix(c(1, 2, 3, 4, 9, 5, 10, 8, 8, 12), nrow = 5, ncol = 2)
print(t)

moyen_x1 <- mean(t[, 1])
moyen_x2 <- mean(t[, 2])
print("les moyens")
print(moyen_x1)
print(moyen_x2)

variance <- function(x) {
  return(var(x) * (length(x) - 1) / length(x))
}
ecart_type <- function(x) {
  return(sqrt(variance(x)))
}


print("les variances")
variance_x1 <- variance(t[, 1])
variance_x2 <- variance(t[, 2])
print(variance_x1)
print(variance_x2)

print("les ecart_types")
ecart_type_x1 <- ecart_type(t[, 1])
ecart_type_x2 <- ecart_type(t[, 2])
print(ecart_type_x1)
print(ecart_type_x2)

xc <- matrix(c((t[, 1] - moyen_x1), (t[, 2] - moyen_x2)), nrow = 5, ncol = 2)
print(xc)

xct <- t(xc)
matrice_variance_covariance <- function(xc) {
  return((1 / length(xc[, 1])) * xct %*% xc)
}

v <- matrice_variance_covariance(xc)
print("matrice Variance Covariance")
print(v)

matrice_centre_reduit <- function(xc, ecart_types, i, j) {
  d <- diag(1, i, j) / ecart_types
  return(xc %*% d)
}

ecart_types <- c(ecart_type_x1, ecart_type_x2)
print(ecart_types)

xcr <- matrice_centre_reduit(xc = xc, ecart_types = ecart_types, i = 2, j = 2)
print("matrice reduit centre:")
print(xcr)

matrice_correlation <- function(xcr) {
  return((1 / length(xcr[, 1])) * (t(xcr) %*% xcr))
}
r <- matrice_correlation(xcr)
print("matrice correlation")
print(r)

valeur_propr <- eigen(r)$values
vector_propr <- eigen(r)$vectors
print("valeur_proprs :")
print(valeur_propr)
print("vector_proprs :")
print(vector_propr)

composent_principals <- function(xcr) {
  return(xcr %*% vector_propr)
}
cp <- composent_principals(xcr)
print("composent_principals:")
print(cp)
cordonnee_variables <- function(valeur_propr, vector_propr) {
  return(vector_propr * sqrt(valeur_propr))
}
coordonnees <- cordonnee_variables(valeur_propr, vector_propr)
print(coordonnees)
