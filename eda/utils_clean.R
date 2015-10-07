# Funci?n para definir variables categ?ricas como factores
# gracias a Carlos, Amanda y Liliana (itam-dm, 2014)
factorClass <- function(base){
    tipos <- lapply(base, function(x) class(x))
    i <- which(ifelse(tipos == 'character', TRUE, FALSE))
    base[i] <- lapply(base[i], function(x) as.factor(x))
    base
}