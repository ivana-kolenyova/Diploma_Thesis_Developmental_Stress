TPSsiet <- function(X, Y, n) {
    # X(vzor)
    # Y(obraz)
    # pocet uzlov siete = n
    xm <- min(Y[,1])
    ym <- min(Y[,2])
    xM <- max(Y[,1])
    yM <- max(Y[,2])
    rX <- xM - xm ; rY <- yM - ym
    a <- seq(xm - 1 / 5 * rX, xM + 1 / 5 * rX, length = n)
    b <- seq(ym - 1 / 5 * rX, yM + 1 / 5 * rX, by =(xM - xm) * 7 /(5 *(n -1)))
    m <- round(0.5 +(n -1) *(2 / 5 * rX + yM - ym) /(2 / 5 * rX + xM - xm))
    M <- as.matrix(expand.grid(a, b))
    ngrid <- tps2d(M,X, Y)
    plot(ngrid, type ="n", cex =0.2, asp =1, axes = FALSE, xlab ="", ylab ="")
    for(i in 1:m) lines(ngrid[(1:n) +(i -1) *n,], col ="grey")
    for(i in 1:n) lines(ngrid[(1:m) * n - i +1,], col ="grey")
}

tps2d <- function(M, X, Y) {
    # M = uzly siete(pocet uzlov = n)
    # X(vzor)
    # Y(obraz)
    k <- dim(X)[1]; n <- dim(M)[1]
    P <- matrix(NA, k, k)
    for(i in 1:k) {
        for(j in 1:k) {
            r2 <- sum((X[i,] - X[j,]) ^2)
            P[i, j] <- r2 * log(r2)
        }
    }
    P[which(is.na(P))] <- 0
    Q <- cbind(1, X)
    L <- rbind(cbind(P, Q), cbind(t(Q), matrix(0,3,3)))
    Y2 <- rbind(Y, matrix(0, 3, 2))
    coefx <- solve(L) %*% Y2[,1]
    coefy <- solve(L) %*% Y2[,2]

    fx <- function(X, M, coef) {
        Xn <- numeric(n)
        for(i in 1:n) {
        W <- apply((X - matrix(M[i,],k,2, byrow = TRUE)) ^2,1, sum)
        Xn[i] <- coef[k +1]+ coef[k +2] * M[i,1]+ coef[k +3] * M[i,2]+ sum(coef[1:k] *(W * log(W)))
        }
        Xn
    }
    Ytps <- matrix(NA, n, 2)
    Ytps[,1] <- fx(X, M, coefx)
    Ytps[,2] <- fx(X, M, coefy)
    return(Ytps)
}

uhol.2D <- function(v1 , v2) {
    # uhol dvoch vektorov
    cos.uhol <- sum(v1 * v2) /(sqrt(sum(v1 ^2)) * sqrt(sum(v2 ^2)))
    uhol <- acos(cos.uhol)
    return(uhol)
}
