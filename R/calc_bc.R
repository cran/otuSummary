calc_bc <- function(df){
    ## please note that the OTU table (df) must be with sites in rows and species in columns
    nr = nrow(df)
    si = rownames(df)
    mat = matrix(nrow = nr, ncol = nr, byrow = FALSE)
    colnames(mat) = rownames(mat) = si
    # calc rowSums
    rsum = rowSums(df,na.rm = TRUE)
    rsum_mat = as.dist(outer(rsum, rsum, FUN = "+"))
    dis_mat = dist(df, method = 'minkowski', diag = FALSE, upper = FALSE, p=1)
    ## return values of bray-curtis
    return(dis_mat/rsum_mat)
}


