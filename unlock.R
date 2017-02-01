require(tabulizer)
require(stringr)
require(plyr)

check <- function(line) {
    ## timeline
    if (sum(str_detect(line, "[0-9]{2}:[0-9]{2}")) / length(line) > 0.5 ) {
        return(0)
    }
    ## station name
    if (str_detect(line, "^[A-Z][0-9]+ ")[1]) {
        return(1)
    }
    ## percentline
    if (sum(str_detect(line, "\\.")) / length(line) > 0.5) {
        return(2)
    }
    ## dataline
    if (sum(str_detect(line, "[0-9]{1,10} ?,?[0-9]{1,10}")) / length(line) > 0.5) {
        return(3)
    }
    ## garbage
    return(4)
}


cleancube <- function(cube, percentage = FALSE) {
    if (is.vector(cube)) {
        datacube2 <- sapply(cube, str_split, pattern = " +")
        datacube2 <- do.call(rbind, datacube2)
        finaldata <- datacube2[,colSums(apply(datacube2, 2, function(x) x == "")) == 0]
        return(finaldata)
    }
    if (percentage) {
        cube <- apply(cube, 2, str_trim)
    }
    datacube2 <- t(apply(cube, 1, function(x) unlist(str_split(x, " +"))))
    finaldata <- datacube2[,colSums(apply(datacube2, 2, function(x) x == "")) == 0]
    return(finaldata)
}


extract <- function(p) {
    out1 <- extract_tables("./2016-LCE-VT-by-PS.pdf", pages = c(p,p+1))
    checkres <- apply(out1[[1]], 1, check)
    sname <- str_trim(out1[[1]][checkres==1,][,1])
    p1data <- cleancube(out1[[1]][checkres==3,])
    if (length(out1) > 1) {
        checkres2 <- apply(out1[[2]], 1, check)
        p2data <- cleancube(out1[[2]][checkres2==3,])
        if (nrow(p2data) != nrow(p1data)) {
            missingrow <- nrow(p1data) - nrow(p2data)
            filler <- matrix(rep(NA, ncol(p2data) * missingrow), nrow = missingrow)
            p2data <- rbind(p2data, filler)
        }
        cbind(sname, p1data, p2data)
    } else {
        cbind(sname, p1data)
    }
}



do_extract <- function(p) {
    res1 <- tryCatch({
        print(p)
        res <- extract(p)
        write.csv(res, paste0("res", p, ".csv"), row.names = FALSE)
        p
    }, error = function(e) {
        print(error)
        NA
    })
    return(res1)
}



res <- sapply(seq(1, 163, by = 2), do_extract)

coln <- sapply(seq(1, 163, by = 2), function(x) ncol(read.csv(paste0('res', x, '.csv'))))

### irratic pages # fix by hand 29, 117, 163
seq(1,163, by = 2)[coln != 18]


readdata <- function(x) {
    res <- read.csv(paste0('res', x, '.csv'), stringsAsFactors = FALSE)
    colnames(res) <- c("sname", paste0('c', 1:17))
    return(res)
}

everything <- ldply(seq(1,163, by = 2), readdata)

write.csv(everything[,-15], "everthing.csv", row.names = FALSE)
