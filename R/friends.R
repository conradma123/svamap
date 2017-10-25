##' Find the friends of a PPN based on being reported in the same
##' ownership chain.
##'
##' \code{friends} calculated the distance between PPNs in a network
##' based on ownership structure. A PPN has a reported owner or animal
##' manager. One PPN can have multiple people associated and these
##' people could also be associated with multiple PPNs. This function
##' finds the social network chains that exist in this data. For
##' example John and Alice own farm "A"; Alice and Jane own farm "B";
##' Jane and Bill own farm "C". Ann and Henry own farm "D". In this
##' case Farm "A" and "B" have a distance of 1 and A and C a distance
##' of 2. Farm D does not have any connections. These relationships
##' may reveal links between herds that are not reported in the animal
##' movement data.
##' @title friends
##' @param df A dataframe read from the PPN data in rapportportalen
##' @return list A list of dataframes equal in length to the total
##'     unique PPNs.
##' @importFrom igraph graph_from_edgelist
##' @importFrom igraph distances
##' @importFrom Matrix Matrix
##' @export
##' @author Thomas Rosendal
##'
##' @examples
##' \dontrun{
##' df <- read.csv(file = "/media/ubuntu1/PPN_records.csv",
##'                sep=";", header=T, stringsAsFactors = FALSE,
##'                dec=",", encoding = "UTF-8")
##' df <- df[1:1000,]
##' friends <- friends(df)
##' }
friends <- function(df) {
    ## Just keep the gällande
    df <- df[df$Platsstatus == "Gällande", ]
    ## Create a hash to match by
    df$hash <- paste0(trimws(df$Namn), trimws(df$Mobilnummer), trimws(df$Telefonnummer.1))
    df <- df[, c("Ppn", "hash")]
    ## Create an edge list
    friends <- do.call("rbind", tapply(df$Ppn, as.factor(df$hash), function(x){
        expand.grid(unique(x), unique(x))[
            as.vector(
                upper.tri(
                matrix(NA, nrow = length(unique(x)), ncol = length(unique(x))),
                diag = FALSE)
            ), ]
    }))
    ## Get rid of duplicated edges
    friends <- t(apply(friends, 1, function(x){
        as.character(x)
    }))
    friends <- friends[!duplicated(friends), ]
    ## Convert to graph
    friends <- graph_from_edgelist(friends, directed = FALSE)
    ## Measure the distance
    friends <- distances(friends)
    friends <- friends + 1
    friends[is.infinite(friends)] <- 0
    friends <- Matrix::Matrix(friends, sparse = TRUE)
    ## Convert thsi to a series of dataframes
    apply(friends, 1,  function(x){
        data.frame(ppn = names(x[which(x > 0)]),
                   distance = x[which(x > 0)] - 1, stringsAsFactors = FALSE,
                   row.names = NULL)
    })
}
