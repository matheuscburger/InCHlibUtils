library(rjson)

#' Get the parent node of i
#'
#' @param i  node index
#' @param cluster a hclust object
#'
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' getParent(-15, hc)   # 1
getParent <- function(i, cluster){
    return(unname(which(cluster[['merge']] == i, arr.ind=TRUE)[, 1]))
}

#' Get the number of leafs in node i
#'
#' @param i       node index
#' @param cluster a hclust object
#'
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' getCount(-15, hc)    # 1
#' getCount(40, hc)     # 9
getCount <- function(i, cluster){
    count <- 0
    if( i < 0 ){
        count <- 1
    }else{
        for( j in 1:2 ){
            if(cluster[['merge']][i, j] < 0){
                count = count + 1
            }else{
                count = count + getCount(cluster[['merge']][i, j], cluster)
            }
        }
    }
    return(count)
}

#' Get a dendrogram using the InCHlib format
#'
#' @param cluster   a hclust object
#' @param leafNames a character list containing the names of each leaf
#' @param values    a data.frame containing the data values (if row dendrogram)
#'
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' hc.col <- hclust(dist(t(USArrests)), "ave")
#' getDendro(hc, rownames(USArrests), USArrests)
#' getDendro(hc.col, colnames(USArrests))
getDendro <- function(cluster, leafNames, values=NA){
    getName <- function(id, leafNames){
        if(length(id) == 1 && id < 0){
            return(leafNames[abs(id)])
        }else{
            return(paste("nodes@", id, sep=""))
        }
        
    }
    nodes <- list()
    isRow <- FALSE
    if(is.data.frame(values)){
        isRow <- TRUE
    }
    for(i in 1:nrow(cluster[['merge']])){
        cat("i>> ");cat(i); cat(", ")
        nodeName <- getName(i, leafNames)
        nodes[[nodeName]] <- list("left_child"=getName(cluster[['merge']][i, 1], leafNames),
                                  "right_child"=getName(cluster[['merge']][i, 2], leafNames),
                                  "count"=getCount(i, cluster),
                                  "distance"=cluster[['height']][i] 
                                  )
        if(length(parent <- getParent(i, cluster))) 
            nodes[[nodeName]][["parent"]] <- getName(parent)
        for(j in 1:2){
            cat("j>> ");cat(j);cat("\n")
            id <- cluster[['merge']][i, j]
            cat("id>> ");cat(id); cat("\n")
            print(cluster[['merge']][i, ])
            if( id < 0 ){
                leaf <- getName(id, leafNames)
                nodes[[leaf]] <- list(
                                     "count"=1,
                                     "parent"=nodeName,
                                     "distance"=0,
                                     "objects"=list("object_id")
                                     )
                if( isRow )
                    nodes[[leaf]][['features']] <- unname(as.numeric(values[leaf, ]))

            }
        }
    }
    return(nodes)
}


#' Get a metadata list using the InCHlib format
#'
#' @param meta    a data.frame containing the metadata
#'
getMetadata <- function(meta){
    return(lapply(apply(meta, 1, function(x) list(unname(x))), '[[', 1))
}

#' Create a InCHlib heatmap
#'
#' @param hclust.row   a hclust object
#' @param hclust.col   a hclust object
#' @param val.df       a data.frame containing the data values
#' @param meta.df      a data.frame containing the metadata
#'
#' @export
#'
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' hc.col <- hclust(dist(t(USArrests)), "ave")
#' inch <- InCHlib(hc, hc.col, USArrests)
#' writeLines(toJSON(inch), "heatmap.json")
InCHlib <- function(hclust.row, hclust.col, val.df, meta.df=NA){
    inch <- list('data'=list('nodes'=getDendro(hclust.row, rownames(val.df), val.df), 'feature_names'=colnames(val.df)),
                'column_dendrogram'=list('nodes'=getDendro(hclust.col, colnames(val.df))))
    if(is.data.frame(meta.df)){
        inch[['metadata']]=list("nodes"=getMetadata(meta.df), 'feature_names'=colnames(meta.df))
    }
    return(inch)
}

