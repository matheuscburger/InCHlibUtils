
#' Get the parent node of i
#'
#' @param i  node index
#' @param cluster a hclust object
#'
#' @examples
#' #hc <- hclust(dist(USArrests), "ave")
#' #getParent(-15, hc)   # 1
getParent <- function(i, cluster){
    return(unname(which(cluster[['merge']] == i, arr.ind=TRUE)[, 1]))
}

#' Get the number of leafs in node i
#'
#' @param i       node index
#' @param cluster a hclust object
#'
#' @examples
#' #hc <- hclust(dist(USArrests), "ave")
#' #getCount(-15, hc)    # 1
#' #getCount(40, hc)     # 9
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
#' #hc <- hclust(dist(USArrests), "ave")
#' #hc.col <- hclust(dist(t(USArrests)), "ave")
#' #getDendro(hc, rownames(USArrests), USArrests)
#' #getDendro(hc.col, colnames(USArrests))
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
        nodeName <- getName(i, leafNames)
        nodes[[nodeName]] <- list("left_child"=getName(cluster[['merge']][i, 1], leafNames),
                                  "right_child"=getName(cluster[['merge']][i, 2], leafNames),
                                  "count"=getCount(i, cluster),
                                  "distance"=cluster[['height']][i] 
                                  )
        if(length(parent <- getParent(i, cluster))) 
            nodes[[nodeName]][["parent"]] <- getName(parent)
        for(j in 1:2){
			id <- cluster[['merge']][i, j]
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

#' Create an InCHlib heatmap
#'
#' @param hclustRow   a hclust object
#' @param hclustCol   a hclust object
#' @param valDf       a data.frame containing the data values
#' @param metaDf      a data.frame containing the metadata
#'
#' @export
#'
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' hc.col <- hclust(dist(t(USArrests)), "ave")
#' inch <- InCHlib(hc, hc.col, USArrests)
#' library(rjson)
#' writeLines(toJSON(inch), "heatmap.json")
InCHlib <- function(hclustRow, hclustCol, valDf, metaDf=NA){
	# reorder cols
	valDf <- valDf[, hclustCol[['order']]]
    inch <- list('data'=list('nodes'=getDendro(hclustRow, rownames(valDf), valDf), 'feature_names'=colnames(valDf)),
                'column_dendrogram'=list('nodes'=getDendro(hclustCol, colnames(valDf))))
	# reorder columns dendrogram
	inch[['column_dendrogram']] <- inch[['column_dendrogram']][c(colnames(valDf), grep("nodes@", names(inch[['column_dendrogram']])))]
    if(is.data.frame(metaDf)){
        inch[['metadata']]=list("nodes"=getMetadata(metaDf), 'feature_names'=colnames(metaDf))
    }
	class(inch) <- "InCHlib"
    return(inch)
}

