#trim results function --------------------------------------
#' Trim Ontology Results
#'
#'
#' Trims off the namespace from ontology items.
#'
#' This function trims off the namespace information from
#' ontology items. It should only be used when an rdfs:label has not
#' been specified for the class or individual.
#'
#' @param x an object of class \code{vector} that contains ontology class
#' or individual names with the namespace attached.
#'
#'
#' @return trimmed_results a \code{vector} object that only contains the class
#' or individual names.
#'
#' @examples
#' \dontrun{
#' x <- "<http://aopkb.org/aop_ontology#Steatosis>"
#' trim_results(x)
#' }
#'
#' @export
trim_results <- function(x){
  trimmed_results <- gsub(".*#(.*)>", "\\1", x, perl=TRUE, ignore.case=TRUE)
}


#list_adverse_outcomes --------------------------------------
#' List Adverse Outcomes
#'
#' Lists all of the adverse outcomes currently in the AOPO
#'
#' This function queries the AOPO for all of the available adverse outcomes.
#'
#' @param url a \code{character} object that is the URL to the Fuseki server. The
#' default is http://localhost:3030/ds
#'
#' @return adverse_outcomes a \code{vector} object that only contains the
#' adverse outcomes currently in the AOPO.
#'
#' @examples
#' \dontrun{
#' list_adverse_outcomes()
#' }
#'
#'
#' @importFrom SPARQL SPARQL
#'
#' @export
list_adverse_outcomes <- function(url="http://localhost:3030/ds"){
  query = "
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  PREFIX aop: <http://aopkb.org/aop_ontology#>
  PREFIX bao: <http://www.bioassayontology.org/bao#>

  SELECT DISTINCT ?ao_label
  WHERE {
    ?ao rdfs:subClassOf* aop:AdverseOutcome.
    ?ind_ao a ?ao.
    ?ind_ao rdfs:label ?ao_label.
  }
  "
  aos <- SPARQL(url, query=query)
  adverse_outcomes <- as.vector(t(aos$results[1,]))
  return(adverse_outcomes)
}


#get_aopn --------------------------------------
#' Get an Adverse Outcome Pathway Network (AOPN)
#'
#' Lists all of the key events within an AOPN
#'
#' This function queries the AOPO for all of the key events associated with
#' an adverse outcome network so that the network can be built.
#'
#' @param ao_name an object of class \code{character} that is the name of the
#' adverse outcome of interest.
#'
#' @param url a \code{character} object that is the URL to the Fuseki server. The
#' default is http://localhost:3030/ds
#'
#' @return aopn_graph an \code{igraph} object that contains the AOPN -- a graph
#' of the key events and their relationships.
#'
#' @examples
#' \dontrun{
#' x <- get_aopn("Steatosis")
#' plot(x)
#' }
#'
#' @import igraph
#' @import RCurl
#' @import XML
#' @importFrom SPARQL SPARQL
#'
#' @export
get_aopn <- function(ao_name, url="http://localhost:3030/ds"){
  query_prefix = "
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  PREFIX aopo: <http://aopkb.org/aop_ontology#>

  SELECT DISTINCT ?upke ?downke
  WHERE {
    ?aop rdfs:subClassOf* aopo:AdverseOutcomePathway.
    ?ind_aop a ?aop.
    ?ind_aop aopo:has_key_event_relationship ?ker.
    ?ker aopo:has_upstream_key_event ?upke.
    ?ker aopo:has_downstream_key_event ?downke.
    ?ind_aop aopo:has_adverse_outcome ?ao.
    ?ao rdfs:label ?ao_label.

    FILTER regex(?ao_label,"

  query <- paste(query_prefix, "\"", ao_name, "\",", "\"i\").\n}", sep="")
  aopn <- SPARQL(url, query=query)
  aopn_kes <- apply(aopn$results, 2, trim_results)
  aopn_graph <- graph_from_edgelist(aopn_kes)
  return(aopn_graph)
}

#send_aopn --------------------------------------
#' Send Adverse Outcome Pathway Network (AOPN) to Cytoscape
#'
#' Sends an AOPN to Cytoscape for visualization
#'
#' This function sends an AOPN to Cytoscape for visualization
#'
#' @param cytoscape_aopn an object of class \code{character} that is a JSON
#' encoded AOPN
#'
#' @param url an object of class \code{character} that is the base URL for
#' connecting to Cytoscape via CyREST. Default is http://localhost:1234/v1.
#'
#' @return res an result object.
#'
#' @examples
#' \dontrun{
#' x <- get_aopn("Steatosis")
#' cytoscape_aopn <- toCytoscape(x)
#' send_aopn(cytoscape_aopn)
#'}
#'
#' @import igraph
#' @import httr
#'
#' @export
send_aopn <- function(cytoscape_aopn, url = "http://localhost:1234/v1"){
  network_url <- paste(url, "networks", sep="/")
  res <- httr::POST(url=network_url, body=cytoscape_aopn, encode="json")
  return(res)
}

#get_aopn_kes --------------------------------------
#' Get the AOPN Key Events
#'
#' Retrieve the AOPN Key Events
#'
#' This function retrieves the AOPN key events
#'
#' @param aopn_graph an object of class \code{igraph} that contains the
#' AOPN -- a graph of the key events and their relationships.
#'
#' @return aopn_kes a \code{vector} object that contains the AOPN key events
#'
#' @examples
#' \dontrun{
#' x <- get_aopn("Steatosis")
#' get_aopn_kes(x)
#'}
#'
#' @import igraph
#'
#' @export
get_aopn_kes <- function(aopn_graph){
  aopn_kes <- V(aopn_graph)$name
  return(aopn_kes)
}

#associate_data_aopn --------------------------------------
#' Associate data with the AOPN
#'
#' Associate data with the AOPN Key Events
#'
#' This function associates data with the AOPN key events.
#'
#' @param x a \code{list} object of data vectors (each list element is a
#' different KE, or \code{vector} where the first column is the gene
#' symbol from the AOPN, and the subsequent columns are different conditions,
#' and the data within the table is some sort of response
#'
#' @param aopn_graph an object of class \code{igraph} that contains the
#' AOPN -- a graph of the key events and their relationships.
#'
#' @return aopn_graph an object of class \code{igraph} that contains the
#' AOPN -- a graph of the key events and their relationships, as well as the
#' associated data
#'
#' @examples
#' \dontrun{
#' x <- get_aopn("Steatosis")
#' aopn_kes <- get_aopn_kes(x)
#' expression_data <- c(2, 10, 10, NA, 3, 1, 1, 3, 2, 3, 2)
#' associate_data_aopn(expression_data, x)
#'}
#'
#' @import igraph
#'
#' @export
associate_data_aopn <- function(x, aopn_graph){
  V(aopn_graph)$data <- x
  return(aopn_graph)
}

#toCytoscape --------------------------------------
#' Converts AOPN Igraph Object to CytoscapeJSON
#'
#' Converts the AOPN to CytoscapeJSON
#'
#' This function converts an igraph network to a cytoscape JSON network
#'
#' @param igraphobj an object of class \code{igraph} that is the AOPN of
#' interest
#'
#' @return cytoscape_aopn a \code{character} object that contains the JSON encoded AOPN
#'
#' @examples
#' \dontrun{
#' x <- get_aopn("Steatosis")
#' cytoscape_aopn <- toCytoscape(x)
#'}
#'
#' @import igraph
#' @import RJSONIO
#'
#' @export
toCytoscape <- function (igraphobj) {
  # Extract graph attributes
  graph_attr = graph.attributes(igraphobj)

  # Extract nodes
  node_count = length(V(igraphobj))
  if('name' %in% list.vertex.attributes(igraphobj)) {
    V(igraphobj)$id <- V(igraphobj)$name
  } else {
    V(igraphobj)$id <- as.character(c(1:node_count))
  }

  nodes <- V(igraphobj)
  v_attr = vertex.attributes(igraphobj)
  v_names = list.vertex.attributes(igraphobj)

  nds <- array(0, dim=c(node_count))
  for(i in 1:node_count) {
    if(i %% 1000 == 0) {
      print(i)
    }
    nds[[i]] = list(data = mapAttributes(v_names, v_attr, i))
  }

  edges <- get.edgelist(igraphobj)
  edge_count = ecount(igraphobj)
  e_attr <- edge.attributes(igraphobj)
  e_names = list.edge.attributes(igraphobj)

  attr_exists = FALSE
  e_names_len = 0
  if(identical(e_names, character(0)) == FALSE) {
    attr_exists = TRUE
    e_names_len = length(e_names)
  }
  e_names_len <- length(e_names)

  eds <- array(0, dim=c(edge_count))
  for(i in 1:edge_count) {
    st = list(source=toString(edges[i,1]), target=toString(edges[i,2]))

    # Extract attributes
    if(attr_exists) {
      eds[[i]] = list(data=c(st, mapAttributes(e_names, e_attr, i)))
    } else {
      eds[[i]] = list(data=st)
    }

    if(i %% 1000 == 0) {
      print(i)
    }
  }

  el = list(nodes=nds, edges=eds)

  x <- list(data = graph_attr, elements = el)
  print("Done.  To json Start...")
  cytoscape_aopn <- toJSON(x)
  return (cytoscape_aopn)
}

#getCommunityEdge --------------------------------------
#' Returns edge attributes for member edges
#'
#' Returns edge attributes for member edges.
#'
#' This function returns edge attributes for member edges. This was taken from
#' the CyREST cytoscape_util.R file.
#'
#' @param g an object of class \code{igraph} that is the AOPN of
#' interest
#'
#' @param community the community
#'
#' @return edge.community
#'
#' @import igraph
#'
#' @export
getCommunityEdge <- function(g, community) {
  num.edges <- ecount(g)
  edge.community <- array(0, dim=c(num.edges))
  edges <- get.edges(g, 1:num.edges)
  comms <- array(community)
  sources <- array(edges[,1])
  targets <- array(edges[,2])
  for(i in 1:num.edges) {
    if(i %% 1000 == 0) {
      print(i)
    }
    sidx <- sources[i]
    tidx <- targets[i]
    source <- comms[sidx]
    target <- comms[tidx]

    if(source == target) {
      edge.community[[i]] <- source
    }
  }
  return(edge.community)
}

#mapAttributes --------------------------------------
#' Maps attributes
#'
#' Maps attributes
#'
#' Maps attributes.
#'
#' @param attr.names the attribute names
#'
#' @param all.attr all of the attributes
#'
#' @param i not sure
#'
#' @return attr
#'
#' @import igraph
#'
#' @export
mapAttributes <- function(attr.names, all.attr, i) {
  attr = list()
  cur.attr.names = attr.names
  attr.names.length = length(attr.names)

  for(j in 1:attr.names.length) {
    if(is.na(all.attr[[j]][i]) == FALSE) {
      #       attr[j] = all.attr[[j]][i]
      attr <- c(attr, all.attr[[j]][i])
    } else {
      cur.attr.names <- cur.attr.names[cur.attr.names != attr.names[j]]
    }
  }
  names(attr) = cur.attr.names
  return (attr)
}



