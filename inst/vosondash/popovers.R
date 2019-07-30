po_reseed_graph <- function() {
  title <- "Re-seed Graph"
  content <- "Generate a new random number to seed the graph layout."
  
  list(title = title, content = content)
}

po_graph_layout <- function() {
  title <- "Graph Layout Algorithms"
  content <- paste0("<code>FR</code> Fruchterman-Reingold<br>",
                    "<code>KK</code> Kamada-Kawai<br>",
                    "<code>DH</code> Davidson-Harel<br>",
                    "<code>LGL</code> Large Graph Layout<br>",
                    "<code>DrL</code> Distributed Recursive Layout<br>",
                    "<code>GEM</code> GEM Force-Directed Layout<br>",
                    "<code>MDS</code> Multidimensional Scaling Layout<br><br>",
                    "<i class = 'fa fa-book-reader'></i>&nbsp;&nbsp;",
                    "<a href = 'https://igraph.org/c/doc/igraph-Layout.html' target = '_blank'>igraph Layouts</a>"
                    )
  
  list(title = title, content = content)
}

po_cat_filter <- function() {
  title <- "Categorical Filter"
  content <- paste0("Categorical variables are identified by graph vertex attributes with names that begin with the ",
                    "prefix code <code>vosonCA_</code>. VOSON Dash does not provide an interface for adding ",
                    "these vertex attributes at this time, so they must be added to the graph in a seperate data ",
                    "coding process.<br><br>",
                    "When found these variables appear in the <code>Category</code> select list and can be used to ",
                    "filter graph vertices using the list of category values under <code>View</code>.")
  
  list(title = title, content = content)
}

po_twit_query <- function() {
  title <- "Twitter Search Query"
  content <- paste0("A full-range of search operators and filters can be used in the <code>Search Query</code> input, ",
                    "or search terms can be entered and used with the provided controls, or a combination of both.",
                    "<br><br>",
                    "<i class = 'fa fa-book-reader'></i>&nbsp;&nbsp;",
                    "<a href = 'https://developer.twitter.com/en/docs/tweets/rules-and-filtering/overview/standard-",
                    "operators' target = '_blank'>Twitter Standard Search Operators</a>"
                    )
  
  list(title = title, content = content)
}

po_twit_id_range <- function() {
  title <- "Twitter ID Range"
  content <- paste0("Set the bounds of a search. <code>Since ID</code> requests the twitter API to return only ",
                    "tweets tweeted after a particular tweet or status ID. <code>Max ID</code> requests the return ",
                    "of only tweets tweeted before a tweet or status ID."
  )
  
  list(title = title, content = content)
}

po_twit_lang <- function() {
  title <- "Tweet Language"
  content <- paste0("Requests the twitter API to return tweets in the language entered as two character ",
                    "<code>ISO 639-1</code> code. Language detection is best-effort.<br><br>",
                    "<i class = 'fa fa-book-reader'></i>&nbsp;&nbsp;",
                    "<a href = 'https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes'",
                    "target = '_blank'>ISO 639-1 Language Codes</a>"
  )
  
  list(title = title, content = content)
}
