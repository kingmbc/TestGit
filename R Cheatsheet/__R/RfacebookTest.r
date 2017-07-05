#http://blog.revolutionanalytics.com/2013/11/how-to-analyze-you-facebook-friends-network-with-r.html

install.packages("Rfacebook");
install.packages("igraph");
require(Rfacebook)
fb_oauth <- fbOAuth(app_id="959118060779353", 
                    app_secret="533b4fe95e52dc33ea7da4df1b2a2cc4",
                    extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth.Rd")
load("fb_oauth.Rd")

me <- getUsers("me", token=fb_oauth)
my_friends <- getFriends(token=fb_oauth, simplify=TRUE)
my_friends_info <- getUsers(my_friends$id, token=fb_oauth, private_info=TRUE)
my_network <- getNetwork(fb_oauth, format="adj.matrix")
singletons <- rowSums(my_network)==0 # friends who are friends with me alone
?
require(igraph)
my_graph <- graph.adjacency(my_network[!singletons,!singletons])
layout <- layout.drl(my_graph,options=list(simmer.attraction=0))
plot(my_graph, vertex.size=2, 
     #vertex.label=NA, 
     vertex.label.cex=0.5,
     edge.arrow.size=0, edge.curved=TRUE,layout=layout)
