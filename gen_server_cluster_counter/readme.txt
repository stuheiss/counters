https://erlangcentral.org/wiki/A_Framework_for_Clustering_Generic_Server_Instances

To run id_server in a cluster of two connected Erlang nodes, we first start two shells using erl -sname node1 and erl -sname node2, respectively, and then establish a connection between them, e.g. using net_adm:ping.

We start the cluster at node1 by calling id_server:start_cluster:

Next we increase the cluster by starting a second server at node2 which becommes a local server. To demonstrate server availability provided by this cluster, we stop the current global server using a function of gen_server_cluster: gen_server_cluster:stop(id_server,global) (alternatively, we can simply cancel node1.) As the output at node2 shows, the former local server at node2 is now the new global server. The value returned when calling id_server:next_value proves that the state of id_server was correctly synchronized.

