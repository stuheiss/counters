OBJS=counter.beam gen_server_cluster.beam

.SUFFIXES: %.beam %.erl
%.beam : %.erl
	erlc -o $@ $<

all: $(OBJS)
	-@echo "To run counter in a cluster of two connected Erlang nodes, we first start two shells using erl -sname node1 and erl -sname node2, respectively, and then establish a connection between them, e.g. using net_adm:ping."
	-@echo "We start the cluster at node1 by calling counter:start_cluster:"
	-@echo "Next we increase the cluster by starting a second server at node2 which becommes a local server. To demonstrate server availability provided by this cluster, we stop the current global server using a function of gen_server_cluster: gen_server_cluster:stop(counter,global) (alternatively, we can simply cancel node1.) As the output at node2 shows, the former local server at node2 is now the new global server. The value returned when calling counter:read/0 proves that the state of counter was correctly synchronized."
	@echo "Try on 3 nodes:"
	@echo "node1> make node1global"
	@echo "node2> make node2"
	@echo "node3> make node3"
	@echo "Try stopping the global counter by killing node1 and watch a new one get elected"
	@echo "Try rejoining node1 to the cluster"
	@echo "node1> make node1"
  @echo "Try node(global:whereis_name(counter)) to see which node is the master"

node1global:
	@echo "Start global counter on node1"
	erl -sname node1 -eval 'net_adm:ping(node1@HSA008437),timer:sleep(500),counter:start_cluster()'

node1:
	@echo "Start local counter on node1"
	erl -sname node1 -eval 'net_adm:ping(node2@HSA008437),timer:sleep(500),counter:start_cluster()'

node2:
	@echo "Start local counter on node2"
	erl -sname node2 -eval 'net_adm:ping(node1@HSA008437),timer:sleep(500),counter:start_cluster()'

node3:
	@echo "Start local counter on node2"
	erl -sname node3 -eval 'net_adm:ping(node1@HSA008437),timer:sleep(500),counter:start_cluster()'

clean:
	rm -f $(OBJS)
