Concurrency and Counters in Erlang
A Study in Distributed Highly Available Applications

This repo explores techniques to leveredge Erlang's process and message passing
model and OTP behaviours to create highly available applications. Such
applications can withstand software and hardware faults as well as enable
code updates without disruption on a running system.

Erlang's process model with no shared memory is used to isolate and minimize
the impacct of software and hardware faults.

Erlang basic process primitives include spawn, send, and receive. Additional
primitives include make_ref, link, monitor, and spawn_link.

A counter will be used to illustration ways to make an application robust.

We begin with a very simple counter running in an isolated process.
A counter process can be started, stopped, read, and ticked.

The simple counter (simple/{counter.erl,counter_rpc.erl) illustrates basic
concurrency primatives spawn, send, and receive. The rpc variant illustrates
how to organize send and receive into an rcp functional interface.

The gen_server counter (gen_server_counter/counter.erl) introduces the
gen_server behavior. This behaviour allows us to delegate all concurrency
primitives to gen_server and introduces the concept of linking one process
to another as a foundation for delegating fault handling to the calling
process.

The gen_supervisor counter (gen_supervisor_counter/counter.erl) introduces the
gen_supervisor behavior. The behavior formalizes the concept of supervision.
The supervisor will now take responibility for starting, stopping, and
handling faults in the counter server process.

The applicaton counter (application/counter-1.0) introduces the concept of
bundling a counter as an application which can be distributed across multiple
servers. The counter APIs are available on all servers. If a server fails,
the counter will fail-over to another server afters a timeout, but with loss
of state.

The gen_server_cluster counter (gen_server_cluster/counter.erl) introduces
the gen_server_cluster behaviour. This behaviour abstracts a distributed
counter that provides fast fail-over and maintains state across servers.

The code_server_counter (code_server/counter.erl) introduces the feature of
hot code loading where running code can be changed on the fly thus allowing
bug fixes and new features to a codebase without stopping a running instance
and maintaining state.
