OBJS=ebin/counter.beam ebin/counter_app.beam ebin/counter_sup.beam

.SUFFIXES: %.beam %.erl
ebin/%.beam : src/%.erl
	erlc -o ebin/ $<

all: $(OBJS)

try:
	erl -pa ebin -eval 'application:start(crypto), application:start(counter)'
trya:
	erl -sname a -config config/a -pa ebin -eval 'application:start(crypto), application:start(counter)'
tryb:
	erl -sname b -config config/b -pa ebin -eval 'application:start(crypto), application:start(counter)'
tryc:
	erl -sname c -config config/c -pa ebin -eval 'application:start(crypto), application:start(counter)'
clean:
	rm -f ebin/*.beam
