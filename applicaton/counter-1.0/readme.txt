Note, must start crypto before starting counter!
compile src files and put beam files in ebin

$ cd ebin && erl
> application:start(crypto).
> application:start(counter).
> counter:tick(42).
> counter:read().
> counter:reset().

Start distributed in 3 shells:
$ erl -sname a -config config/a -pa ebin/
$ erl -sname b -config config/b -pa ebin/
$ erl -sname c -config config/c -pa ebin/

Check where applications are running:
> application:which_applications().
> whereis(counter).

Start distributed with app on boot:
$ erl -sname a -config config/a -pa ebin -eval 'application:start(crypto), application:start(counter)'
$ erl -sname b -config config/b -pa ebin -eval 'application:start(crypto), application:start(counter)'
$ erl -sname c -config config/c -pa ebin -eval 'application:start(crypto), application:start(counter)'

Start single instance:
$ erl -pa ebin -eval 'application:start(crypto), application:start(counter)'
