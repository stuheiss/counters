<?php

/*
    This isn't built in as it's a pretty simple task to achieve for your self
    I can't really think of any functions that are labourious enough that you
    would want to execute them by themselves in a thread of their own. Maybe
    you have functions within your own code that could be called in parallel without
    refactoring for multi-threading capabilities ...

    I personally think you shouldn't try to make chocolate from cheese and you
    would be better of refactoring your code to be multi-threaded ...

    But here's an example of how you would achieve such a task:
*/
class Caller extends Thread {
    /**
    * Provide a passthrough to call_user_func_array
    **/
    public function __construct(callable $method, ...$params){
        $this->method = $method;
        $this->params = $params;
        $this->result = null;
        $this->joined = false;
    }

    /**
    * The smallest thread in the world
    **/
    public function run() {
        $this->result = ($this->method)(...$this->params); /* gotta love php7 :) */
    }

    /**
    * Static method to create your threads from functions ...
    **/
    public static function call($method, ...$params){
        $thread = new Caller($method, ...$params);
        if($thread->start()){
            return $thread;
        }
    }

    /**
    * Do whatever, result stored in $this->result, don't try to join twice
    **/
    public function __toString(){
        if(!$this->joined) {
            $this->joined = true;
            $this->join();
        }

        return $this->result;
    }


    private $method;
    private $params;
    private $result;
    private $joined;
}

class Counter
{
    private $counter;
    public function __construct()
    {
        $this->counter=0;
    }
    public function read()
    {
        return $this->counter;
    }
    public function tick($value)
    {
        $this->counter += $value;
    }
}

function counterDo($tick) {
    $c = new Counter;
    $c->tick($tick);
    //return $c->read();
    $res = $c->read();
    return $res;
}

// main
$options = getopt('n:c:stp');
$ncounters=isset($options['n']) ? $options['n'] : 0;
$concurrency=isset($options['c']) ? $options['c'] : 100; // default concurrency for pcntl processes
if ($ncounters <= 0) {
    die("usage: \"$argv[0] -n num {-s | -t | -p [-c concur]}\" where num > 0 and one or more of -s (simple counters), -t (pthreaded counters), -p (pcntl process counters) [-c concur] where concur defaults to 100\n");
}

if (isset($options['s'])) {
    echo "create $ncounters simple counters\n";
    $t = -microtime(true);
    for ($i = 0; $i < $ncounters; ++$i) {
        counterDo(42);
    }
    $t += microtime(true);
    $t *= 1000;
    echo "done in $t ms\n";
}

if (isset($options['t'])) {
    echo "create $ncounters pthread counters\n";
    $t = -microtime(true);
    for ($i = 0; $i < $ncounters; ++$i) {
        $future = Caller::call("counterDo", 123);
        $future->join();
        //echo $future->result . PHP_EOL;
    }
    $t += microtime(true);
    $t *= 1000;
    echo "done in $t ms\n";
}

if (isset($options['p'])) {
    echo "create $ncounters pcntl/process counters with max concurrency of $concurrency\n";
    $t = -microtime(true);
    $pids = array();
    $nprocesses = 0;
    for ($i = 0; $i < $ncounters; ++$i) {
        if ($nprocesses >= $concurrency) {
            foreach ($pids as $pid) {
                pcntl_wait($status);
            }
            $pids = array();
            $nprocesses = 0;
        }
        $nprocesses++;
        $pid = pcntl_fork();
        if ($pid == -1) {
            die("could not fork\n");
        } else if ($pid) {
            $pids[] = $pid;
        } else {
            counterDo(42);
            exit;
        }
    }
    foreach ($pids as $pid) {
        pcntl_wait($status);
    }
    $t += microtime(true);
    $t *= 1000;
    echo "done in $t ms\n";
}

exit;
