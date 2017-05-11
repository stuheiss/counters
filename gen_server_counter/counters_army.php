<?php

class Counter
{
    private $value;

    public function __construct()
    {
        $this->value=0;
    }

    public function read()
    {
        return $this->value;
    }

    public function tick($n)
    {
        $this->value += $n;
    }
}

$counters=array();

function demo($n) {

    echo "Create $n counters\n";
    for ($i=0; $i<$n; ++$i) {
        $counters[$i]=new Counter;
    }

    echo "Set $n counters to [1..$n]\n";
    for ($i=0; $i<$n; ++$i) {
        $counters[$i]->tick($i+1);
    }

    echo "Sum all counters\n";
    $v = array_reduce($counters, function($acc, $elem) { return $acc + $elem->read(); }, 0);
    echo "Sum $n counters returns $v\n";

    echo "Set $n counters to sqrt of their values\n";
    for ($i=0; $i<$n; ++$i) {
        $j=$counters[$i]->read();
        $k=sqrt($j);
        $counters[$i]->tick($k-$j);
    }

    echo "Sum all counters\n";
    $v = array_reduce($counters, function($acc, $elem) { return $acc + $elem->read(); }, 0);
    echo "Sum $n counters returns $v\n";
}

$n = $argv[1];
demo($n);
