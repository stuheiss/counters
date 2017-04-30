{application, counter,
 [{descripton, "Counter application"},
  {vsn, "1.0"},
  {modules, [counter, counter_sup, counter_app]},
  {registered, [counter_sup, counter]},
  {applications, [kernel, stdlib]},
  {env, [{env, 0}]},
  {mod, {counter_app, []}}]}.
