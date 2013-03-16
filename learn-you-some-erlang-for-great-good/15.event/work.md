
```````````````````````sh
neon $ erl -pa ebin
Erlang R15B02 (erts-5.9.2) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:fal
Eshell V5.9.2  (abort with ^G)
``````````````````````````

````````````````````````erlang
1> evserv:start().
<0.33.0>
2> evserv:subscribe(self()).
{ok,#Ref<0.0.0.32>}
3> evserv:add_event("hey there", "test", FutureDateTime).
* 1: variable 'FutureDateTime' is unbound
4> evserv:add_event("hey there", "test", "2012-12-12").
{error,bad_timeout}
5> evserv:add_event("hey there", "test", {{2012,12,12},{01,01,01}}).
ok
6> evserv:listen(5).
[]
7> evserv:add_event("next minutessss", "test2", {{2012,11,29},{17,25,12}}).
ok
8> evserv:listen(500).
[{done,"next minutessss","test2"}]
``````````````````````````
