-module(rs).
-export([start/0,get_pid/0,put/3,get/2,list_all_buckets/0,list_keys/1,delete/2]).

start() ->
	ok = application:start(protobuffs),
	ok = application:start(riak_pb),
	ok = application:start(riakc),
	ok = application:start(riak_store),
	io:format("simple riak client started ...").


get_pid() ->
	{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
	Pid.

%% convert bucket and key values to binary before storing

put(B,K,Value) ->
	Pid = get_pid(),
	{Bucket,Key} = to_bin({B,K}),
	Obj2Put = 
		case riakc_pb_socket:get(Pid, Bucket, Key) of
            {ok, Obj} ->
                riakc_obj:update_value(Obj, Value);
            {error, notfound} ->
                riakc_obj:new(Bucket, Key, Value)
        end,
	riakc_pb_socket:put(Pid, Obj2Put).

get(B, K) ->
	Pid = get_pid(),
	{Bucket,Key} = to_bin({B,K}),
	case riakc_pb_socket:get(Pid, Bucket, Key) of
		{ok, Obj} ->
            Value = riakc_obj:get_value(Obj),
            binary_to_term(Value);
        {error, notfound} ->
        	{error, notfound}
    end.

delete(B,K) ->
	Pid = get_pid(),
	{Bucket,Key} = to_bin({B,K}),
	riakc_pb_socket:delete(Pid, Bucket, Key).

list_all_buckets() ->
	riakc_pb_socket:list_buckets(get_pid()).

list_keys(B) ->
	riakc_pb_socket:list_keys(get_pid(),to_bin(B)).



to_bin({B,K}) ->
	rs_utils:to_bin({B,K});
to_bin(A) ->
	rs_utils:to_bin(A).



