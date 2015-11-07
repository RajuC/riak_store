-module(rs_utils).
-export([to_bin/1,to_list/1,get_time/0,get_bin_proplist/2,
	form_http_params/2,to_bin_proplist/2,get_list_proplist/2,make_http_request/3]).
-define(CONT_TYPE,"application/json").


get_time() ->
	{{YY,MM,DD},{Hr,Min,Sec}} = erlang:localtime(),
	Time = to_list(MM)++"-"++to_list(DD)++"-"++ to_list(YY)++
	" "++ to_list(Hr)++":"++to_list(Min)++":"++to_list(Sec),
	to_bin(Time).



to_list({A,B})  ->
	{to_list(A),to_list(B)};
to_list({A,B,C}) ->
	{to_list(A),to_list(B),to_list(C)};
to_list(Data) when is_list(Data) ->
	Data;
to_list(Data) when is_atom(Data) ->
	atom_to_list(Data);
to_list(Data) when is_integer(Data) ->
	integer_to_list(Data);
to_list(Data) when is_binary(Data) ->
	binary_to_list(Data).

get_list_proplist([],ResultBinPropList) ->
	lists:reverse(ResultBinPropList);
get_list_proplist([{Key,Value}|Tail],Result) ->
	get_list_proplist(Tail,[to_list({Key,Value})]++Result).



to_bin({A,B})  ->
	{to_bin(A),to_bin(B)};
to_bin({A,B,C}) ->
	{to_bin(A),to_bin(B),to_bin(C)};
to_bin(A) when is_list(A) ->
	list_to_binary(A);
to_bin(A) when is_integer(A) ->
	integer_to_binary(A);
to_bin(A) when is_atom(A) ->
	list_to_binary(to_list(A));
to_bin(A) when is_binary(A) ->
	A.

%%[{"key",["Secret"]},
%%  {"template_content",[{"name","name_missing"},{"content","content missing"}]}].
%%general list to binary proplist
to_bin_proplist([],Result) ->
	lists:reverse(Result);
to_bin_proplist([{Key,Val}|Tail],Result) when length(Val) == 1 ->
	to_bin_proplist(Tail,[to_bin({Key,Val})]++Result);
to_bin_proplist([{Key,Val}|Tail],Result) when length(Val) > 1 ->
	to_bin_proplist(Tail,[{to_bin(Key),get_bin_proplist(Val,[])}]++Result).	

%%proplist to binary proplist
get_bin_proplist([],ResultBinPropList) ->
	lists:reverse(ResultBinPropList);
get_bin_proplist([{Key,Value}|Tail],Result) ->
	get_bin_proplist(Tail,[to_bin({Key,Value})]++Result).


%%action=getscore&message=hii
form_http_params([],QueryParamsList) ->
	string:join(lists:reverse(QueryParamsList),"&");
 form_http_params([{Key,Value}|Tail],QueryParamsList) ->
 	form_http_params(Tail,[string:join([Key,
 		http_uri:encode(Value)],"=")]++QueryParamsList).


%%%%% test Requests 
%%curl -H "Content-Type: application/json" -X POST -d '{"Action":"PublishToTarget","Message":"Abey voo saalee","EndPointArn":"arn:aws:sns:us-east-1:742333168435:endpoint/GCM/GCM-dev/c0f6bbfc-a71f-3de6-b08d-fccb7c3b2561"}' http://localhost:8044/
%%curl -H "Content-Type: application/json" -X POST -d '{"Action":"CreatePlatformEndpoint","Platform":"GCM","CustomUserData":"UserId%3D13456","DeviceToken":"742333168435:fccb7c3b2561"}' http://localhost:8044/


make_http_request(get,HostUrl,Headers) ->
	case httpc:request(get, 
	    	{HostUrl, Headers},[{version,"HTTP/1.1"}], []) of
		{ok,{{"HTTP/1.1",200,_},_Headers,Response}} ->
			case Response of
				"[]" ->
					{error,[]};
				OtherResp ->
					{ok,OtherResp}
			end;
		{ok,{{"HTTP/1.1",ResponseCode,_Error},_Headers,Response}} ->
			{error,ResponseCode,Response};	
		{error,Reason} ->
			{error,Reason}
	end;


make_http_request(post,HostUrl,QueryParams) ->
%%	io:format("~nQueryParams:~p~n",[QueryParams]),
	PostParams = form_params(QueryParams,[]),
%%	io:format("~nPostParams:~p~n",[PostParams]),
	case httpc:request(post, {HostUrl, [], ?CONT_TYPE,
		PostParams}, [{version,"HTTP/1.1"}], []) of
	{ok,{{"HTTP/1.1",200,_},_Headers,Response}} ->
		{ok,Response};
    {ok,{{"HTTP/1.1",ResponseCode,_},_Headers,Response}} ->
		{error,ResponseCode,Response};
	{error,Reason} ->
		{error,Reason}
	end.

form_params([],QueryParamsList) ->
	string:join(lists:reverse(QueryParamsList),"&");
form_params([{Key,Value}|Tail],QueryParamsList) ->
 	form_params(Tail,[string:join([Key,
 		http_uri:encode(Value)],"=")]++QueryParamsList).