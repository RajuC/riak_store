#### simple riak client for interacting riakc

must use erlang/otp version < 18 for using riakc
make sure to start the riak server before starting this application 
for reference https://github.com/basho/riak 


##### riak store - a simple riak client

- git clone git@github.com:RajuC/riak_store.git
- make
-./_rel/riak_store_release/bin/riak_store_release console
-(riak_store@127.0.0.1)1> 
-(riak_store@127.0.0.1)1>
-(riak_store@127.0.0.1)1> rs:put("images","srk.jpg",[{"WxH","1020x800"},{"size","30kb"}]).
-ok
-(riak_store@127.0.0.1)2> rs:get("images","srk.jpg").
-[{"WxH","1020x800"},{"size","30kb"}]





######### still developing :)



