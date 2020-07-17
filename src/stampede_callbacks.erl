%% Copyright (c) 2020, Jan Uhlig <j.uhlig@mailingwork.de>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(stampede_callbacks).

-export([if_anyof/1]).
-export([if_allof/1]).
-export([if_not/1]).
-export([if_any/0]).
-export([if_process/0]).
-export([if_port/0]).
-export([if_supervisor/0]).
-export([if_module/1]).
-export([if_child/2]).
-export([if_tcp/0]).
-export([if_udp/0]).
-export([if_sctp/0]).
-export([if_portname/1]).
-export([if_portinfo/2]).

-spec if_anyof([stampede:callback()]) -> stampede:callback().
if_anyof(Funs) ->
	fun (PidOrPort) ->
		lists:any(
			fun (Fun) ->
				Fun(PidOrPort)
			end,
			Funs
		)
	end.

-spec if_allof([stampede:callback()]) -> stampede:callback().
if_allof(Funs) ->
	fun (PidOrPort) ->
		lists:all(
			fun (Fun) ->
				Fun(PidOrPort)
			end,
			Funs
		)
	end.

-spec if_not(stampede:callback()) -> stampede:callback().
if_not(Fun) ->
	fun (PidOrPort) ->
		 not Fun(PidOrPort)
	end.

-spec if_any() -> fun((pid() | port()) -> true).
if_any() ->
	fun (_) ->
		true
	end.

-spec if_process() -> stampede:callback().
if_process() ->
	fun (Pid) ->
		is_pid(Pid)
	end.

-spec if_port() -> stampede:callback().
if_port() ->
	fun (Port) ->
		is_port(Port)
	end.

-spec if_supervisor() -> stampede:callback().
if_supervisor() ->
	if_module([supervisor]).

-spec if_module([module()]) -> fun ((pid() | port()) -> boolean()).
if_module(Modules) ->
	fun
		(Pid) when is_pid(Pid) ->
			case erlang:process_info(Pid, initial_call) of
				{initial_call, {proc_lib, init_p, 5}} ->
					{Module, _, _} = proc_lib:translate_initial_call(Pid),
					lists:member(Module, Modules);
				_ ->
					false
			end;
		(_) ->
			false
	end.

-spec if_child(pos_integer() | infinity, pid() | atom()) -> stampede:callback().
if_child(Depth, Supervisor) ->
	fun
		(Pid) when is_pid(Pid) ->
			if_child1(Pid, Supervisor, Depth);
		(_) ->
			false
	end.

if_child1(Pid, Supervisor, Depth) ->
	try
		supervisor:which_children(Supervisor)
	of
		Children when Depth=:=1 ->
			lists:keymember(Pid, 2, Children);
		Children ->
			lists:any(
				fun
					({_, undefined, _, _}) ->
						false;
					({_, restarting, _, _}) ->
						false;
					({_, Child, _, _}) when Child=:=Pid ->
						true;
					({_, SubSupervisor, supervisor, _}) when Depth=:=infinity ->
						if_child1(Pid, SubSupervisor, Depth);
					({_, SubSupervisor, supervisor, _}) ->
						if_child1(Pid, SubSupervisor, Depth - 1);
					({_, _, _, _}) ->
						false
				end,
				Children
			)
	catch
		_:_ ->
			false
	end.

-spec if_portname(term()) -> stampede:callback().
if_portname(Name) ->
	if_portinfo(name, Name).

-spec if_portinfo(atom(), term()) -> stampede:callback().
if_portinfo(Item, Value) ->
	fun
		(Port) when is_port(Port) ->
			try
				erlang:port_info(Port, Item)
			of
				{_, Value1} ->
					Value1=:=Value;
				_ ->
					false
			catch
				error:badarg ->
					false
			end;
		(_) ->
			false
	end.

-spec if_tcp_port() -> stampede:callback().
if_tcp_port() ->
	if_portname("tcp_inet").

-spec if_tcp_socket() -> stampede:callback().
if_tcp_socket() ->
	fun
		(Pid) when is_pid(Pid) ->
			case proc_lib:initial_call(Pid) of
				{gen_tcp_socket, _, _} ->
					true;
				_ ->
					false
			end;
		(_) ->
			false
	end.

-spec if_tcp() -> stampede:callback().
if_tcp() ->
	if_anyof([if_tcp_port(), if_tcp_socket()]).

-spec if_udp() -> stampede:callback().
if_udp() ->
	if_portname("udp_inet").

-spec if_sctp() -> stampede:callback().
if_sctp() ->
	if_portname("sctp_inet").
