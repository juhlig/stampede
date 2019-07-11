%% Copyright (c) 2019, Jan Uhlig <j.uhlig@mailingwork.de>
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

-export([if_not/1]).
-export([if_any/0]).
-export([if_supervisor/0]).
-export([if_module/1]).
-export([if_child/2]).

-spec if_not(fun((pid()) -> boolean())) -> fun((pid()) -> boolean()).
if_not(Fun) ->
	fun (Pid) -> not Fun(Pid) end.

-spec if_any() -> fun((pid()) -> true).
if_any() ->
	fun (_) -> true end.

-spec if_supervisor() -> fun((pid()) -> boolean()).
if_supervisor() ->
	fun (Pid) -> (if_module([supervisor]))(Pid) end.

-spec if_module([module()]) -> fun ((pid()) -> boolean()).
if_module(Modules) ->
	fun (Pid) ->
		case erlang:process_info(Pid, initial_call) of
			{initial_call, {proc_lib, init_p, 5}} ->
				{Module, _, _} = proc_lib:translate_initial_call(Pid),
				lists:member(Module, Modules);
			_ ->
				false
		end
	end.

-spec if_child(pos_integer() | infinity, pid() | atom()) -> fun((pid()) -> boolean()).
if_child(Depth, Supervisor) ->
	fun (Pid) -> if_child1(Pid, Supervisor, Depth) end.

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
		exit:{noproc, _} ->
			false
	end.
