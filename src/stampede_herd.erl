%% Copyright (c) 2020-2021, Jan Uhlig <juhlig@hnc-agency.org>
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

-module(stampede_herd).

-behavior(gen_server).

-export([start_link/2]).
-export([activate/1]).
-export([deactivate/1]).
-export([is_active/1]).
-export([set_opts/2]).
-export([get_opts/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tab, killer, opts, pin}).

-spec start_link(ets:tab(), stampede:opts()) -> {ok, pid()}.
start_link(Tab, Opts) ->
	gen_server:start_link(?MODULE, {Tab, Opts}, []).

-spec activate(pid()) -> ok.
activate(Pid) ->
	gen_server:call(Pid, activate).

-spec deactivate(pid()) -> ok.
deactivate(Pid) ->
	gen_server:call(Pid, deactivate).

-spec is_active(pid()) -> boolean().
is_active(Pid) ->
	gen_server:call(Pid, is_active).

-spec set_opts(pid(), stampede:opts()) -> ok.
set_opts(Pid, Opts) ->
	gen_server:cast(Pid, {set_opts, Opts}).

-spec get_opts(pid()) -> stampede:opts().
get_opts(Pid) ->
	gen_server:call(Pid, get_opts).

init({Tab, Opts}) ->
	{ok, #state{tab=Tab, opts=Opts}}.

handle_call(activate, _, State=#state{pin=undefined}) ->
	Pin=make_ref(),
	schedule_kill(0, 0, Pin),
	{reply, ok, State#state{pin=Pin}};
handle_call(deactivate, _, State) ->
	{reply, ok, State#state{pin=undefined}};
handle_call(is_active, _, State=#state{pin=undefined}) ->
	{reply, false, State};
handle_call(is_active, _, State) ->
	{reply, true, State};
handle_call(get_opts, _, State=#state{opts=Opts}) ->
	{reply, Opts, State};
handle_call(_, _, State) ->
	{noreply, State}.

handle_cast({set_opts, Opts}, State=#state{pin=undefined}) ->
	{noreply, State#state{opts=Opts}};
handle_cast({set_opts, Opts=#{interval:={Min, Max}}}, State) ->
	Pin=make_ref(),
	schedule_kill(Min, Max, Pin),
	{noreply, State#state{opts=Opts, pin=Pin}};
handle_cast(_, State) ->
	{noreply, State}.

handle_info({kill_something, Pin}, State=#state{tab=Tab, killer=undefined, opts=#{before_kill:=Fun}, pin=Pin}) ->
	Monitor=spawn_monitor(
		fun () ->
			lists:any(
				fun ({PidOrPort}) ->
					case Fun(PidOrPort) of
						true ->
							exit(PidOrPort, kill),
							true;
						false ->
							false
					end
				end,
				shuffle_list(ets:tab2list(Tab))
			)
		end
	),
	{noreply, State#state{killer=Monitor}};
handle_info({'DOWN', Ref, process, Pid, normal}, State=#state{killer={Pid, Ref}, pin=undefined}) ->
	{noreply, State#state{killer=undefined}};
handle_info({'DOWN', Ref, process, Pid, normal}, State=#state{killer={Pid, Ref}, opts=#{interval:={Min, Max}}, pin=Pin}) ->
	schedule_kill(Min, Max, Pin),
	{noreply, State#state{killer=undefined}};
handle_info({'DOWN', Ref, process, Pid, Reason}, State=#state{killer={Pid, Ref}}) ->
	{stop, Reason, State#state{killer=undefined}};
handle_info(_, State) ->
	{noreply, State}.

terminate(_, _) ->
	ok.

code_change(_, State, _) ->
	{ok, State}.

shuffle_list(List) ->
	[E || {_, E} <- lists:sort([{rand:uniform(), E} || E <- List])].

schedule_kill(Min, Max, Pin) ->
	NextKill=trunc(Min+rand:uniform()*(Max-Min)),
	_=if
		NextKill>0 ->
			erlang:send_after(NextKill, self(), {kill_something, Pin});
		true ->
			self() ! {kill_something, Pin}
	end,
	ok.
