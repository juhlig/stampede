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

-module(stampede_herd).

-behavior(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tab, killer, opts}).

-spec start_link(ets:tab(), stampede:opts()) -> {ok, pid()}.
start_link(Tab, Opts) ->
	gen_server:start_link(?MODULE, {Tab, Opts}, []).

init({Tab, Opts}) ->
	self() ! kill_something,
	{ok, #state{tab=Tab, opts=Opts}}.

handle_call(_, _, State) ->
	{noreply, State}.

handle_cast(_, State) ->
	{noreply, State}.

handle_info(kill_something, State=#state{tab=Tab, killer=undefined, opts=#{before_kill:=Fun}}) ->
	Monitor=spawn_monitor(
		fun () ->
			lists:any(
				fun ({Pid}) ->
					case Fun(Pid) of
						true ->
							exit(Pid, kill),
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
handle_info({'DOWN', Ref, process, Pid, normal}, State=#state{killer={Pid, Ref}, opts=#{interval:={Min, Max}}}) ->
	_=case trunc(Min+rand:uniform()*(Max-Min)) of
		NextKill when NextKill=<0 ->
			self() ! kill_something;
		NextKill ->
			erlang:send_after(NextKill, self(), kill_something)
	end,
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
