%%%----------------------------------------------------------------
%%% @author  Charlie Sharpsteen <source@sharpsteen.net>
%%% @doc
%%% @end
%%% @copyright 2010 Charlie Sharpsteen
%%%----------------------------------------------------------------
-module( rserve_server ).

-behaviour( gen_server ).

%% API
-export([ init/1, terminate/2,
          handle_info/2, handle_cast/2, handle_call/3,
          code_change/3
]).

%%%===================================================================
%%% API functions
%%%===================================================================

init([]) ->
  { ok, #state{} }.

terminate( _Reason, _State ) ->
  ok.

handle_cast( _Mst, State ) ->
  { noreply, State }.

handle_call( _Request, _From, State ) ->
  Reply = ok,
  { reply, Reply, State }.

handle_info( _Info, State ) ->
  { noreply, State }.

code_change( _OldVsn, State, _Extra ) ->
  { ok, State }.

%%%===================================================================
%%% Server callbacks
%%%===================================================================


%%%===================================================================
%%% Internal functions
%%%===================================================================
