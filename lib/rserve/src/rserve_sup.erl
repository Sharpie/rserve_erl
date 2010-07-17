%%%----------------------------------------------------------------
%%% @author  Charlie Sharpsteen <source@sharpsteen.net>
%%% @doc
%%% @end
%%% @copyright 2010 Charlie Sharpsteen
%%%----------------------------------------------------------------
-module( rserve_sup ).

-behaviour( supervisor ).

%% API
-export([ start_link/0, start_link/1 ]).

%% Supervisor callbacks
-export([ init/1 ]).

-define( SERVER, ?MODULE ).
-define( DEFAULT_RSERVE_PORT, 6311 ).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  start_link( ?DEFAULT_RSERVE_PORT ).

start_link( Port ) ->
  supervisor:start_link( {local, ?SERVER}, ?MODULE, [Port] ).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init( [Port] ) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Rserve = {'rserve_server', {'rserve_server', start_link, [Port]},
              Restart, Shutdown, Type, ['rserve_server']},

    {ok, {SupFlags, [Rserve]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


