%%%----------------------------------------------------------------
%%% @author  Charlie Sharpsteen <source@sharpsteen.net>
%%%          [http://www.github.com/Sharpie]
%%% @copyright 2010 Charlie Sharpsteen
%%% @doc An Erlang interface to the Rserve binary R server.  Rserve
%%%      allows remote users to control R sessions through commands
%%%      encoded in binary and transmitted over TCP.  This Erlang
%%%      application aims to simplify that interface by handling
%%%      the binary translation details and making it easier to 
%%%      expose Rserve by leveraging the Erlang ecosystem.
%%% @end
%%%----------------------------------------------------------------
-module( rserve_server ).

-behaviour( gen_server ).

%% API
-export([ start_link/0, start_link/1, stop/0, eval/1 ]).

%% gen_server callbacks
-export([ init/1, terminate/2,
          handle_info/2, handle_cast/2, handle_call/3,
          code_change/3
]).

-define( SERVER, ?MODULE ).
-define( DEFAULT_RSERVE_PORT, 6311 ).

% Definition for the Rserve TCP link.



-define( RSERVE_TCP_OPTIONS, [ 
  binary, {packet, 0},% implies that communication will be via the
                      % exchange of binary data. The packet 0 clause
                      % indicats that there is no defined length to
                      % the data transmissions- everything is stremed.
  {active, false} % A nonactive socket does not send messages to the
                  % Erlang process that created it containing recieved
                  % data.  Instead, the data piles up and must be
                  % explicitly retrieved via gen_tcp:recv().
  ]
).

-define( RSERV_EVAL_COMMAND, <<
  % 16 byte header- integers in little endian format
  3:32/integer-little, % First 4 bytes contain the number 3 which tells
                       % Rserve "prepare to recieve a text string
                       % containing a command"
  Length:32/integer-little, % Next four bytes contain the length of 
                            % the command plus a four byte header.
  % Remaining 8 bytes transmit two zeros.
  0:32/integer-little, 0:32/integer-little,
  % Four byte object header. First byte contains the number
  % 4 which indicates a text string is encoded.  Remaining
  % 3 bytes give the length of the text string plus a 0 byte
  % that acts as a null terminator. 
  4:8/integer-little, (Length-4):24/integer-little,
  % Actual command as a binary-encoded string.  The 0 is a
  % null terminator.
  (list_to_binary(Payload))/binary, 0>>
).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  start_link( ?DEFAULT_RSERVE_PORT ).

start_link( Port ) ->
  gen_server:start_link( {local, ?SERVER}, ?MODULE,
    [Port], [] 
  ).

eval( Expression ) ->
  gen_server:call( ?SERVER, {eval, Expression} ).

stop() ->
  gen_server:cast( ?SERVER, stop ).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init( [Port] ) ->
  {ok, Socket} = rserve_connect( Port ),
  {ok, Socket}. % A little redundant- but the server state will change


handle_call( {eval, Expression}, _From, State ) ->
  {ok, Response} = rserve_eval( State, Expression ),
  {reply, Response, State}.


handle_cast( stop, State ) ->
  {stop, normal, State}.


terminate( _Reason, State ) ->
  rserve_shutdown( State ).


handle_info( _Info, State ) ->
  { noreply, State }.


code_change( _OldVsn, State, _Extra ) ->
  { ok, State }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
rserve_connect( Port ) ->
  {ok, Socket} = gen_tcp:connect( localhost, Port, ?RSERVE_TCP_OPTIONS ),

  case gen_tcp:recv( Socket, 32 ) of

    {ok, <<"Rsrv0103QAP1", _/binary>>} ->
      error_logger:info_msg( "Connected to Rserve.~2n" ),
      {ok, Socket};

    _ ->
      {error, badhandshake}

  end.

rserve_eval( Socket, Command ) ->

  Payload = "paste(capture.output(eval(quote({" ++ Command ++ "}))),collapse='\n')",
  Length = string:len( Payload ) + 5,
  gen_tcp:send( Socket, ?RSERV_EVAL_COMMAND ),

  case gen_tcp:recv( Socket, 16 ) of

    {ok, <<1,0,_/binary>>} ->
      rserve_recieve( Socket );

    {ok, <<BitString/binary>>} ->
      io:fwrite("~p~2n",[BitString]),
      {error, BitString}

  end.

rserve_recieve( Socket ) ->
  { ok, <<_:32,34,Length:24/integer-little>> } = gen_tcp:recv( Socket, 8 ),
  { ok, BitString } = gen_tcp:recv( Socket, Length ),

  error_logger:info_msg( "Recieved response of length:~2n    ~b~2nContaining:~2n    ~p~2n", [ Length, BitString ] ),
  Response = rserve_decode( BitString ),
  {ok, Response}.

rserve_decode( StringVec ) ->
  Result = extract_string( StringVec ),

  error_logger:info_msg("Decoded as containing:~2n", [] ),
  lists:map( fun( Line ) -> error_logger:info_msg( "~s~n", [ Line ] ) end,
    string:tokens( Result, "\n" )
  ),
  error_logger:info_msg("~2n", []),
  Result.


rserve_shutdown( Socket ) ->
  gen_tcp:close( Socket ),
  error_logger:info_msg( "Closed socket: ~p~n", [Socket] ),
  ok.


% Tail-recursive function to extract a null-terminated string from a bitstring.
% Probably implemented in some standard library that I could not find.
%
% NOTE: Ahh... binary module for standard library was added in R14-
% R14 still in beta at this time (7/2010).
extract_string( <<BitString/binary>> ) ->
  lists:reverse(extract_string( BitString, [] )).

extract_string( <<0,_/binary>>, List ) ->
  List;
extract_string( <<Char,Rest/binary>>, List ) ->
  extract_string( Rest, [ Char | List ] ).
