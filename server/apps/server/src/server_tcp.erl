-module(server_tcp).
-behaviour(gen_server).

-export([start_link/0, accept_connection/1, handle_connection/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, server_tcp}, server_tcp, [],[]).

init(_) ->
    case gen_tcp:listen(5426, [binary, {packet,0},{active, false}]) of
        {ok, ListenSocket} ->
            io:format("Server started on port 5426~n"),
            spawn(fun() -> accept_connection(ListenSocket) end),
            {ok,ListenSocket};       
        {error, Reason} ->
            io:format("Failed to start server: ~p~n", [Reason]),
            {stop, Reason}    
    end.


accept_connection(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Connection accepted~n"),
            spawn(fun() -> handle_connection(Socket) end),
            accept_connection(ListenSocket);
        {error, Reason} ->
            io:format("Connection not accepted: ~p~n", [Reason])
    end.

handle_connection(Socket) -> 
    case inet:peername(Socket) of
        {ok,{IpAddress, Port}} ->
            case gen_tcp:recv(Socket, 0) of
                {ok, ClientNameBinary} ->
                    ClientName = binary_to_list(ClientNameBinary),
                    io:format("Client name: ~p, connected: ~p from port ~p~n",[ClientName ,IpAddress, Port]),
                    Msg = [<<"Welcome ">>, ClientNameBinary],
                    gen_tcp:send(Socket, Msg ),
                    receive_message(Socket,ClientName); 
                {error, Reason} ->
                    io:format("Failed to receive message ~p~n",[Reason]),    
                    gen_tcp:close(Socket)    
        end;
        {error, Reason} ->
            io:format("Error to get peername: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

receive_message(Socket, ClientName) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, MessageBinary} ->
            Message =  binary_to_list(MessageBinary),
            gen_server:cast(server_tcp, {message_received, {Socket,ClientName}, Message}),
            receive_message(Socket, ClientName);
        {error, closed} -> 
            io:format("Connection closed by client ~p~n", [ClientName]),
            gen_tcp:close(Socket);    
        {error, Reason} ->
            io:format("Error: ~p~n",[Reason]),
            gen_tcp:close(Socket)
    end. 
    

%% Callback 
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({message_received, {Socket, ClientName}, Message}, State) ->
    io:format("Message from ~p: ~p~n", [ClientName, Message]),
    gen_tcp:send(Socket, <<"Message ok">>),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Terminating server ~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.