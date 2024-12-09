-module(server_tcp).
-behaviour(gen_server).

-export([start_link/0, accept_connection/1,  server_response/2, generate_room_name/0, get_client_map/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, server_tcp}, server_tcp, [],[]).

init(_) ->
    case gen_tcp:listen(5426, [binary, {packet,0},{active, false}]) of
        {ok, ListenSocket} ->
            io:format("Server started on port 5426~n"),
            spawn(fun() -> accept_connection(ListenSocket) end),
            {ok,#{ListenSocket => ListenSocket, client_map => #{}}};       
        {error, Reason} ->
            io:format("Failed to start server: ~p~n", [Reason]),
            {stop, Reason}    
    end.



accept_connection(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Connection accepted~n"),
            ClientPID = client:start(Socket),
            receive
                {ClientPid, ClientName} ->
                    %io:format("Cliend PID: ~p, Name: ~p~n", [ClientPID, ClientName]),
                    gen_server:cast(server_tcp, {add_client, ClientName, ClientPid, Socket})
            end,
            %io:format("ClientPID: ~p~n", [ClientPID]),
            accept_connection(ListenSocket);
        {error, Reason} ->
            io:format("Connection not accepted: ~p~n", [Reason])
    end.




server_response(Socket, Message) ->
    gen_tcp:send(Socket, Message).

%% Callback 

handle_call(_, _From, State) ->1,
    {reply, ok, State}.

get_client_map(State) ->
    maps:get(client_map, State, #{}).
    
generate_room_name() ->
    ID = erlang:unique_integer([positive]),
    "Room " ++ integer_to_list(ID). 

handle_cast({add_client, ClientName, ClientPID, Socket}, State) ->
    NewClientMap = maps:put(ClientName, {ClientPID, Socket}, maps:get(client_map, State, #{})),
    io:format("Added client ~p with PID ~p to the map~n", [ClientName, ClientPID]),
    {noreply, State#{client_map => NewClientMap}}; 
    

handle_cast({message_received, {Socket, ClientName, ClientPID}, Message}, State) ->
    case Message of 
        "/create-room" ->
            {ok, RoomName, RoomPid} = gen_server:call(room_manager, {create_room, generate_room_name()}),
            ClientPID ! {add_room, {RoomName, RoomPid}},
            MessageResponse = io_lib:format("Room created, Room name: ~p~n", [RoomName]),
            server_response(Socket, list_to_binary(MessageResponse));
        "/destroy-room" ->
            io:format("Distruzione stanza~n"),
            server_response(Socket, <<"Select a room to destroy">>),
            case gen_tcp:recv(Socket, 0) of
                {ok, RoomName} ->
                    RoomNameString = binary_to_list(RoomName),
                    io:format("Client ~p, selected room: ~p~n",[Socket,RoomName]),
                    ClientPID ! {destroy_room, {RoomNameString}},
                    server_response(Socket, <<"Room deleted">>);
                {error, Reason} ->
                    io:format("Failed to receive message in join room: ~p~n",[Reason]),    
                    gen_tcp:close(Socket)
            end;
        "/list-rooms" ->
            case gen_server:call(room_manager, {list_room}) of 
                {ok, RoomMaps} ->
                    ListOfRooms = maps:keys(RoomMaps),
                    io:format("Room manager state: ~p~n", [ListOfRooms]),
                    case ListOfRooms of
                       [] -> 
                            server_response(Socket, [<<"No rooms available">>]);
                        _ -> 
                            RoomsString =string:join(ListOfRooms,"\n"),
                            RoomsBinary = lists:flatten(RoomsString),
                            server_response(Socket, [<<"Rooms available: \n">>, RoomsBinary])
                    end
            end;
            
        "/join-room" ->
            server_response(Socket, <<"Select a room to enter">>),
            case gen_tcp:recv(Socket, 0) of
                {ok, RoomName} ->
                    RoomNameString = binary_to_list(RoomName),
                    io:format("Client ~p, selected room: ~p~n",[Socket,RoomNameString]),
                    ClientPID ! {restart_receive_message},
                    MessageResponse = io_lib:format("Room selected: ~p~n", [RoomNameString]),
                    server_response(Socket, list_to_binary(MessageResponse)),
                    {ok} =  gen_server:call(room_manager, {add_client_to_room, ClientName, ClientPID, RoomNameString});
                {error, Reason} ->
                    io:format("Failed to receive message in join room: ~p~n",[Reason]),    
                    gen_tcp:close(Socket)
            end;
        "/leave-room" ->
            server_response(Socket, <<"Select room to leave:">>),
            case gen_tcp:recv(Socket, 0) of
                {ok, RoomName} ->
                    RoomNameString = binary_to_list(RoomName),
                    %io:format("Client ~p, selected room: ~p~n",[ClientName,RoomName]),
                    ClientPID ! {restart_receive_message},
                    MessageResponse = io_lib:format("Room selected: ~p~n", [RoomNameString]),
                    server_response(Socket, list_to_binary(MessageResponse)),
                    {ok} =  gen_server:call(room_manager, {remove_client_to_room, ClientName, ClientPID, RoomNameString});
                {error, Reason} ->
                    io:format("Failed to receive message in leave-room: ~p~n",[Reason]),    
                    gen_tcp:close(Socket)
            end;
        "/send-message" ->
            server_response(Socket, <<"Select room to send a message">>),
            {ok, RoomName} = gen_tcp:recv(Socket, 0),
            RoomNameString = binary_to_list(RoomName),
            %io:format("Client ~p, selected room: ~p~n",[ClientName,RoomNameString]),
            MessageResponse = io_lib:format("Room selected: ~p, Write a message:~n", [RoomNameString]),
            server_response(Socket, list_to_binary(MessageResponse)),
            {ok, MessageToSend} = gen_tcp:recv(Socket, 0),
            MessageToSendString = binary_to_list(MessageToSend),
            {ok} = gen_server:call(room_manager, {send_message_room, ClientName, RoomNameString, MessageToSendString}),
            receive
                {send_message_to_all, {Members}} ->
                    maps:fold(
                        fun(Key, Value, Acc) ->
                            io:format("Key: ~p, Value: ~p~n", [Key, Value]),
                            ClientMap = get_client_map(State),
                            {PID, SocketClient} = maps:get(Key, ClientMap),
                            MessageFrom = io_lib:format("From ~p on ~p: ~p~n", [ClientName, binary_to_list(RoomName) , MessageToSendString]),
                            gen_tcp:send(SocketClient, list_to_binary(MessageFrom)),
                            Acc
                        end, 
                        ok, Members),
                        ClientPID ! {restart_receive_message}
            end;
        "/send-private-message" ->
            ClientMap = get_client_map(State),
            ClientMapWithoutItself = maps:remove(ClientName, ClientMap),
            MessageResponse = io_lib:format("Choose who to send a message to:~p ~n", [maps:keys(ClientMapWithoutItself)]),
            server_response(Socket, list_to_binary(MessageResponse)),
            {ok, ClientToSend} = gen_tcp:recv(Socket, 0),
            MessageResponseWrite = io_lib:format("Write message to ~p ~n", [binary_to_list(ClientToSend)]),
            server_response(Socket, list_to_binary(MessageResponseWrite)),
            {ok, MessageToSend} = gen_tcp:recv(Socket, 0),
            {PID, SocketClient} = maps:get(binary_to_list(ClientToSend), ClientMapWithoutItself),
            MessageFrom = io_lib:format("From ~p: ~p~n", [ClientName, binary_to_list(MessageToSend)]),
            gen_tcp:send(SocketClient, list_to_binary(MessageFrom)),
            ClientPID ! {restart_receive_message};
         _ ->
            io:format("Command not found~n"), 
            server_response(Socket, <<"Command not found">>)
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Terminating server ~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.