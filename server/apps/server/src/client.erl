-module(client).
-export([start/1, init/3,handle_connection/3]).

start(Socket) ->
    Parent = self(),
    spawn(client, init, [Socket, #{}, Parent]).

init(Socket, Rooms, Parent) ->
    handle_connection(Socket, Rooms, Parent).

handle_connection(Socket, Rooms, Parent) -> 
    case inet:peername(Socket) of
        {ok,{IpAddress, Port}} ->
            case gen_tcp:recv(Socket, 0) of
                {ok, ClientNameBinary} ->
                    ClientName = binary_to_list(ClientNameBinary),
                    %io:format("Client name: ~p, connected: ~p from port ~p~n",[ClientName ,IpAddress, Port]),
                    Msg = [<<"Welcome ">>, ClientNameBinary],
                    gen_tcp:send(Socket, Msg ),
                    Parent ! {self(), ClientName},
                    receive_message(Socket, ClientName, Rooms);   
                {error, Reason} ->
                    io:format("Failed to receive message ~p~n",[Reason]),    
                    gen_tcp:close(Socket)    
            end;
        {error, Reason} ->
            io:format("Error to get peername: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

receive_message(Socket, ClientName, Rooms) ->
    io:format("Rooms created by client ~p: ~p~n", [self(), Rooms]),
    case gen_tcp:recv(Socket, 0) of
        {ok, MessageBinary} ->
            Message =  binary_to_list(MessageBinary),
            if 
                Message == "/create-room" -> 
                    gen_server:cast(server_tcp, {message_received, {Socket,ClientName, self()}, Message}),  
                    receive
                        {add_room, {RoomName, RoomPID}} ->
                        %io:format("Add room: ~p with PID: ~p ~n", [RoomName, RoomPID]),
                        RoomsUpdate = maps:put(RoomName, RoomPID, Rooms),
                        receive_message(Socket, ClientName, RoomsUpdate)
                    end;
                Message == "/join-room" ->
                    gen_server:cast(server_tcp, {message_received, {Socket,ClientName, self()}, Message}),
                    receive
                        {restart_receive_message} ->
                            receive_message(Socket, ClientName, Rooms)
                    end;
                Message == "/leave-room" ->
                    gen_server:cast(server_tcp, {message_received, {Socket,ClientName, self()}, Message}),
                    receive
                        {restart_receive_message} ->
                            receive_message(Socket, ClientName, Rooms)
                    end;
                Message == "/destroy-room" ->
                    gen_server:cast(server_tcp, {message_received, {Socket,ClientName, self()}, Message}),
                    receive
                        {destroy_room, {RoomName}} ->
                            case maps:is_key(RoomName, Rooms) of
                                true ->
                                    RoomPid = maps:get(RoomName, Rooms),
                                    UpdatedRooms = maps:remove(RoomName, Rooms),
                                    {ok} =  gen_server:call(room_manager, {destroy_room, ClientName, RoomName}),
                                    exit(RoomPid, normal),
                                    receive_message(Socket, ClientName, UpdatedRooms);
                                false ->
                                    io:format("Client isn't the creator of the room ~p~n",[RoomName]),
                                    receive_message(Socket, ClientName, Rooms)
                            end
                    end;
                Message == "/send-message" ->
                    gen_server:cast(server_tcp, {message_received, {Socket,ClientName, self()}, Message}),
                    receive
                        {restart_receive_message} ->
                            receive_message(Socket, ClientName, Rooms)
                    end;
                true ->
                    gen_server:cast(server_tcp, {message_received, {Socket,ClientName, self()}, Message}),  
                    receive_message(Socket, ClientName, Rooms)  
            end;
        {error, closed} -> 
            io:format("Connection closed by client ~p~n", [ClientName]),
            gen_tcp:close(Socket);    
        {error, Reason} ->
            io:format("Error: ~p~n",[Reason]),
            gen_tcp:close(Socket)
    end. 

