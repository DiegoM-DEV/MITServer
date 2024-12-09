-module(room).
-export([start/1, init/2]).

start(RoomName)->
    spawn(room, init,[RoomName,#{}]).

init(RoomName, Members) ->
    io:format("Name: ~p, Members: ~p ~n", [RoomName, Members]),
    room_manager ! {room_name, RoomName},
    handle_message(RoomName, Members).
    

handle_message(RoomName, Members) -> 
    io:format("Members: ~p ~n", [Members]),
    receive
        {add_member, {ClientName, ClientPid}} ->
            UpdatedMembers = maps:put(ClientName, ClientPid, Members),
            io:format("~p added to ~p ~n", [ClientName, RoomName]),
            handle_message(RoomName, UpdatedMembers);
        {remove_member, {ClientName, ClientPid}} ->
            UpdatedMembers = maps:remove(ClientName, Members),
            io:format("~p removed to ~p ~n", [ClientName, RoomName]),
            handle_message(RoomName, UpdatedMembers);
        {send_message, {ClientName, Message}} ->
            case maps:is_key(ClientName, Members) of
                true ->
                    MembersWithoutSender = maps:remove(ClientName, Members),
                    server_tcp ! {send_message_to_all,{MembersWithoutSender}};
                false ->
                    io:format("Client: ~p isn't in the room~n", [ClientName])
            end,
            handle_message(RoomName, Members)
    end.


