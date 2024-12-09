-module(room_manager).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, terminate/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, room_manager}, room_manager, [],[]).


init(_) ->
    io:format("Room manager started~n"),
    {ok,#{}}.

handle_call({create_room, RoomName}, _From, State) ->
    RoomPid = room:start(RoomName),
    io:format("Room created in room manager whit PID:~p~n",[RoomPid] ),
    NewState = State#{RoomName => RoomPid},
    {reply, {ok, RoomName, RoomPid}, NewState};

handle_call({list_room}, _From, State) ->
    io:format("Rooms available: ~p~n",[State] ),
    {reply, {ok, State}, State};

handle_call({add_client_to_room,ClientName, ClientPID, RoomName}, _From, State) -> 
    RoomPid = maps:get(RoomName, State),
    RoomPid ! {add_member, {ClientName, ClientPID}},
    {reply, {ok}, State};

handle_call({remove_client_to_room, ClientName, ClientPID, RoomName}, _From, State) -> 
    RoomPid = maps:get(RoomName, State),
    RoomPid ! {remove_member, {ClientName, ClientPID}},
    {reply, {ok}, State};

handle_call({destroy_room, ClientName, RoomName}, _From, State) -> 
    UpdatedState = maps:remove(RoomName, State),
    {reply, {ok}, UpdatedState};

handle_call({send_message_room, ClientName, RoomName, Message}, _From ,State) ->
    case maps:is_key(RoomName, State) of
        true -> 
            RoomPid = maps:get(RoomName, State),
            RoomPid ! {send_message, {ClientName, Message}};
        false ->
            io:format("Room doesn't exist")
    end,
    {reply, {ok}, State}.



handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    io:format("Terminating room manager ~n"),
    ok.