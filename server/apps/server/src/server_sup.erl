%%%-------------------------------------------------------------------
%% @doc server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(server_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
   SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{id => server_tcp,                    
        start => {server_tcp, start_link, []}, 
        restart => permanent,                 
        shutdown => brutal_kill,              
        type => worker,                       
        modules => [server_tcp]},

        #{id => room_manager,                    
        start => {room_manager, start_link, []}, 
        restart => permanent,                 
        shutdown => brutal_kill,              
        type => worker,                       
        modules => [room_manager]}
    ],
    {ok, {SupFlags, ChildSpecs}}.



