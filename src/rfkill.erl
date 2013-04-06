%% Copyright (c) 2011-2013, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%%
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%%
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(rfkill).
-export([
        block/0, unblock/0,
        list/0,

        decode/1, state/1, type/1, op/1
    ]).

-include("wierl.hrl").
-include("rfkill.hrl").


block() ->
    write(block(on)).
unblock() ->
    write(block(off)).

block(on) -> block(?BLOCK);
block(off) -> block(?UNBLOCK);

block(Block) when is_integer(Block) ->
    <<?UINT32(0),                            % idx
    0:8,                                    % type
    ?RFKILL_OP_CHANGE_ALL:8,                % op
    Block:8,                                % soft
    0:8                                     % hard
    >>.

event(<<?UINT32(Idx),
    Type:8,
    Op:8,
    Soft:8,
    Hard:8>>) ->
    #rfkill_event{
        idx = Idx,
        type = Type,
        op = Op,
        soft = Soft,
        hard = Hard
    }.

list() ->
    case procket:dev("rfkill") of
        {ok, FD} ->
            Res = list_1(FD),
            ok = procket:close(FD),
            Res;
        Error ->
            Error
    end.

list_1(FD) ->
    {ok, Data} = procket:read(FD, 8),

    case event(Data) of
        #rfkill_event{op = ?RFKILL_OP_ADD} = Event ->
            Event;
        {error, eagain} ->
            timer:sleep(10),
            list_1(FD);
        Error ->
            Error
    end.


write(Event) when is_binary(Event) ->
    case procket:dev("rfkill") of
        {ok, FD} ->
            write_1(FD, Event);
        Error ->
            Error
    end.

write_1(FD, Event) ->
    Result = procket:write(FD, Event),
    ok = procket:close(FD),
    Result.

decode({event, Event}) when is_list(Event) ->
    [ decode(N) || N <- Event ];

decode({state, Type}) -> {state, state(Type)};
decode({op, Type}) -> {op, op(Type)};
decode({type, Type}) -> {type, type(Type)};
decode({Block, ?BLOCK}) when Block == soft; Block == hard ->
    {Block, true};
decode({Block, ?UNBLOCK}) when Block == soft; Block == hard ->
    {Block, false};
decode({_,_} = N) -> N.


state(soft_blocked) -> ?RFKILL_STATE_SOFT_BLOCKED;
state(unblocked) -> ?RFKILL_STATE_UNBLOCKED;
state(hard_blocked) -> ?RFKILL_STATE_HARD_BLOCKED;

state(?RFKILL_STATE_SOFT_BLOCKED) -> soft_blocked;
state(?RFKILL_STATE_UNBLOCKED) -> unblocked;
state(?RFKILL_STATE_HARD_BLOCKED) -> hard_blocked.


op(add) -> ?RFKILL_OP_ADD;
op(del) -> ?RFKILL_OP_DEL;
op(change) -> ?RFKILL_OP_CHANGE;
op(change_all) -> ?RFKILL_OP_CHANGE_ALL;

op(?RFKILL_OP_ADD) -> add;
op(?RFKILL_OP_DEL) -> del;
op(?RFKILL_OP_CHANGE) -> change;
op(?RFKILL_OP_CHANGE_ALL) -> change_all.


type(all) -> ?RFKILL_TYPE_ALL;
type(wlan) -> ?RFKILL_TYPE_WLAN;
type(bluetooth) -> ?RFKILL_TYPE_BLUETOOTH;
type(uwb) -> ?RFKILL_TYPE_UWB;
type(wimax) -> ?RFKILL_TYPE_WIMAX;
type(wwan) -> ?RFKILL_TYPE_WWAN;
type(gps) -> ?RFKILL_TYPE_GPS;
type(fm) -> ?RFKILL_TYPE_FM;

type(?RFKILL_TYPE_ALL) -> all;
type(?RFKILL_TYPE_WLAN) -> wlan;
type(?RFKILL_TYPE_BLUETOOTH) -> bluetooth;
type(?RFKILL_TYPE_UWB) -> uwb;
type(?RFKILL_TYPE_WIMAX) -> wimax;
type(?RFKILL_TYPE_WWAN) -> wwan;
type(?RFKILL_TYPE_GPS) -> gps;
type(?RFKILL_TYPE_FM) -> fm.
