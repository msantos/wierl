%% Copyright (c) 2011, Michael Santos <michael.santos@gmail.com>
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

-define(RFKILL_STATE_SOFT_BLOCKED, 0).
-define(RFKILL_STATE_UNBLOCKED, 1).
-define(RFKILL_STATE_HARD_BLOCKED, 2).

-define(RFKILL_OP_ADD, 0).
-define(RFKILL_OP_DEL, 1).
-define(RFKILL_OP_CHANGE, 2).
-define(RFKILL_OP_CHANGE_ALL, 3).

-define(RFKILL_TYPE_ALL, 0).
-define(RFKILL_TYPE_WLAN, 1).
-define(RFKILL_TYPE_BLUETOOTH, 2).
-define(RFKILL_TYPE_UWB, 3).
-define(RFKILL_TYPE_WIMAX, 4).
-define(RFKILL_TYPE_WWAN, 5).
-define(RFKILL_TYPE_GPS, 6).
-define(RFKILL_TYPE_FM, 7).

-define(BLOCK, 1).
-define(UNBLOCK, 0).

%% struct rfkill_event {
%%     __u32 idx;
%%     __u8  type;
%%     __u8  op;
%%     __u8  soft, hard;
%% } __packed;
-record(event, {
        idx = 0,
        type = 0,
        op = 0,
        soft = 0,
        hard = 0
    }).

block() ->
    Event = block(on),
    write(Event).
unblock() ->
    Event = block(off),
    write(Event).

block(on) -> block(?BLOCK);
block(off) -> block(?UNBLOCK);

block(Block) when is_integer(Block) ->
    <<0:4/native-unsigned-integer-unit:8,   % idx
    0:8,                                    % type
    ?RFKILL_OP_CHANGE_ALL:8,                % op
    Block:8,                                % soft
    0:8                                     % hard
    >>.

event(<<Idx:4/native-unsigned-integer-unit:8,
    Type:8,
    Op:8,
    Soft:8,
    Hard:8>>) ->
    #event{
        idx = Idx,
        type = Type,
        op = Op,
        soft = Soft,
        hard = Hard
    }.

list() ->
    {ok, FD} = procket:dev("rfkill"),
    Res = list_1(FD),
    ok = procket:close(FD),
    Res.

list_1(FD) ->
    {ok, Data} = procket:read(FD, 8),

    Event = case event(Data) of
        #event{op = ?RFKILL_OP_ADD} = E ->
            E;
        _ -> % XXX not tail recursive
            list_1(FD)
    end,

    {event, [{idx, Event#event.idx},
            {type, Event#event.type},
            {op, Event#event.op},
            {soft, Event#event.soft},
            {hard, Event#event.hard}]}.


write(Event) when is_binary(Event) ->
    {ok, FD} = procket:dev("rfkill"),
    ok = procket:write(FD, Event),
    ok = procket:close(FD),
    ok.

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
