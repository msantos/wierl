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
-module(wierl_config).
-export([
        param/1, param/2, param/3,
        open/0, close/1,
        up/1, down/1
    ]).

-include("wierl.hrl").

-define(IFF_RUNNING, 16#40).
-define(IFF_UP, 16#01).

-define(SIOCGIFFLAGS, 16#8913).
-define(SIOCSIFFLAGS, 16#8914).


open() ->
    procket:socket(inet, dgram, 0).
close(Socket) ->
    procket:close(Socket).


%%
%% Retrieve all wireless parameters
%%
param(Dev) when is_binary(Dev) ->
    {ok, Socket} = open(),

    Attr = case ioctl(Socket, Dev, ?SIOCGIWNAME) of
        {ok, <<>>} ->
            {error, enotsup};
        Name ->
            [{name, Name}] ++
            [ {N, param(Socket, Dev, N)} || N <-
                [nwid, freq, mode, essid,
                    encode, range, ap, rate, power] ]
        end,
    close(Socket),
    Attr.

param(Dev, Param) when is_binary(Dev) ->
    {ok, Socket} = open(),
    Res = param(Socket, Dev, Param),
    close(Socket),
    Res.

%%
%% Retreive wireless setting
%%
param(Socket, Dev, essid) ->
    ioctl_point(Socket, Dev, ?SIOCGIWESSID, ?IW_ESSID_MAX_SIZE+2);
param(Socket, Dev, encode) ->
    ioctl_point(Socket, Dev, ?SIOCGIWENCODE, ?IW_ENCODING_TOKEN_MAX);
param(Socket, Dev, range) ->
    % wireless-tools uses sizeof(iwrange)*2
    ioctl_point(Socket, Dev, ?SIOCGIWRANGE, 1024);
param(Socket, Dev, Key) when is_atom(Key) ->
    ioctl(Socket, Dev, req(Key));

%%
%% Change wireless setting
%%
param(Socket, Dev, {Key, Val}) when is_atom(Key), is_integer(Val) ->
    Struct = <<Val:?UINT32, 0:(12*8)>>,
    ioctl(Socket, Dev, set(Key), Struct);
param(Socket, Dev, {essid, Val}) when is_binary(Val) ->
    ioctl_point(Socket, Dev, set(essid), Val, 1);
param(Socket, Dev, {Key, Val}) when is_atom(Key), is_binary(Val) ->
    ioctl_point(Socket, Dev, set(Key), Val);
param(Socket, Dev, {Key, Val, Flag}) when is_atom(Key), is_binary(Val) ->
    ioctl_point(Socket, Dev, set(Key), Val, Flag);

param(_Socket, _Dev, {_Key, _Value}) ->
    {error, unsupported}.


%%
%% ioctl
%%

%% null buffer
ioctl(Socket, Dev, Req) ->
    ioctl(Socket, Dev, Req, <<0:(16*8)>>).

ioctl(Socket, Dev, Req, Buf) ->
    Len = byte_size(Dev),
    Bits = (?IFNAMSIZ - Len - 1) * 8,
    Struct = <<Dev/binary, 0:Bits, 0, Buf/binary>>,

    case procket:ioctl(Socket, Req, Struct) of
        {ok, <<Dev:Len/bytes, 0:Bits, 0, Value/binary>>} ->
            {ok, Value};
        % XXX Ignore unsupported parameters
        {error, enotsup} ->
            {ok, <<>>};
        {error, _} = Error ->
            Error
    end.


%%
%% ioctl using an iw_point structure
%%
ioctl_point(Socket, Dev, Req, Alloc) ->
    ioctl_point(Socket, Dev, Req, Alloc, 0).

ioctl_point(Socket, Dev, Req, Alloc, Flags) ->
    ReqLen = if
        is_binary(Alloc) -> byte_size(Alloc);
        is_integer(Alloc) -> Alloc
    end,

    Len = byte_size(Dev),
    Bits = (?IFNAMSIZ - Len - 1) * 8,

    {ok, Struct, [Res]} = procket:alloc([
            <<Dev/bytes, 0:Bits, 0>>,
            {ptr, Alloc},
            <<ReqLen:?UINT16, Flags:?UINT16>>
        ]),

    Pointer = erlang:system_info({wordsize, external}) * 8,

    case procket:ioctl(Socket, Req, Struct) of
        {ok, <<Dev:Len/bytes, 0:Bits, 0, _Ptr:Pointer, ValLen:?UINT16, _Flag:?UINT16>>} ->
            {ok, <<Val:ValLen/bytes, _/binary>>} = procket:buf(Res),
            {ok, Val};
        {error, _} = Error ->
            Error
    end.

%%
%% set interface RUNNING/UP status
%%
up(Dev) when byte_size(Dev) < ?IFNAMSIZ ->
    {ok, Socket} = open(),

    {ok, Flag} = get_flag(Socket, Dev),
    ok = set_flag(Socket, Dev, Flag bor ?IFF_RUNNING bor ?IFF_UP),

    ok = close(Socket),
    ok.

down(Dev) when byte_size(Dev) < ?IFNAMSIZ ->
    {ok, Socket} = open(),

    {ok, Flags} = get_flag(Socket, Dev),
    ok = set_flag(Socket, Dev, Flags band bnot(?IFF_UP)),

    ok = close(Socket),
    ok.

set_flag(FD, Dev, Flag) ->
    Res = procket:ioctl(FD, ?SIOCSIFFLAGS,
        <<Dev/bytes, 0:((15-byte_size(Dev))*8), 0:8, Flag:?UINT16, 0:(14*8)>>),
    case Res of
        {ok, _} -> ok;
        Error -> Error
    end.

get_flag(FD, Dev) ->
    Res = procket:ioctl(FD, ?SIOCGIFFLAGS,
        <<Dev/bytes, 0:((15-byte_size(Dev))*8), 0:(16*8)>>),
    case Res of
        {ok, <<_:(16*8), Flag:?UINT16, _/binary>>} ->
            {ok, Flag};
        Error ->
            Error
    end.


req(nwid) -> ?SIOCGIWNWID;
req(freq) -> ?SIOCGIWFREQ;
req(mode) -> ?SIOCGIWMODE;
req(essid) -> ?SIOCGIWESSID;
req(encode) -> ?SIOCGIWENCODE;
req(range) -> ?SIOCGIWRANGE;
req(bssid) -> ?SIOCGIWAP;
req(ap) -> ?SIOCGIWAP;
req(rate) -> ?SIOCGIWRATE;
req(power) -> ?SIOCGIWPOWER.

set(essid) -> ?SIOCSIWESSID;
set(mode) -> ?SIOCSIWMODE;
set(freq) -> ?SIOCSIWFREQ;
set(channel) -> ?SIOCSIWFREQ;
set(bit) -> ?SIOCSIWRATE;
set(rate) -> ?SIOCSIWRATE;
set(encode) -> ?SIOCSIWENCODE;
set(key) -> ?SIOCSIWENCODE;
set(power) -> ?SIOCSIWPOWER;
set(name) -> set(commit);
set(commit) -> ?SIOCSIWCOMMIT.
