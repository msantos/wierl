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
-module(wierl_config).
-export([
        attr/1, attr/3,
        open/0, close/1
    ]).

-include("wierl.hrl").

-define(FLAGS, 0).


open() ->
    procket:socket(inet, dgram, 0).
close(Socket) ->
    procket:close(Socket).


%%
%% Retrieve all wireless parameters
%%
attr(Dev) when is_binary(Dev) ->
    {ok, Socket} = open(),

    Attr = case info(Socket, Dev, ?SIOCGIWNAME) of
        <<>> ->
            {error, enotsup};
        Name ->
            [{name, Name}] ++
            [ {N, attr(Socket, Dev, N)} || N <-
                [nwid, freq, mode, essid,
                    encode, range, ap, rate, power] ]
        end,
    close(Socket),
    Attr.

%%
%% Retreive wireless setting
%%
attr(Socket, Dev, essid) ->
    info(Socket, Dev, ?SIOCGIWESSID, ?IW_ESSID_MAX_SIZE+2);
attr(Socket, Dev, encode) ->
    info(Socket, Dev, ?SIOCGIWENCODE, ?IW_ENCODING_TOKEN_MAX);
attr(Socket, Dev, range) ->
    % wireless-tools uses sizeof(iwrange)*2
    info(Socket, Dev, ?SIOCGIWRANGE, 1024);
attr(Socket, Dev, Key) when is_atom(Key) ->
    info(Socket, Dev, req(Key));

%%
%% Change wireless setting
%%
attr(Socket, Dev, {Key, Val}) when is_binary(Val); is_integer(Val) ->
    info(Socket, Dev, set(Key), Val);

attr(_Socket, _Dev, {_Key, _Value}) ->
    {error, unsupported}.


%%
%% ioctl
%%

%% null buffer
info(Socket, Dev, Req) ->
    Len = byte_size(Dev),
    Bits = (?IFNAMSIZ - Len) * 8,
    Struct = <<Dev/binary, 0:((?IFNAMSIZ - byte_size(Dev))*8), 0:(16*8)>>,

    case procket:ioctl(Socket, Req, Struct) of
        {ok, <<Dev:Len/bytes, 0:Bits, Value/binary>>} ->
            Value;
        % XXX Ignore unsupported attributes
        {error, enotsup} ->
            <<>>;
        {error, _} = Error ->
            Error
    end.


%%
%% ioctl with a dynamic buffer
%%
info(Socket, Dev, Req, Alloc) ->
    ReqLen = if
        is_binary(Alloc) -> byte_size(Alloc);
        is_integer(Alloc) -> Alloc
    end,

    Len = byte_size(Dev),
    Bits = (?IFNAMSIZ - Len) * 8,

    {ok, Struct, [Res]} = procket:alloc([
            <<Dev/bytes, 0:( (?IFNAMSIZ - byte_size(Dev))*8)>>,
            {ptr, Alloc},
            <<ReqLen:?UINT16, ?FLAGS:?UINT16>>
        ]),

    case procket:ioctl(Socket, Req, Struct) of
        {ok, <<Dev:Len/bytes, 0:Bits, _Ptr:?UINT32, ValLen:?UINT16, _Flag:?UINT16>>} ->
            {ok, <<Val:ValLen/bytes, _/binary>>} = procket:buf(Res),
            Val;
        {error, _} = Error ->
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
set(mode) -> ?SIOCSIWMODE ; 
set(freq) -> ?SIOCSIWFREQ;
set(channel) -> ?SIOCSIWFREQ;
set(bit) -> ?SIOCSIWRATE;
set(rate) -> ?SIOCSIWRATE;
set(encode) -> ?SIOCSIWENCODE;
set(key) -> ?SIOCSIWENCODE;
set(power) -> ?SIOCSIWPOWER;
set(commit) -> ?SIOCSIWCOMMIT.
