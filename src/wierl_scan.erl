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
-module(wierl_scan).
-export([start/1]).

-include("wierl_scan.hrl").


%%
%% Open an unprivileged, datagram socket using
%% procket
%%

start(Dev) when is_binary(Dev) ->
    {ok, Socket} = procket:socket(inet, dgram, 0),
    scan(Dev, Socket),
    procket:close(Socket).

%%
%% Initiate the scan
%%

scan(Dev, Socket) when byte_size(Dev) < ?IFNAMSIZ ->
    Req = <<Dev/binary, 0:((?IFNAMSIZ - byte_size(Dev))*8), 0:(16*8)>>,
    {ok, _Req} = procket:ioctl(Socket, ?SIOCSIWSCAN, Req),
    result(Dev, Socket).

%%
%% Retrieve the scan results by specifying a buffer for the
%% kernel to return the results
%%

result(Dev, Socket) ->
    {ok, Req, [Res]} = procket:alloc([
        <<Dev/bytes, 0:( (?IFNAMSIZ - byte_size(Dev))*8)>>,
        {ptr, 4096},
        <<4096:?UINT16, 0:?UINT16>>
    ]),
    case procket:ioctl(Socket, ?SIOCGIWSCAN, Req) of
        {ok, <<_Ifname:16/bytes, _Ptr:?UINT32, Len:?UINT16, _Flag:?UINT16>>} ->
            {ok, <<Stream:Len/bytes, _/binary>>} = procket:buf(Res),
            event(Stream);

        % Poll the socket for the results
        {error, eagain} ->
            timer:sleep(1000),
            result(Dev, Socket);

        Error ->
            Error
    end.

% The events are returned in the form: length, command, data
% (length bytes)
% 
% We believe the length returned in the packet and print out
% the binary data. If the length is wrong, we'll just crash.
event(<<>>) ->
    ok;
event(<<Len:?UINT16, Cmd:?UINT16, Bin/binary>>) ->
    EventLen = Len - 4,
    <<Event:EventLen/bytes, Rest/binary>> = Bin,
    io:format("~p:~p~n", [cmd(Cmd), Event]),
    event(Rest).

% Convert the integer command values to almost human readable atoms
cmd(?SIOCGIWNAME) -> name;
cmd(?SIOCGIWNWID) -> nwid;
cmd(?SIOCGIWFREQ) -> freq;
cmd(?SIOCGIWMODE) -> mode;
cmd(?SIOCGIWSENS) -> sens;
cmd(?SIOCGIWRANGE) -> range;
cmd(?SIOCGIWPRIV) -> priv;
cmd(?SIOCGIWSTATS) -> stats;
cmd(?SIOCGIWSPY) -> spy;
cmd(?SIOCGIWTHRSPY) -> thrspy;
cmd(?SIOCGIWAP) -> bssid;
cmd(?SIOCGIWAPLIST) -> aplist;
cmd(?SIOCGIWESSID) -> essid;
cmd(?SIOCGIWNICKN) -> nickn;
cmd(?SIOCGIWRATE) -> rate;
cmd(?SIOCGIWRTS) -> rts;
cmd(?SIOCGIWFRAG) -> frag;
cmd(?SIOCGIWTXPOW) -> txpow;
cmd(?SIOCGIWRETRY) -> retry;
cmd(?SIOCGIWENCODE) -> encode;
cmd(?SIOCGIWPOWER) -> power;
cmd(?SIOCGIWMODUL) -> modul;
cmd(?SIOCGIWGENIE) -> genie;
cmd(?SIOCGIWAUTH) -> auth;
cmd(?SIOCGIWENCODEEXT) -> encodeext;
cmd(?IWEVGENIE) -> genie;
cmd(?IWEVQUAL) -> qual;
cmd(?IWEVCUSTOM) -> custom;
cmd(Unknown) -> {unknown, Unknown}.
