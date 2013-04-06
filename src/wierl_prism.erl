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

%%
%% Decode 802.11 prism headers
%%
-module(wierl_prism).
-export([
        header/1
    ]).

-include("wierl.hrl").
-include("wierl_frame.hrl").

-define(PRISM_VALUE, (4+2+2+4)/bytes).

%%-------------------------------------------------------------------------
%%% PRISM header
%%-------------------------------------------------------------------------

%% See:
%% http://home.martin.cc/linux/prism

header(<<
    Msgcode:?UINT32,
    Msglen:?UINT32,
    Devname:16/bytes,

    Hosttime:?PRISM_VALUE,
    Mactime:?PRISM_VALUE,
    Channel:?PRISM_VALUE,
    Rssi:?PRISM_VALUE,
    Sq:?PRISM_VALUE,
    Signal:?PRISM_VALUE,
    Noise:?PRISM_VALUE,
    Rate:?PRISM_VALUE,
    Istx:?PRISM_VALUE,
    Frmlen:?PRISM_VALUE,

    Frame/binary>>) ->

    {#ieee802_11_prism{
            msgcode = Msgcode,
            msglen = Msglen,
            devname = Devname,
            hosttime = prism_value(Hosttime),
            mactime = prism_value(Mactime),
            channel = prism_value(Channel),
            rssi = prism_value(Rssi),
            sq = prism_value(Sq),
            signal = prism_value(Signal),
            noise = prism_value(Noise),
            rate = prism_value(Rate),
            istx = prism_value(Istx),
            frmlen = prism_value(Frmlen)
        }, Frame};
header(#ieee802_11_prism{
        msgcode = Msgcode,
        msglen = Msglen,
        devname = Devname,
        hosttime = Hosttime,
        mactime = Mactime,
        channel = Channel,
        rssi = Rssi,
        sq = Sq,
        signal = Signal,
        noise = Noise,
        rate = Rate,
        istx = Istx,
        frmlen = Frmlen
    }) ->


    list_to_binary([
            <<Msgcode:?UINT32,
            Msglen:?UINT32,
            (devname(Devname))/bytes>>,

            rec_to_bin(Hosttime),
            rec_to_bin(Mactime),
            rec_to_bin(Channel),
            rec_to_bin(Rssi),
            rec_to_bin(Sq),
            rec_to_bin(Signal),
            rec_to_bin(Noise),
            rec_to_bin(Rate),
            rec_to_bin(Istx),
            rec_to_bin(Frmlen)
        ]).


rec_to_bin(Value) when is_binary(Value) ->
    Value;
rec_to_bin(#prism_value{} = Value) ->
    prism_value(Value).


prism_value(<<
    Did:?UINT32,
    Status:?UINT16,
    Len:?UINT16,
    Data:?UINT32
    >>) ->
    #prism_value{
        did = Did,
        status = Status,
        len = Len,
        data = Data
    };
prism_value(#prism_value{
        did = Did,
        status = Status,
        len = Len,
        data = Data
    }) ->
    <<
    Did:?UINT32,
    Status:?UINT16,
    Len:?UINT16,
    Data:?UINT32
    >>.

% Pad the device name to IFNAMSIZ bytes
devname(Dev) when byte_size(Dev) == 16 ->
    Dev;
devname(Dev) when byte_size(Dev) < 16 ->
    <<Dev/bytes, 0:((16-(byte_size(Dev)))*8)>>.
