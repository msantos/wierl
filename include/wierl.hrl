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

-define(WORDALIGN(X,Y), (((X)+((Y)-1)) band (bnot((Y)-1)))).

-define(UINT16, 2/native-unsigned-integer-unit:8).
-define(UINT32, 4/native-unsigned-integer-unit:8).
-define(UINT64, 8/native-unsigned-integer-unit:8).

-define(UINT16LE, 2/little-unsigned-integer-unit:8).
-define(UINT32LE, 4/little-unsigned-integer-unit:8).
-define(UINT64LE, 8/little-unsigned-integer-unit:8).

-define(INT16, 2/native-signed-integer-unit:8).
-define(INT32, 4/native-signed-integer-unit:8).

-define(INT16LE, 2/little-signed-integer-unit:8).
-define(INT32LE, 4/little-signed-integer-unit:8).

-define(IFNAMSIZ, 16).
-define(SIOCSIWSCAN, 16#8B18).  % Trigger a scan
-define(SIOCGIWSCAN, 16#8B19).  % Retrieve the scan results

-define(IW_MAX_BITRATES, 32).
-define(IW_MAX_ENCODING_SIZES, 8).
-define(IW_MAX_TXPOWER, 8).
-define(IW_ENCODING_TOKEN_MAX, 64).

% Scan a specific ESSID
-define(ARPHRD_ETHER, 1).
-define(IW_ESSID_MAX_SIZE, 32).
-define(IW_MAX_FREQUENCIES, 32).
-define(IW_SCAN_THIS_ESSID, 16#0002).


-define(IW_POWER_ON, 16#0000).
-define(IW_POWER_TYPE, 16#F000).
-define(IW_POWER_PERIOD, 16#1000).
-define(IW_POWER_TIMEOUT, 16#2000).
-define(IW_POWER_SAVING, 16#4000).
-define(IW_POWER_MODE, 16#0F00).
-define(IW_POWER_UNICAST_R, 16#0100).
-define(IW_POWER_MULTICAST_R, 16#0200).
-define(IW_POWER_ALL_R, 16#0300).
-define(IW_POWER_FORCE_S, 16#0400).
-define(IW_POWER_REPEATER, 16#0800).
-define(IW_POWER_MODIFIER, 16#000F).
-define(IW_POWER_MIN, 16#0001).
-define(IW_POWER_MAX, 16#0002).
-define(IW_POWER_RELATIVE, 16#0004).

% Event types
-define(SIOCGIWAP, 16#8B15).
-define(SIOCGIWAPLIST, 16#8B17).
-define(SIOCGIWAUTH, 16#8B33).
-define(SIOCGIWENCODE, 16#8B2B).
-define(SIOCGIWENCODEEXT, 16#8B35).
-define(SIOCGIWESSID, 16#8B1B).
-define(SIOCGIWFRAG, 16#8B25).
-define(SIOCGIWFREQ, 16#8B05).
-define(SIOCGIWGENIE, 16#8B31).
-define(SIOCGIWMODE, 16#8B07).
-define(SIOCGIWMODUL, 16#8B2F).
-define(SIOCGIWNAME, 16#8B01).
-define(SIOCGIWNICKN, 16#8B1D).
-define(SIOCGIWNWID, 16#8B03).
-define(SIOCGIWPOWER, 16#8B2D).
-define(SIOCGIWPRIV, 16#8B0D).
-define(SIOCGIWRANGE, 16#8B0B).
-define(SIOCGIWRATE, 16#8B21).
-define(SIOCGIWRETRY, 16#8B29).
-define(SIOCGIWRTS, 16#8B23).
-define(SIOCGIWSENS, 16#8B09).
-define(SIOCGIWSPY, 16#8B11).
-define(SIOCGIWSTATS, 16#8B0F).
-define(SIOCGIWTHRSPY, 16#8B13).
-define(SIOCGIWTXPOW, 16#8B27).

-define(IWEVGENIE, 16#8C05).
-define(IWEVQUAL, 16#8C01).
-define(IWEVCUSTOM, 16#8C02).

% Modes
-define(IW_MODE_AUTO, 0).
-define(IW_MODE_ADHOC, 1).
-define(IW_MODE_INFRA, 2).
-define(IW_MODE_MASTER, 3).
-define(IW_MODE_REPEAT, 4).
-define(IW_MODE_SECOND, 5).
-define(IW_MODE_MONITOR, 6).

% Quality
-define(IW_QUAL_QUAL_UPDATED, 16#01).

-define(SIOCSIWESSID, 16#8B1A).
-define(SIOCSIWMODE, 16#8B06).
-define(SIOCSIWFREQ, 16#8B04).
-define(SIOCSIWRATE, 16#8B20).
-define(SIOCSIWENCODE, 16#8B2A).
-define(SIOCSIWPOWER, 16#8B2C).
-define(SIOCSIWCOMMIT, 16#8B00).
