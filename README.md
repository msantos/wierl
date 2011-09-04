# Erlang interface for manipulating 802.11 wireless network devices

wierl is a set of Erlang modules for interacting with 802.11 wireless
devices on Linux.

## NOTE ON PRIVILEGES
 
To run this code, Erlang will either have to run as root or have
CAP\_NET\_ADMIN privileges:

    setcap cap_net_admin=ep /path/to/beam


## QUICK SETUP

    cd wierl
    make

    sudo setcap cap_net_admin=ep /path/to/beam

    ./start.sh
    % Scan using the "wlan0" interface 
    wierl_scan:list(<<"wlan0">>).

    # Monitor mode: make sure the network manager is stopped. For example,
    # on Ubuntu: service network-manager stop
    {ok, Socket} = wierl_monitor:open(<<"wlan0">>),
    {ok, Frame} = wierl_monitor:read(Socket),
    wierl_monitor:frame(Socket, Frame).

    # If you want to remove the privs
    sudo setcap -r /path/to/beam


## USAGE

### wierl_scan

    wierl_scan:list() ->  [{Interface, AccessPoints}]
    wierl_scan:list(Interface) ->  AccessPoints
    wierl_scan:list(Interface, Options) ->  AccessPoints

        Types   Interface = binary()
                AccessPoints = [AccessPoint]
                AccessPoint = {BSSID, ScanInfo}
                BSSID = binary()
                ScanInfo = [Info]
                Info = {Key, binary()}
                Key = custom
                    | encode
                    | essid
                    | freq
                    | genie
                    | mode
                    | qual
                    | rate
                Options = [{essid, binary()}]

    Initiate a wireless scan and return the scan list.

    wierl:format(AccessPoints) -> proplist()

    Decode some of the binary data values returned in the list of
    access points.

### wierl_config

    param(Ifname) -> Parameters
    param(Ifname, Attr) -> binary() | {error, unsupported}
            | {error, posix()}
    param(Socket, Ifname, Attr) -> binary() | {error, unsupported}
            | {error, posix()}

        Types   Ifname = binary()
                Socket = int()
                Attr = {Key,Value} | Key
                Key = name
                    | nwid
                    | freq
                    | mode
                    | essid
                    | encode
                    | range
                    | ap
                    | rate
                    | power
                Value = binary() | integer()
                Parameters = [Parameter]
                Parameter = {name, binary()}
                    | {nwid, binary()}
                    | {freq, binary()}
                    | {mode, binary()}
                    | {essid, binary()}
                    | {encode, binary()}
                    | {range, binary()}
                    | {ap, binary()}
                    | {rate, binary()}
                    | {power, binary()}

    Query or set a wireless parameter.

    param/1 lists all parameters for the interface.

    param/3 queries or sets a single parameter.

    param/2 is a wrapper around param/3 that will open and close the
    netlink socket for the caller.

    Use the wierl module to decode the values, e.g.,

        1> wierl_config:param(<<"wlan0">>, freq).
        <<108,9,0,0,6,0,0,0,0,0,0,0,0,0,0,0>>

        2> wierl:decode({freq,<<108,9,0,0,6,0,0,0,0,0,0,0,0,0,0,0>>}).
        {frequency,2.412e9}

    To set a parameter, use a key/value as the attribute, e.g.,

        1> wierl_config:param(<<"wlan0">>, {essid, <<"MY ESSID">>}).
        <<"MY ESSID">>

    Depending on the parameter, the value can be either an integer or a
    binary (which will be converted to a pointer to an iw_point struct
    and may require assigning a value to the flag field of the structure).

    To change some parameters, the interface must first be brought
    down. For example, to put the interface into monitor mode:

        wierl_config:down(<<"wlan0">>),
        wierl_config:param(<<"wlan0">>, {mode, wierl:mode(monitor)}),
        wierl_config:up(<<"wlan0">>).


    up(Ifname) -> ok

        Types   Ifname = binary()

    Configure the interface as up and running.


    down(Ifname) -> ok

        Types   Ifname = binary()

    Bring down the interface.


    open() -> {ok, FD}

        Types   FD = integer()

    Obtain a netlink socket file descriptor.


    close(FD) -> ok

        Types   FD = integer()

    Close the file descriptor.

### wierl

### wierl_monitor

    wierl_monitor:open(Interface) ->  {ok, Socket} | {error, posix()}

        Types   Interface = binary()
                Socket = pid()
		
    Place a wireless network interface into monitor mode, returning a
    file descriptor (the pid of a gen_server holding the fd) that can
    be used for reading 802.11 frames.

    wierl_monitor:close(Socket) ->  ok | {error, posix()}

        Types   Socket = pid()
                Interface = binary()

    Close the file descriptor associated with the wireless device. close/1
    leaves the device in monitor mode.

    wierl_monitor:read(Socket) -> {ok, Frame} | {error, posix()}
    wierl_monitor:read(Socket, Size) -> {ok, Frame} | {error, posix()}

        Types   Socket = pid()
                Size = integer()
                Frame = binary()

    Attempt to read a frame from the wireless device.

    wierl_monitor:write(Socket, Frame) -> ok | {error, posix()}

        Types   Socket = pid()
                Frame = binary()

    Attempt to write a frame to the wireless device.

    wierl_monitor:frame(Socket, Frame) -> {Radiotap, FrameControl,
        FrameBody, FrameCheckSequence} | {error, bad_frame}
    wierl_monitor:frame(Socket, {FrameControl, FrameBody}) -> Frame

        Types   Socket = pid()
                Frame = binary()
                Radiotap = #ieee802_11_radiotap{}
                FrameControl = #ieee802_11_fc{}
                FrameBody = #ieee802_11_management{}
                    | #ieee802_11_cf_rts{},
                    | #ieee802_11_cf_cts{},
                    | #ieee802_11_cf_ack{},
                    | #ieee802_11_cf_ps{},
                    | #ieee802_11_cf_cfend{},
                    | #ieee802_11_cf_bar{},
                    | #ieee802_11_cf_ba{},
                    | #ieee802_11_data{}
                FrameCheckSequence = integer()

    Encode or decode an 802.11 wireless frame between binaries and
    records. Include the wierl_frame header to have access to the
    record structures:

    -include("include/wierl_frame.hrl")


### rfkill

rfkill is a wireless soft kill switch.

    block() -> ok | {error, posix()}

    Disable wireless devices.

    unblock() -> ok | {error, posix()}

    Enable wireless devices.

    list() -> ok | {error, posix()}

    Monitor rfkill device events.


## TODO

* fix padding issues between 32/64-bit archs
    * e.g., on 64-bit, this is broken:

        wierl:decode({freq, wierl_config:param(<<"wlan0">>, freq)}).

* get stats from /proc/net/wireless

* wierl_monitor
    * sending frames
    * cleanup: redundant constants between wierl/procket
    * frame/1: allow the caller to specify the header type

* support Mac OS X
