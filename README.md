
# Erlang interface for manipulating 802.11 wireless interfaces

## NOTE ON PRIVILEGES

To run this code, Erlang will have to either run as root or have
CAP\_NET\_ADMIN privileges:

    setcap cap_net_admin=ep /path/to/beam


## QUICK SETUP

    cd wierl
    make

    sudo setcap cap_net_admin=ep /path/to/beam

    ./start.sh
    % Scan using the "wlan0" interface 
    wierl_scan:start(<<"wlan0">>).

    # If you want to remove the privs
    sudo setcap -r /path/to/beam


## TODO

* put interfaces into monitor mode

* dump 802.11 frames

