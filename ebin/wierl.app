{application, wierl,
    [
    {description, "Erlang interface for manipulating 802.11 devices"},
    {vsn, "0.01"},
    {modules, [
        wierl_scan,
        wierl_config
            ]},
    {registered, []},
    {applications, [
        kernel,
        stdlib
            ]},
    {env, []}
    ]}.

