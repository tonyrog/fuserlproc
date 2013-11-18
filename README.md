fuserlproc 
==========

demo of fuserl (Erlang FUSE binding) which provides a 
proc-like filesystem.

# compile

    rebar compile

# run

    [ do once 
      cp sys.config to local.config
      <edit> local.config
    ]
    erl -config local.config -s fuserlproc

Now you shoule be able to peek into the $HOME/erlproc and
be able to inspect the Erlang node.
