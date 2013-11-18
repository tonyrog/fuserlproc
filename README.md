fuserlproc 
==========

demo of fuserl (Erlang FUSE binding) which provides a 
proc-like filesystem.

# compile

    rebar compile

# run

    [ do once 
      cp sys.config local.config
      <edit> local.config
    ]
    erl -config local.config -s fuserlproc

Now you should be able find the directory $HOME/erlproc (or what ever you called your mount point)
and be able to inspect the Erlang node.
