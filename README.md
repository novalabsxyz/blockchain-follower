helium-follower
=====

`helium-follower` is a skeleton application which contains all that you'll need to write a simple application which follows the helium blockchain.  All you'll need to do is clone it, do a whole-repo search replace for `helium_follower` => `my_app_name`, open `hf_follower.erl` and look for the `TODO` comment.  If this doesn't work, or you don't understand the comment says, please open an issue on the Github repo.

This repo is permissively licensed, so you don't need to tell us what you're doing with it, but we would love to know if you'd like to share.

Basic instructions
-----
First, you'll need to have Erlang installed.  You can use a later 21.x series OTP, or OTP 22+.  Anything earlier is unlikely to work because of our use of persistent terms.  You'll also need rebar3 installed (see [here](https://www.rebar3.org/docs/getting-started) for help with both).  once you have them installed, you can type:


    $ rebar3 release
    $ _build/default/rel/helium_follower/bin/helium_follower start
    $ tail -F _build/default/rel/helium_follower/log/console.log

Within a few minutes, you should see the chain begin to sync.  After it has synced up to the assumed valid height, it will begin to apply all the blocks, and you'll see the code in `hf_follower.erl`'s `add_block` `handle_info` clause start to fire.  After the sync, it will continue to sync the chain until it's up to date with cluster gossip.  Every time it appends a block, it will fire that `handle_info` clause.
