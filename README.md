zlang: zlang
============

What is it?
-----------
zlang is a dangerous idea wrapped in an intractable paradigm.

Code Guide
----------
Check out `examples/` for example zlang syntax.

Replace filenames and paths as necessary in the examples below.  All files are relative
to where you started your erlang VM.

You can (probably) compile the examples to an expanded LFE syntax
(macroexpand-1 basically) by:
    zlang:lfe_module_debug(iolist_to_binary(zlang:mod(zlang:compile({file, "editor.zlang"}), poopin))).

You can run a file directly by:
    f(), {ok, Name, Bin} =  zlang:lfe_module_binary(iolist_to_binary(zlang:mod(zlang:compile({file, "editor.zlang"}), poopin))).
    zlang:load_lfe_binary(Name, Bin).
    poopin:run_tests(zstub).

If you want to see the zlang lisp before it gets passed to LFE, do a quick:
    io:format("~s~n", [zlang:mod(zlang:compile({file, "editor.zlang"}), poop)]).


Building
--------
Get deps:
        ./rebar get-deps

Build:
        ./rebar compile

Testing
-------
Ha!  Tests?


When to use zlang
-----------------
Never.  Never use zlang.
