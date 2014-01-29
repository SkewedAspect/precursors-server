# Precursors Server

[![Build Status](https://travis-ci.org/SkewedAspect/precursors-server.png?branch=master)](https://travis-ci.org/SkewedAspect/precursors-server)

This is the MMO server for the RFI:Precursors project. Written in a hybrid
of Erlang and C++, it currently supports authentication, chat, physics
simulation, and high levels of concurrency.

# Sponsors

[IntelliJ IDEA](http://www.jetbrains.com/idea/index.html) IDE generously
donated by [JetBrains](http://jetbrains.com/), through their
[open source program](http://www.jetbrains.com/idea/buy/buy.jsp#openSource).
If you haven't seen their IDE yet, it's amazing.

![JetBrains](http://skewedaspect.com/logos/logo_jetbrains_140.png "Jetbrains")

# Building and Running

## For development
If you're just doing everyday development:

    make devrel
	./devrel start

This will build the project, create a development release, and start the server
running detached from your terminal; you may then run `./devrel attach` to
attach to the running server. Alternatively, you can run `./devrel console` to
start the server and keep it attached to your terminal.

After running `make devrel` for the first time, you should be able to simply
run `make` after making code changes; regenerating the dev release is not
necessary.

## For production
For a production release, run:

    make rel
    ./rel/precursors_server/bin/precursors_server start

