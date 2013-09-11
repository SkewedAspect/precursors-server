# Precursors Server

This is the MMO server for the RFI:Precursors project. Written in a hybrid
of Erlang and C++, it currently supports authentication, chat, physics
simulation, and high levels of concurrency.

# Sponsors

[IntelliJIDEA](http://www.jetbrains.com/idea/index.html) IDE generously
donated by [JetBrains](http://jetbrains.com/), through their
[open source program](http://www.jetbrains.com/idea/buy/buy.jsp#openSource).
If you haven't seen their IDE yet, it's amazing.

![JetBrains](http://skewedaspect.com/logos/logo_jetbrains_140.png "Jetbrains")

# Building and Running

If you just want a quick and dirty start up:

    ./devboot

This will do a build and create a development enviroment, dumping you into
the erlang console at start up.

For a production build and run:

		./buildcerts
    make rel
		./rel/precursors_server/bin/precursors_server start

