% Simulation interval
-define(INTERVAL, 33). % about 1/30th of a second. (33 ms)

% Simulation time at which we start warning about our load.
-define(WARN_INTERVAL, (?INTERVAL - (?INTERVAL * 0.05))). % 5% of Interval
