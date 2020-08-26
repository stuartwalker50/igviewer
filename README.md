# IG Viewer

* Connects to the IG broker through their api to view open trades (based on [ig-haskell](https://github.com/wjdhamilton/ig-haskell))
* Consumes tick data received via ZMQ (zeroMQ) and calculates moving averages over different timescales
* Multiple securities can be monitored by calculating metrics in separate threads, using STM to make results available for display without conflicts
* Displays realtime updates to metrics using the declarative Terminal UI library [Brick](https://github.com/jtdaugherty/brick)
