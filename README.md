# ekg-elastic: elastic backend for ekg

This library lets you send metrics gathered by the ekg family of
packages (e.g. ekg-core and ekg) to
[elastic](https://github.com/etsy/elastic/). While elastic fulfills a
very similar role to ekg, it supports many more backends/graphing
systems (e.g. Graphite). By sending your metrics to elastic, you can
have your ekg metrics appear in these systems.

# Getting started

Exporting metrics to elastic is simple. Either create an empty metric
store and register some metrics

    import System.Metrics
    import System.Remote.Monitoring.Elastic

    main = do
        store <- newStore
        registerGcMetrics store
        forkElastic defaultElasticOptions store
        ...

or use the default metrics and metric store provided by the ekg
package

    import System.Remote.Monitoring
    import System.Remote.Monitoring.Elastic

    main = do
        handle <- forkServer "localhost" 8000
        forkElastic defaultElasticOptions (serverMetricStore handle)
        ...

`forkElastic` starts a new thread the will periodically send your
metrics to elastic using UDP.

# Get involved!

Please report bugs via the
[GitHub issue tracker](https://github.com/tibbe/ekg-elastic/issues).

Master [git repository](https://github.com/tibbe/ekg-elastic):

    git clone https://github.com/tibbe/ekg-elastic.git

# Authors

This library is written and maintained by Johan Tibell,
<johan.tibell@gmail.com>.
