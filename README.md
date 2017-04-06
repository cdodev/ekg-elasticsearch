# ekg-elastic: elastic backend for ekg

This library lets you send metrics gathered by the ekg family of
packages (e.g. ekg-core and ekg) to
[elastic](https://www.elastic.co). 

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
[GitHub issue tracker](https://github.com/cdodev/ekg-elastic/issues).

Master [git repository](https://github.com/cdodev/ekg-elastic):

    git clone https://github.com/cdodev/ekg-elastic.git

# Authors

This library is written and maintained by Ben Ford,
<ben@perurbis.com>.
