# NSI Safnari

A NSI v2.0 Aggregator (coordinator) implementation. The aggregator implements the Network Service Interface Connection Service v2.0 (NSI CS 2.0) using the [Play framework](http://www.playframework.com).

## Getting Safnari running

Safnari uses a PostgreSQL database to store the in en out going messages. The default configuration is in `conf/application.conf`. To setup Postgres run the following commands:

```
createuser -D -R -S nsi-safnari-user
createdb -O nsi-safnari-user nsi-safnari-dev
createdb -O nsi-safnari-user nsi-safnari-test
```

After this Safnari can be started with `sbt run` or running the tests with `sbt test`.  
Sbt can be downloaded from [http://www.scala-sbt.org](http://www.scala-sbt.org) or installed by `brew install sbt`

By default Safnari will use a dummy PCE and a dummy NSI requester. To use a real PCE and NSI requester change the `pce.actor` and `nsi.actor` properties in `conf/application.conf` to `real`.

`sbt stage` will create an executable in `target/universal/stage/bin/nsi-safnari` which can be used to run Safnari in production like environment. You can give it a configuration file that overrides the default per environment.
Like: `nsi-safnari -Dconfig.resource=prod.conf`. For further information you can look at the [Play site](http://www.playframework.com/documentation/2.2.x/Production).

## PCE

Safnari uses a JSON API to talk to the PCE (Path Computation Engine). Safnari is developed and testsed with the [nsi-pce](https://github.com/BandwidthOnDemand/nsi-pce).
On how to get this running look at the [README](https://github.com/BandwidthOnDemand/nsi-pce/blob/master/README) of the nsi-pce project. The address on which the PCE listens can be configured with the `pce.endpoint` property.

