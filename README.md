# NSI Safnari

A Nsi Aggregator (coordinator) implementation.

## Setup

Safnari uses a PostgreSQL database in production mode. The default configuration is in `conf/application.conf`. To setup Postgres run the following commands:

```
createuser -D -R -S nsi-safnari-user
createdb -O nsi-safnari-user nsi-safnari-dev
createdb -O nsi-safnari-user nsi-safnari-test
```
