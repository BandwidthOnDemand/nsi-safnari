# syntax=docker/dockerfile:1
#
FROM openjdk:8

ENV VERSION 2.2.1
ENV REPOSITORY  https://github.com/BandwidthOnDemand/nsi-safnari
ENV PORT=9000
ENV ADDRESS="0.0.0.0"
ENV CONFIG=/config/config-overrides.conf
ENV EXTRA="-J-Xms512m -J-Xmx512m -J-server -J-verbose:gc -J-XX:+PrintGCDetails -J-XX:+PrintGCDateStamps -J-XX:+UseGCLogFileRotation -J-XX:NumberOfGCLogFiles=10 -J-XX:GCLogFileSize=10M -J-XX:+UseParallelGC -J-XX:+UseParallelOldGC"
# default trust store in configured here, the key store is configured in the config file
ENV TRUSTSTORE="-Djavax.net.ssl.trustStoreType=jks -Djavax.net.ssl.trustStorePassword=secret -Djavax.net.ssl.trustStore=target/universal/stage/conf/nsi-safnari-truststore.jks"

RUN apt update && apt -y install zip
RUN adduser --disabled-password --uid 12345 --gecos 'Safnari user' safnari
USER safnari:safnari
WORKDIR /nsi-safnari
RUN curl --location --output nsi-safnari-$VERSION.zip  $REPOSITORY/releases/download/v$VERSION/nsi-safnari-$VERSION.zip
RUN unzip nsi-safnari-$VERSION.zip
RUN mv nsi-safnari-$VERSION/* .
RUN rmdir nsi-safnari-$VERSION

EXPOSE 9000/tcp
CMD /nsi-safnari/bin/nsi-safnari -Dconfig.file=$CONFIG -DapplyEvolutions.default=true $TRUSTSTORE $EXTRA
