# syntax=docker/dockerfile:1
#
FROM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.2_13_1.10.2_3.5.1 AS build
RUN apt-get update && apt-get install -y nodejs
WORKDIR /usr/local/src/nsi-safnari
COPY build.sbt .
COPY .git/ .git/
COPY app/ app/
COPY conf/ conf/
COPY project/ project/
COPY public/ public/
RUN sbt packageZipTarball


FROM eclipse-temurin:21

ENV PORT=9000
ENV ADDRESS="0.0.0.0"
ENV CONFIG=/config/config-overrides.conf
ENV EXTRA="-J-Xms512m -J-Xmx512m -J-server -J-verbose:gc -J-XX:+PrintGCDetails -J-XX:+PrintGCDateStamps -J-XX:+UseGCLogFileRotation -J-XX:NumberOfGCLogFiles=10 -J-XX:GCLogFileSize=10M"
# default trust store in configured here, the key store is configured in the config file
ENV TRUSTSTORE="-Djavax.net.ssl.trustStoreType=jks -Djavax.net.ssl.trustStorePassword=secret -Djavax.net.ssl.trustStore=target/universal/stage/conf/nsi-safnari-truststore.jks"

RUN useradd --uid 12345 --comment 'Safnari user' safnari
USER safnari:safnari
WORKDIR /nsi-safnari
COPY --from=build /usr/local/src/nsi-safnari/target/universal/*.tgz nsi-safnari.tgz
RUN tar xvzf nsi-safnari.tgz --strip-components=1 && rm nsi-safnari.tgz

EXPOSE 9000/tcp
CMD /nsi-safnari/bin/nsi-safnari -Dconfig.file=$CONFIG -Dplay.evolutions.db.default.autoApply=true $TRUSTSTORE $EXTRA
