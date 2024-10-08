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
RUN --mount=type=secret,id=github_token <<EOF
set -e
export GITHUB_TOKEN="$(cat /run/secrets/github_token)"
sbt packageZipTarball
EOF


FROM eclipse-temurin:21

ENV PORT=9000
ENV ADDRESS="0.0.0.0"
ENV CONFIG=/config/config-overrides.conf
ENV EXTRA="-XX:MaxRAMPercentage=75 -J-Xlog:gc:file=./logs/gc.log:time,level,tags:filecount=5,filesize=10M -J-XshowSettings:vm"
# default trust store in configured here, the key store is configured in the config file
ENV TRUSTSTORE="-Djavax.net.ssl.trustStoreType=jks -Djavax.net.ssl.trustStorePassword=secret -Djavax.net.ssl.trustStore=target/universal/stage/conf/nsi-safnari-truststore.jks"

RUN useradd --uid 12345 --comment 'Safnari user' safnari
USER safnari:safnari
WORKDIR /nsi-safnari
RUN mkdir -p logs
COPY --from=build /usr/local/src/nsi-safnari/target/universal/*.tgz nsi-safnari.tgz
RUN tar xvzf nsi-safnari.tgz --strip-components=1 && rm nsi-safnari.tgz

EXPOSE 9000/tcp
CMD /nsi-safnari/bin/nsi-safnari -Dconfig.file=$CONFIG -Dplay.evolutions.db.default.autoApply=true $TRUSTSTORE $EXTRA
