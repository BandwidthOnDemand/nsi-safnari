# syntax=docker/dockerfile:1
#
# first stage uses mozilla sbt image to stage safnari
#
FROM mozilla/sbt:8u292_1.5.4 AS SBT_BUILD

RUN apt-get update && apt-get --yes install curl

ENV BUILD_HOME /home/nsi-safnari
COPY . $BUILD_HOME
WORKDIR $BUILD_HOME
RUN sbt stage
 
#
# second stage uses open jdk8 on alpine3.9 with the staged safnari
#
FROM openjdk:8-jre-alpine3.9

RUN apk update && apk add bash

ENV BUILD_HOME /home/nsi-safnari
WORKDIR /nsi-safnari
COPY --from=SBT_BUILD $BUILD_HOME/target/universal/stage/. .

ENV USER=safnari
ENV GROUP=safnari
ENV HOME=/var/local/safnari
ENV PORT=9000
ENV ADDRESS="0.0.0.0"
ENV CONFIG=/config/config-overrides.conf
ENV EXTRA="-J-Xms512m -J-Xmx512m -J-server -J-verbose:gc -J-XX:+PrintGCDetails -J-XX:+PrintGCDateStamps -J-Xloggc:./nsi-safnari/logs/gc.log -J-XX:+UseGCLogFileRotation -J-XX:NumberOfGCLogFiles=10 -J-XX:GCLogFileSize=10M -J-XX:+UseParallelGC -J-XX:+UseParallelOldGC"

EXPOSE 9000/tcp
CMD /nsi-safnari/bin/nsi-safnari -Dconfig.file=$CONFIG -Dhttp.port=$PORT -Dhttp.address=$ADDRESS -DapplyEvolutions.default=true $EXTRA
#CMD /nsi-safnari/bin/nsi-safnari -Dhttp.port=$PORT -Dhttp.address=$ADDRESS -DapplyEvolutions.default=true $EXTRA
