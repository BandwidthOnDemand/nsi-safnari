#FROM openjdk:8-jdk-slim AS SBT_BUILD
#
#RUN apt-get update && apt-get --yes install curl
#
##ARG MAVEN_VERSION=3.6.3
##ARG SHA=c35a1803a6e70a126e80b2b3ae33eed961f83ed74d18fcd16909b2d44d7dada3203f1ffe726c17ef8dcca2dcaa9fca676987befeadc9b9f759967a8cb77181c0
##ARG BASE_URL=https://apache.osuosl.org/maven/maven-3/${MAVEN_VERSION}/binaries
##ARG SBT_VERSION=1.5.0
#ARG SBT_VERSION=0.13.18
##ARG SBT_SHA=aadf110a48ea77c02ce63a9cc033d9fcd02b634e89e8f22dfe13ed21592042b6
#ARG SBT_SHA=afe82322ca8e63e6f1e10fc1eb515eb7dc6c3e5a7f543048814072a03d83b331
#ARG SBT_URL=https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz
#
#RUN curl -fsSL -o /tmp/sbt.tar.gz ${SBT_URL} \
#  && echo "${SBT_SHA}  /tmp/sbt.tar.gz" | sha256sum -c - \
#  && tar -xzf /tmp/sbt.tar.gz -C /opt \
#  && rm -f /tmp/sbt.tar.gz
#
#ENV BUILD_HOME /home/nsi-safnari
#COPY . $BUILD_HOME
#WORKDIR $BUILD_HOME
#RUN /opt/sbt/bin/sbt stage
# 
## the second stage of our build will use open jdk 8 on alpine 3.9
FROM openjdk:8-jre-alpine3.9

#ENV DDS_BUILD_HOME /home/safnari/nsi-dds

RUN apk update && apk add bash

WORKDIR /nsi-safnari
 
## copy only the artifacts we need from the first stage and discard the rest
#COPY --from=SBT_BUILD $DDS_BUILD_HOME/target/dds.jar .
#COPY --from=SBT_BUILD $DDS_BUILD_HOME/config ./config

COPY target/universal/stage/. .

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
