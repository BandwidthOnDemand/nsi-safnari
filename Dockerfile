# syntax=docker/dockerfile:1
#
# The staged app is pure JVM bytecode, identical on every platform, so build it once natively
# instead of under QEMU for each target platform.
FROM --platform=$BUILDPLATFORM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.2_13_1.10.2_3.5.1 AS build
RUN apt-get update && apt-get install -y nodejs
WORKDIR /usr/local/src/nsi-safnari
COPY . .
RUN --mount=type=secret,id=github_token,env=GITHUB_TOKEN,required=true <<EOF
set -e
sbt stage
mkdir -p target/universal/stage/logs
EOF


FROM gcr.io/distroless/java21-debian12

ENV PORT=9000
ENV ADDRESS="0.0.0.0"

USER nonroot:nonroot
WORKDIR /nsi-safnari
COPY --from=build --chown=nonroot:nonroot /usr/local/src/nsi-safnari/target/universal/stage /nsi-safnari

EXPOSE 9000/tcp
ENTRYPOINT ["java", \
  "-Duser.dir=/nsi-safnari", \
  "-XX:MaxRAMPercentage=75.0", \
  "-Xlog:gc:file=/nsi-safnari/logs/gc.log:time,level,tags:filecount=5,filesize=10M", \
  "-XshowSettings:vm", \
  "-Djavax.net.ssl.trustStoreType=jks", \
  "-Djavax.net.ssl.trustStorePassword=secret", \
  "-Djavax.net.ssl.trustStore=/nsi-safnari/conf/nsi-safnari-truststore.jks", \
  "-Dconfig.file=/config/config-overrides.conf", \
  "-Dplay.evolutions.db.default.autoApply=true", \
  "-cp", "/nsi-safnari/conf/:/nsi-safnari/lib/*", \
  "play.core.server.ProdServerStart"]
