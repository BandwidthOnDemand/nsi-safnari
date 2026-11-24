# syntax=docker/dockerfile:1@sha256:572cebbc2b15e6f84c077742952674889cb63cd5e9b80abdffcbe587b6f19183
#
# The staged app is pure JVM bytecode, identical on every platform, so build it once natively
# instead of under QEMU for each target platform.
FROM --platform=$BUILDPLATFORM node:24.22.3-slim@sha256:873f586f3794f52281bd039bf02c048a0be08fde619338f9941bad1eb6b34530 AS node

FROM --platform=$BUILDPLATFORM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.2_13_1.10.2_3.5.1@sha256:32c8dfdcbd9220d9d392a53e20699995a386deb678773490d195f108bd6f5356 AS build
# sbt-less compiles the stylesheets with node; jammy's apt package is Node 12
COPY --from=node /usr/local/bin/node /usr/local/bin/node
WORKDIR /usr/local/src/nsi-safnari
COPY . .
RUN --mount=type=secret,id=github_token,env=GITHUB_TOKEN,required=true <<EOF
set -e
sbt stage
mkdir -p target/universal/stage/logs
EOF


FROM gcr.io/distroless/java21-debian12@sha256:f34fd3e4e2d7a246d764d0614f5e6ffb3a735930723fac4cfc25a72798950262

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
