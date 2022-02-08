# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

## [2.2.1] - 2022-02-08
### Added
  - default allow all hosts, see play.filters.hosts in application.conf
### Changed
- Working default configuration:
  - log configuration now in conf/logback.conf
  - configure application secret with play.http.secret.key in application.conf
  - configure key- and truststore with play.ws.ssl in application.conf
### Fixed
  - HTTP client does not do SNI [#21](https://github.com/BandwidthOnDemand/nsi-safnari/issues/21)

## [2.2.0] - 2022-02-04
### Added
- Change log.
- Release all artifacts to GitHub.
### Changed
- Upgrade Play framework:
  - Play 2.7.9
  - Scala 2.13.7
  - Sbt 1.5.5
- Using default Akka web server instead of Netty
