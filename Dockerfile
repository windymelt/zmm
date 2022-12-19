FROM sbtscala/scala-sbt:eclipse-temurin-17.0.4_1.8.0_2.13.10

RUN mkdir /app
WORKDIR /app
COPY ./build.sbt /app/
COPY ./project/* /app/project/
# TODO: highlight.jsどうする？
COPY ./src/* /app/src/
COPY ./version.sbt /app/

RUN sbt assembly

FROM 
