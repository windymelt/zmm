FROM sbtscala/scala-sbt:eclipse-temurin-17.0.4_1.8.0_2.13.10 AS build

RUN mkdir /app
WORKDIR /app
COPY ./build.sbt /app/
COPY ./project /app/project
# TODO: highlight.jsどうする？
COPY ./src /app/src
COPY ./version.sbt /app/

RUN sbt compile assembly
RUN ls -lah /app/target/scala-2.13/
RUN cp /app/target/scala-2.13/zmm-assembly-*.jar /app/target/zmm.jar
RUN java -jar /app/target/zmm.jar -v

FROM amazoncorretto:17

COPY --from=build /app/target/zmm.jar /zmm.jar
COPY --from=build /app/target/scala-2.13/*.jar /
ENTRYPOINT ["java", "-jar", "/zmm.jar"]
