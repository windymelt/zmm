# ずんだムービーメーカー(仮)

まだアルファクオリティです

## Sample

```sh
$ docker compose up voicevox-cpu
```

```sh
$ sbt 'run sample.xml'
```

## Prerequisites

- sbt
  - for building Scala code
- docker compose
  - for VOICEVOX
- ffmpeg
  - for concatenating wav file
- chromium
  - for generating screenshot from HTML file

## Credits

- Thanks to VOICEVOX.
