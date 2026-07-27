# robot - imagen docker

## Build (en la maquina de desarrollo, dentro de este repo)
```
docker/build_image.sh
```
Compila el uberjar, el cljs de robot (v1) y de robot2 (v2), y construye
(y sube con `--push`) la imagen multi-arquitectura `quantumlabs/robot:$VERSION`,
donde `$VERSION` sale de `project.clj`. Tambien deja una copia de
`project.clj` en este mismo directorio (`docker/execution-template/`) para
que `start.sh` pueda leer esa misma version sin necesitar Leiningen en el
servidor.

## Deploy (en el servidor destino)
Copiar todo este directorio (`execution-template/`, con `start.sh`,
`project.clj` y `data/`) al servidor, por ejemplo a `/opt/quantum/robot/`.
`data/` es lo que el contenedor monta en `/opt/quantumlabs/robot/data`
(configuracion en `data/config/`; `data/app-log/` y `data/konservedb/` los
crea el propio contenedor en su primera corrida).

## Usage
```
./start.sh
```
Lanza `docker run` usando la version leida de `project.clj` (mismo
directorio) y los volumenes/puertos de produccion.
