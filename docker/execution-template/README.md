# robot - imagen docker

## Build (en la maquina de desarrollo, dentro de este repo)
```
docker/build_image.sh
```
Compila el uberjar, el cljs de robot (v1) y de robot2 (v2), y construye
(y sube con `--push`) la imagen multi-arquitectura `quantumlabs/robot:$VERSION`,
donde `$VERSION` sale de `project.clj` (unica fuente de verdad para la
version). Tambien actualiza `VERSION` e `IMAGE_NAME` en
`data/config/instance.env`, para que `start.sh` no necesite Leiningen ni
`project.clj` en el servidor.

## Deploy (en el servidor destino)
Copiar `data/` (con `start.sh`, `data/config/instance.env` y el resto de
`data/config/`) a donde sea en el servidor -- `start.sh` se autoubica y
monta ese mismo directorio como `/opt/quantumlabs/robot/data` en el
contenedor (`data/app-log/` y `data/konservedb/` los crea el propio
contenedor en su primera corrida). `data/config/instance.env` trae
`CONTAINER_NAME`, `TIMEZONE`, `MININUM_MEMORY`, `MAXIMUM_MEMORY`,
`HTTPS_PORT` y `HTTP_PORT` editables a mano por instancia.

## Usage
```
data/start.sh
```
Lanza `docker run` usando los parametros de `data/config/instance.env`
(mismo directorio) y los volumenes/puertos de produccion.
