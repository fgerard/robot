# Release Notes

Formato libre, en orden cronológico inverso. Las versiones se taggean en git como `vX.Y.Z`.

## 4.1.0 — 2026-08-31

### Telegram: recibir imágenes

- `telegram-get` ahora acepta fotos. La foto se baja al tmpdir y su ruta queda en una
  variable aparte, `<nodo>-image-path` (un nodo `:get-request` deja
  `:get-request-image-path`), y no pegada a los parámetros: como un parámetro más, una
  foto con caption `/get identifications 5` correría la ruta al lugar del user-id y
  rompería todo comando posicional al mandar imagen.
- Sin imagen esa variable queda en `""` y no en `nil`: `contextualize` solo sustituye
  llaves con valor, así que un `nil` dejaba el `:keyword` literal en el campo y
  `telegram-send` lo tomaba por una ruta — fallaba `.exists`, se iba por la rama de
  base64, tronaba y se comía el mensaje entero.
- El caption del mensaje es el comando; una foto sin caption se ignora.
- Los comandos ya no necesitan parámetros: `/get help` en vez de `/get help x`.

### Mensajes largos y monoespaciado

- `telegram-send` acepta `format`, con la opción `mono` para mandar en bloque
  monoespaciado. Ojo al escribirlos: Telegram reacomoda ese bloque a ~47 caracteres y
  manda el sobrante a la columna 0.
- Los mensajes largos se paginan en vez de cortarse a 1024 caracteres.

### Temporales en un solo lugar

- Todo lo temporal —las imágenes que llegan, las que se mandan en base64 y los
  archivos de los nodos `:clojure`— vive en `java.io.tmpdir` con prefijo `robot-`, con
  un solo barrido cada hora que borra lo que pase de 24 h. Antes eran tres lugares
  distintos y solo uno se limpiaba.
- El barrido borra por prefijo y nunca por edad sola: así no puede llevarse nada que
  no haya creado el robot.

### Núcleo

- El ciclo de estados corre con `send-off` y no con `send`. El pool de `send` es fijo
  (2+cores) y aquí toda operación bloquea, así que las instancias de más se formaban
  detrás de una dormida y los pasos tardaban de más sin razón aparente.
- Arreglado que una instancia se quedara corriendo e inmune a stop, individual o
  masivo. Se perdía la referencia a su agente por dos vías: `swap!` reintentando
  `robot-cmd`, que tiene efectos, e `instantiate` rehaciendo `:instances` sin
  `:inst-agent`. Un agente que no está en el mapa no se puede parar nunca, y encima
  sigue publicando `:running`, que es lo que se ve en la UI.

### Designer

- La v2 es la oficial; la v1 queda deprecada.
- Botones para prender y apagar todas las instancias de una app de un golpe.
- El navegador ya no sirve un esquema de operaciones cacheado.
- `max-length` y `max-messages` aparecen en el esquema que lee la v2.

### Build

- `build_image.sh` arma imagen local por omisión; `--push` además la sube al registro.

### Al actualizar

- `instance.env` de cada instalación: `VERSION=4.1.0`.
- Los nodos `:clojure` que creen temporales con `createTempFile` tienen que usar el
  prefijo `robot-` para que el barrido los alcance; si no, sus archivos se acumulan.

## 4.0.2 — 2026-08-18

### Tooling de desarrollo: migración de lein-figwheel a shadow-cljs

**Por qué:** `lein-figwheel 0.5.7` (2017) depende de `hawk`/`barbary-watchservice`, una
librería de watch de archivos nativa (vía JNA) de la era Intel, sin soporte para Apple
Silicon. En una Mac M1 el watcher nunca detectaba los archivos `.cljs` guardados —
cada cambio requería forzar una recompilación a mano y un hard-refresh del navegador
para verlo reflejado. `shadow-cljs` resuelve esto con un watcher que sí funciona en M1
(confirmado con pruebas en vivo: guardar un archivo dispara recompilación y hot-reload
en el navegador sin intervención manual).

- **`shadow-cljs.edn`** (nuevo) + **`package.json`** (nuevo): compila los builds
  `robot` (v1) y `robot2` (v2) con `:lein true` — sigue usando `project.clj` como
  única fuente de verdad para las dependencias Maven (reagent, re-frame, re-com, etc.),
  no hace falta duplicarlas en `deps.edn`.
- **`project.clj`**:
  - `org.clojure/clojurescript` de `1.10.520` (2019) a `1.11.132` — la vieja no
    compila con shadow-cljs 2.28.5 (le faltan namespaces del analyzer).
  - Agregado `thheller/shadow-cljs` (la contraparte JVM del CLI de npm, necesaria
    para `:lein true`).
  - Pineado `com.google.guava/guava` a una versión moderna y excluido el
    `closure-compiler-unshaded` viejo que traía clojurescript transitivamente —
    chocaban con el closure-compiler que usa shadow-cljs internamente.
  - Agregado `org.mozilla/rhino` explícito: `robot.core.operations` (el operador
    `:js` del backend) lo usa directo, pero antes solo llegaba al classpath de
    rebote, transitivo del closure-compiler viejo que se acaba de excluir.
- `react`/`react-dom`/`codemirror` ahora también se instalan vía npm (mismas
  versiones que los jars `cljsjs` que ya se usaban) — shadow-cljs no soporta esos
  paquetes `cljsjs` para librerías con interop JS moderno.
- Requires de CodeMirror (`operations/dialog.cljs`, `state_editor.cljs`) migrados de
  `cljsjs.codemirror.*` a imports npm reales (`["codemirror/mode/..."]`, etc.).
- Quitado un require muerto a `cljsjs.svgjs` en `svg.cljs` (v1) que ya no se usaba
  (su única referencia estaba comentada).

### Build de producción

`docker/build_image.sh` y el perfil `:prod` de `project.clj` ahora compilan el
cljs de ambas UIs con `npx shadow-cljs release robot robot2` en vez de
`lein cljsbuild once min` / `robot2-min robot2-min` (este último con un
workaround feo por un bug de parseo de argumentos de lein-cljsbuild). Verificado
con una comparación real del bundle de v1 (que sí está trackeado en git): el
nuevo build con shadow-cljs + closure-compiler moderno da **10% menos bytes raw
y 4.3% menos gzip** que el build anterior — el cambio de compilador no perdió
eficiencia, la mejoró. El servidor que corra `build_image.sh` ahora necesita
Node.js/npm instalados (mismo prerequisito que ya tenía para JDK/lein), y correr
`npm install` antes del primer build.

### Fixes de layout en robot2 (Designer)

- "Application Parameters" y "Application Instances" ahora viven en un `v-split`
  con manija arrastrable (antes un límite fijo de 50% de altura) — cada quien usa
  el espacio que necesita, con scroll si no alcanza, y el usuario controla la
  proporción con el mouse.
- Corregido un bug donde los parámetros de una **instancia** (no de la app) no se
  mostraban en pantalla aunque sí se guardaban correctamente en disco: un
  `:size "1"` (`flex-basis: 0%`) que solo tiene sentido para paneles con altura
  garantizada colapsaba a 0 la lista cuando el contenedor no tenía altura definida.
- El botón de "Sign in with Google" ahora reintenta durante ~5s si el script de
  Google (carga `async`) todavía no está listo, en vez de rendirse al primer
  intento — timing distinto del bundle de shadow-cljs vs. el de lein-cljsbuild
  exponía una condición de carrera preexistente.
- Corregido el alto fijo (`85%`) del área del Designer, que asumía que la barra de
  tabs (Console/Designer/Configuration) siempre mide el mismo % de la ventana —
  causaba que el diagrama se recortara por abajo o dejara un hueco antes del log
  de eventos, dependiendo del tamaño de ventana.
- El canvas del diagrama (SVG) ahora recibe una altura real de su contenedor
  (antes no la tenía explícita, y no seguía el resize del splitter horizontal
  entre el diagrama y el panel de contexto/log).

## 4.0.1 — 2026-08-17

### Docker / deploy

- `robot-01-config.edn`: agrega `:http-host`/`:https-host "0.0.0.0"` — sin esto el
  servidor solo escuchaba en loopback dentro del contenedor, y el tráfico
  reenviado por Docker desde afuera nunca lo alcanzaba.
- `start.sh` se autoubica (usa su propio directorio como volumen de datos) y lee
  sus parámetros (`VERSION`, `IMAGE_NAME`, `CONTAINER_NAME`, memoria, puertos) de
  `config/instance.env` en vez de tenerlos hardcodeados o parsear `project.clj` en
  el servidor; `build_image.sh` sincroniza `VERSION`/`IMAGE_NAME` en ese archivo en
  cada build.
- Nuevo `BIND_EXTRA_IPS` en `instance.env`: por default los puertos solo se
  publican en loopback; se pueden agregar IPs adicionales del host (o `0.0.0.0`
  para exponer en todas las interfaces).
- Agrega `--add-host=host.docker.internal:host-gateway` al contenedor.
- `bin/build-ui.sh` apunta a los builds de producción (`min`, `robot2-min`) en vez
  de compilar todos los builds — evitaba que fallara por el build de tests
  (`robot2-test`, que necesita `lein doo`, no `cljsbuild` genérico).

### robot2 (Designer)

- Al guardar una app, se limpia el estado en vivo (`:applications :ready`) de sus
  instancias, para que el editor refleje el reset real que hace el backend al
  reinstanciar, sin borrar el diagrama (a diferencia de la v1, que sí lo borraba).
- Corrige texto y triangulito cortados en los combos de selección de app y de
  operación (`app-selector`, `opr-selector`) — altura fija con márgenes negativos
  como parche, reemplazado por flexbox real.
- El panel de contexto en vivo de una instancia aprovecha toda la altura
  disponible en vez de un límite fijo de 7em.

## 4.0.0 y anteriores

Ver historial de `git log` — reescritura de la UI (`robot2`, en `src/cljs/robot2`)
sobre re-frame/re-com moderno buscando paridad de funcionalidad con la v1
(`src/cljs/robot`), empaquetado Docker multi-arquitectura, y varias décadas
combinadas de mejoras incrementales al motor de automatización desde la versión 1.
