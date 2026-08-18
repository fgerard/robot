# robot

Motor de automatización: máquinas de estado configurables (diseñadas visualmente)
que ejecutan operaciones (HTTP, Selenium, Telegram, SQL, colas MQ, etc.) y se
administran vía una UI web. Backend en Clojure; dos UIs en ClojureScript:

- **`src/cljs/robot`** — v1 (la UI original).
- **`src/cljs/robot2`** — v2 (reescritura sobre re-frame/re-com moderno, en
  desarrollo activo; es la que se usa día a día).

Ver [`RELEASE_NOTES.md`](RELEASE_NOTES.md) para el historial de cambios, y
[`docker/execution-template/README.md`](docker/execution-template/README.md) para
build/deploy de la imagen Docker de producción.

## Requisitos

- **Java 21** y **Leiningen 2.12+** (el backend y la compilación Clojure).
- **Node.js** (compilación de ClojureScript vía shadow-cljs) — probado con Node 25.

## Setup inicial

```bash
npm install
```

Instala `shadow-cljs`, `react`, `react-dom` y `codemirror` (declarados en
`package.json`). Las dependencias de Clojure/ClojureScript (reagent, re-frame,
re-com, etc.) las trae Leiningen normalmente, no hace falta nada aparte.

## Correr la app en desarrollo (con hot-reload)

Se necesitan dos procesos corriendo en paralelo, cada uno en su propia terminal.

**1. Backend** (API + archivos estáticos, sirve en `http://localhost:8050`,
config en `config/robot-01-config.edn`):

```bash
lein run
```

**2. Compilador de ClojureScript con hot-reload**, para la UI que estés tocando:

```bash
npx shadow-cljs watch robot2   # UI v2 (la activa)
npx shadow-cljs watch robot    # UI v1
```

(se puede correr `watch robot robot2` para ambas a la vez). La primera vez tarda
más porque instala dependencias adicionales. Cuando termina de compilar queda
esperando conexión del navegador y luego recompilando solo cada vez que guardas
un `.cljs` — sin recargar la página a mano.

**3. (Opcional) Auto-watch de LESS**, si vas a tocar `.less` además de `.cljs`:

```bash
lein less auto
```

Recompila `src/less/index.less` → `resources/public/css/index.css` en cada
guardado; el navegador lo recoge sin recargar la página.

Con backend + shadow-cljs corriendo, abre:

- `http://localhost:8050/robot2.html` — UI v2
- `http://localhost:8050/index.html` — UI v1

El login pide una cuenta de Google autorizada (ver `robot2/events/api.cljs` /
`externs.js` para el flujo de Google Identity Services) o un usuario/password ya
guardado en el `konservedb`/leveldb local.

### Si algo no compila después de cambiar dependencias

`lein clean` — hay compilación AOT (`robot.main.starter`, etc.) que puede quedar
desincronizada con `project.clj` si cambiaste versiones de librerías; sin limpiar
`target/` a veces da errores de classpath confusos (`NoClassDefFoundError` con
clases que sí existen en el jar correcto).

### Depurar en el REPL de shadow-cljs

`npx shadow-cljs watch <build>` deja un prompt de REPL de ClojureScript conectado
al navegador (una vez que la página cargó). Ahí funcionan cosas como
`(build-once "robot2")` (fuerza una recompilación sin depender del watcher) o
cualquier expresión cljs evaluada en vivo contra la app corriendo.

## Compilar para producción

```bash
lein less once                     # -> resources/public/css/index.css
npm install                        # primera vez / si cambio package.json
npx shadow-cljs release robot robot2   # -> resources/public/js/compiled/{robot,robot2}-ui.js
lein with-profile prod uberjar     # backend -> target/robot-X.Y.Z-standalone.jar
```

`docker/build_image.sh` hace exactamente esto (más el build/push de la imagen
Docker multi-arquitectura) — requiere Node.js/npm además de JDK/Leiningen en la
máquina donde se corra. Ver
[`docker/execution-template/README.md`](docker/execution-template/README.md)
para el resto del build/deploy.
