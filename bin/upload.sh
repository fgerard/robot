# Primero sube el archivo y luego lo instancia para que esté listo para correr
curl -XPOST -H "content-type: application/edn" -H "Accept: application/edn" --data-binary @$1 http://localhost:8051/store/$2
curl -XPOST -H "content-type: application/edn" -H "Accept: application/edn"  http://localhost:8051/instantiate/$2
