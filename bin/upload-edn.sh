curl -v -XPOST --data-binary @$1 -H "content-type: application/edn" -H "Accept: application/edn" http://localhost:8051/store/$2
curl -v -XPOST  -H "content-type: application/edn" -H "Accept: application/edn" http://localhost:8051/instantiate/$2
