# docker-robot

## Download
```
git clone https://github.com/quantumlabsai/docker-robot.git
```

## Build
```
cd docker-robot/
rm -rf robot
bin/build_image.sh
```

## Usage
```
sudo docker run --rm -ti -p 4050:4050 -p 8050:8050 quantumlabs/robot:3.0.2
```
