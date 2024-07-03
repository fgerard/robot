#!/bin/bash

# Obtén el directorio donde se encuentra el script
script_dir=$(dirname "$(realpath "$0")")

# Define la ruta del archivo de log basado en la ubicación del script
logfile="$script_dir/../logs/robot.log"
script_logfile="$script_dir/../check_log_and_restart.log"

# Verifica si el archivo de log existe
if [ ! -f "$logfile" ]; then
    echo "$(date): El archivo de log no existe: $logfile" >> "$script_logfile"
    exit 1
fi

# Extrae la última línea que comienza con una fecha y hora
last_line=$(grep -E '^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}' "$logfile" | tail -n 1)

# Verifica si se encontró una línea válida
if [ -z "$last_line" ]; then
    echo "$(date): No se encontró una línea válida en el archivo de log." >> "$script_logfile"
    exit 1
fi

# Obtén la hora del log de la segunda columna
log_time=$(echo "$last_line" | awk '{print $2}')

# Convierte la hora del log a segundos desde el epoch
log_epoch=$(date -d "$log_time" +%s)

# Obtén la hora actual del servidor en segundos desde el epoch
current_epoch=$(date +%s)

# Calcula la diferencia en segundos
time_diff=$((current_epoch - log_epoch))

# Verifica si la diferencia es mayor a 300 segundos (5 minutos)
if [ $time_diff -gt 60 ]; then
    echo "$(date): La diferencia es mayor a 1 minutos. Reiniciando contenedor Docker..." >> "$script_logfile"
    docker restart robot >> "$script_logfile" 2>&1
else
    echo $script_logfile
    echo "$(date): La diferencia es menor o igual a 1 minutos." >> "$script_logfile"
fi