head -3 /proc/meminfo | tail -1 | awk '{print $2}'
