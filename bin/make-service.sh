export ROBOT_BIN="$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export ROBOT_HOME=$(dirname $ROBOT_BIN)
echo $ROBOT_HOME
SERVICE=/etc/systemd/system/robot.service

echo "[Unit]" > $SERVICE
echo "Description = robot automaton" >> $SERVICE
echo "After=network.target" >> $SERVICE
echo ""  >> $SERVICE

echo "[Service]" >> $SERVICE
echo "Type = simple" >> $SERVICE
echo "PIDFile = "$ROBOT_HOME"/robot.pid" >> $SERVICE
echo "ExecStart = "$ROBOT_BIN"/robot start" >> $SERVICE
echo "ExecStop = "$ROBOT_BIN"/robot stop" >> $SERVICE
echo "Restart = on-failure" >> $SERVICE

echo ""  >> $SERVICE
echo "[Install]" >> $SERVICE
echo "WantedBy = multi-user.target" >> $SERVICE

echo "******" $SERVICE "******"
cat $SERVICE

systemctl daemon-reload # Run if .service file has changed
systemctl restart

echo "RUN:"
echo "\tchcon -t bin_t -R " $ROBOT_HOME
echo "\tsudo systemctl enable robot"
