#!/bin/bash

app_env="development"

start_command="clackup --server :woo --port 8080 app.lisp"

APP_ENV=$app_env $start_command &

while RES=$(inotifywait -r -e modify *.lisp --format "%w%f" .);
do 
	echo RES is $RES at `date`; 
	kill `ps aux | grep 'clackup' | grep -v 'grep' | awk '{print $2}'` && APP_ENV=$app_env $start_command &
done;




