#!/bin/sh
cd /home/dsl/gemini/chrust_plus/gemini_upgrade

#git add .
#git commit -am "automatic update"
#git push -f

#git pull
git add *
timestamp(){
   date +"%d.%m.%Y um %H:%M"
}
git commit -am "Auto Server Commit $(timestamp)"
git push  #--repo https://[user]:[password]@github.com/[organisation]/[repo].git
