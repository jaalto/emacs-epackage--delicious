
cvs log > CHANGELOG
cd /home/johnsu01/documents/cvs/code/lisp
tar cvzf delicious-el.tar.gz ./delicious/delicious.el ./delicious/delicioapi.el ./delicious/gpl.txt ./delicious/CHANGELOG
scp delicious-el.tar.gz freeshell.org:~/html/downloads/
