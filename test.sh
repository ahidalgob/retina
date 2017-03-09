echo "Archivo de prueba"
ls ins3 | egrep $1.*\.rtn
echo ""
./retina ins3/$1*