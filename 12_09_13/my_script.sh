#!/bin/bash
declare -a ARRAY
let count=0
echo -e "Пожалуйста, укажите директорию для работы. Мы будем заменять КИТ на ШРИ в заголовках html файлов.:"
read  directory
if [ -d $directory ]; then
	echo "Обрабатываем..."
    for file in $directory/*.html
	do
		echo "$file"
 		ARRAY[$count]="$file"
    	((count++))
    	perl -p -i.bak -e 's/(<title>)(.*?)(КИТ)(.*?)(<\/title>)/$1$2ШРИ$4$5/g' "$file"
	done
	echo "Всего обработано файлов: ${#ARRAY[@]}"
	echo "Если вы хотите отменить выполненную операцию, нажмите 1. Если вы хотите завершить работу, нажмите 2"
	read choice
	if [ $choice -eq 1 ] ; then
		for i in "${ARRAY[@]}"
		do
	   		rm "$i"
	   		mv "$i.bak" "$i"
		done 
	else
		echo "Удаляем временные файлы.."
		for i in "${ARRAY[@]}"
		do
	   		rm "$i.bak"
		done 
	fi
else
    echo "Такой директории нет"
fi