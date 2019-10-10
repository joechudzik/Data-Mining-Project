#!/bin/bash

#inspired by https://github.com/vs49688/dnc-downloader

# curl -OJL https://wikileaks.org/dnc-emails//get/<id-here>
# IDs range from [1, 22456]

rm -rf email
mkdir email

rm -rf emailSTDErr
mkdir emailSTDErr

i=0
r=$RETRY_COUNT

while ((i <= 22456)); do
	let i=i+1
	echo "Downloading $i..." | tee -a $FD_PATH
	curl "https://wikileaks.org/dnc-emails//get/$i" -o "email/$i.eml" 2> "emailSTDErr/$i.stderr"
	CURLRET=$?

	if [ $CURLRET -ne 0 ]; then
		rm -rf *
		echo " * Failed to download: cURL returned $CURLRET. See email/$i.stderr for more information." | tee -a $FD_PATH
		continue
	fi

done