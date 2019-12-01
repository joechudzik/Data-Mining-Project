#!/bin/bash

#inspired by https://github.com/vs49688/dnc-downloader

# curl -OJL https://wikileaks.org/dnc-emails//get/<id-here>
# IDs range from [1, 22456]

i=0
r=$RETRY_COUNT

while ((i <= 22456)); do
	let i=i+1
	echo "Downloading $i..." | tee -a $FD_PATH
	curl --connect-timeout 5 --retry 8 --retry-delay 0 --retry-max-time 120 "https://wikileaks.org/dnc-emails//get/$i"  -o "email/$i.eml" 2> "emailSTDErr/$i.stderr" --fail
	CURLRET=$?

	if [ $CURLRET -ne 0 ]; then
		echo " * Failed to download: cURL returned $CURLRET. See email/$i.stderr for more information." | tee -a $FD_PATH
		read -n1 -r -p "Press any key to continue..." key
		continue
	fi

done