if [ ! -f "key" ]; then
	echo "RSA key does not exist, generating..."
	ssh-keygen -t rsa -f key -N ""
	RES=$?
	if [ $RES != 0 ]; then
		echo "Key generation failed with error $RES!"
		exit $RES
	fi
fi
	
if [ ! -f "precursors.csr" ]; then
	echo "Certificate Signing Request does not exist, generating..."
	openssl req -new -key key -out precursors.csr
	RES=$?
	if [ $RES != 0 ]; then
		echo "CSR generation failed with error $RES"
		exit $RES
	fi
fi

if [ ! -f "precursors.crt" ]; then
	echo "Certificate does not exists, generating self-signed for a year..."
	openssl x509 -req -days 365 -in precursors.csr -signkey key -out precursors.crt
	RES=$?
	if [ $RES != 0 ]; then
		echo "Certificate generation failed with error $RES"
		exit $RES
	fi
fi
