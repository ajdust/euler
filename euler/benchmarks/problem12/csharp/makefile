
build: main.cs
	dotnet publish -c Release -r linux-x64
	echo "#!/bin/bash" > main
	echo "./bin/Release/netcoreapp2.1/linux-x64/publish/factorbench" >> main
	chmod +x main

clean:
	rm -rf bin
	rm -rf obj
	rm main
