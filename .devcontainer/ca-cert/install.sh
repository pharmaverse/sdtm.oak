#!/bin/sh

set -e

# Copy CA certificate to `/etc/ssl/cert`
if ls *.crt 1> /dev/null 2>&1; then
    sudo cp *.crt /usr/local/share/ca-certificates
    sudo update-ca-certificates
fi

echo "Done!"