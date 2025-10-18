# Network connection

## WiFi

To have a WiFi connection in the live environment:

```sh
rfkill ublock wlan
ip link set <interface> up
wpa_passphrase "<SSID>" "<passphrase>" | tee /etc/wpa_supplicant.conf
wpa_supplicant -B -c /etc/wpa_supplicant.conf -i <interface>
dhcpd <interface>
```
