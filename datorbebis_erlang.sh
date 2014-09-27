sudo hciconfig hci0 up
sudo hciconfig hci0 piscan
sudo erl -name pi@192.168.2.2 -pa ebin deps/bluetooth/ebin deps/erlang-rpi-hw-drivers/ebin -cookie "erlang-rocks"
