sudo hciconfig hci0 up
sudo hciconfig hci0 piscan
sudo erl -name pi@192.168.2.4 -pa ebin deps/bluetooth/ebin -cookie "erlang-rocks"
