sudo hciconfig hci0 up
sudo hciconfig hci0 piscan
sudo erl -name pi@192.168.2.3 -pa ebin deps/bluetooth/ebin deps/jsx/ebin -cookie "erlang-rocks"
