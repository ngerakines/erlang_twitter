all: code

code: clean
	erl -make

clean:
	rm -rfv *.beam *.rel *.script *.boot erl_crash.dump erlang_twitter/ *.deb

install-osx: code
	mkdir -p erlang_twitter-0.3/ebin/ && cp *.beam erlang_twitter-0.3/ebin/
	mkdir -p erlang_twitter-0.3/include/ && cp include/* erlang_twitter-0.3/include/
	cp -R erlang_twitter-0.3 /usr/local/lib/erlang/lib/erlang_twitter-0.3