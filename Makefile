all: code

code: clean
	erl -make

clean:
	rm -rfv *.beam *.rel *.script *.boot erl_crash.dump erlang_twitter/ *.deb

package-debian: code
	mkdir -p erlang_twitter/usr/lib/erlang/lib/erlang_twitter-0.3/ebin/ && cp erlang_twitter.beam erlang_twitter/usr/lib/erlang/lib/erlang_twitter-0.3/ebin/erlang_twitter.beam
	mkdir -p erlang_twitter/DEBIAN/ && cp control erlang_twitter/DEBIAN/control
	dpkg -b erlang_twitter erlang_twitter.deb

install-debian: package-debian
	dpkg -i erlang_twitter.deb
