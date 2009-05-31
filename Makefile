LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION=0.4.3

all:
		mkdir -p ebin/
		(cd src;$(MAKE))

test: all
		prove -v t/*.t

clean:
		(cd src;$(MAKE) clean)
		rm -rf erl_crash.dump *.beam *.hrl erlang_twitter-$(VERSION).tgz

package: clean
		@mkdir erlang_twitter-$(VERSION)/ && cp -rf include src support t Makefile README.markdown erlang_twitter-$(VERSION)
		@COPYFILE_DISABLE=true tar zcf erlang_twitter-$(VERSION).tgz erlang_twitter-$(VERSION)
		@rm -rf erlang_twitter-$(VERSION)/

install:
		for d in ebin include; do mkdir -p $(prefix)/$(LIBDIR)/erlang_twitter-$(VERSION)/$$d ; done
		for i in include/*.hrl ebin/*.beam ebin/*.app; do install $$i $(prefix)/$(LIBDIR)/erlang_twitter-$(VERSION)/$$i ; done
