aio:
	ant abcl-aio.jar

incremental:
	ant abcl-aio.jar -Dabcl.build.incremental=true

clean:
	ant clean

test:
	ant test

all:	aio
