

all:
	mkdir target
	scalac -version
	scalac -verbose -d target PPTDiff.scala

clean: 
	rm -r target
