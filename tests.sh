echo "RUNNING NOCHECK TESTS"
	for f in tests/passing_nocheck/*; do echo $f; done;
		echo "RUNNING: $f"
		./bin/boa $f -nocheck
	echo "RUNNING PASSING TESTS"	
	for f in tests/passing/*; do echo $f; done;
		echo "RUNNING: $f"
		./bin/boa $f 
	for f in tests/failing/*; do echo $f; done;
		echo "RUNNING $f"
		./bin/boa $f 