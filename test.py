import sys
import os
import subprocess
import re

ptr = re.compile("&([0-9]+)")

def passed(test):
	print(test + " PASSED")

def failed(test):
	print(test + " FAILED")

def is_error(result):
	result = result.split()
	if len(result) == 0:
		return True
	return result.split()[0] != "-----END-----"

def unroll_ptr(heap, v):
	return re.sub(ptr, lambda m: unroll_ptr(heap, heap[m.group(1)]), v)

def parse_result(result):
	result = result.decode("utf-8")
	result = result.split("\n")
	heap = {}
	for var in result[result.index("HEAP:")+1:]:
		if "=" not in var:
			continue
		[id, v] = var.split(" = ")
		heap[id] = v
	parsed_result = {}
	for var in result[2:result.index("HEAP:")]:
		[id, v] = var.split(" = ")
		parsed_result[id] = unroll_ptr(heap, v)
	print(parsed_result)
	return parsed_result

def test_error(sut, test):
	result = subprocess.run([sut, test], stdout=subprocess.PIPE)
	if is_error(result.stdout):
		passed(test)
	else:
		failed(test)

def test_correctnes(sut, test, expected):
	result = subprocess.run([sut, test], stdout=subprocess.PIPE)
	actual_result = parse_result(result.stdout)
	expected_result = eval(open(expected).read())
	if actual_result == expected_result:
		passed(test)
	else:
		failed(test)

def main():
	if (len(sys.argv) != 3):
		print("Provide name of binary as a first argument and tests dir as a second.")
	sut = sys.argv[1]
	tests_dir = sys.argv[2]
	for test_name in os.listdir(tests_dir):
		if test_name[-4:] == (".wpp"):
			test = tests_dir + "/" + test_name
			if "err" in test_name:
				test_error(sut, test)
			else:
				test_correctnes(sut, test, test[:-4] + ".py")




if __name__ == '__main__':
	main()