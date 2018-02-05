REBOL []

do %micro-test-ctx.r

cd %ops/
foreach file read %./*.tst [
	print ['Testing mold file]
	test file
]
cd %..


