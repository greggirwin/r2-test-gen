REBOL []

comment {
	x: [[a b c]	[1 2 3]]
	y: [[yes no] [-4.0 -5.0]]
	z: [[! & ?]  ["" {} []]]
	
	x: [a b c 1 2 3]
	y: [yes no -4.0 -5.0]
	z: [! & ? "" #{} <>]
	xyz: reduce [x y z]

	recombine: func [start blocks] [
		collect blk [
			either tail? blocks [blk: reduce [start]] [
				foreach val first blocks [
					blk: recombine append copy start val next blocks
				]
			]
		]
	]
}


func-spec-ctx: context [
	func-spec: context [
		desc: none
		attr: none
		params: copy []
		refinements: copy []
		locals: copy []
	]

	set 'parse-func-spec func [
		"Parses a function spec and returns an object model of it."
		spec [block! any-function!]
		/local 
			emit-name emit-attr emit-target	res
			func-desc= attr-val= func-attr= 
			param-name= param-type=	param-desc= param-attr= param= 
			ref-name= ref-desc= ref-param= refinement= 
			locals= spec=
			=val 
	][
		emit-name: func [val] [
			append/only emit-target reduce [:val]
			new-line back tail emit-target on
		]
		emit-attr: func [val] [append/only last emit-target val]
		 
		func-desc=:  [set =val string! (res/desc: =val)]
		attr-val=:   ['catch | 'throw]
		func-attr=:  [into [copy =val some attr-val= (res/attr: =val)]]
		param-name=: [set =val [word! | get-word! | lit-word!] (emit-name :=val)]
		;!! param types are word! vals under R3 alpha currently.
		;	add typeset support?
		param-type=: [into [copy =val some [word! | datatype!]] (emit-attr =val)] 
		param-desc=: [set =val string! (emit-attr =val)]
		param-attr=: [opt param-type= opt param-desc=]
		param=:      [param-name= param-attr=]
		ref-name=:   [set =val refinement! (emit-name =val)]
		ref-desc=:   [set =val string! (emit-attr =val)] 
		ref-param=:  [param-name= param-attr=]
		refinement=: [
			ref-name= opt ref-desc= 
			(emit-target: last res/refinements)
			any ref-param=
			(emit-target: res/refinements)
		]
		locals=: [/local copy =val any word! (res/locals: =val)]
		spec=: [
			opt func-desc= opt func-attr=  
 			(emit-target: res/params)
 			any param= (foreach param res/params [new-line/all param off])
 			(emit-target: res/refinements)
 			any [locals= to end | refinement=]
 			(foreach param res/refinements [new-line/all param off]) 
		]
	
		if any-function? :spec [spec: third :spec]
		res: make func-spec [
			params: copy []
			refinements: copy []
			locals: copy []
		]
		either parse spec spec= [res] [none]
	]

]

comment {
	Generate combinations of test value calls from known values; find good
	boundary values for each type.
	
	integer! [
	    0 1 -1 2 -2 
	    255 -255 256 -256 32767 -32767 32768 -32768
	    65535 -65535 65536 -65536
	    to integer! (2 ** 31)  negate to integer! (2 ** 31)
	]
	decimal! [0.0 1.0 -1.0 ]
	number!	 [union integer! decimal!]
	money!
	time!	[0:0:0 0:0:1 0:1:0 1:0:0 1:1:1]
	date!	[to date! [0 0 0]  to date! [0 0 9999]  now/date   to date! [31 12 9999]]
	tuple!	[0.0.0 0.0.1 0.1.0 1.0.0 1.1.0 0.1.2 1.1.1 0.0.255 0.255.255 255.255.255 255.0.0 255.255.0]
	string!	["" " " "^@" "^^" "^/" "^-"]
	tag!	[<> < > <^@>]
	file!	[%"" %" " %"^@"]
	block!	[[] [[]] [[[]]] [()] [none] [#[unset!]]]
	list!	[make list! []]
	hash!	[make hash! []]
	paren!	[() (()) ((())) ([]) (none) (#[unset!])]
	
	test-vals-all-types: [
		0 1 -1
	]

	equal values
	values close together
	values far apart
	one value is zero
	one value is maximized
	negative values
	
	series at head
	series at tail
	series at first (next head)
	series at last  (back tail)
	series not at head or tail
	
	empty series
	series with one item
	series with two items
	series with an odd number of items
	series with an even number of items
	series with  0 <> mod n  items
	series with  0 =  mod n  items
}

do %typesets.r
do %datatypes.r

test-gen-ctx: context [

    change-all: func [
        "Change each value in the series by applying a function to it"
        series  [series!]
        fn      [any-function!] "Function that takes one arg"
    ][
        forall series [change series fn first series]
        head series
    ]

	recombine: func [start blocks] [
		collect blk [
			either tail? blocks [blk: reduce [start]] [
				foreach val first blocks [
					blk: recombine append copy start val next blocks
				]
			]
		]
	]
	
    flatten-in-place: func [block] [
        forall block [
            if block? block/1 [change/part block block/1 1]
        ]
        probe head block
    ]

; 	set 'generate-param-combinations func [
; 		typeset-blocks [block!] "block of blocks containing datatype values; one block per func param."
; 	][
; 		foreach block typeset-blocks [
; 			change-all block :expand-typeset
; 			print mold block
; 			forall block [
; 				if find/skip datatype-test-values type: to word! first block 2 [
; 					change/only block select datatype-test-values type
; 				]
; 			]
; 			print mold block
; 			;change-all block :flatten-in-place
; 			;foreach item block [change-all item :flatten-in-place]
; 		]
; 		change-all typeset-blocks :probe ;:flatten-in-place
; 		typeset-blocks
; 	]
	; print mold generate-param-combinations [[number!] [string!]]

	
	;!! Nothing here accounts for refinements yet.

	set 'all-param-typesets func [
		spec [object! block! any-function!]
		/local type
	][
		if not object? :spec [spec: parse-func-spec :spec]
		collect/only types [
			foreach param spec/params [
				types: either block? type: pick param 2 [type] [none]
			]
		]
	]
	
	set 'generate-param-combinations func [
		typeset-blocks [block!] "block of blocks containing datatype values; one block per func param."
	][
		new-line/all recombine copy [] collect/only param-vals [
			foreach block typeset-blocks [
				param-vals: collect vals [
					foreach type compose [(block)] [
						foreach type expand-typeset type [
							if find/skip datatype-test-values type: to word! type 2 [
								vals: new-line/all select datatype-test-values type off
							]
						]
					]
				]
			]
		] on
	]
	; print mold generate-param-combinations [[number!] [string!]]
	; print mold generate-param-combinations [[number! pair!] [any-string!]]
	
	set 'gen-test-param-sets func [fn [word! any-function!]] [
		if word? :fn [fn: get/any :fn]
		generate-param-combinations all-param-typesets :fn
	]
	
	set 'generate-func-tests func [
		fn [word! lit-word!] ; [any-function!]
		/local buff emit count ;res test
	][
		if not any-function? get/any :fn [
			print [mold :fn "is not a function"]
			exit
		]

		buff: copy ""
		emit: func [line] [append buff rejoin [line crlf]]
		
		count: 0		

		; If we get an error due to an expected val type problem, we can
		; probably skip all the tests that have the same set of types.		
		foreach param-set gen-test-param-sets get/any :fn [
			if error? set/any 'res try [do test: head insert param-set fn] [
				res: disarm res
				;res: reduce ['make-err res/code to lit-word! res/type to lit-word! res/id]
				res: reduce ['make-err to lit-word! res/type to lit-word! res/id]
			]
			incr count
			
			; 2-Jul-2007 Put strings in a block because we DO the expected result
			; when evaluating tests.
			if any-string? res [res: reduce [res]]
			
			;!! MULTIPLY kills this with an assertion #999 when trying to 
			; Generates tests in Carl's preferred format.
			emit mold to block! reform [mold/only/all res '== mold/only/all test]
			; generates in my preferred format
			;emit reform [">>" mold/all test '== mold/all res]
		]		
		print [count "tests generated for" :fn]
		buff
	]

	set 'gen-and-save-func-tests func [
		fn [word! lit-word!] 
		/local file
	][
		make-dir %generated-tests/
		file: replace form fn "?" "_"
		file: rejoin [%generated-tests/ file %.tst]
		print [newline mold :fn tab mold file]
		attempt [delete file]
		write file generate-func-tests fn
	]
	
	
	generate-action-tests: func [fn [word!]] [
		; get number of args
		buff: copy ""
		emit: func [line] [append buff rejoin [line newline]]
		; get number of args
		if not op? get fn [print "generate-op-tests arg must refer to an action"]
		spec: parse-func-spec get fn
		foreach arg-1-type expand-typeset pick spec/params/1 2 [
			foreach arg-2-type expand-typeset pick spec/params/2 2 [
				if all [
					find/skip datatype-test-values to word! arg-1-type 2
					find/skip datatype-test-values to word! arg-2-type 2
				][
					print [mold arg-1-type :fn  mold arg-2-type]
					emit reform ["^/;--" mold arg-1-type :fn  mold arg-2-type]
					foreach val-1 select datatype-test-values to word! arg-1-type [
						;print [mold arg-2-type type? arg-2-type]
						foreach val-2 select datatype-test-values to word! arg-2-type [
							;emit val-1 op val-2
							if error? set/any 'res try [do reduce [val-1 fn val-2]] [
								res: disarm res
								;res: reduce ['make-err res/code to lit-word! res/type to lit-word! res/id]
								res: reduce ['make-err to lit-word! res/type to lit-word! res/id]
							]
							emit reform  [">>" mold :val-1 :fn mold :val-2 '== mold/all res]
						]
					]
				]
			]
		]
		buff
	]
	
	;ops: [!= *  ** +  -  /  // <  <= <> =  == =? >  >= and or xor]
	use [ops] [	
		ops: [
; 			* "mul" ** "power" + "add"  - "sub" 
; 			/ "div" // "mod" 
; 			<  "less" 
; 			<= "le" 
			<> "ne" 
			> "greater" 
			>= "ge" 
			= "eq" 
			== "eq2" 
			=? "eq3" 
			and "and" or "or"  xor "xor"
		]
		set 'generate-all-op-tests func [/local spec emit res buff] [
			foreach [word file] ops [
				file: rejoin [%ops/ file %.tst]
				print [newline mold :word tab mold file]
				attempt [delete file]
				;print [newline mold :word tab mold rejoin [%ops/ file %.tst]]
				write file generate-op-tests word
				wait 0:0:2
			]
			print ""
		]
	]

 	set 'get-special-op func [op-name [string!]] [
 		get bind to word! op-name 'system
 	]
	
	set 'gen-and-save-op-tests func [
		fn [word! op!] 
		file [any-string!]
	][
		file: rejoin [%ops/ file %.tst]
		print [newline mold :fn tab mold file]
		attempt [delete file]
		write file generate-op-tests :fn
	]
	
	incr: :++
	
	set 'generate-op-tests func [fn [word!] /local spec emit res buff count] [
		buff: copy ""
		emit: func [line] [append buff rejoin [line crlf]]
		; get number of args
		;if word? fn [
			if not op? get fn [print ["generate-op-tests arg must refer to an op, not a" type? get fn]]
		;	fn: get fn
		;]
		spec: parse-func-spec get fn
		count: 0
		foreach arg-1-type expand-typeset pick spec/params/1 2 [
			foreach arg-2-type expand-typeset pick spec/params/2 2 [
				if all [
					find/skip datatype-test-values to word! arg-1-type 2
					find/skip datatype-test-values to word! arg-2-type 2
				][
					prin '. 
					print [mold arg-1-type :fn  mold arg-2-type]
					emit reform [crlf ";--" mold arg-1-type :fn  mold arg-2-type]
					foreach val-1 select datatype-test-values to word! arg-1-type [
						;print [mold arg-2-type type? arg-2-type]
						foreach val-2 select datatype-test-values to word! arg-2-type [
							;emit val-1 op val-2
							if error? set/any 'res try [do reduce [val-1 fn val-2]] [
								res: disarm res
								;res: reduce ['make-err res/code to lit-word! res/type to lit-word! res/id]
								res: reduce ['make-err to lit-word! res/type to lit-word! res/id]
							]
							incr count
							
							; 2-Jul-2007 Put strings in a block because we DO the expected result
							; when evaluating tests.
							if any-string? res [res: reduce [res]]
							
							emit reform  [">>" mold :val-1 :fn mold :val-2 '== mold/all res]
						]
					]
				]
			]
		]
		emit reform [crlf ";--" count "tests generated"]
		print [newline count "tests generated"]
		buff
	]
	;gen-and-save-op-tests '* %mul
	;gen-and-save-op-tests '** %power
	;gen-and-save-op-tests '+ %add
	; doesn't work yet.
	;;;;gen-and-save-op-tests first [get-special-op "<"]  %less
	;gen-and-save-op-tests first [<=]  %le
		

	
	ops: [
		add subtract multiply divide remainder power and~ or~ xor~ 
		same? equal? strict-equal? not-equal? strict-not-equal? 
		greater? lesser? greater-or-equal? lesser-or-equal? minimum maximum
	]
	set 'generate-all-math-compat-tests func [
		/local spec emit res buff file
	][
		;if not exists? %math-compat/ [make-dir/deep %math-compat/]
		foreach word ops [
			file: replace to file! word "?" "_"
			file: rejoin [%math-compat/ file %.tst]
			print [newline mold :word tab mold file]
			attempt [delete file]
			write file generate-math-compat-tests word
		]
		print ""
	]
	set 'generate-math-compat-tests func [
		fn [word!] 
		/local spec emit res buff get-test-val
	][
		test-val-avail?: func [type] [find/skip datatype-single-test-values to word! type 2]
		get-test-val: func [type] [select datatype-single-test-values to word! type]
		buff: copy ""
		emit: func [line] [append buff rejoin [line newline]]
		; get number of args
		;if not action? get fn [print "generate-op-math-compat-tests arg must refer to an action"]
		spec: parse-func-spec get fn
		foreach arg-1-type expand-typeset pick spec/params/1 2 [
			foreach arg-2-type expand-typeset pick spec/params/2 2 [
				if all [
					test-val-avail? arg-1-type
					test-val-avail? arg-2-type
				][
					prin '.
					;print [mold arg-1-type :fn  mold arg-2-type]
					emit reform ["^/;--" mold arg-1-type :fn  mold arg-2-type]
					
					val-1: get-test-val arg-1-type 
					val-2: get-test-val arg-2-type 
					;emit val-1 op val-2
					if error? set/any 'res try [do reduce [fn val-1 val-2]] [
						res: disarm res
						;res: reduce ['make-err res/code to lit-word! res/type to lit-word! res/id]
						res: reduce ['make-err to lit-word! res/type to lit-word! res/id]
					]
							
					; 2-Jul-2007 Put strings in a block because we DO the expected result
					; when evaluating tests.
					if any-string? res [res: reduce [res]]
					
					emit reform  [">>" :fn mold :val-1 mold :val-2 '== attempt [mold/all res]]
					
; 					foreach val-1 select datatype-single-test-values to word! arg-1-type [
; 						;print [mold arg-2-type type? arg-2-type]
; 						foreach val-2 select datatype-single-test-values to word! arg-2-type [
; 							;emit val-1 op val-2
; 							if error? set/any 'res try [do reduce [val-1 fn val-2]] [
; 								res: disarm res
; 								;res: reduce ['make-err res/code to lit-word! res/type to lit-word! res/id]
; 								res: reduce ['make-err to lit-word! res/type to lit-word! res/id]
; 							]
; 							emit reform  [">>" mold :val-1 :fn mold :val-2 '== mold/all res]
; 						]
; 					]
				]
			]
		]
		buff
	]
	

	ops: [
		add subtract multiply divide remainder power and~ or~ xor~ 
		same? ;equal? strict-equal? not-equal? strict-not-equal? 
		greater? ;lesser? greater-or-equal? lesser-or-equal? 
		minimum ;maximum
	]
	;ops: [add]
	set 'generate-all-math-compat-tables func [
		/local spec emit res buff file
	][
		;if not exists? %math-compat/ [make-dir/deep %math-compat/]
		write %math-compat/_all-tables.html s: collect tbl [
			foreach word ops [
				file: replace to file! word "?" "_"
				print [mold :word tab mold rejoin [%math-compat/ file %.tbl]]
				write rejoin [%math-compat/ file %.tbl]	tbl: generate-math-compat-table word
			] 
			print ""
		]
	]
	; non-COLLECT version
	set 'generate-all-math-compat-tables func [
		/local buff tbl file
	][
		buff: copy ""
		insert buff {
			<em>Note:</em> <br>^/
			The SAME? table applies to all equality tests. <br>^/
			The GREATER? table applies to all comparison tests. <br>^/
			The MINIMUM table also applies to MAXIMUM. <br>^/
		}
		;if not exists? %math-compat/ [make-dir/deep %math-compat/]
		foreach word ops [
			file: replace to file! word "?" "_"
			file: rejoin [%math-compat/ file %.tbl]
			print [mold :word tab mold file]
			append buff tbl: generate-math-compat-table word
			append buff crlf
			attempt [delete file]
			write file tbl
		] 
		print ""
		attempt [delete %math-compat/_all-tables.html]
		write %math-compat/_all-tables.html buff
		buff
	]
	
	set 'generate-math-compat-table func [
		fn [word!] 
		/local spec emit res buff get-test-val
	][
		test-val-avail?: func [type] [find/skip datatype-single-test-values to word! type 2]
		get-test-val: func [type] [select datatype-single-test-values to word! type]
		buff: copy ""
		emit: func [line] [append buff reform [line crlf]]
		; get number of args
		;if not action? get fn [print "generate-op-math-compat-tests arg must refer to an action"]
		spec: parse-func-spec get fn
		
		emit reduce [<h3> :fn </h3>]
		emit <table>
		emit <tr>
		emit [<th> </th>] ; cell at 0x0
		foreach arg-2-type expand-typeset pick spec/params/2 2 [
			if test-val-avail? arg-2-type [
				emit reduce [<th> form arg-2-type </th>]
			]
		]
		emit </tr>	
	
		ct: 0
		foreach arg-1-type expand-typeset pick spec/params/1 2 [
			emit <tr>
			if test-val-avail? arg-1-type [
				emit reduce [<th> form arg-1-type </th>]
			]
			;print [pick spec/params/1 2  pick spec/params/2 2]
			foreach arg-2-type expand-typeset pick spec/params/2 2 [
				if all [
					test-val-avail? arg-1-type
					test-val-avail? arg-2-type
				][
					val-1: get-test-val arg-1-type 
					val-2: get-test-val arg-2-type 
					;error? set/any 'res try [do reduce [fn val-1 val-2]]
					error? set/any 'res try [do reduce [fn :val-1 :val-2]]
; 					if error? get/any 'res [
; 						print [mold :fn mold :val-1 mold :val-2 mold :res]
; 						;halt
; 						ct: ct + 1
; 						if ct > 5 [halt]
; 					]
					emit reduce [<td> pick ['- 'OK] error? get/any 'res </td>]
					; showing the type is nice for some things, but not for others.
					; e.g. for equality, they all show up as logic.
; 					emit reduce [
; 						<td> 
; 						either error? get/any 'res ['-] [type? get/any 'res]
; 						</td>
; 					]
				]
			]
			emit </tr>
		]
		emit </table>
		emit <br>
		buff
	]

	;---------------------------------------------------------------------------	
	; replace doesn't work correctly yet under R3
; 	html-ize: func [value] [
; 		replace/all value "<" "&lt;"
; 		replace/all value ">" "&gt;"
; 	]
	html-ize: func [value] [:value]
	
	use [format-base mold-all ops op-names] [	
	
		format-val: func [value] [format [] :value]
		mold-all: func [value] [mold/all :value]
		
		ops: [to-string form mold mold-all format-val]
		op-names: [to-string form mold "mold/all" format]
		
		set 'generate-all-form-tables func [
			/local buff tbl file ops
		][
			buff: copy ""
			if not exists? %form/ [make-dir/deep %form/]
; 			foreach word ops [
; 				file: rejoin [%form/ file %.tbl]
; 				print [mold :word tab mold file]
; 				append buff tbl: generate-form-table word
; 				append buff crlf
; 				attempt [delete file]
; 				write file tbl
; 			] 
			append buff tbl: generate-form-table
			print ""
			attempt [delete %form/_all-tables.html]
			write %form/_all-tables.html buff
			buff
		]
		
		set 'generate-form-table func [
			/local emit res buff test-val-avail? get-test-val
		][
			test-val-avail?: func [type] [find/skip datatype-single-test-values to word! type 2]
			get-test-val: func [type] [select datatype-single-test-values to word! type]
			buff: copy ""
			emit: func [line] [append buff reform [line crlf]]
			
			emit reduce [<h3> "String conversions" </h3>]
			emit <table>
			emit <tr>
			emit [<th> </th>] ; cell at 0x0
			foreach op op-names [emit reduce [<th> form op </th>]]
			emit </tr>	
		
			ct: 0
			foreach [type val] datatype-single-test-values [
				emit <tr>
				if test-val-avail? type [
					emit reduce [<th> form type </th>]
				]
				foreach op ops [
					if test-val-avail? type [
						error? set/any 'res try [do reduce [op first [:val]]]
						emit reduce [<td> either error? get/any 'res [mold #ERR] [html-ize res] </td>]
					]
				]
				emit </tr>
			]
			emit </table>
			emit <br>
			buff
		]
		
	]

	;---------------------------------------------------------------------------	
	; replace doesn't work correctly yet under R3
; 	html-ize: func [value] [
; 		replace/all value "<" "&lt;"
; 		replace/all value ">" "&gt;"
; 	]
	html-ize: func [value] [:value]
	
	use [format-val mold-all ops op-names res] [	
	
		format-val: func [value] [format [] :value]
		mold-all: func [value] [mold/all :value]
		
		ops: [to make]
		op-names: [to make]
		
		set 'gen-all-MAKE-tables func [
			/local buff tbl file ops
		][
			buff: copy ""
			if not exists? %make/ [make-dir/deep %make/]
			foreach type reduce datatypes-ctx/datatypes [
				file: rejoin [%make/ type %.tbl]
				print [mold :type tab mold file]
				append buff tbl: generate-MAKE-table type
				append buff crlf
				attempt [delete file]
				write file tbl
			]
			;append buff tbl: generate-MAKE-table
			print ""
			attempt [delete %make/_all-tables.html]
			write %make/_all-tables.html buff
			buff
		]
		
		set 'generate-MAKE-table func [
			target-type [datatype!]			
			/local emit res buff test-val-avail? get-test-val
		][
			test-val-avail?: func [type] [find/skip datatype-single-test-values to word! type 2]
			get-test-val: func [type] [select datatype-single-test-values to word! type]
			buff: copy ""
			emit: func [line] [append buff reform [line crlf]]
			
			emit reduce [<h3> "MAKE/TO" target-type </h3>]
			emit <table>
			emit <tr>
			emit [<th> 'value </th>] ; cell at 0x0
			foreach op op-names [emit reduce [<th> form op 'type </th>  <th> form op 'result</th>]]
			emit </tr>	
		
			ct: 0
			foreach [type val] datatype-single-test-values [
				print type
				emit <tr>
				if test-val-avail? type [
					emit reduce [<th> form type </th>  <th> form :val </th>]
				]
				foreach op ops [
					if test-val-avail? type [
						error? set/any 'res try [do reduce [op target-type first [:val]]]
						emit reduce [
							<td> either error? get/any 'res [mold #ERR] [type? get/any 'res] </td>
							<td> either error? get/any 'res [mold #ERR] [attempt [html-ize res]] </td>
						]
					]
				]
				emit </tr>
			]
			emit </table>
			emit <br>
			buff
		]
		
	]
		
]

print "Try the gen-and-save-func-tests func"
;halt