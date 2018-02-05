REBOL []

micro-test-ctx: context [
	lit-rt-chevron: to lit-word! ">>"
	=base: copy [] 
	=code: =ext: =expected: none
	call: none
	expected: actual: none
	count: failures: 0
	
	incr: :++
	
	test-failed: func [call actual [any-type!] expected [any-type!]][
		incr failures
	    print [
	        "Test Failed:" mold call newline
	        tab "actual:" either error? actual [
	        	 actual: disarm actual
	        	 ;reform ['ERROR actual/code actual/type actual/id]
	        	 reform ['ERROR actual/type actual/id]
	        ] [mold actual]
	        tab rejoin ["(" mold type? actual ")"]
	        newline
	        tab "expected:" either error? expected [
	        	;if not object? expected [
	        		expected: disarm expected
        		;]
	        	;reform ['ERROR expected/code expected/type expected/id]
	        	reform ['ERROR expected/type expected/id]
	        ] [mold expected]
	        tab rejoin ["(" mold type? expected ")"]
	    ]
	]
	
	same-error?: func [a b] [
		; Don't compare codes, they will change between R2 and R3.
		;all [a/code = b/code  a/type = b/type  a/id = b/id]
		all [a/type = b/type  a/id = b/id]
	]
	
	run-test: does [
		incr count
    	set/any 'actual try [do call: append copy =base =ext]
    	set/any 'result get/any 'actual
    	;set/any 'expected try [do bind =expected 'actual]
    	set/any 'expected try [do =expected]
        if not any [
        	;all [=check?  true? expected]
       		;all [unset? get/any 'actual  unset? get/any '=expected]
        	all [
        		not error? get/any 'actual  
        		strict-equal? get/any 'actual get/any 'expected
        	]
        	all [
        		error? get/any 'actual  
        		; can't make errors under R3 correctly yet
        		;any [error? get/any 'expected  object? get/any 'expected]
        		error? get/any 'expected
        		same-error? :actual :expected
        	]
        ][
        	;either =check? [
        	;	check-failed =expected
    		;][
        		test-failed call actual expected
    		;]
    	]
	]

	main-rule=: [
		some [
			;(=expected: none  clear =base)
			['do set =code block! (do =code)]
			;| set =base block! (print mold =base) into [
			| set =base block! into [
					some [
						(=ext: none  =check?: false)
						into [
							[
								copy =ext to '== skip 
								set =expected any-type! (run-test)
							]
							| [
								copy =ext [to 'expect | to 'check (=check?: true)] skip
								copy =expected to end
								;(if =check? [print mold head insert =expected 'true?])
								;(print [mold append copy =base =ext '=== mold =expected])
								(run-test)
								mark-a:
							]
						]
						mark-b:
					]
					mark-c:
				]
				mark-d:
			| [
				lit-rt-chevron  copy =ext to '== skip  set =expected any-type! 
				mark-e:
				(
					;print [tab mold =ext tab mold =expected tab type? =expected] 
					=base: copy []
					run-test
				)
			]
			
		]
		mark:
	]
	
	
	set 'make-err func [
		;code [integer!] 
		type [word!] id [word!]
	][
		; Don't use code when making errors, it overrides the error type and 
		; causes problems.
		;make error! compose [code: (code) type: (to lit-word! :type) id: (to lit-word! :id)]
		make error! compose [type: (to lit-word! :type) id: (to lit-word! :id)]
	]
	set 'ERR! :make-err
	
	true?:  func [val] [to logic! all [logic? val  val]]
	false?: func [val] [to logic! all [logic? val  not val]]
	
	set 'test func [input [block! file!]] [
		if file? input [input: load input]
		count: failures: 0
		dbg: :input
		test-parse-res: parse input main-rule=
		print [count "tests run, " failures "tests failed"]
		zero? failures
	]
]

; tests: [
; 	>> add 0 0 == 0
; 	>> add 1 1 == 2
; 	>> add 123 456 == 579
; 	>> 10 + 20 == 30
; 	[add] [
; 		[1 2 == 3]
; 		[4 5 == 9]
; 	]
; ; 	[split-path] [
; ; 		[%/c/rebol/tools/test/test.r == [%/c/rebol/tools/test/ %test.r]]
; ; 		[%/c/rebol/tools/test/       == [%/c/rebol/tools/ %test/]]
; ; 	]
;     [i: 0  while] [
;         [[i < 10] [i: i + 1] i == 10]
;         [[false]  [i: i + 1] i == 0]
;     ]
; ]

;test tests