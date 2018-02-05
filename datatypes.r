REBOL []


datatypes-ctx: context [
	datatypes: [
		action! binary! bitset! block! char! closure! datatype! date! decimal! 
		map! email! end! error! event! file! frame! function! get-path! 
		get-word! gob! handle! image! integer! issue! library! lit-path! 
		lit-word! logic! module! money! native! none! object! op! pair! paren! 
		path! percent! port! rebcode! refinement! routine! set-path! set-word! 
		string! struct! tag! task! time! tuple! typeset! unicode! unset! url! 
		utype! vector! word!
	]
	literal-types: reduce [ ; have direct lexical form
		binary! block! char! datatype! date! decimal! email! file! get-path!
		get-word! integer! issue! lit-path! lit-word! logic! money! none! pair! 
		paren! path! percent! refinement! set-path! set-word! string! tag! 
		time! tuple! url! word!
	]
	common-types: reduce [
		binary! bitset! block! char! date! decimal! email! file! get-word! 
		integer! issue! lit-path! lit-word! logic! money! pair! paren! 
		refinement! set-word! string! tag! time! tuple! url! word!
		; percent! 
	]
	common-types-vals: [
		#{} 
		#[bitset! #{0000000000000000000000000000000000000000000000000000000000000000}]
		[] #"^@" 1-jan-2000 0.0 abc@xyz.com %filename :get-word 
		0 #issue 'a/b/c 'a false $0.00 0x0 () 
		/refinement set-word: "string" <tag> 0:0:0 0.0.0 http://www.rebol.com abc
		;0%
	]
; 	foreach type common-types [
; 		prin [type tab tab '|]
; 		foreach val common-types-vals [
; 			prin either attempt [to type val] ['*] [" "] 
; 		] 
; 		print ""
; 	]
; 	foreach type common-types [
; 		print ["^/;--" uppercase form type]
; 		foreach val common-types-vals [
; 			print [">> to" mold type mold :val '== mold/all attempt [do ['to type val]]]
; 		] 
; 		print ""
; 	]

; 	s: copy ""
; 	foreach type common-types [
; 		repend s reform ["^/;--" uppercase form type newline]
; 		foreach val common-types-vals [
; 			repend s reform  [">> to" mold type mold :val '== mold/all attempt [do reduce ['to type val]] newline]
; 		] 
; 		repend s newline
; 	]
; 	save/all %genx.tst s


	; foreach word b [
	; 	print [
	; 		:word  
	; 		any [
	; 			attempt [mold make to datatype! word none]
	; 			attempt [mold make to datatype! word 0] ; use 1 instead of 0?
	; 			attempt [mold make to datatype! word "test"]
	; 			'NA
	; 		]
	; 	]
	; ]

	; Using 1 instead of 0 in places, to avoid divide-by-zero errors.
	set 'datatype-single-test-values compose [
		action! 	:add
		binary! 	#{}
		bitset! 	#[bitset! #{}]
		block! 		[]
		char! 		#"A"
		closure! 	(closure [a b] [])
		datatype! 	#[datatype! action!]
		date! 		1-jan-2000
		decimal! 	1.5
		email!		a@b.c
		end! 		NA
		error! 		(disarm try [make error! ""])
		event!		NA
		file! 		%""
		frame! 		NA
		function! 	(func [a b] [])
		get-path! 	:a/b/c
		get-word! 	:test
		gob! 		(make gob! [])
		handle! 	NA
		image! 		(make image! 1x1)
		integer!	1
		issue! 		#abc
		library! 	NA
		lit-path! 	'a/b/c
		lit-word! 	'test
		logic! 		false
		map! 		(make map! [])
		module! 	NA
		money! 		$0.01
		native! 	NA
		none! 		#[none]
		object! 	(context [a: b: none])
		op! 		:+
		pair! 		1x1
		paren! 		(to paren! [])
		path!		a/b/c
		percent! 	1%
		port! 		NA
		rebcode! 	NA
		refinement! /test
		routine! 	NA
		set-path! 	a/b/c:
		set-word! 	test:
		string! 	"abc"
		struct! 	NA
		tag! 		<abc>
		task! 		NA
		time! 		0:0:1
		tuple! 		0.0.1
		typeset! 	(make typeset! [scalar! tag!])
		unicode! 	NA
		unset! 		(make unset! none)	'unset-placeholder ; compose eliminates the unset val
		url!		http://abc
		utype! 		NA
		vector! 	(make vector! 10)
		word! 		test
	]
	;insert next find datatype-single-test-values 'unset! ()
	
	set 'datatype-test-values [
		;action!         
		binary! [
			#{} #{00}
			2#{} 2#{00000000} 2#{00000001} 2#{10000000} 2#{11111111}
			16#{} 16#{00} 16#{FF} 16#{0F} 16#{F0} 16#{01} 16#{FE}
			16#{0000} 16#{FFFF} 16#{FF00} 16#{00FF} 16#{0001} 16#{FE00} 
			64#{}
		]        
		bitset! [
			; ""
			make bitset! #{0000000000000000000000000000000000000000000000000000000000000000} 
			;complement charset ""
			make bitset! #{FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
		]
		block!	[
			[] [[]] [[[]]] [()] [none] [#[unset!]]
			(head [1 2 3]) (next [1 2 3]) (back tail [1 2 3]) (tail [1 2 3])
		]
; 		char! [
; 			; 0
; 			#"^@"
; 			; 1 - 31
; 			#"^A"  #"^B"  #"^C"  #"^D"  #"^E"  #"^F"  #"^G"  #"^H"  #"^-"  #"^/"  
; 			#"^K"  #"^L"  #"^M"  #"^N"  #"^O"  #"^P"  #"^Q"  #"^R"  #"^S"  #"^T"
; 			#"^U"  #"^V"  #"^W"  #"^X"  #"^Y"  #"^Z"  #"^["  #"^\"  #"^]"  #"^!"
; 			#"^_" 
; 			; 32 - 47
; 			#" "  #"!"  #"^""  #"#"  #"$"  #"%"  #"&"  #"'"  #"("  #")"  #"*"  
; 			#"+"  #","  #"-"  #"."  #"/"
; 			
; 		]
		char! [
			; 0 1 2 31 32 126 127 128 253 254 255
			#"^@"  #"^A"  #"^B"  ;#"^_"  #" "  #"~"  #"^~"  #"€"  #"ý"  #"þ"  #"ÿ"
		]
		;closure!        
		datatype! [
			binary! block! char! date! decimal! integer! string!
; 			action! binary! bitset! block! char! closure! datatype! date! decimal! 
; 			map! email! end! error! event! file! frame! function! get-path! 
; 			get-word! gob! handle! image! integer! issue! library! lit-path! 
; 			lit-word! logic! module! money! native! none! object! op! pair! paren! 
; 			path! percent! port! rebcode! refinement! routine! set-path! set-word! 
; 			string! struct! tag! task! time! tuple! typeset! unicode! unset! url! 
; 			utype! vector! word!
		]
		date!	[
			;(to date! [0 0 0])  
			;(now/date)     ; hard to use now/date, because then we have to calc all
			;				; results based on when the test was generated vs run.
			;(to date! [1 1 0])  (to date! [31 12 0]) ;cause crashes
			(to date! [1 1 2000])  (to date! [31 12 2000])
			(to date! [0 0 9999])  (to date! [31 12 9999])
		]
		decimal! [
			0.0 -0.0 1.0 -1.0 
			;.01 .02 .001 .0001 .00001 .000001 .0000001
			;-.01 -.02 -.001 -.0001 -.00001 -.000001 -.0000001
		]
		email! [
			x@ x@y x@.com x@y.com 
			xyz@ xyz@y xyz@.com xyz@y.com
			xyz@.com abc@xyz.com
		]
		;end!            
		;error!          
		;event!          
		file!	[%"" %" "] ; %"^@" creates an unloadable value: %%00
		;frame!          
		;function!       
		;get-path!       
;		get-word!       
		;gob!            
		;handle!         
		;hash!	[make hash! []]
;		image!          

		; >> to-hex -1
		; == #FFFFFFFFFFFFFFFF
		; >> to integer! #7FFFFFFFFFFFFFFF
		; == 9223372036854775807
		; >> to integer! #8000000000000000
		; == -9223372036854775808
		integer! [
		    0 1 -1 2 -2 
		    ;255 -255 256 -256 32767 -32767 32768 -32768
		    ;65535 -65535 65536 -65536
		    ;(to integer! (2 ** 31 - 1))  
		    ;(negate to integer! (2 ** 31 - 1))
		]
		issue! [# #a #Z #abc ## #123#]
		;library!        
		;list!	[make list! []]
		lit-path! ['a/b 'a/b/c]
		lit-word! ['a 'abc]
		logic! [true false] ;[true false yes no on off]
		map!   [(make map! [])]
		;module!         
; 		money! [
; 		    $0 $1 -$1 $2 -$2 
; 		    ;$255 -$255 $256 -$256 $32767 -$32767 $32768 -$32768
; 		    ;$65535 -$65535 $65536 -$65536
; 		    ;$0.01 $0.02 -$0.01 -$0.02
; 		    ;$1.01 $2.02 -$1.01 -$2.02
; 		]         
		;native!         
		none!    [#[none]]
		object!  [(make object! [])]
		;op!             
		pair! [0x0 1x1 0x1 1x0 -1x0 0x-1 -1x-1]
		;paren!	[() (()) ((())) ([]) (none) (#[unset!])]
		paren! [
			(to paren! []) (to paren! [()]) (to paren! [(())]) (to paren! [([])])
			(to paren! none) (to paren! ())
		]
;		path!           


		; >> 9223372036854775807%
		; == 9223372036854775800%
 		percent! [
; 			 0%  1%  100%  .5%  .01%  .125%  100'000%  4'294'967'296%  4'294'967'296.5%
; 			-0% -1% -100% -.5% -.01% -.125% -100'000% -4'294'967'296% -4'294'967'296.5%
			999999999999999%  ; max
			-999999999999999% ; min
 		]
		;port!           
		;rebcode!        
		refinement! [/a /abc /abc-123 /abcÿ123 /! /ÿ] ; /abc_123 /abc~123 /abc!123 
		;routine!        
;		set-path! 
		set-word!  [a: abc:]
		string!	[
			"" " " "^@" "^^" "^/" "^-"
			"-" "_" "ÿ" 
			"^@^@^@^@^@" "ÿÿÿÿÿÿÿ"
			"0" "1" "-1"
		]
		;struct!         
		tag!	[
			(to tag! "") 
			;(to tag! " ")  ; this generates things that can't be loaded; e.g. < 0>
			<^@>
		]
		;task!           
		time!	[
			0:0:0 0:0:1 0:1:0 1:0:0 1:1:1
			;0:0:0.1 0:0:0.01 0:0:0.001 0:0:0.0001
			
			225:41:05  ; 225:41:05 - 1 causes an overflow error in A67

			; These can produce unloadable results when ops are applied.
			;596523:14:07.999999999  ; max
			;-596523:14:07.999999999 ; min
		]
		tuple!	[
			0.0.0 0.0.1 0.1.0 1.0.0 1.1.0 0.1.2 1.1.1 
			;0.0.255 0.255.255 255.255.255 255.0.0 255.255.0
			;0.0.0.0.0.0.0.0.0.0 255.255.255.255.255.255.255.255.255.255
			;0.1.2.3.4.5.6.7.8.9
		]
		;typeset!        
		;unicode!        
		unset!  [(#[unset!])]
		url!    [http://www.rebol.com ftp://user:pass@my-ftp-site.com]
		;utype!          
		;vector!         
		word!   [a abc]       
	]

	;SCALAR!:[integer! decimal! percent! money! char! pair! tuple! time!]
	; union in LHS type with list of compatible types
	set 'datatype-math-compatibility [
		action! []
		binary! [any-type!]
		bitset! []
		block! []
		char! [char! number!]
		closure! []
		datatype! []
		date! [date! number!]
		decimal! [number! time! percent!]
		email! [any-type!]
		end! []
		error! []
		event! []
		file! [any-type!]
		frame! []
		function! []
		get-path! []
		get-word! []
		gob! []
		handle! []
		image! []
		integer! [scalar!]
		issue! [any-type!]
		library! []
		lit-path! []
		lit-word! []
		logic! []
		map! []
		module! []
		money! [money! number!] ; percent!
		native! []
		none! []
		object! []
		op! []
		pair! [pair! number!]
		paren! []
		path! []
		percent! [percent! number!]
		port! []
		rebcode! []
		refinement! []
		routine! []
		set-path! []
		set-word! []
		string! [any-type!]
		struct! []
		tag! [any-type!]
		task! []
		time! [time! number!] ; percent!
		tuple! [tuple! number!]
		typeset! []
		unicode! []
		unset! []
		url! []
		utype! []
		vector! []
		word! []
	]

	set 'datatype-addition-compatibility [
		binary! [any-type!]
		char! [char! number!]
		date! [date! number!]
		decimal! [number! time! percent!]
		email! [any-type!]
		file! [any-type!]
		integer! [scalar!]
		issue! [any-type!]
		money! [money! number!] ; percent!
		pair! [pair! number!]
		percent! [percent! number!]
		string! [any-type!]
		tag! [any-type!]
		time! [time! number!] ; percent!
		tuple! [tuple! number!]
		;unicode! [] ???
		url! [any-type!]
		;vector! [] ???
	]
		
	set 'datatype-math-compatibility [
		char! [char! number!]
		date! [date! number!]
		decimal! [number! time! percent!]
		integer! [scalar!]
		money! [money! number!] ; percent!
		pair! [pair! number!]
		percent! [percent! number!]
		time! [time! number!] ; percent!
		tuple! [tuple! number!]
		;vector! [] ???
	]
	
]
