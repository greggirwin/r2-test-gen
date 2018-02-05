REBOL []

;any-block!
;any-function!
;any-string!
;any-type!
;any-word!
;number!
;scalar!
;series!

collect: func [
    [throw]
    {Collects block evaluations.}
    'word "Word to collect (as a set-word! in the block)"
    block [any-block!] "Block to evaluate"
    /into dest [series!] "Where to append results"
    /only "Insert series results as series"
    /local code marker at-marker? marker* mark replace-marker rules
] [
    block: copy/deep block
    dest: any [dest make block! []]
    ; "not only" forces the result to logic!, for use with PICK.
    ; insert+tail pays off here over append.
    ;code: reduce [pick [insert insert/only] not only 'tail 'dest]
    ; FIRST BACK allows pass-thru assignment of value. Speed hit though.
    ;code: reduce ['first 'back pick [insert insert/only] not only 'tail 'dest]
    code: compose [first back (pick [insert insert/only] not only) tail dest]
    marker: to set-word! word
    at-marker?: does [mark/1 = marker]
    ; We have to use change/part since we want to replace only one
    ; item (the marker), but our code is more than one item long.
    replace-marker: does [change/part mark code 1]
    ;if debug [probe code probe marker]
    marker*: [mark: set-word! (if at-marker? [replace-marker])]
    parse block rules: [any [marker* | into rules | skip]]
    do block
    head :dest
]


;typesets-ctx: context [
	
	typesets: [
		any-block! [block! paren! path! set-path! get-path! lit-path! map!]
		any-function! [native! action! routine! rebcode! op! closure! function!]
		any-string! [string! binary! file! email! url! tag!]
		any-type! [
		    unset! none! logic! integer! decimal! percent! money! char! pair!
		    tuple! time! date! string! binary! file! email! url! tag! issue! 
		    bitset! unicode! vector! image! block! paren! path! set-path! 
		    get-path! lit-path! map! datatype! typeset! word! set-word! 
		    get-word! lit-word! refinement! native! action! routine! rebcode! 
		    op! closure! function! frame! object! module! error! task! port! 
		    gob! event! handle! struct! library! utype!
		]
		any-word! [word! set-word! get-word! lit-word! refinement!]
		number! [integer! decimal! percent!]
		scalar! [integer! decimal! percent! money! char! pair! tuple! time!]
		series! [
		    string! binary! file! email! url! tag! issue! bitset! unicode! vector! 
		    image! block! paren! path! set-path! get-path! lit-path! dictionary!
		]
	]
	
	expand-typeset: func [type] [
		if none? type [
			return head remove find copy select typesets 'any-type! 'unset!
		]
		unique collect val [
			foreach t compose [(type)] [
				; R2 can't reduce unknown datatypes from R3
				val: any [select typesets to word! :t  to word! :t]
				;val: any [reduce select typesets to word! :t  :t]
			]
		]
	]
	;print mold res: expand word!
	;print mold res: expand [integer! time!]
	;print mold res: expand [scalar! any-word!]
	
	expand-typeset: func [type /local res] [
		if none? type [
			return head remove find copy select typesets 'any-type! 'unset!
		]
		res: copy []
		foreach t compose [(type)] [
			; R2 can't reduce unknown datatypes from R3
			append res any [select typesets to word! :t  to word! :t]
			;append res any [reduce select typesets to word! :t  :t]
		]
		res
	]
	
	
;]

;halt