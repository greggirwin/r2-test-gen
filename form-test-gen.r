REBOL []

set 'datatype-single-test-values compose [
    action!     :add
    binary!     #{}
    bitset!     #[bitset! #{}]
    block!      []
    char!       #"A"
    closure!    (closure [a b] [])
    datatype!   #[datatype! action!]
    date!       1-jan-2000
    decimal!    1.5
    email!      a@b.c
    end!        NA
    error!      (disarm try [make error! ""])
    event!      NA
    file!       %""
    frame!      NA
    function!   (func [a b] [])
    get-path!   :a/b/c
    get-word!   :test
    gob!        (make gob! [])
    handle!     NA
    image!      (make image! 1x1)
    integer!    1
    issue!      #abc
    library!     NA
    lit-path!   'a/b/c
    lit-word!   'test
    logic!      false
    map!        (make map! [])
    module!     NA
    money!      $0.01
    native!     NA
    none!       #[none]
    object!     (context [a: b: none])
    op!         :+
    pair!       1x1
    paren!      (to paren! [])
    path!       a/b/c
    percent!    1%
    port!        NA
    rebcode!    NA
    refinement! /test
    routine!    NA
    set-path!   a/b/c:
    set-word!   test:
    string!     "abc"
    struct!     NA
    tag!        <abc>
    task!       NA
    time!       0:0:1
    tuple!      0.0.1
    typeset!    (make typeset! [scalar! tag!])
    unicode!    NA
    unset!      (make unset! none)    'unset-placeholder ; compose eliminates the unset val
    url!        http://abc
    utype!      NA
    vector!     (make vector! 10)
    word!       test
]
;insert next find datatype-single-test-values 'unset! ()

;---------------------------------------------------------------------------    
; replace didn't work correctly under R3 when this was first written, but 
; maybe it does now.
html-ize: func [value] [
    replace/all value "<" "&lt;"
    replace/all value ">" "&gt;"
]
;html-ize: func [value] [:value]

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
;             foreach word ops [
;                 file: rejoin [%form/ file %.tbl]
;                 print [mold :word tab mold file]
;                 append buff tbl: generate-form-table word
;                 append buff crlf
;                 attempt [delete file]
;                 write file tbl
;             ] 
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


generate-all-form-tables