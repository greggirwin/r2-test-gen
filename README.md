# r2-test-gen
Old R2 Test Generation Code and Ideas

Contents of %notes.txt
---------------------------

=== Files

%datatypes.r - defines test values for each datatype.

%micro-test-ctx.r - basic test engine.

%run-op-tests.r - shows how to call a folder of tests using %micro-test-ctx.r.

%test-gen.r - this is the big, ugly dumping ground. See below for details.

%test-init.r - bootstrap for console sessions.

%typesets.r - used for typeset expansion of pseudo-types to their subtypes.


--- %test-gen.r

If you run this file, it will prompt you to try the gen-and-save-func-tests
func. 

>> gen-and-save-func-tests 'mold

mold     %generated-tests/mold.tst
145 tests generated for mold
== port!

It looks like I updated it to emit the block format Carl prefers, from the
console-like test format I used originally. Though I seem to recall that
there were some issues with that change.

---

func-spec-ctx - for parsing function specs into objects, so we can 
generate tests based on their args and refinements.

Comment sections are "thought dumps".

--- test-gen-ctx

A couple generic functions, and then a number of things for 
generating typesets from function specs.

all-param-typesets returns a block of typesets, one for each of a 
func's parameters.

generate-param-combinations takes the result of all-param-typesets,
expands all the typesets, gets the test values for each type in the
expanded typesets, and recombines them into parameters for test calls.

gen-test-param-sets wraps the two calls above, give a word or function.

generate-func-tests wraps the above calls, using each set of parameters
with a test call to the given function, generates an expected result,
and returns the test calls, with those results, as a loadable string.

gen-and-save-func-tests calls generate-func-tests and writes the
result to an automatically named file, in a %generated-tests/ sub-folder.

generate-action-tests looks like it should be ignored. Generate-op-tests
is the correct infix version.

generate-all-op-tests looks like it is probably obsolete. The main
thing it did was to map ops names to file names, for ops that are
invalid filenames themselves. 

gen-and-save-op-tests is like gen-and-save-func-tests, but for ops. It
takes a filename as a param, because not all ops can be used as file
names (e.g. <).

generate-op-tests is like generate-func-tests, but for ops, because they 
are infix.

*-math-compat-* funcs were, IIRC, just to help build the compatibility
tables for docs.

generate-all-form-tables and gen-all-MAKE-tables were also just for
generating docs.


=== Test generation thoughts

Matching errors

Generating params for funcs like NONE? and STRING? (none! issue)

Copying series for funcs that mod them, e.g. UPPERCASE

Special result cases like "TAIL X@y.com" (empty email)

  >> mold/all tail X@y.com
  == {#[email! "X@y.com" 8]}

money and percent datatypes

** Syntax error: Invalid file -- %%00999999999999999
** Where: to foreach generate-func-tests gen-and-save-func-tests
** Near: (line 1) #[file! "^@999999999999999" 2] == change %%00999999999999999 #"^@"
