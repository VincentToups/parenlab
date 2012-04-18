[I am two people](https://www.youtube.com/watch?v=xFlc9q6AHX4) - one
of them is a numerical scientist who spends most of his time writing
code in Matlab to do data analysis.  The other is a freelance Lisp
programmer, where I do regular software engineering.  I've long been a
Lisp enthusiast, and I do a fair amount of Lisp programming in Emacs
Lisp as part of my scientific work, since Emacs is my Matlab IDE, but
I didn't start _really_ programming in Lisp for "real things" until
recently.  As the number of hours dedicated to Lisp increased in my
life, I began to really miss the features in Lisp that I didn't have
in Matlab; in particular s-expression motion and editing and
metaprogramming.

I can't abandon Matlab, however.  Matlab's support for numerical
programming is exceptional, true, but the real value is that its
plotting system is rich, well integrated and customizable.  Plus, I
have 7+ years of Matlab utilities which it would take significant time
to reproduce in another numerical analysis language, even if it was
feature complete otherwise.

So I brought the mountain to Muhammad. 

Parenlab
========

[Parenlab](https://github.com/VincentToups/parenlab) is a Lisp which
compiles to Matlab by way of Emacs Lisp.  What this means is that the
syntax is Emacs Lisp and the Semantics is Matlab.  Metaprogramming is
possible via macros which transform the elisp representation of Matlab
code before the code is translated to Matlab.  Because I use Emacs as
my Matlab IDE, this is a reasonable solution - just use the Emacs Lisp
interpreter as the macro language.  One day, Parenlab may be
self-hosting.

Examples:
=========

The parenlab code:

    (let* ((x (range 0 (* 2 pi) 100))
           (y (sin x)))
     (plot x y :color "g"))

Is converted to:

    funcall(...
	    @(x)funcall(@(y)plot(x, y, 'color', 'g'), sin(x)),...
         range(0, mtimes(2, pi), 100))

Or:

    (defun (a b c) some-function (a b c)
       "Documentation."
       (setq a (++ a b c))
       (setq b (++ a b c))
       (setq c (++ a b c)))

Is converted to a file, called someFunction.m which contains:

    function [a,b,c] = someFunction(a,b,c)
    %Documentation.
    'Documentation.';
    a = plusplus(a, b, c);
    b = plusplus(a, b, c);
    c = plusplus(a, b, c);

`defun`s can be sprinkled throughout your script files - they will be
transcoded as they are encountered and produce no output in the
currently building function.  One can also say:

    (script some-script-name 
     <parenlab-code>)

Which indicates the code beneath should be transcoded to a script file
rather than "in place."

Parenlab is still a work in progress, but it is rapidly approaching
the point where any functionality you wish can be expressed in
Parenlab.  

Notes on Usage
==============

Mangling
--------

Parenlab, like Parenscript, tries to make life easy by letting you
write lisp-style identifiers.  In doing so, it mangles names during
translation.  Eg, a parenlab symbol:

    some-function

Will be rendered as:

    someFunction

eg, dashes followed by letters are converted to camel case.  Other
"special" characters are transcoded according to the following table:

    ("+" "plus")
    ("-" "minus")
    ("*" "mtimes")
    ("<" "lessThan")
    (">" "greaterThan")
    ("$" "cash")
    ("=" "equal")
    ("!" "bang")
    ("?" "who")
    (":" ":")
    ("/" "divide")
    ("\\" "mdivide")
    ("#" "hash")
    ("@" "at"))

These choices are made to improve transcoding, so that many operators
don't need special cases.  Eg `(* 10 10)` translates to
`mtimes(10,10)` - conveniently using Matlab's built in `mtimes`
function, which is equivalent to Matlab's `*` operator.

One wrinkle is that `:` is reserved to preserve simple matrix
construction expressions.  For instance,

    x:some-variable:y

transcodes to 

    x:someVariable:y

This syntax is limited by how the lisp reader reads symbols.  For
complex generation expressions, use the `(: )` macro.  Eg:

    (: start step stop)

Keywords, that is symbols starting with `:`, are transcoded to mangled
strings, so that `:a-keyword` becomes `'aKeyword'`.

Since many matlab functions use strings as keyword arguments, this
lets you use keywords for them instead.

`nil` transcodes to `[]`.  There are no true and false values in
Matlab.

Scope and Variables
-------------------

Scope in Matlab is funny - only limited lambdas are allowed in scripts
and functions, although nested functions with full definitions and
full lexical scope behavior are allowed within functions (though not
anonymously).

Sort of like in Python, `lambda`s are restricted to
single-expression-only bodies.  While they _do_ form closures over
their lexical environment, those closures are _static_, that is, they
do not permit side effects of any kind.

That is, 

    (setq y 10)
    (setq f 
     (lambda (x) 
       (setq y x)))

Will compile fine in Parenlab, but produce an error, because the inner
`setq` is not an expression, but a statement, which isn't allowed.
Parenlab does provide a `progn` form, so you might try:

    (setq y 10)
    (setq f 
     (lambda (x) 
       (progn (setq y 10)
              nil)))

This will generate Matlab code which uses `eval` to produce code which
expresses this intent, but you'll still get an error, saying that the
lambda tries to modify its static environment, which is not allowed.
`let` and `let*` are implemented using `lambda`, so they create static
lexical environments.  It is conceivable that major cross compilation
tricks could be used to simulate dynamic scopes, but I'd prefer to
keep things simple.  

You shouldn't be using side effects anyway.

The rule of thumb is write Matlab code with s-expressions, not Lisp
code that calls Matlab functions.  Parenlab tries to bridge the gap
but in some ways Matlab is too limited.

Cell Arrays
-----------

Indexing is identical function calls, syntactically, so the index
expression:

    x(1:10)

Is written as

    (x 1:10)

In parenlab.  Matlab also has cell array indexing, which looks like
this:

    x{1:10}

Which is expressed like this in Parenlab:

    ({} x 1:10)

These are macros, not functions, and so the use of `end` as an
identifier inside them is fine, eg:


    ({} x 1:end)

Will convert to:

    x{1:end}

Structs
-------

Structure access can be written as `symbol.name` since the parenlab
mangler leaves dots in symbol names.  Programmatic access, where a
string is used, is written with `->`, eg:

    (-> s :field)

Which is equivalent to `s.('field')`.  `->` supports nested access, so
you can say;

    (-> s :f1 :f2 :f3)

Which is like this:  `s.('f1').('f2').('f3')` which may not actually
be valid Matlab syntax.  Parenlab implements this as a function call.


Macros
------

In elisp, parenlab macros can be defined with `pl:def-pl-macro`.
During transcoding, the form `defmacro` causes a new parenlab macro to
be defined and generates no output.  Parenlab macros have the same
semantics as regular Macros.  You get syntax passed to the macro as
arguments, you transform it into valid parenlab, and you return the
result.  

For instance, a macro which introduces a lexical variable binding
called `with`, like this:

    (with x 10 (* 2 x)) ;-> 20

Is implemented like this:

    (pl:def-pl-macro with (symbol value expression)
     `(funcall (lambda (,symbol) ,expression) ,value))

Or, inside some Parenlab code:

    (defmacro with (symbol value expression)
     `(funcall (lambda (,symbol) ,expression) ,value))


Other Notes
===========

Right now you use parenlab by invoking `pl:transcode` on an
s-expression.  This will transcode in the current buffer.
`pl:transcode-to-string` will transcode to a string instead.  

Parenlab requires `matlab-mode` so that it can indent its outputted
code correctly.

I have a highly idiosyncratic Matlab setup, so I put some integration
code in `auxilliary.el`.  If you have a Matlab process running in a
buffer called `*evalshell*`, then this code will let you load a
parenlab file and press `C-c C-c` to "compile" and execute the code
therein.  This process will write any script and defun forms to files
before executing the code.  `C-x C-e` will evaluate the last
s-expression as Matlab code in the interpreter.

Parenlab depends on [Shadchen-el](https://github.com/VincentToups/shadchen-el) my pattern matching library.

