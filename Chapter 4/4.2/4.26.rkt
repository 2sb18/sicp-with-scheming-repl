;Exercise 4.26.  Ben Bitdiddle and Alyssa P. Hacker disagree over the importance of lazy evaluation for implementing things such as unless. Ben points out that it's possible to implement unless in applicative order as a special form. Alyssa counters that, if one did that, unless would be merely syntax, not a procedure that could be used in conjunction with higher-order procedures. Fill in the details on both sides of the argument. Show how to implement unless as a derived expression (like cond or let), and give an example of a situation where it might be useful to have unless available as a procedure, rather than as a special form.

; Ben's point of making a special form for "unless" is valid. One problem with it though is that every time to want a procedure to act like it's being evaluated lazily, you have to modify the language.

; to implement "unless" as a derived expression, you could derive it from if
; ie (unless condition usual-value exceptional-value)

(make-if condition exceptional-value usual-value) 

; what's an example of where having "unless" available as a procedure is 
; advantageous?

; a-ha! one example is where you want to be able to pass whatever procedure
; you want into another procedure, and in some case you want to pass "unless"
; into the procedure. maybe you could pass it by wrapping it in an anonymous
; procedure.



