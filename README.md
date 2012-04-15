# lusa

lusa mixes in lua-like syntax into scheme. The implementation is based on Racket's reader system.

## Basic syntax

`~' is used to embrace the new syntax.

	~array0 = {~self.0 = "a"~
        	   ~self.1 = "b"~}~
	~array0.each = {|cb| (hash-map self cb)}~
	~array0.each () {|k v|
        	         (display k)
                	 (display v)}~


## Examples

* Create a closure: `|' follows `{'

	~{|p1 p2| (+ p1 p2) }~  => (lambda (p1 p2) (+ p1 p2))

* Define a hash table: `= {}' means a hash table is created.

	~foo = {} ~ => (define foo (make-hash))

* Add a property into a hash table:

	~foo.bar.a = 1~  => (hash-set! (hash-ref foo 'bar) 'a 1)

* Refer to a property of a hash table:

	~foo.bar~ => (hash-ref foo 'bar)

* Insert a closure to a hash table: a 'self' will be added as closure's first argument

	~foo.bar = {|| ~self.a~}~ => (hash-set! foo 'bar (lambda (self) (hash-ref self 'a)))

* Invake a closure from a hash table: parent will be passed to first argument of closure

	~foo.bar ()~ => ((hash-ref foo 'bar) foo)

* Invake a closure from a hash table plus a closure at the end(ruby-like):

	~foo.bar () {|| (+ 1 1)}~ => ((hash-ref foo 'bar) foo (lambda (self) (+ 1 1)))



