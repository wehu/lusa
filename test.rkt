#lang reader "lusa.rkt"

(+ 1 2)

~foo = {}~

~foo.bar = {~self.a = 1~
            ~self.1 = 2~}~

~foo.bar.1~
~foo.bar = {|a cb| (+ a 3) (cb)}~
~foo.bar (1) {|| 3}~
~foo.c = {}~
~foo.c.a=1~


~foo.b = {|a| (+ a 4)}~
~foo.b (~foo.c.a~)~

~array0 = {~self.0 = "a"~
           ~self.1 = "b"~}~

~array0.[0]~
~array0.each = {|cb| (hash-map self cb)}~
~array0.each () {|k v|
                 (display k)
                 (display v)}~


