#lang reader "lusa.rkt"

(+ 1 2)

~foo = {}~

~foo.bar = {~self.a = 1~
            ~self.1 = 2~}~

~foo.bar.1~
~foo.bar = {|a cb| (+ a 3) (cb self)}~
~foo.bar (1) {|| 3}~
~foo.c = {}~
~foo.c.a=1~


~foo.b = {|a| (+ a 4)}~
~foo.b (~foo.c.a~)~




