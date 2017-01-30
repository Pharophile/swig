" Pharo script for testing simple example "

" Load module "

" Call our gcd() function"

| g |
x := 42
y := 105
g := example gcd:x y:y
Transcript << ('The gcd of {1] and {2} is {3}') format: { x. y. g }); cr.

" Manipulate the Foo global variable "

" Output its current value "
Transcript << ('Foo = {1}' format: {Example Foo}); cr.

"  Change its value "
Example Foo: 3.1415926

" See if the change took effect "
Transcript << ('Foo = {1}' format: {Example Foo}); cr.

