// const enum SWIGTYPE & typemaps
%typemap(ctype) const enum SWIGTYPE & "int"
%typemap(imtype) const enum SWIGTYPE & "int"

%typemap(in) const enum SWIGTYPE & ($*1_ltype temp)
%{ temp = ($*1_ltype)$input; 
   $1 = &temp; %}
%typemap(out) const enum SWIGTYPE & %{ $result = (int)*$1; %}

%typemap(nbin) const enum SWIGTYPE & "$nbinput"
%typemap(nbout, excode=SWIGEXCODE) const enum SWIGTYPE & %{
    | ret |
    ret := $imcall.$excode
    ^ ret.
  %}

// enum SWIGTYPE typemaps
%typemap(ctype) enum SWIGTYPE "int"
%typemap(imtype) enum SWIGTYPE "int"

%typemap(in) enum SWIGTYPE %{ $1 = ($1_ltype)$input; %}
%typemap(out) enum SWIGTYPE %{ $result = (int)$1; %}

%typemap(nbin) enum SWIGTYPE "$nbinput"
%typemap(nbout, excode=SWIGEXCODE) enum SWIGTYPE %{
    | ret |
    ret := $imcall.$excode
    ^ ret.
  %}

