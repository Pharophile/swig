/* -----------------------------------------------------------------------------
 * pharo.swg
 *
 * Pharo typemaps
 * ----------------------------------------------------------------------------- */

%include <pharohead.swg>

/* Primitive types */
%typemap(ctype) bool,               const bool &               "unsigned int"
%typemap(ctype) char,               const char &               "char"
%typemap(ctype) signed char,        const signed char &        "signed char"
%typemap(ctype) unsigned char,      const unsigned char &      "unsigned char"
%typemap(ctype) short,              const short &              "short"
%typemap(ctype) unsigned short,     const unsigned short &     "unsigned short"
%typemap(ctype) int,                const int &                "int"
%typemap(ctype) unsigned int,       const unsigned int &       "unsigned int"
%typemap(ctype) long,               const long &               "long"
%typemap(ctype) unsigned long,      const unsigned long &      "unsigned long"
%typemap(ctype) long long,          const long long &          "long long"
%typemap(ctype) unsigned long long, const unsigned long long & "unsigned long long"
%typemap(ctype) float,              const float &              "float"
%typemap(ctype) double,             const double &             "double"
%typemap(ctype) void                                           "void"

%typemap(imtype) bool,               const bool &               "bool"
%typemap(imtype) char,               const char &               "char"
%typemap(imtype) signed char,        const signed char &        "char"
%typemap(imtype) unsigned char,      const unsigned char &      "uchar"
%typemap(imtype) short,              const short &              "short"
%typemap(imtype) unsigned short,     const unsigned short &     "ushort"
%typemap(imtype) int,                const int &                "int"
%typemap(imtype) unsigned int,       const unsigned int &       "uint"
%typemap(imtype) long,               const long &               "int"
%typemap(imtype) unsigned long,      const unsigned long &      "uint"
%typemap(imtype) long long,          const long long &          "long"
%typemap(imtype) unsigned long long, const unsigned long long & "ulong"
%typemap(imtype) float,              const float &              "float"
%typemap(imtype) double,             const double &             "double"
%typemap(imtype) void                                           "void"

%typemap(ctype) char *, char *&, char[ANY], char[]   "char *"
%typemap(imtype) char *, char *&, char[ANY], char[]   "String"

/* Non primitive types */
%typemap(ctype) SWIGTYPE "void *"
%typemap(imtype) SWIGTYPE "void*"

%typemap(ctype) SWIGTYPE [] "void *"
%typemap(imtype) SWIGTYPE [] "void*"

%typemap(ctype) SWIGTYPE * "void *"
%typemap(imtype) SWIGTYPE * "void*"

%typemap(ctype) SWIGTYPE & "void *"
%typemap(imtype) SWIGTYPE & "void*"

%typemap(ctype) SWIGTYPE && "void *"
%typemap(imtype) SWIGTYPE && "void*"

/* pointer to a class member */
/*%typemap(ctype) SWIGTYPE (CLASS::*) "char *"
%typemap(imtype) SWIGTYPE (CLASS::*) "char*"
*/

/* The following are the in and out typemaps. These are the PInvoke code generating typemaps for converting from C# to C and visa versa. */

/* primitive types */
%typemap(in) bool
%{ $1 = $input ? true : false; %}

%typemap(in) char, 
             signed char, 
             unsigned char, 
             short, 
             unsigned short, 
             int, 
             unsigned int, 
             long, 
             unsigned long, 
             long long, 
             unsigned long long, 
             float, 
             double
%{ $1 = ($1_ltype)$input; %}

%typemap(out) bool               %{ $result = $1; %}
%typemap(out) char               %{ $result = $1; %}
%typemap(out) signed char        %{ $result = $1; %}
%typemap(out) unsigned char      %{ $result = $1; %}
%typemap(out) short              %{ $result = $1; %}
%typemap(out) unsigned short     %{ $result = $1; %}
%typemap(out) int                %{ $result = $1; %}
%typemap(out) unsigned int       %{ $result = $1; %}
%typemap(out) long               %{ $result = $1; %}
%typemap(out) unsigned long      %{ $result = (unsigned long)$1; %}
%typemap(out) long long          %{ $result = $1; %}
%typemap(out) unsigned long long %{ $result = $1; %}
%typemap(out) float              %{ $result = $1; %}
%typemap(out) double             %{ $result = $1; %}

/* char * - treat as String */
%typemap(in) char * %{ $1 = ($1_ltype)$input; %}
%typemap(out) char * %{ $result = $1; %}

/* char *& - treat as String */
%typemap(in) char *& ($*1_ltype temp = 0) %{ 
  temp = ($*1_ltype)$input;
  $1 = &temp;
%}
%typemap(out) char *& %{ if ($1) $result = *$1; %}

%typemap(out, null="") void ""

/* primitive types by const reference */
%typemap(in) const bool & ($*1_ltype temp)
%{ temp = $input ? true : false; 
   $1 = &temp; %}

%typemap(in) const char & ($*1_ltype temp), 
             const signed char & ($*1_ltype temp), 
             const unsigned char & ($*1_ltype temp), 
             const short & ($*1_ltype temp), 
             const unsigned short & ($*1_ltype temp), 
             const int & ($*1_ltype temp), 
             const unsigned int & ($*1_ltype temp), 
             const long & ($*1_ltype temp), 
             const unsigned long & ($*1_ltype temp), 
             const long long & ($*1_ltype temp), 
             const unsigned long long & ($*1_ltype temp), 
             const float & ($*1_ltype temp), 
             const double & ($*1_ltype temp)
%{ temp = ($*1_ltype)$input; 
   $1 = &temp; %}

%typemap(out) const bool &               %{ $result = *$1; %}
%typemap(out) const char &               %{ $result = *$1; %}
%typemap(out) const signed char &        %{ $result = *$1; %}
%typemap(out) const unsigned char &      %{ $result = *$1; %}
%typemap(out) const short &              %{ $result = *$1; %}
%typemap(out) const unsigned short &     %{ $result = *$1; %}
%typemap(out) const int &                %{ $result = *$1; %}
%typemap(out) const unsigned int &       %{ $result = *$1; %}
%typemap(out) const long &               %{ $result = *$1; %}
%typemap(out) const unsigned long &      %{ $result = (unsigned long)*$1; %}
%typemap(out) const long long &          %{ $result = *$1; %}
%typemap(out) const unsigned long long & %{ $result = *$1; %}
%typemap(out) const float &              %{ $result = *$1; %}
%typemap(out) const double &             %{ $result = *$1; %}

/* Default handling. Object passed by value. Convert to a pointer */
%typemap(in, canthrow=1) SWIGTYPE ($&1_type argp)
%{ argp = ($&1_ltype)$input; 
   if (!argp) {
     SWIG_PharoSetPendingExceptionArgument(SWIG_PharoArgumentNullException, "Attempt to dereference null $1_type", 0);
     return $null;
   }
   $1 = *argp; %}

%typemap(out) SWIGTYPE 
#ifdef __cplusplus
%{ $result = new $1_ltype((const $1_ltype &)$1); %}
#else
{
  $&1_ltype $1ptr = ($&1_ltype) malloc(sizeof($1_ltype));
  memmove($1ptr, &$1, sizeof($1_type));
  $result = $1ptr;
}
#endif

/* Generic pointers and references */
%typemap(in) SWIGTYPE * %{ $1 = ($1_ltype)$input; %}
%typemap(in, fragment="SWIG_UnPackData") SWIGTYPE (CLASS::*) %{ 
  SWIG_UnpackData($input, (void *)&$1, sizeof($1));
%}
%typemap(in, canthrow=1) SWIGTYPE & %{ $1 = ($1_ltype)$input;
  if (!$1) {
    SWIG_PharoSetPendingExceptionArgument(SWIG_CSharpArgumentNullException, "$1_type type is null", 0);
    return $null;
  } %}
%typemap(in, canthrow=1) SWIGTYPE && %{ $1 = ($1_ltype)$input;
  if (!$1) {
    SWIG_PharoSetPendingExceptionArgument(SWIG_CSharpArgumentNullException, "$1_type type is null", 0);
    return $null;
  } %}
%typemap(out) SWIGTYPE * %{ $result = (void *)$1; %} 
%typemap(out, fragment="SWIG_PackData") SWIGTYPE (CLASS::*) %{
  char buf[128];
  char *data = SWIG_PackData(buf, (void *)&$1, sizeof($1));
  *data = '\0';
  $result = SWIG_pharo_string_callback(buf);
%}
%typemap(out) SWIGTYPE & %{ $result = (void *)$1; %} 
%typemap(out) SWIGTYPE && %{ $result = (void *)$1; %} 

/* Default array handling */
%typemap(in) SWIGTYPE [] %{ $1 = ($1_ltype)$input; %}
%typemap(out) SWIGTYPE [] %{ $result = $1; %} 

/* char arrays - treat as String */
%typemap(in) char[ANY], char[] %{ $1 = ($1_ltype)$input; %}
%typemap(out) char[ANY], char[] %{ $result = SWIG_csharp_string_callback((const char *)$1); %}

/* Typecheck typemaps - The purpose of these is merely to issue a warning for overloaded C++ functions 
 * that cannot be overloaded in C# as more than one C++ type maps to a single C# type */

%typecheck(SWIG_TYPECHECK_BOOL)
    bool,
    const bool &
    ""

%typecheck(SWIG_TYPECHECK_CHAR)
    char, 
    const char &
    ""

%typecheck(SWIG_TYPECHECK_INT8)
    signed char,
    const signed char &
    ""

%typecheck(SWIG_TYPECHECK_UINT8)
    unsigned char, 
    const unsigned char & 
    ""

%typecheck(SWIG_TYPECHECK_INT16)
    short, 
    const short &
    ""

%typecheck(SWIG_TYPECHECK_UINT16)
    unsigned short, 
    const unsigned short &
    ""

%typecheck(SWIG_TYPECHECK_INT32)
    int, 
    long, 
    const int &, 
    const long &
    ""

%typecheck(SWIG_TYPECHECK_UINT32)
    unsigned int, 
    unsigned long, 
    const unsigned int &, 
    const unsigned long &
    ""

%typecheck(SWIG_TYPECHECK_INT64)
    long long, 
    const long long &
    ""

%typecheck(SWIG_TYPECHECK_UINT64)
    unsigned long long,
    const unsigned long long &
    ""

%typecheck(SWIG_TYPECHECK_FLOAT)
    float,
    const float &
    ""

%typecheck(SWIG_TYPECHECK_DOUBLE)
    double,
    const double &
    ""

%typecheck(SWIG_TYPECHECK_STRING)
    char *,
    char *&,
    char[ANY],
    char[]
    ""

%typecheck(SWIG_TYPECHECK_POINTER)
    SWIGTYPE, 
    SWIGTYPE *, 
    SWIGTYPE &, 
    SWIGTYPE &&, 
    SWIGTYPE *const&, 
    SWIGTYPE [],
    SWIGTYPE (CLASS::*)
    ""

/* Typemaps for code generation in proxy classes and Pharo type wrapper classes */

/* The nbin typemap is used for converting function parameter types from the type 
 * used in the proxy, module or type wrapper class to the type used in the im class. */
%typemap(nbin)   bool,               const bool &,
                 char,               const char &,
                 signed char,        const signed char &,
                 unsigned char,      const unsigned char &,
                 short,              const short &,
                 unsigned short,     const unsigned short &,
                 int,                const int &,
                 unsigned int,       const unsigned int &,
                 long,               const long &,
                 unsigned long,      const unsigned long &,
                 long long,          const long long &,
                 unsigned long long, const unsigned long long &,
                 float,              const float &,
                 double,             const double &
    "$nbinput"
%typemap(nbin) char *, char *&, char[ANY], char[] "$nbinput"
%typemap(nbin) SWIGTYPE "($nbinput swigValidCPtr)"
%typemap(nbin) SWIGTYPE *, SWIGTYPE &, SWIGTYPE &&, SWIGTYPE [] "($nbinput swigValidCPtr)"
%typemap(nbin) SWIGTYPE (CLASS::*) "($nbinput swigValidCPtr)"

/* The nbout typemap is used for converting function return types from the return type
 * used in the PInvoke class to the type returned by the proxy, module or type wrapper class.
 * The $excode special variable is replaced by the excode typemap attribute code if the
 * method can throw any exceptions from unmanaged code, otherwise replaced by nothing. */

// Macro used by the $excode special variable
%define SWIGEXCODE "\n    $imclassname throwPendingException." %enddef
%define SWIGEXCODE2 "\n      $imclassname throwPendingException." %enddef

%typemap(nbout, excode=SWIGEXCODE)
    bool, const bool &,
    char, const char &,
    signed char, const signed char &,
    unsigned char, const unsigned char &,
    short, const short &,
    unsigned short, const unsigned short &,
    int, const int &,
    unsigned int, const unsigned int &,
    long, const long &,
    unsigned long, const unsigned long &,
    long long, const long long &,
    unsigned long long, const unsigned long long &,
    float, const float &,
    double, const double & %{
    | ret |
    ret := $imcall.$excode
    ^ ret.
  %}

%typemap(nbout, excode=SWIGEXCODE) void %{
    $imcall.$excode
  %}

%typemap(nbout, excode=SWIGEXCODE) SWIGTYPE %{
    | ret |
    ret := $&nbclassname fromCPtr: ($imcall) owner: true.$excode
    ^ ret
  %}

%typemap(nbout, excode=SWIGEXCODE) SWIGTYPE & %{
    | ret |
    ret := $&nbclassname fromCPtr: ($imcall) owner: $owner.$excode
    ^ ret
  %}

%typemap(nbout, excode=SWIGEXCODE) SWIGTYPE && %{
    | ret |
    ret := $&nbclassname fromCPtr: ($imcall) owner: $owner.$excode
    ^ ret
  %}

%typemap(nbout, excode=SWIGEXCODE) SWIGTYPE *, SWIGTYPE [] %{
    | ret |
    ret := $&nbclassname fromCPtr: ($imcall) owner: $owner.$excode
    ^ ret
  %}

%typemap(nbout, excode=SWIGEXCODE) SWIGTYPE (CLASS::*) %{
    self error: 'Not implemented member method wrapping'
  %}

/* Typemaps used for the generation of proxy and type wrapper class code */
%typemap(nbbase)        SWIGTYPE, SWIGTYPE *, SWIGTYPE &, SWIGTYPE &&, SWIGTYPE [], SWIGTYPE (CLASS::*) ""
%typemap(nbinstvars)    SWIGTYPE, SWIGTYPE *, SWIGTYPE &, SWIGTYPE &&, SWIGTYPE [], SWIGTYPE (CLASS::*) ""
%typemap(nbclassvars)   SWIGTYPE, SWIGTYPE *, SWIGTYPE &, SWIGTYPE &&, SWIGTYPE [], SWIGTYPE (CLASS::*) ""
%typemap(nbpools)        SWIGTYPE, SWIGTYPE *, SWIGTYPE &, SWIGTYPE &&, SWIGTYPE [], SWIGTYPE (CLASS::*) ""
%typemap(nbcategory)        SWIGTYPE, SWIGTYPE *, SWIGTYPE &, SWIGTYPE &&, SWIGTYPE [], SWIGTYPE (CLASS::*) ""

%typemap(nbfinalize, selector="finalize") SWIGTYPE
%{finalize
    self destroy
%}

%typemap(nbdestroy, selector="destroy") SWIGTYPE 
%{destroy
    swigCPtr ~~ nil ifTrue: [
        swigCMemOwn and: [ swigSession == Smalltalk session] ifTrue: [
            swigCMemOwn := false.
            $imcall
        ].
        swigCPtr := nil
    ]
%}

/* Object construction */
%typemap(nbconstruct) SWIGTYPE 
%{
    ^ self fromCPtr: ($imcall)
%}

%typemap(nbfromcptr_owner, selector="fromCPtr:owner") SWIGTYPE 
%{fromCPtr: swigCPtr owner: swigOwner
    ^ self basicNew initWithCPtr: swigCPtr owner: swigOwner; yourself
%}

%typemap(nbfromcptr, selector="fromCPtr:") SWIGTYPE 
%{fromCPtr: swigCPtr
    ^ self fromCPtr: swigCPtr owner: true
%}

%typemap(nbinitwith_owner, selector="swigInitWithCPtr:owner:") SWIGTYPE 
%{initWithCPtr: newSwigCPtr owner: newSwigCMemOwn
    swigCPtr := newSwigCPtr.
    swigCMemOwn := newSwigCMemOwn.
    swigSession := Smalltalk session
%}

%typemap(nbvalidcptr, selector="swigValidCPtr") SWIGTYPE 
%{swigValidCPtr
    (swigSession ~~ Smalltalk session or: [swigCPtr == nil])
        ifTrue: [ self error: 'trying to use an invalid swig wrapper.'].
    ^ swigCPtr
%}

/* Some ANSI C typemaps */
%apply unsigned long { size_t };
%apply const unsigned long & { const size_t & };

/* enum handling */
%include "enums.swg"

