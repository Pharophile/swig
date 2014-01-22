/* -----------------------------------------------------------------------------
 * Pharohead.swg
 *
 * Support code for exceptions if the SWIG_PHARO_NO_EXCEPTION_HELPER is not defined
 * Support code for strings if the SWIG_PHARO_NO_STRING_HELPER is not defined
 * ----------------------------------------------------------------------------- */

%insert(runtime) %{
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
%}

#if !defined(SWIG_PHARO_NO_EXCEPTION_HELPER)
%insert(runtime) %{
/* Support for throwing Pharo errors from C/C++. There are two types: 
 * Exceptions that take a message and ArgumentExceptions that take a message and a parameter name. */
typedef enum {
  SWIG_PharoApplicationException,
  SWIG_PharoArithmeticException,
  SWIG_PharoDivideByZeroException,
  SWIG_PharoIndexOutOfRangeException,
  SWIG_PharoInvalidCastException,
  SWIG_PharoInvalidOperationException,
  SWIG_PharoIOException,
  SWIG_PharoNullReferenceException,
  SWIG_PharoOutOfMemoryException,
  SWIG_PharoOverflowException,
  SWIG_PharoSystemException
} SWIG_PharoExceptionCodes;

typedef enum {
  SWIG_PharoArgumentException,
  SWIG_PharoArgumentNullException,
  SWIG_PharoArgumentOutOfRangeException
} SWIG_PharoExceptionArgumentCodes;

static SWIG_PharoExceptionCodes SWIG_PharoPendingExceptionCode;
static const char* SWIG_PharoPendingExceptionMsg;

static void SWIGUNUSED SWIG_PharoSetPendingException(SWIG_PharoExceptionCodes code, const char *msg) {
    SWIG_PharoPendingExceptionCode = code;
    SWIG_PharoPendingExceptionMsg = msg;
}

static void SWIGUNUSED SWIG_PharoSetPendingExceptionArgument(SWIG_PharoExceptionArgumentCodes code, const char *msg, const char *param_name) {
    // TODO:
}

#ifdef __cplusplus
extern "C" {
#endif

SWIGEXPORT SWIG_PharoExceptionCodes SWIGSTDCALL SWIG_PharoGetPendingExceptionCode() {
  return SWIG_PharoPendingExceptionCode;
}

SWIGEXPORT const char *SWIGSTDCALL SWIG_PharoGetPendingExceptionMessage() {
  return SWIG_PharoPendingExceptionMsg;
}

char *SWIG_csharp_string_callback(const char *s)
{
    return (char*)s;
}

#ifdef __cplusplus
}
#endif

%}
#endif // SWIG_PHARO_NO_EXCEPTION_HELPER

