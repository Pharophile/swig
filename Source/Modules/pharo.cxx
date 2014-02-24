/* -----------------------------------------------------------------------------
 * This file is part of SWIG, which is licensed as a whole under version 3 
 * (or any later version) of the GNU General Public License. Some additional
 * terms also apply to certain portions of SWIG. The full details of the SWIG
 * license and copyrights can be found in the LICENSE and COPYRIGHT files
 * included with the SWIG source code as distributed by the SWIG developers
 * and at http://www.swig.org/legal.html.
 *
 * pharo.cxx
 *
 * Pharo language module for SWIG.
 * ----------------------------------------------------------------------------- */
#include <ctype.h>
#include "swigmod.h"

static void show_usage() {
  fprintf(stderr,
"Pharo Options (available with -pharo)\n\
     -fileout <fo>   - Pharo code file out\n\
     -impname <in>   - Library to import symbols from\n\
");
}

/**
 * Pharo NativeBoost binding generator.
 */
class PHARO : public Language {
  const String *empty_string;
  const String *object_string;
  const String *swig_wrapper_category;
  const String *nativeboost_primitive;

  Hash *swig_types_hash;
  File *f_begin;
  File *f_runtime;
  File *f_header;
  File *f_wrappers;
  File *f_init;

  File *f_fileout_runtime;
  File *f_fileout;
  File *f_class_declarations;
  File *f_class_methods;

  bool proxy_flag;
  bool native_function_flag;  // Flag for when wrapping a native function
  bool enum_constant_flag;        // Flag for when wrapping an enum or constant
  bool static_flag;                // Flag for when wrapping a static functions or member variables
  bool variable_wrapper_flag;        // Flag for when wrapping a nonstatic member variable
  bool wrapping_member_flag;        // Flag for when wrapping a member variable/enum/const
  bool global_variable_flag;        // Flag for when wrapping a global variable
  bool old_variable_names;        // Flag for old style variable names in the intermediary class
  bool generate_property_declaration_flag;        // Flag for generating properties

  String *fileout;
  String *category;
  String *libname;
  String *imclass_name;    // intermediary class name
  String *module_class_name;  // module class name
  String *module_class_constants;
  String *module_class_constants_code;
  String *reference_class_name;
  String *wrapped_object_class_name;
  String *variable_name;
  String *proxy_class_name;
  String *proxy_class_constants;
  String *proxy_class_constants_code;
  String *proxy_class_instance_variables;
  String *proxy_class_variables;
  String *proxy_class_pool_dictionaries;
  String *destructor_call;

public:
  PHARO()
    : empty_string(NewString("")),
      object_string(NewString("Object")),
      swig_wrapper_category(NewString("swig wrapper")),
      nativeboost_primitive(NewString("<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>")),
      swig_types_hash(NULL),
      f_begin(NULL),
      f_runtime(NULL),
      f_header(NULL),
      f_wrappers(NULL),
      f_init(NULL),
      f_fileout_runtime(NULL),
      f_fileout(NULL),
      f_class_declarations(NULL),
      f_class_methods(NULL),
      proxy_flag(true),
      native_function_flag(false),
      enum_constant_flag(false),
      static_flag(false),
      variable_wrapper_flag(false),
      wrapping_member_flag(false),
      global_variable_flag(false),
      old_variable_names(false),
      generate_property_declaration_flag(false),
      fileout(NULL),
      category(NULL),
      libname(NULL),
      imclass_name(NULL),
      module_class_name(NULL),
      module_class_constants(NULL),
      module_class_constants_code(NULL),
      reference_class_name(NULL),
      wrapped_object_class_name(NULL),
      variable_name(NULL),
      proxy_class_name(NULL),
      proxy_class_constants(NULL),
      proxy_class_constants_code(NULL),
      proxy_class_instance_variables(NULL),
      proxy_class_variables(NULL),
      proxy_class_pool_dictionaries(NULL),
      destructor_call(NULL) {
  }

  virtual void main(int argc, char *argv[]) {
    SWIG_library_directory("pharo");

    for (int i = 1; i < argc; i++) {
      if (argv[i]) {
        if(strcmp(argv[i], "-fileout") == 0) {
          if (argv[i + 1]) {
            fileout = NewString("");
            Printf(fileout, argv[i + 1]);
            Swig_mark_arg(i);
            Swig_mark_arg(i + 1);
            i++;
          } else {
            Swig_arg_error();
          }
        } else if(strcmp(argv[i], "-category") == 0) {
          if (argv[i + 1]) {
            category = NewString("");
            Printf(category, argv[i + 1]);
            Swig_mark_arg(i);
            Swig_mark_arg(i + 1);
            i++;
          } else {
            Swig_arg_error();
          }
        } else if(strcmp(argv[i], "-impname") == 0) {
          if (argv[i + 1]) {
            libname = NewString("");
            Printf(libname, argv[i + 1]);
            Swig_mark_arg(i);
            Swig_mark_arg(i + 1);
            i++;
          } else {
            Swig_arg_error();
          }
        }else if(strcmp(argv[i], "-help") == 0) {
          show_usage();
        }
      }
    }

    /* Set language-specific preprocessing symbol */
    Preprocessor_define("SWIGPHARO 1", 0);

    /* Set language-specific configuration file */
    SWIG_config_file("pharo.swg");

    /* Set typemap language (historical) */
    SWIG_typemap_lang("pharo");

    /* Allow overloading */
    allow_overloading();
  }

  virtual int top(Node *n) {
    // Get any options set in the module directive
    Node *optionsnode = Getattr(Getattr(n, "module"), "options");

    if (optionsnode) {
      if (Getattr(optionsnode, "imclassname"))
        imclass_name = Copy(Getattr(optionsnode, "imclassname"));
    }

    // Get the output file name
    String *outfile = Getattr(n,"outfile");

    // Initialize I/O
    f_begin = NewFile(outfile, "w", SWIG_output_files());
    if (!f_begin) {
       FileErrorDisplay(outfile);
       SWIG_exit(EXIT_FAILURE);
    }
    f_runtime = NewString("");
    f_init = NewString("");
    f_header = NewString("");
    f_wrappers = NewString("");
    f_fileout_runtime = NewString("");

    // Make sure there's a fileout name
    if(!fileout)
      fileout = NewStringf("%s%s.st", SWIG_output_directory(), Getattr(n, "name"));

    // Smalltalk file out.
    f_fileout = NewFile(fileout, "w", SWIG_output_files());
    if (!f_fileout) {
       FileErrorDisplay(fileout);
       SWIG_exit(EXIT_FAILURE);
    }
    f_class_declarations = NewString("");
    f_class_methods = NewString("");

    // Register file targets with the SWIG file handler
    Swig_register_filebyname("begin", f_begin);
    Swig_register_filebyname("header", f_header);
    Swig_register_filebyname("wrapper", f_wrappers);
    Swig_register_filebyname("runtime", f_runtime);
    Swig_register_filebyname("fileout_runtime", f_fileout_runtime);
    Swig_register_filebyname("init", f_init);

    swig_types_hash = NewHash();

    // Make the intermediary class and module class names. The intermediary class name can be set in the module directive.
    const String *module_name = Getattr(n, "name");
    if (!imclass_name) {
      imclass_name = NewStringf("%sCInterface", module_name);
      module_class_name = Copy(module_name);
    } else {
      // Rename the module name if it is the same as intermediary class name - a backwards compatibility solution
      if (Cmp(imclass_name, module_name) == 0)
        module_class_name = NewStringf("%sModule", module_name);
      else
        module_class_name = Copy(module_name);
    }

    // Reference class name and wrapped object class name.
    reference_class_name = NewStringf("%sSwigRef", module_name);
    wrapped_object_class_name = NewStringf("%sSwigObject", module_name);

    // module class and intermediary classes are always created
    if (!addSymbol(imclass_name, n))
      return SWIG_ERROR;
    if (!addSymbol(module_class_name, n))
      return SWIG_ERROR;
    if (!addSymbol(reference_class_name, n))
      return SWIG_ERROR;
    if (!addSymbol(wrapped_object_class_name, n))
      return SWIG_ERROR;

    // Create strings for the module.
    module_class_constants_code = NewString("");
    module_class_constants = NewString("");

    // Ensure there's a category
    if(!category)
      category = Copy(module_name);

    // Ensure there's a library name
    if(!libname)
      libname = Copy(module_name);

    // Declare the intermediate class
    subclass(object_string, imclass_name,
             /*instanceVariables:*/ empty_string,
             /*classVariables:*/ empty_string,
             /*poolDictionary:*/ empty_string, category);

    // Emit intermediate class library import
    addLibraryImportToClass(imclass_name, libname);

    // Output module initialization code
    Swig_banner(f_begin);

    Swig_name_register("wrapper", "PharoNB_%f");

    Printf(f_wrappers, "\n#ifdef __cplusplus\n");
    Printf(f_wrappers, "extern \"C\" {\n");
    Printf(f_wrappers, "#endif\n\n");

    // Emit code for children
    Language::top(n);

    Printf(f_wrappers, "#ifdef __cplusplus\n");
    Printf(f_wrappers, "}\n");
    Printf(f_wrappers, "#endif\n");

    // Finish the module class
    subclass(object_string, module_class_name,
             /*instanceVariables:*/ empty_string,
             /*classVariables:*/ module_class_constants,
             /*poolDictionary:*/ empty_string, category);
    emitModuleInitialize();

    // Output a Smalltalk type wrapper class for each SWIG type
    for (Iterator swig_type = First(swig_types_hash); swig_type.key; swig_type = Next(swig_type)) {
      emitTypeWrapperClass(swig_type.key, swig_type.item);
    }

    // Replace the module name, reference class name, wrapped object name in
    // the common runtime
    Replaceall(f_fileout_runtime, "$module", module_name);
    Replaceall(f_fileout_runtime, "$refclass", reference_class_name);
    Replaceall(f_fileout_runtime, "$objclass", wrapped_object_class_name);
    Replaceall(f_fileout_runtime, "$category", category);
    
    // Convert newlines Smalltalk code into in CR.
    convertNewlines(f_fileout_runtime);
    convertNewlines(f_class_declarations);
    convertNewlines(f_class_methods);

    // Write Smalltalk code to th file.
    Dump(f_fileout_runtime, f_fileout);
    Dump(f_class_declarations, f_fileout);
    Dump(f_class_methods, f_fileout);

    // Write all to the file
    Dump(f_runtime, f_begin);
    Dump(f_header, f_begin);
    Dump(f_wrappers, f_begin);
    Wrapper_pretty_print(f_init, f_begin);

    Delete(swig_types_hash);
    swig_types_hash = NULL;

    // Cleanup strings
    Delete(fileout);
    Delete(libname);
    Delete(category);
    Delete(imclass_name);
    Delete(module_class_name);
    Delete(module_class_constants_code);
    Delete(module_class_constants);

    // Cleanup files
    Delete(f_runtime);
    Delete(f_header);
    Delete(f_wrappers);
    Delete(f_init);
    Delete(f_begin);

    Delete(f_class_declarations);
    Delete(f_class_methods);
    Delete(f_fileout_runtime);
    Delete(f_fileout);

    return SWIG_OK;
  }

  void convertNewlines(String *str) {
    Replaceall(str, "\r\n", "\r"); // Windows
    Replaceall(str, "\n", "\r"); // Unix
  }

  void emitModuleInitialize() {
    const char *header =
"initialize\n\
\"\n\
\tself initialize\n\
\"\n\
";

    String *initialize_code = NewString("");
    Printv(initialize_code, header, module_class_constants_code, NIL);
    methodForClass(module_class_name, swig_wrapper_category, initialize_code);
    Delete(initialize_code);
  }

  virtual int nativeWrapper(Node *n) {
    String *wrapname = Getattr(n, "wrap:name");

    if (!addSymbol(wrapname, n, imclass_name))
      return SWIG_ERROR;

    if (Getattr(n, "type")) {
      Swig_save("nativeWrapper", n, "name", NIL);
      Setattr(n, "name", wrapname);
      native_function_flag = true;
      functionWrapper(n);
      Swig_restore(n);
      native_function_flag = false;
    } else {
      Swig_error(input_file, line_number, "No return type for %%native method %s.\n", Getattr(n, "wrap:name"));
    }

    return SWIG_OK;
  }

  virtual int functionWrapper(Node *n) {
    String *symname = Getattr(n, "sym:name");
    SwigType *type = Getattr(n, "type");
    ParmList *params = Getattr(n, "parms");
    String *typeMap;
    Parm *param;
    int i;
    String *c_return_type = NewString("");
    String *im_body = NewString("");
    String *im_callout = NewString("");
    String *im_selector = NewString("");
    String *im_return_type = NewString("");
    String *cleanup = NewString("");
    String *outarg = NewString("");
    String *body = NewString("");
    int num_arguments = 0;
    bool is_void_return;
    String *overloaded_name = getOverloadedName(n);

    if (!Getattr(n, "sym:overloaded")) {
      if (!addSymbol(Getattr(n, "sym:name"), n, imclass_name))
        return SWIG_ERROR;
    }

    /*
       The rest of this function deals with generating the intermediary class wrapper function (that wraps
       a c/c++ function) and generating the PInvoke c code. Each C# wrapper function has a 
       matching PInvoke c function call.
     */

    // A new wrapper function object
    Wrapper *wrapper = NewWrapper();

    // Make a wrapper name for this function
    String *wname = Swig_name_wrapper(overloaded_name);

    /* Attach the non-standard typemaps to the parameter list. */
    Swig_typemap_attach_parms("ctype", params, wrapper);
    Swig_typemap_attach_parms("imtype", params, wrapper);

    /* Get return types */
    if ((typeMap = Swig_typemap_lookup("ctype", n, "", 0))) {
      String *ctypeout = Getattr(n, "tmap:ctype:out");  // the type in the ctype typemap's out attribute overrides the type in the typemap
      if (ctypeout)
        typeMap = ctypeout;
      Printf(c_return_type, "%s", typeMap);
    } else {
      Swig_warning(WARN_PHARO_TYPEMAP_CTYPE_UNDEF, input_file, line_number, "No ctype typemap defined for %s\n", SwigType_str(type, 0));
    }

    if ((typeMap = Swig_typemap_lookup("imtype", n, "", 0))) {
      String *imtypeout = Getattr(n, "tmap:imtype:out");  // the type in the imtype typemap's out attribute overrides the type in the typemap
      if (imtypeout)
        typeMap = imtypeout;
      Printf(im_return_type, "%s", typeMap);
    } else {
      Swig_warning(WARN_PHARO_TYPEMAP_NBTYPE_UNDEF, input_file, line_number, "No imtype typemap defined for %s\n", SwigType_str(type, 0));
    }

    is_void_return = (Cmp(c_return_type, "void") == 0);
    if (!is_void_return)
      Wrapper_add_localv(wrapper, "nbresult", c_return_type, "nbresult", NIL);

    Printv(wrapper->def, " SWIGEXPORT ", c_return_type, " ", wname, "(", NIL);

    // Emit all of the local variables for holding arguments.
    emit_parameter_variables(params, wrapper);

    /* Attach the standard typemaps */
    emit_attach_parmmaps(params, wrapper);

    // Parameter overloading
    Setattr(n, "wrap:parms", params);
    Setattr(n, "wrap:name", wname);

    // Wrappers not wanted for some methods where the parameters cannot be overloaded in C#
    if (Getattr(n, "sym:overloaded")) {
      // Emit warnings for the few cases that can't be overloaded in C# and give up on generating wrapper
      Swig_overload_check(n);
      if (Getattr(n, "overload:ignore")) {
        DelWrapper(wrapper);
        return SWIG_OK;
      }
    }

    /* Get number of required and total arguments */
    num_arguments = emit_num_arguments(params);
    int gencomma = 0;

    // Start building the selector.
    Printv(im_selector, overloaded_name, (num_arguments > 0) ? "_" : "", NIL);

    // Start making the NB callout.
    Printf(im_callout, "\t^ self nbCall: #( %s %s ( ", im_return_type, wname);

    // Now walk the function parameter list and generate code to get arguments
    for (i = 0, param = params; i < num_arguments; i++) {

      while (checkAttribute(param, "tmap:in:numinputs", "0")) {
        param = Getattr(param, "tmap:in:next");
      }

      SwigType *param_type = Getattr(param, "type");
      String *local_name = Getattr(param, "lname");
      String *im_param_type = NewString("");
      String *c_param_type = NewString("");
      String *arg = NewString("");

      Printf(arg, "nb%s", local_name);

      /* Get the ctype types of the parameter */
      if ((typeMap = Getattr(param, "tmap:ctype"))) {
        Printv(c_param_type, typeMap, NIL);
      } else {
        Swig_warning(WARN_PHARO_TYPEMAP_CTYPE_UNDEF, input_file, line_number, "No ctype typemap defined for %s\n", SwigType_str(param_type, 0));
      }

      /* Get the intermediary class parameter types of the parameter */
      if ((typeMap = Getattr(param, "tmap:imtype"))) {
        const String *inattributes = Getattr(param, "tmap:imtype:inattributes");
        Printf(im_param_type, "%s%s", inattributes ? inattributes : empty_string, typeMap);
      } else {
        Swig_warning(WARN_PHARO_TYPEMAP_NBTYPE_UNDEF, input_file, line_number, "No imtype typemap defined for %s\n", SwigType_str(param_type, 0));
      }

      /* Add parameter to intermediary class method */
      if (gencomma)
        Printf(im_selector, " ");
      Printf(im_selector, "%s: %s", arg, arg);

      /* Add parameter to the call out */
      if (gencomma)
        Printf(im_callout, ", ");
      Printf(im_callout, "%s %s", im_param_type, arg);

      // Add parameter to C function
      Printv(wrapper->def, gencomma ? ", " : "", c_param_type, " ", arg, NIL);

      gencomma = 1;

      // Get typemap for this argument
      if ((typeMap = Getattr(param, "tmap:in"))) {
        canThrow(n, "in", param);
        Replaceall(typeMap, "$source", arg);  /* deprecated */
        Replaceall(typeMap, "$target", local_name);  /* deprecated */
        Replaceall(typeMap, "$arg", arg);  /* deprecated? */
        Replaceall(typeMap, "$input", arg);
        Setattr(param, "emit:input", arg);
        Printf(wrapper->code, "%s\n", typeMap);
        param = Getattr(param, "tmap:in:next");
      } else {
        Swig_warning(WARN_TYPEMAP_IN_UNDEF, input_file, line_number, "Unable to use type %s as a function argument.\n", SwigType_str(param_type, 0));
        param = nextSibling(param);
      }
      Delete(im_param_type);
      Delete(c_param_type);
      Delete(arg);
    }

    /* Insert constraint checking code */
    for (param = params; param;) {
      if ((typeMap = Getattr(param, "tmap:check"))) {
        canThrow(n, "check", param);
        Replaceall(typeMap, "$target", Getattr(param, "lname"));        /* deprecated */
        Replaceall(typeMap, "$arg", Getattr(param, "emit:input"));        /* deprecated? */
        Replaceall(typeMap, "$input", Getattr(param, "emit:input"));
        Printv(wrapper->code, typeMap, "\n", NIL);
        param = Getattr(param, "tmap:check:next");
      } else {
        param = nextSibling(param);
      }
    }

    /* Insert cleanup code */
    for (param = params; param;) {
      if ((typeMap = Getattr(param, "tmap:freearg"))) {
        canThrow(n, "freearg", param);
        Replaceall(typeMap, "$source", Getattr(param, "emit:input"));        /* deprecated */
        Replaceall(typeMap, "$arg", Getattr(param, "emit:input"));        /* deprecated? */
        Replaceall(typeMap, "$input", Getattr(param, "emit:input"));
        Printv(cleanup, typeMap, "\n", NIL);
        param = Getattr(param, "tmap:freearg:next");
      } else {
        param = nextSibling(param);
      }
    }

    /* Insert argument output code */
    for (param = params; param;) {
      if ((typeMap = Getattr(param, "tmap:argout"))) {
        canThrow(n, "argout", param);
        Replaceall(typeMap, "$source", Getattr(param, "emit:input"));        /* deprecated */
        Replaceall(typeMap, "$target", Getattr(param, "lname"));        /* deprecated */
        Replaceall(typeMap, "$arg", Getattr(param, "emit:input"));        /* deprecated? */
        Replaceall(typeMap, "$result", "jresult");
        Replaceall(typeMap, "$input", Getattr(param, "emit:input"));
        Printv(outarg, typeMap, "\n", NIL);
        param = Getattr(param, "tmap:argout:next");
      } else {
        param = nextSibling(param);
      }
    }

    // Look for usage of throws typemap and the canthrow flag
    ParmList *throw_parm_list = NULL;
    if ((throw_parm_list = Getattr(n, "catchlist"))) {
      Swig_typemap_attach_parms("throws", throw_parm_list, wrapper);
      for (param = throw_parm_list; param; param = nextSibling(param)) {
        if (Getattr(param, "tmap:throws")) {
          canThrow(n, "throws", param);
        }
      }
    }

    String *null_attribute = 0;
    // Now write code to make the function call
    if (!native_function_flag) {
      if (Cmp(nodeType(n), "constant") == 0) {
        // Wrapping a constant hack
        Swig_save("functionWrapper", n, "wrap:action", NIL);

        // below based on Swig_VargetToFunction()
        SwigType *ty = Swig_wrapped_var_type(Getattr(n, "type"), use_naturalvar_mode(n));
        Setattr(n, "wrap:action", NewStringf("%s = (%s)(%s);", Swig_cresult_name(), SwigType_lstr(ty, 0), Getattr(n, "value")));
      }

      Swig_director_emit_dynamic_cast(n, wrapper);
      String *actioncode = emit_action(n);

      if (Cmp(nodeType(n), "constant") == 0)
        Swig_restore(n);

      /* Return value if necessary  */
      if ((typeMap = Swig_typemap_lookup_out("out", n, Swig_cresult_name(), wrapper, actioncode))) {
        canThrow(n, "out", n);
        Replaceall(typeMap, "$source", Swig_cresult_name()); /* deprecated */
        Replaceall(typeMap, "$target", "nbresult");        /* deprecated */
        Replaceall(typeMap, "$result", "nbresult");

        if (GetFlag(n, "feature:new"))
          Replaceall(typeMap, "$owner", "1");
        else
          Replaceall(typeMap, "$owner", "0");

        Printf(wrapper->code, "%s", typeMap);
        null_attribute = Getattr(n, "tmap:out:null");
        if (Len(typeMap))
          Printf(wrapper->code, "\n");
      } else {
        Swig_warning(WARN_TYPEMAP_OUT_UNDEF, input_file, line_number, "Unable to use return type %s in function %s.\n", SwigType_str(type, 0), Getattr(n, "name"));
      }
      emit_return_variable(n, type, wrapper);
    }

    /* Output argument output code */
    Printv(wrapper->code, outarg, NIL);

    /* Output cleanup code */
    Printv(wrapper->code, cleanup, NIL);

    /* Look to see if there is any newfree cleanup code */
    if (GetFlag(n, "feature:new")) {
      if ((typeMap = Swig_typemap_lookup("newfree", n, Swig_cresult_name(), 0))) {
        canThrow(n, "newfree", n);
        Replaceall(typeMap, "$source", Swig_cresult_name());        /* deprecated */
        Printf(wrapper->code, "%s\n", typeMap);
      }
    }

    /* See if there is any return cleanup code */
    if (!native_function_flag) {
      if ((typeMap = Swig_typemap_lookup("ret", n, Swig_cresult_name(), 0))) {
        canThrow(n, "ret", n);
        Replaceall(typeMap, "$source", Swig_cresult_name());        /* deprecated */
        Printf(wrapper->code, "%s\n", typeMap);
      }
    }

    /* Finish C function and intermediary class function definitions */
    Printf(im_callout, " ) )");

    Printf(wrapper->def, ") {");

    if (!is_void_return)
      Printv(wrapper->code, "    return nbresult;\n", NIL);
    Printf(wrapper->code, "}\n");

    /* Substitute the cleanup code */
    Replaceall(wrapper->code, "$cleanup", cleanup);

    /* Substitute the function name */
    Replaceall(wrapper->code, "$symname", symname);

    /* Contract macro modification */
    if (Replaceall(wrapper->code, "SWIG_contract_assert(", "SWIG_contract_assert($null, ") > 0) {
      Setattr(n, "pharo:canthrow", "1");
    }

    if (!null_attribute)
      Replaceall(wrapper->code, "$null", "0");
    else
      Replaceall(wrapper->code, "$null", null_attribute);

    /* Dump the function out */
    if (!native_function_flag) {
      Wrapper_print(wrapper, f_wrappers);

      // A very simple check (it is not foolproof) to help typemap/feature writers for
      // throwing C# exceptions from unmanaged code. It checks for the common methods which
      // set a pending C# exception... the 'canthrow' typemap/feature attribute must be set
      // so that code which checks for pending exceptions is added in the C# proxy method.
      if (!Getattr(n, "csharp:canthrow")) {
        if (Strstr(wrapper->code, "SWIG_exception")) {
          Swig_warning(WARN_PHARO_CANTHROW, input_file, line_number,
                       "Unmanaged code contains a call to SWIG_exception and C# code does not handle pending exceptions via the canthrow attribute.\n");
        } else if (Strstr(wrapper->code, "SWIG_CSharpSetPendingException")) {
          Swig_warning(WARN_PHARO_CANTHROW, input_file, line_number,
                       "Unmanaged code contains a call to a SWIG_CSharpSetPendingException method and C# code does not handle pending exceptions via the canthrow attribute.\n");
        }
      }
    }

    /* Emit the intermediate method. */
    Printv(im_body, im_selector, "\n\t", nativeboost_primitive, "\n", im_callout, NIL);
    methodForClass(imclass_name, swig_wrapper_category, im_body);

    if (!(proxy_flag && is_wrapping_class()) && !enum_constant_flag) {
      moduleClassFunctionHandler(n);
    }

    /* 
     * Generate the proxy class properties for public member variables.
     * Not for enums and constants.
     */
    if (proxy_flag && wrapping_member_flag && !enum_constant_flag) {
      Setattr(n, "proxyfuncname", variable_name);
      Setattr(n, "imfuncname", symname);

      proxyClassFunctionHandler(n);
    }

    /* Cleanup */
    Delete(c_return_type);
    Delete(im_body);
    Delete(im_callout);
    Delete(im_selector);
    Delete(im_return_type);
    Delete(cleanup);
    Delete(outarg);
    Delete(body);
    Delete(overloaded_name);
    DelWrapper(wrapper);

    return SWIG_OK;
  }

  virtual int variableWrapper(Node *n) {
    Language::variableWrapper(n);
    return SWIG_OK;
  }

  virtual int globalvariableHandler(Node *n) {

    generate_property_declaration_flag = true;
    variable_name = Getattr(n, "sym:name");
    global_variable_flag = true;
    int ret = Language::globalvariableHandler(n);
    global_variable_flag = false;
    generate_property_declaration_flag = false;

    return ret;
  }

  String *getOverloadedName(Node *n) {

    /* A NBExternAddress (or something like that) is used for all classes in the
     * SWIG intermediary class.
     * The intermediary class methods are thus mangled when overloaded to give
     * a unique name. */
    String *overloaded_name = NewStringf("%s", Getattr(n, "sym:name"));

    if (Getattr(n, "sym:overloaded")) {
      Printv(overloaded_name, Getattr(n, "sym:overname"), NIL);
    }

    return overloaded_name;
  }

  void moduleClassFunctionHandler(Node *n) {
    SwigType *type = Getattr(n, "type");
    ParmList *params = Getattr(n, "parms");
    String *type_map;
    Parm *param;
    int i;
    String *imcall = NewString("");
    String *function_code = NewString("");
    int num_arguments = 0;
    String *overloaded_name = getOverloadedName(n);
    String *func_name = NULL;
    bool setter_flag = false;
    String *pre_code = NewString("");
    String *post_code = NewString("");
    String *terminator_code = NewString("");

    if (params) {
      if (SwigType_type(Getattr(params, "type")) == T_VOID) {
        params = nextSibling(params);
      }
    }

    /* Attach the non-standard typemaps to the parameter list */
    Swig_typemap_attach_parms("nbin", params, NULL);

    /* Change function name for global variables */
    if (proxy_flag && global_variable_flag) {
      func_name = Copy(variable_name);
      setter_flag = (Cmp(Getattr(n, "sym:name"), Swig_name_set(getNSpace(), variable_name)) == 0);
    } else {
      func_name = Copy(Getattr(n, "sym:name"));
    }

    /* Start generating the function */
    Printf(function_code, "%s", func_name);
    Printv(imcall, imclass_name, " ", overloaded_name, "", NIL);

    /* Get number of required and total arguments */
    num_arguments = emit_num_arguments(params);

    int genSelectorName = 0;

    /* Output each parameter */
    for (i = 0, param = params; i < num_arguments; i++) {

      /* Ignored parameters */
      while (checkAttribute(param, "tmap:in:numinputs", "0")) {
        param = Getattr(param, "tmap:in:next");
      }

      SwigType *pt = Getattr(param, "type");
      String *param_type = NewString("");

      String *arg = makeParameterName(n, param, i, setter_flag);
      String *sel_name = makeSelectorName(n, param, i);

      String *local_name = Getattr(param, "lname");
      String *local_arg = NewString("");
      Printf(local_arg, "nb%s", local_name);

      if (genSelectorName) {
        Printf(imcall, " %s: ", local_arg);
        Printf(function_code, " %s: %s", sel_name, arg);
      }
      else {
        Printf(imcall, "_%s: ", local_arg);
        Printf(function_code, ": %s", arg);
      }
      genSelectorName = 1;

      // Use typemaps to transform type used in Pharo wrapper function (in proxy class) to type used in PInvoke function (in intermediary class)
      if ((type_map = Getattr(param, "tmap:nbin"))) {
        substituteClassname(pt, type_map);
        Replaceall(type_map, "$nbinput", arg);
        String *pre = Getattr(param, "tmap:nbin:pre");
        if (pre) {
          substituteClassname(pt, pre);
          Replaceall(pre, "$nbinput", arg);
          if (Len(pre_code) > 0)
            Printf(pre_code, "\n");
          Printv(pre_code, pre, NIL);
        }
        String *post = Getattr(param, "tmap:nbin:post");
        if (post) {
          substituteClassname(pt, post);
          Replaceall(post, "$nbinput", arg);
          if (Len(post_code) > 0)
            Printf(post_code, "\n");
          Printv(post_code, post, NIL);
        }
        String *terminator = Getattr(param, "tmap:nbin:terminator");
        if (terminator) {
          substituteClassname(pt, terminator);
          Replaceall(terminator, "$nbinput", arg);
          if (Len(terminator_code) > 0)
            Insert(terminator_code, 0, "\n");
          Insert(terminator_code, 0, terminator);
        }
        Printv(imcall, type_map, NIL);
      } else {
        Swig_warning(WARN_PHARO_TYPEMAP_NBIN_UNDEF, input_file, line_number, "No nbin typemap defined for %s\n", SwigType_str(pt, 0));
      }

      /* Add parameter to module class function */
      param = Getattr(param, "tmap:in:next");
      Delete(arg);
      Delete(local_arg);
      Delete(sel_name);
      Delete(param_type);
    }

    // Transform return type used in NBCallout function (in intermediary class) to type used in Pharo wrapper function (in module class)
    if ((type_map = Swig_typemap_lookup("nbout", n, "", 0))) {
      excodeSubstitute(n, type_map, "nbout", n);
      bool is_pre_code = Len(pre_code) > 0;
      bool is_post_code = Len(post_code) > 0;
      bool is_terminator_code = Len(terminator_code) > 0;
      if (is_pre_code || is_post_code || is_terminator_code) {
        Replaceall(type_map, "\n ", "\n   "); // add extra indentation to code in typemap
        if (is_post_code) {
          Insert(type_map, 0, "\n    [");
          Printv(type_map, "] ensure: [\n", post_code, "\n    ]", NIL);
        } else {
          Insert(type_map, 0, "\n    ");
        }
        if (is_pre_code) {
          Insert(type_map, 0, pre_code);
          Insert(type_map, 0, "\n");
        }
        if (is_terminator_code) {
          Printv(type_map, "\n", terminator_code, NIL);
        }
      }
      if (GetFlag(n, "feature:new"))
        Replaceall(type_map, "$owner", "true");
      else
        Replaceall(type_map, "$owner", "false");
      substituteClassname(type, type_map);
      Replaceall(type_map, "$imcall", imcall);
    } else {
      Swig_warning(WARN_PHARO_TYPEMAP_NBOUT_UNDEF, input_file, line_number, "No nbout typemap defined for %s\n", SwigType_str(type, 0));
    }

    // Normal function call
    Printf(function_code, " %s", type_map ? (const String *) type_map : empty_string);

    // Emit the function.
    methodForClass(module_class_name, swig_wrapper_category, function_code);

    Delete(pre_code);
    Delete(post_code);
    Delete(terminator_code);
    Delete(function_code);
    Delete(imcall);
    Delete(func_name);
  }

  virtual int classHandler(Node *n) {

    String *nspace = Getattr(n, "sym:nspace");
    // Save class local variables
    String *old_proxy_class_name = proxy_class_name;
    String *old_destructor_call = destructor_call;
    String *old_proxy_class_constants_code = proxy_class_constants_code;
    String *old_proxy_class_constants = proxy_class_constants;
    String *old_proxy_class_instance_variables = proxy_class_instance_variables;
    String *old_proxy_class_variables = proxy_class_variables;
    String *old_proxy_class_pool_dictionaries = proxy_class_pool_dictionaries;

    if (proxy_flag) {
      proxy_class_name = NewString(Getattr(n, "sym:name"));
      if (Node *outer = Getattr(n, "nested:outer")) {
        String *outerClassesPrefix = Copy(Getattr(outer, "sym:name"));
        for (outer = Getattr(outer, "nested:outer"); outer != 0; outer = Getattr(outer, "nested:outer")) {
          Push(outerClassesPrefix, "_");
          Push(outerClassesPrefix, Getattr(outer, "sym:name"));
        }
        String *fnspace = nspace ? NewStringf("%s_%s", nspace, outerClassesPrefix) : outerClassesPrefix;
        Push(proxy_class_name, "_");
        Push(proxy_class_name, fnspace);
        if (!addSymbol(proxy_class_name, n))
          return SWIG_ERROR;
        if (nspace)
          Delete(fnspace);
        Delete(outerClassesPrefix);
      }
      else {
        if (!addSymbol(proxy_class_name, n, nspace))
          return SWIG_ERROR;
      }

      destructor_call = NewString("");
      proxy_class_constants_code = NewString("");
      proxy_class_constants = NewString("");
      proxy_class_instance_variables = NewString("");
      proxy_class_variables = NewString("");
      proxy_class_pool_dictionaries = NewString("");
    }

    if(!isupper(*Char(proxy_class_name))) {
        Swig_warning(WARN_PHARO_INVALID_CLASSNAME, input_file, line_number, "Invalid Smalltalk class name for %s\n", proxy_class_name);
        return SWIG_NOWRAP;
    }

    Language::classHandler(n);

    if (proxy_flag) {
      String *nbclazzname = proxy_class_name; // mangled full proxy class name

      Replaceall(proxy_class_constants_code, "$nbclassname", proxy_class_name);
      Replaceall(proxy_class_constants_code, "$nbclazzname", nbclazzname);
      Replaceall(proxy_class_constants_code, "$module", module_class_name);
      Replaceall(proxy_class_constants_code, "$imclassname", imclass_name);

      emitProxyClassDefAndCPPCasts(n);

      Delete(proxy_class_name);
      proxy_class_name = old_proxy_class_name;
      Delete(destructor_call);
      destructor_call = old_destructor_call;
      Delete(proxy_class_constants);
      proxy_class_constants = old_proxy_class_constants;
      Delete(proxy_class_constants_code);
      proxy_class_constants_code = old_proxy_class_constants_code;
      Delete(proxy_class_instance_variables);
      proxy_class_instance_variables = old_proxy_class_instance_variables;
      Delete(proxy_class_variables);
      proxy_class_variables = old_proxy_class_variables;
      Delete(proxy_class_pool_dictionaries);
      proxy_class_pool_dictionaries = old_proxy_class_pool_dictionaries;
    }
    return SWIG_OK;
  }

  virtual int memberfunctionHandler(Node *n) {
    Language::memberfunctionHandler(n);

    if (proxy_flag) {
      String *overloaded_name = getOverloadedName(n);
      String *intermediary_function_name = Swig_name_member(getNSpace(), getClassPrefix(), overloaded_name);
      Setattr(n, "proxyfuncname", Getattr(n, "sym:name"));
      Setattr(n, "imfuncname", intermediary_function_name);
      proxyClassFunctionHandler(n);
      Delete(overloaded_name);
    }
    return SWIG_OK;
  }

  virtual int staticmemberfunctionHandler(Node *n) {

    static_flag = true;
    Language::staticmemberfunctionHandler(n);

    if (proxy_flag) {
      String *overloaded_name = getOverloadedName(n);
      String *intermediary_function_name = Swig_name_member(getNSpace(), getClassPrefix(), overloaded_name);
      Setattr(n, "proxyfuncname", Getattr(n, "sym:name"));
      Setattr(n, "imfuncname", intermediary_function_name);
      proxyClassFunctionHandler(n);
      Delete(overloaded_name);
    }
    static_flag = false;

    return SWIG_OK;
  }

  /* -----------------------------------------------------------------------------
   * proxyClassFunctionHandler()
   *
   * Function called for creating a Pharo wrapper function around a c++ function in the 
   * proxy class. Used for both static and non-static C++ class functions.
   * C++ class static functions map to Pharo class side functions.
   * Two extra attributes in the Node must be available. These are "proxyfuncname" - 
   * the name of the C# class proxy function, which in turn will call "imfuncname" - 
   * the intermediary (NBCallout) function name in the intermediary class.
   * ----------------------------------------------------------------------------- */
  void proxyClassFunctionHandler(Node *n) {

    SwigType *type = Getattr(n, "type");
    ParmList *params = Getattr(n, "parms");
    String *intermediary_function_name = Getattr(n, "imfuncname");
    String *proxy_function_name = Getattr(n, "proxyfuncname");
    bool setter_flag = false;
    String *type_map;
    Parm *param;
    int i;

    if (!proxy_flag)
      return;

    // Wrappers not wanted for some methods where the parameters cannot be overloaded in C#
    if (Getattr(n, "overload:ignore"))
      return;

    // Don't generate proxy method for additional explicitcall method used in directors
    if (GetFlag(n, "explicitcall"))
      return;

    if (params) {
      if (SwigType_type(Getattr(params, "type")) == T_VOID) {
        params = nextSibling(params);
      }
    }

    String *imcall = NewString("");
    String *function_code = NewString("");

    String *pre_code = NewString("");
    String *post_code = NewString("");
    String *terminator_code = NewString("");

    /* Attach the non-standard typemaps to the parameter list */
    Swig_typemap_attach_parms("in", params, NULL);
    Swig_typemap_attach_parms("nbin", params, NULL);

    /* Check for setter */
    if (wrapping_member_flag && !enum_constant_flag)
      setter_flag = (Cmp(Getattr(n, "sym:name"), Swig_name_set(getNSpace(), Swig_name_member(0, getClassPrefix(), variable_name))) == 0);

    // Start generating the proxy function
    Printf(function_code, "%s", proxy_function_name);

    Printv(imcall, imclass_name, " $imfuncname", NIL);
    if (!static_flag)
      Printf(imcall, "_nbarg1: self swigValidCPtr");

    emit_mark_varargs(params);

    int gencomma = !static_flag;
    int start_count = static_flag ? 1 : 2;
    if(variable_wrapper_flag)
      --start_count;
    int gensel = 0;

    /* Output each parameter */
    for (i = 0, param = params; param; i++) {

      /* Ignored varargs */
      if (checkAttribute(param, "varargs:ignore", "1")) {
        param = nextSibling(param);
        continue;
      }

      /* Ignored parameters */
      if (checkAttribute(param, "tmap:in:numinputs", "0")) {
        param = Getattr(param, "tmap:in:next");
        continue;
      }

      /* Ignore the 'this' argument for variable wrappers */
      if (!(variable_wrapper_flag && i == 0)) {
        SwigType *pt = Getattr(param, "type");
        String *param_type = NewString("");

        // Add parameter to the call
        if (gencomma) {
          Printf(imcall, " nbarg%d: ", i + start_count);
        } else {
          Printf(imcall, "_nbarg%d: ", i + start_count);
        }

        // Add selector part.
        String *sel_name = makeSelectorName(n, param, i);
        String *arg = makeParameterName(n, param, i, setter_flag);
        if(gensel) {
          Printf(function_code, " %s: %s", sel_name, arg);
        } else {
          Printf(function_code, ": %s", arg);
        }
        gensel = 1;

        // Use typemaps to transform type used in C# wrapper function (in proxy class) to type used in PInvoke function (in intermediary class)
        if ((type_map = Getattr(param, "tmap:nbin"))) {
          substituteClassname(pt, type_map);
          Replaceall(type_map, "$nbinput", arg);
          String *pre = Getattr(param, "tmap:nbin:pre");
          if (pre) {
            substituteClassname(pt, pre);
            Replaceall(pre, "$nbinput", arg);
            if (Len(pre_code) > 0)
              Printf(pre_code, "\n");
            Printv(pre_code, pre, NIL);
          }
          String *post = Getattr(param, "tmap:nbin:post");
          if (post) {
            substituteClassname(pt, post);
            Replaceall(post, "$nbinput", arg);
            if (Len(post_code) > 0)
              Printf(post_code, "\n");
            Printv(post_code, post, NIL);
          }
          String *terminator = Getattr(param, "tmap:nbin:terminator");
          if (terminator) {
            substituteClassname(pt, terminator);
            Replaceall(terminator, "$nbinput", arg);
            if (Len(terminator_code) > 0)
              Insert(terminator_code, 0, "\n");
            Insert(terminator_code, 0, terminator);
          }
          Printv(imcall, type_map, NIL);
        } else {
          Swig_warning(WARN_PHARO_TYPEMAP_NBIN_UNDEF, input_file, line_number, "No csin typemap defined for %s\n", SwigType_str(pt, 0));
        }

        Delete(arg);
        Delete(sel_name);
        Delete(param_type);
      }
      param = Getattr(param, "tmap:in:next");
    }

    // Transform return type used in PInvoke function (in intermediary class) to type used in C# wrapper function (in proxy class)
    if ((type_map = Swig_typemap_lookup("nbout", n, "", 0))) {
      excodeSubstitute(n, type_map, "nbout", n);
      bool is_pre_code = Len(pre_code) > 0;
      bool is_post_code = Len(post_code) > 0;
      bool is_terminator_code = Len(terminator_code) > 0;
      if (is_pre_code || is_post_code || is_terminator_code) {
        /* TODO: Fix this broken part. */
        Replaceall(type_map, "\n ", "\n   "); // add extra indentation to code in typemap
        if (is_post_code) {
          Insert(type_map, 0, "\n    [ ");
          Printv(type_map, " ensure: [\n", post_code, "\n    ]", NIL);
        } else {
          Insert(type_map, 0, "\n    ");
        }
        if (is_pre_code) {
          Insert(type_map, 0, pre_code);
          Insert(type_map, 0, "\n");
        }
        if (is_terminator_code) {
          Printv(type_map, "\n", terminator_code, NIL);
        }
        Insert(type_map, 0, "[");
        Printf(type_map, "\n  ]");
      }
      if (GetFlag(n, "feature:new"))
        Replaceall(type_map, "$owner", "true");
      else
        Replaceall(type_map, "$owner", "false");
      substituteClassname(type, type_map);
      Replaceall(imcall, "$imfuncname", intermediary_function_name);
      Replaceall(type_map, "$imcall", imcall);
    } else {
      Swig_warning(WARN_PHARO_TYPEMAP_NBOUT_UNDEF, input_file, line_number, "No csout typemap defined for %s\n", SwigType_str(type, 0));
    }

    // Normal function call
    Printf(function_code, "%s", type_map ? (const String *) type_map : empty_string);
    if(static_flag)
      methodForClass(proxy_class_name, swig_wrapper_category, function_code);
    else
      methodFor(proxy_class_name, swig_wrapper_category, function_code);

    Delete(pre_code);
    Delete(post_code);
    Delete(terminator_code);
    Delete(function_code);
    Delete(imcall);
  }

  virtual int constructorHandler(Node *n) {
    ParmList *l = Getattr(n, "parms");
    String *tm;
    Parm *p;
    int i;

    Language::constructorHandler(n);

    // Wrappers not wanted for some methods where the parameters cannot be overloaded in C#
    if (Getattr(n, "overload:ignore"))
      return SWIG_OK;

    if (proxy_flag) {
      String *function_code = NewString("");
      String *pre_code = NewString("");
      String *post_code = NewString("");
      String *terminator_code = NewString("");

      String *overloaded_name = getOverloadedName(n);
      String *mangled_overname = Swig_name_construct(getNSpace(), overloaded_name);
      String *imcall = NewString("");

      Printf(function_code, "new");
      Printv(imcall, imclass_name, " ", mangled_overname, NIL);

      /* Attach the non-standard typemaps to the parameter list */
      Swig_typemap_attach_parms("in", l, NULL);
      Swig_typemap_attach_parms("nbin", l, NULL);

      emit_mark_varargs(l);

      int gencomma = 0;

      /* Output each parameter */
      for (i = 0, p = l; p; i++) {

        /* Ignored varargs */
        if (checkAttribute(p, "varargs:ignore", "1")) {
          p = nextSibling(p);
          continue;
        }

        /* Ignored parameters */
        if (checkAttribute(p, "tmap:in:numinputs", "0")) {
          p = Getattr(p, "tmap:in:next");
          continue;
        }

        SwigType *pt = Getattr(p, "type");

        String *arg = makeParameterName(n, p, i, false);
        String *sel_name = makeSelectorName(n, p, i);

        if (gencomma) {
          Printf(function_code, " %s: %s", sel_name, arg);
          Printf(imcall, " nbarg%d: ", i + 1);
        } else {
          Printf(function_code, ": %s", arg);
          Printf(imcall, "_nbarg%d: ", i + 1);
        }

        // Use typemaps to transform type used in Pharo wrapper function (in proxy class) to type used in NBCallout function (in intermediary class)
        if ((tm = Getattr(p, "tmap:nbin"))) {
          substituteClassname(pt, tm);
          Replaceall(tm, "$nbinput", arg);
          String *pre = Getattr(p, "tmap:nbin:pre");
          if (pre) {
            substituteClassname(pt, pre);
            Replaceall(pre, "$nbinput", arg);
            if (Len(pre_code) > 0)
              Printf(pre_code, "\n");
            Printv(pre_code, pre, NIL);
          }
          String *post = Getattr(p, "tmap:nbin:post");
          if (post) {
            substituteClassname(pt, post);
            Replaceall(post, "$nbinput", arg);
            if (Len(post_code) > 0)
              Printf(post_code, "\n");
            Printv(post_code, post, NIL);
          }
          String *terminator = Getattr(p, "tmap:nbin:terminator");
          if (terminator) {
            substituteClassname(pt, terminator);
            Replaceall(terminator, "$nbinput", arg);
            if (Len(terminator_code) > 0)
              Insert(terminator_code, 0, "\n");
            Insert(terminator_code, 0, terminator);
          }
          Printv(imcall, tm, NIL);
        } else {
          Swig_warning(WARN_PHARO_TYPEMAP_NBIN_UNDEF, input_file, line_number, "No csin typemap defined for %s\n", SwigType_str(pt, 0));
        }

        ++gencomma;
        Delete(arg);
        Delete(sel_name);
        p = Getattr(p, "tmap:in:next");
      }

      /* Insert the nbconstruct typemap, doing the replacement for $directorconnect, as needed */
      Hash *attributes = NewHash();
      String *construct_tm = Copy(typemapLookup(n, "nbconstruct", Getattr(n, "name"),
                                                WARN_PHARO_TYPEMAP_NBCONSTRUCT_UNDEF, attributes));
      if (construct_tm)
        Printv(function_code, " ", construct_tm, NIL);

      excodeSubstitute(n, function_code, "nbconstruct", attributes);

      // TODO: Implement this thing.
      bool is_pre_code = Len(pre_code) > 0;
      bool is_post_code = Len(post_code) > 0;
      bool is_terminator_code = Len(terminator_code) > 0;
      if (is_pre_code || is_post_code || is_terminator_code) {
/*        if (is_pre_code) {
          Printv(helper_code, pre_code, "\n", NIL);
        }
        if (is_post_code) {
          Printf(helper_code, "    try {\n");
          Printv(helper_code, "      return ", imcall, ";\n", NIL);
          Printv(helper_code, "    } finally {\n", post_code, "\n    }", NIL);
        } else {
          Printv(helper_code, "    return ", imcall, ";", NIL);
        }
        if (is_terminator_code) {
          Printv(helper_code, "\n", terminator_code, NIL);
        }
        Printf(helper_code, "\n  }\n");
        String *helper_name = NewStringf("%s.SwigConstruct%s(%s)", proxy_class_name, proxy_class_name, helper_args);
        String *im_outattributes = Getattr(n, "tmap:imtype:outattributes");
        if (im_outattributes)
          Printf(proxy_class_code, "  %s\n", im_outattributes);
        Printv(proxy_class_code, helper_code, "\n", NIL);
        Replaceall(function_code, "$imcall", helper_name);
        Delete(helper_name);*/
      } else {
        Replaceall(function_code, "$imcall", imcall);
      }

      methodForClass(proxy_class_name, swig_wrapper_category, function_code);

      Delete(pre_code);
      Delete(post_code);
      Delete(terminator_code);
      Delete(construct_tm);
      Delete(attributes);
      Delete(overloaded_name);
      Delete(imcall);
    }

    return SWIG_OK;
  }

  virtual int destructorHandler(Node *n) {
    Language::destructorHandler(n);
    String *symname = Getattr(n, "sym:name");

    if (proxy_flag) {
      Printv(destructor_call, imclass_name, " ", Swig_name_destroy(getNSpace(), symname), "_nbarg1: pointerToFree", NIL);
    }
    return SWIG_OK;
  }

  virtual int membervariableHandler(Node *n) {

    generate_property_declaration_flag = true;
    variable_name = Getattr(n, "sym:name");
    wrapping_member_flag = true;
    variable_wrapper_flag = true;
    Language::membervariableHandler(n);
    wrapping_member_flag = false;
    variable_wrapper_flag = false;
    generate_property_declaration_flag = false;

    return SWIG_OK;
  }

  virtual int staticmembervariableHandler(Node *n) {

    generate_property_declaration_flag = true;
    variable_name = Getattr(n, "sym:name");
    wrapping_member_flag = true;
    static_flag = true;
    Language::staticmembervariableHandler(n);
    wrapping_member_flag = false;
    static_flag = false;
    generate_property_declaration_flag = false;

    return SWIG_OK;
  }

  virtual int memberconstantHandler(Node *n) {
    variable_name = Getattr(n, "sym:name");
    wrapping_member_flag = true;
    Language::memberconstantHandler(n);
    wrapping_member_flag = false;
    return SWIG_OK;
  }

  void emitProxyClassDefAndCPPCasts(Node *n) {
    String *baseclass = NULL;
    String *c_baseclassname = NULL;
    SwigType *typemap_lookup_type = Getattr(n, "classtypeobj");

    // Inheritance from pure Smalltalk classes
    Node *attributes = NewHash();
    const String *pure_baseclass = typemapLookup(n, "nbbase", typemap_lookup_type, WARN_NONE, attributes);
    bool purebase_replace = GetFlag(attributes, "tmap:nbbase:replace") ? true : false;
    bool purebase_notderived = GetFlag(attributes, "tmap:nbbase:notderived") ? true : false;
    Delete(attributes);

    // C++ inheritance
    if (!purebase_replace) {
      List *baselist = Getattr(n, "bases");
      if (baselist) {
        Iterator base = First(baselist);
        while (base.item && GetFlag(base.item, "feature:ignore")) {
          base = Next(base);
        }
        if (base.item) {
          c_baseclassname = Getattr(base.item, "name");
          baseclass = Copy(getProxyName(c_baseclassname));
          base = Next(base);
          /* Warn about multiple inheritance for additional base class(es) */
          while (base.item) {
            if (GetFlag(base.item, "feature:ignore")) {
              base = Next(base);
              continue;
            }
            String *proxyclassname = Getattr(n, "classtypeobj");
            String *baseclassname = Getattr(base.item, "name");
            Swig_warning(WARN_PHARO_MULTIPLE_INHERITANCE, Getfile(n), Getline(n),
                         "Warning for %s proxy: Base %s ignored. Multiple inheritance is not supported in Java.\n", SwigType_namestr(proxyclassname), SwigType_namestr(baseclassname));
            base = Next(base);
          }
        }
      }
    }

    bool derived = baseclass && getProxyName(c_baseclassname);
    if (derived && purebase_notderived)
      pure_baseclass = empty_string;
    const String *wanted_base = baseclass ? baseclass : pure_baseclass;

    if (purebase_replace) {
      wanted_base = pure_baseclass;
      derived = false;
      Delete(baseclass);
      baseclass = NULL;
      if (purebase_notderived)
        Swig_error(Getfile(n), Getline(n), "The nbbase typemap for proxy %s must contain just one of the 'replace' or 'notderived' attributes.\n", typemap_lookup_type);
    } else if (Len(pure_baseclass) > 0 && Len(baseclass) > 0) {
      Swig_warning(WARN_PHARO_MULTIPLE_INHERITANCE, Getfile(n), Getline(n),
                   "Warning for %s proxy: Base %s ignored. Multiple inheritance is not supported in C#. "
                   "Perhaps you need one of the 'replace' or 'notderived' attributes in the csbase typemap?\n", typemap_lookup_type, pure_baseclass);
    }

    // Use by default Object as a base.
    if(!wanted_base || !Len(wanted_base))
      wanted_base = wrapped_object_class_name;

    // Add extra instance variables.
    const String *extra_inst_vars = typemapLookup(n, "nbinstvars", typemap_lookup_type, WARN_NONE);
    if(extra_inst_vars)
      Printf(proxy_class_instance_variables, "%s%s", Len(proxy_class_instance_variables) > 0 ? " " : "", extra_inst_vars);

    // Add extra class variables.
    const String *extra_class_vars = typemapLookup(n, "nbclassvars", typemap_lookup_type, WARN_NONE);
    if(extra_class_vars)
      Printf(proxy_class_variables, "%s%s", Len(proxy_class_variables) > 0 ? " " : "", extra_class_vars);

    // Add extra pool dictionaries
    const String *extra_pools = typemapLookup(n, "nbpools", typemap_lookup_type, WARN_NONE);
    if(extra_pools)
      Printf(proxy_class_pool_dictionaries, "%s%s", Len(proxy_class_pool_dictionaries) > 0 ? " " : "", extra_pools);

    // Add the constants
    if(Len(proxy_class_constants) != 0) {
      Printv(proxy_class_variables, Len(proxy_class_variables) > 0 ? " " : "", proxy_class_constants, NIL);
    }

    // Use a custom category
    const String *custom_category = typemapLookup(n, "nbcategory", typemap_lookup_type, WARN_NONE);
    
    // Write the class
    subclass(wanted_base, proxy_class_name,
             proxy_class_instance_variables,
             proxy_class_variables,
             proxy_class_pool_dictionaries, Len(custom_category) > 0 ? custom_category : category);

    // destroySwigCPtr method
    const String *type_map;
    if (derived) {
      type_map = typemapLookup(n, "nbdestroy_swigcptr_derived", typemap_lookup_type, WARN_NONE);
    } else {
      type_map = typemapLookup(n, "nbdestroy_swigcptr", typemap_lookup_type, WARN_NONE);
    }

    if(Len(type_map) > 0) {
      String *destroy_code = Copy(type_map);
      if(Len(destructor_call))
        Replaceall(destroy_code, "$imcall", destructor_call);
      else
        Replaceall(destroy_code, "$imcall", "self error: 'C++ destructor does not have public access'.");

      methodForClass(proxy_class_name, swig_wrapper_category, destroy_code);
      Delete(destroy_code);
    }

    // Make the class initialize method.
    String *initialize_body = NewString("");
    Printv(initialize_body, proxy_class_constants_code, NIL);
    if(Len(initialize_body) > 0) {
      // Add the class initialize.
      String *initialize_code = NewString("initialize\n");
      Printv(initialize_code, initialize_body, NIL);
      methodForClass(proxy_class_name, swig_wrapper_category, initialize_code);
      Delete(initialize_code);

      // Add a call to the class initialize from the module class.
      Printf(module_class_constants_code, "\t%s initialize.\n", proxy_class_name);
    }
    Delete(initialize_body);
  }

  void emitTypeMappedMethodFor(const String *className, Node *n, const char *typemapName, const char *derivedTypemapName, bool derived, bool classSide) {
    SwigType *typemap_lookup_type = Getattr(n, "classtypeobj");

    // Select the adequate typemap.
    const String *type_map;
    if (derived) {
      type_map = typemapLookup(n, derivedTypemapName, typemap_lookup_type, WARN_NONE);
    } else {
      type_map = typemapLookup(n, typemapName, typemap_lookup_type, WARN_NONE);
    }

    // Emit the type map code.
    if(Len(type_map) > 0) {
      String *code = Copy(type_map);
      if(classSide)
        methodForClass(className, swig_wrapper_category, code);
      else
        methodFor(className, swig_wrapper_category, code);
      Delete(code);
    }
  }

  void emitTypeMappedMethod(Node *n, const char *typemapName, const char *derivedTypemapName, bool derived, bool classSide){
    emitTypeMappedMethodFor(proxy_class_name, n, typemapName, derivedTypemapName, derived, classSide);
  }

  /* -----------------------------------------------------------------------------
   * typemapLookup()
   * n - for input only and must contain info for Getfile(n) and Getline(n) to work
   * tmap_method - typemap method name
   * type - typemap type to lookup
   * warning - warning number to issue if no typemaps found
   * typemap_attributes - the typemap attributes are attached to this node and will 
   *   also be used for temporary storage if non null
   * return is never NULL, unlike Swig_typemap_lookup()
   * ----------------------------------------------------------------------------- */

  const String *typemapLookup(Node *n, const_String_or_char_ptr tmap_method, SwigType *type, int warning, Node *typemap_attributes = 0) {
    Node *node = !typemap_attributes ? NewHash() : typemap_attributes;
    Setattr(node, "type", type);
    Setfile(node, Getfile(n));
    Setline(node, Getline(n));
    const String *tm = Swig_typemap_lookup(tmap_method, node, "", 0);
    if (!tm) {
      tm = empty_string;
      if (warning != WARN_NONE)
        Swig_warning(warning, Getfile(n), Getline(n), "No %s typemap defined for %s\n", tmap_method, SwigType_str(type, 0));
    }
    if (!typemap_attributes)
      Delete(node);
    return tm;
  }

  /* -----------------------------------------------------------------------
   * constantWrapper()
   * Used for wrapping constants - #define or %constant.
   * Also for inline initialised const static primitive type member variables (short, int, double, enums etc).
   * Pharo class variables constants and accessors.
   * If the %nbconst(1) feature is used then the C constant value is used to initialise the C# const variable.
   * If not, a NBCallout method is generated to get the C constant value for initialisation of the C# const variable.
   * However, if the %nbconstvalue feature is used, it overrides all other ways to generate the initialisation.
   * Also note that this method might be called for wrapping enum items (when the enum is using %nbconst(0)).
   * ------------------------------------------------------------------------ */

  virtual int constantWrapper(Node *n) {
    String *symname = Getattr(n, "sym:name");
    SwigType *t = Getattr(n, "type");
    String *constants_code = NewString("");
    String *accessor_code = NewString("");
    Swig_save("constantWrapper", n, "value", NIL);
    Swig_save("constantWrapper", n, "tmap:ctype:out", "tmap:imtype:out", "tmap:out:null", NIL);

    bool is_enum_item = (Cmp(nodeType(n), "enumitem") == 0);

    const String *itemname = (proxy_flag && wrapping_member_flag) ? variable_name : symname;
    if (!is_enum_item) {
      String *scope = 0;
      if (proxy_class_name) {
        String *nspace = getNSpace();
        scope = NewString("");
        if (nspace)
          Printf(scope, "%s.", nspace);
        Printf(scope, "%s", proxy_class_name);
      } else {
        scope = Copy(module_class_name);
      }
      if (!addSymbol(itemname, n, scope))
        return SWIG_ERROR;
      Delete(scope);
    }

    // The %nbconst feature determines how the constant value is obtained
    int const_feature_flag = GetFlag(n, "feature:nb:const");

    /* Adjust the enum type for the Swig_typemap_lookup.
     * We want the same jstype typemap for all the enum items so we use the enum type (parent node). */
    if (is_enum_item) {
      t = Getattr(parentNode(n), "enumtype");
      Setattr(n, "type", t);
    }

    // Add the stripped quotes back in
    String *new_value = NewString("");
    if (SwigType_type(t) == T_STRING) {
      Printf(new_value, "\"%s\"", Copy(Getattr(n, "value")));
      Setattr(n, "value", new_value);
    } else if (SwigType_type(t) == T_CHAR) {
      Printf(new_value, "'%s'", Copy(Getattr(n, "value")));
      Setattr(n, "value", new_value);
    }

    // Start making the constant code
    Printf(constants_code, "%s := ", itemname);

    // Check for the %nbconstvalue feature
    String *value = Getattr(n, "feature:nb:constvalue");

    if (value) {
      Printf(constants_code, "%s.\n", value);
    } else if (!const_feature_flag) {
      Printf(constants_code, "%s %s.\n", imclass_name, Swig_name_get(getNSpace(), symname));

      // Each constant and enum value is wrapped with a separate PInvoke function call
      SetFlag(n, "feature:immutable");
      enum_constant_flag = true;
      variableWrapper(n);
      enum_constant_flag = false;
    } else {
      // Alternative constant handling will use the C syntax to make a true Smalltalk constant and hope that it compiles as Smalltalk code
      if (Getattr(n, "wrappedasconstant")) {
        if (SwigType_type(t) == T_CHAR)
          Printf(constants_code, "$%s.\n", Getattr(n, "staticmembervariableHandler:value"));
        else
          Printf(constants_code, "%s.\n", Getattr(n, "staticmembervariableHandler:value"));
      } else {
        Printf(constants_code, "%s.\n", Getattr(n, "value"));
      }
    }

    // Emit the accessor code
    Printf(accessor_code, "%s\n\t%s ifNil: [ %s ].", itemname, itemname, constants_code);
    Printf(accessor_code, "\n\t ^ %s", itemname);

    // Emit the generated code to appropriate place
    // Enums only emit the intermediate and PINVOKE methods, so no proxy or module class wrapper methods needed
    if (proxy_flag && wrapping_member_flag) {
        Printv(proxy_class_constants, Len(proxy_class_constants) > 0 ? " " : "", itemname, NIL);
        //Printv(proxy_class_constants_code, constants_code, NIL);
        methodForClass(proxy_class_name, swig_wrapper_category, accessor_code);
    }
    else {
      Printv(module_class_constants, Len(module_class_constants) > 0 ? " " : "", itemname, NIL);
      //Printv(module_class_constants_code, constants_code, NIL);
      methodForClass(module_class_name, swig_wrapper_category, accessor_code);
    }

    // Cleanup
    Swig_restore(n);
    Delete(accessor_code);
    Delete(new_value);
    Delete(constants_code);
    return SWIG_OK;
  }

  /* -----------------------------------------------------------------------------
   * excodeSubstitute()
   * If a method can throw a Pharo error, additional exception code is added to
   * check for the pending exception so that it can then throw the exception. The
   * $excode special variable is replaced by the exception code in the excode
   * typemap attribute.
   * ----------------------------------------------------------------------------- */

  void excodeSubstitute(Node *n, String *code, const String *typemap, Node *parameter) {
    String *excode_attribute = NewStringf("tmap:%s:excode", typemap);
    String *excode = Getattr(parameter, excode_attribute);
    if (Getattr(n, "pharo:canthrow")) {
      int count = Replaceall(code, "$excode", excode);
      if (count < 1 || !excode) {
        Swig_warning(WARN_PHARO_EXCODE, input_file, line_number,
                     "Pharo exception may not be thrown - no $excode or excode attribute in '%s' typemap.\n", typemap);
      }
    } else {
      Replaceall(code, "$excode", empty_string);
    }
    Delete(excode_attribute);
  }

  /* -----------------------------------------------------------------------------
   * getProxyName()
   *
   * Test to see if a type corresponds to something wrapped with a proxy class.
   * Return NULL if not otherwise the proxy class name, fully qualified with
   * a namespace if the nspace feature is used.
   * ----------------------------------------------------------------------------- */
  
   String *getProxyName(SwigType *t) {
    // FIXME: Add smalltalk changes
     String *proxyname = NULL;
     if (proxy_flag) {
       Node *n = classLookup(t);
       if (n) {
         proxyname = Getattr(n, "proxyname");
         if (!proxyname) {
           String *nspace = Getattr(n, "sym:nspace");
           String *symname = Copy(Getattr(n, "sym:name"));
           if (symname && !GetFlag(n, "feature:flatnested")) {
             for (Node *outer_class = Getattr(n, "nested:outer"); outer_class; outer_class = Getattr(outer_class, "nested:outer")) {
               Push(symname, "_");
               Push(symname, Getattr(outer_class, "sym:name"));
             }
           }
           if (nspace) {
             proxyname = NewStringf("%s_%s", nspace, symname);
           } else {
             proxyname = Copy(symname);
           }
           Setattr(n, "proxyname", proxyname);
           Delete(proxyname);
           Delete(symname);
         }
       }
     }
     return proxyname;
   }

  String *getEnumName(SwigType *t) {
    // FIXME: Add smalltalk changes

    Node *enumname = NULL;
    Node *n = enumLookup(t);
    if (n) {
      enumname = Getattr(n, "enumname");
      if (!enumname) {
        String *symname = Getattr(n, "sym:name");
        if (symname) {
          // Add in class scope when referencing enum if not a global enum
          String *scopename_prefix = Swig_scopename_prefix(Getattr(n, "name"));
          String *proxyname = 0;
          if (scopename_prefix) {
            proxyname = getProxyName(scopename_prefix);
          }
          if (proxyname) {
            enumname = NewStringf("%s_%s", proxyname, symname);
          } else {
            // global enum or enum in a namespace
            String *nspace = Getattr(n, "sym:nspace");
            if (nspace) {
                enumname = NewStringf("%s_%s", nspace, symname);
            } else {
              enumname = Copy(symname);
            }
          }
          Setattr(n, "enumname", enumname);
          Delete(enumname);
          Delete(scopename_prefix);
        }
      }
    }

    return enumname;
  }

  /* -----------------------------------------------------------------------------
   * substituteClassname()
   *
   * Substitute the special variable $csclassname with the proxy class name for classes/structs/unions 
   * that SWIG knows about. Also substitutes enums with enum name.
   * Otherwise use the $descriptor name for the C# class name. Note that the $&csclassname substitution
   * is the same as a $&descriptor substitution, ie one pointer added to descriptor name.
   * Inputs:
   *   pt - parameter type
   *   tm - typemap contents that might contain the special variable to be replaced
   * Outputs:
   *   tm - typemap contents complete with the special variable substitution
   * Return:
   *   substitution_performed - flag indicating if a substitution was performed
   * ----------------------------------------------------------------------------- */

  bool substituteClassname(SwigType *pt, String *tm) {
    bool substitution_performed = false;
    SwigType *type = Copy(SwigType_typedef_resolve_all(pt));
    SwigType *strippedtype = SwigType_strip_qualifiers(type);

    if (Strstr(tm, "$nbclassname")) {
      SwigType *classnametype = Copy(strippedtype);
      substituteClassnameSpecialVariable(classnametype, tm, "$nbclassname");
      substitution_performed = true;
      Delete(classnametype);
    }
    if (Strstr(tm, "$*nbclassname")) {
      SwigType *classnametype = Copy(strippedtype);
      Delete(SwigType_pop(classnametype));
      if (Len(classnametype) > 0) {
        substituteClassnameSpecialVariable(classnametype, tm, "$*nbclassname");
        substitution_performed = true;
      }
      Delete(classnametype);
    }
    if (Strstr(tm, "$&nbclassname")) {
      SwigType *classnametype = Copy(strippedtype);
      SwigType_add_pointer(classnametype);
      substituteClassnameSpecialVariable(classnametype, tm, "$&nbclassname");
      substitution_performed = true;
      Delete(classnametype);
    }

    Delete(strippedtype);
    Delete(type);

    return substitution_performed;
  }

  void substituteClassnameSpecialVariable(SwigType *classnametype, String *tm, const char *classnamespecialvariable) {
    if (SwigType_isenum(classnametype)) {
      String *enumname = getEnumName(classnametype);
      if (enumname)
        Replaceall(tm, classnamespecialvariable, enumname);
      else
        Replaceall(tm, classnamespecialvariable, NewStringf("int"));
    } else {
      String *classname = getProxyName(classnametype);
      if (classname) {
        Replaceall(tm, classnamespecialvariable, classname);        // getProxyName() works for pointers to classes too
      } else {                        // use $descriptor if SWIG does not know anything about this type. Note that any typedefs are resolved.
        String *descriptor = NewStringf("SWIGTYPE%s", SwigType_manglestr(classnametype));
        Replaceall(tm, classnamespecialvariable, descriptor);

        // Add to hash table so that the type wrapper classes can be created later
        Setattr(swig_types_hash, descriptor, classnametype);
        Delete(descriptor);
      }
    }
  }

  void emitTypeWrapperClass(String *classname, SwigType *type) {
    // Pure Smalltalk baseclass
    Node *n = NewHash();
    Setattr(n, "classtypeobj", type);

    const String *pure_baseclass = typemapLookup(n, "nbbase", type, WARN_NONE);

    // Emit the class
    if(Len(pure_baseclass) == 0)
      pure_baseclass = wrapped_object_class_name;

    // Use a custom category
    const String *custom_category = typemapLookup(n, "nbcategory", type, WARN_NONE);
    
    // Write the class
    subclass(pure_baseclass, classname,
             empty_string,
             empty_string,
             empty_string, Len(custom_category) > 0 ? custom_category : category);

    Delete(n);
  }

  /* -----------------------------------------------------------------------------
   * makeParameterName()
   *
   * Inputs: 
   *   n - Node
   *   p - parameter node
   *   arg_num - parameter argument number
   * Return:
   *   arg - a unique parameter name
   * ----------------------------------------------------------------------------- */
  String *makeParameterName(Node *n, Parm *p, int arg_num, bool setter_flag) {

    String *arg = 0;
    String *pn = Getattr(p, "name");

    // Use C parameter name unless it is a duplicate or an empty parameter name
    int count = 0;
    ParmList *plist = Getattr(n, "parms");
    while (plist) {
      if ((Cmp(pn, Getattr(plist, "name")) == 0))
        count++;
      plist = nextSibling(plist);
    }
    String *wrn = pn ? Swig_name_warning(p, 0, pn, 0) : 0;
    arg = (!pn || (count > 1) || wrn) ? NewStringf("arg%d", arg_num) : Copy(pn);

    if (Cmp(arg, "self") == 0) {
      Delete(arg);      
      arg = NewString("this");
    } else if(setter_flag) {
      /* Use a special for setter arguments, to avoid name clashes. */
      Delete(arg);      
      arg = NewString("swigNewValue");
    }

    return arg;
  }

  /* -----------------------------------------------------------------------------
   * makeSelectorName()
   *
   * Inputs: 
   *   n - Node
   *   p - parameter node
   *   arg_num - parameter argument number
   * Return:
   *   arg - a unique parameter name
   * ----------------------------------------------------------------------------- */
  String *makeSelectorName(Node *n, Parm *p, int arg_num) {

    String *arg = 0;
    String *pn = Getattr(p, "name");

    // Use C parameter name unless it is an empty parameter name
    int count = 0;
    ParmList *plist = Getattr(n, "parms");
    while (plist) {
      if ((Cmp(pn, Getattr(plist, "name")) == 0))
        count++;
      plist = nextSibling(plist);
    }
    String *wrn = pn ? Swig_name_warning(p, 0, pn, 0) : 0;
    arg = (!pn || wrn) ? NewStringf("arg%d", arg_num) : Copy(pn);

    if (Cmp(arg, "self") == 0) {
      Delete(arg);      
      arg = NewString("this");
    }

    return arg;
  }
  /* -----------------------------------------------------------------------------
   * canThrow()
   * Determine whether the code in the typemap can throw a C# exception.
   * If so, note it for later when excodeSubstitute() is called.
   * ----------------------------------------------------------------------------- */

  void canThrow(Node *n, const String *typemap, Node *parameter) {
    String *canthrow_attribute = NewStringf("tmap:%s:canthrow", typemap);
    String *canthrow = Getattr(parameter, canthrow_attribute);
    if (canthrow)
      Setattr(n, "csharp:canthrow", "1");
    Delete(canthrow_attribute);
  }

  void subclass(const String *superclass, const String *className, const String *instanceVariables,
                const String *classVariables, const String *poolDictionaries, const String *category) {
    const char *tmpl =
"%s subclass: #%s\n\
  instanceVariableNames: '%s'\n\
  classVariableNames: '%s'\n\
  poolDictionaries: '%s'\n\
  category: '%s'!\n\
\n";
    Printf(f_class_declarations, tmpl, superclass, className, instanceVariables,
           classVariables, poolDictionaries, category);
  }

  void methodFor(const String *className, const String *category, const String *body) {
    const char *tmpl =
"!%s methodsFor: '%s'!\n\
%s\n\
! !\n";
    Printf(f_class_methods, tmpl, className, category, body);
  }

  void methodForClass(const String *className, const String *category, const String *body) {
    const char *tmpl =
"!%s class methodsFor: '%s'!\n\
%s\n\
! !\n\
\n";
    Printf(f_class_methods, tmpl, className, category, body);
  }

  void addLibraryImportToClass(const String *className, const String *libname) {
    const char *tmpl =
"nbLibraryNameOrHandle\n\
\t^ NativeBoost forCurrentPlatform loadModule: '%s'\n\
";
    const char *delegate =
"nbLibraryNameOrHandle\n\
\t^ self class nbLibraryNameOrHandle\n\
";

    /* Add the importing to the class side. */    
    String *body = NewString("");
    Printf(body, tmpl, libname);
    methodForClass(className, swig_wrapper_category, body);
    Delete(body);

    /* Add delegation to the object side. */
    body = NewString(delegate);
    methodFor(className, swig_wrapper_category, body);
    Delete(body);
  }

  bool nestedClassesSupported() const {
    return true;
  }
};

extern "C" Language *
swig_pharo(void) {
  return new PHARO();
}


