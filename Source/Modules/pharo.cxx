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
#include "swigmod.h"

static void show_usage() {
  fprintf(stderr,
"Pharo Options (available with -pharo)\n\
     -fileout <fo>   - Pharo code file out\n\
     -libname <lb>   - Library to load symbols from\n\
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
  String *variable_name;
public:
  PHARO()
    : empty_string(NewString("")),
      object_string(NewString("Object")),
      swig_wrapper_category(NewString("swig wrapper")),
      nativeboost_primitive(NewString("<primitive: #primitiveNativeCall module: #NativeBoostPlugin>")),
      swig_types_hash(NULL),
      f_begin(NULL),
      f_runtime(NULL),
      f_header(NULL),
      f_wrappers(NULL),
      f_init(NULL),
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
      variable_name(NULL) {
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
        } else if(strcmp(argv[i], "-libname") == 0) {
          if (argv[i + 1]) {
            libname = NewString("");
            Printf(category, argv[i + 1]);
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

    /* Get the output file name */
    String *outfile = Getattr(n,"outfile");

    /* Initialize I/O */
    f_begin = NewFile(outfile, "w", SWIG_output_files());
    if (!f_begin) {
       FileErrorDisplay(outfile);
       SWIG_exit(EXIT_FAILURE);
    }
    f_runtime = NewString("");
    f_init = NewString("");
    f_header = NewString("");
    f_wrappers = NewString("");

    /* Make sure there's a fileout name */
    if(!fileout)
      fileout = NewStringf("%s%s.st", SWIG_output_directory(), Getattr(n, "name"));

    /* Smalltalk file out. */
    f_fileout = NewFile(fileout, "w", SWIG_output_files());
    if (!f_fileout) {
       FileErrorDisplay(fileout);
       SWIG_exit(EXIT_FAILURE);
    }
    f_class_declarations = NewString("");
    f_class_methods = NewString("");

    /* Register file targets with the SWIG file handler */
    Swig_register_filebyname("begin", f_begin);
    Swig_register_filebyname("header", f_header);
    Swig_register_filebyname("wrapper", f_wrappers);
    Swig_register_filebyname("runtime", f_runtime);
    Swig_register_filebyname("init", f_init);

    swig_types_hash = NewHash();

    // Make the intermediary class and module class names. The intermediary class name can be set in the module directive.
    if (!imclass_name) {
      imclass_name = NewStringf("%sNBCallout", Getattr(n, "name"));
      module_class_name = Copy(Getattr(n, "name"));
    } else {
      // Rename the module name if it is the same as intermediary class name - a backwards compatibility solution
      if (Cmp(imclass_name, Getattr(n, "name")) == 0)
        module_class_name = NewStringf("%sModule", Getattr(n, "name"));
      else
        module_class_name = Copy(Getattr(n, "name"));
    }

    /* Ensure there's a category */
    if(!category)
      category = Copy(Getattr(n, "name"));

    /* Ensure there's a library name */
    if(!libname)
      libname = Copy(Getattr(n, "name"));

    /* Declare the intermediate class */
    subclass(object_string, imclass_name,
             /*instanceVariables:*/ empty_string,
             /*classVariables:*/ empty_string,
             /*poolDictionary:*/ empty_string, category);

    /* Declare the module class. */
    subclass(object_string, module_class_name,
             /*instanceVariables:*/ empty_string,
             /*classVariables:*/ empty_string,
             /*poolDictionary:*/ empty_string, category);

    /* Emit intermediate class library import */
    addLibraryImportToClass(imclass_name, libname);

    /* Output module initialization code */
    Swig_banner(f_begin);

    Swig_name_register("wrapper", "PharoNB_%f");

    Printf(f_wrappers, "\n#ifdef __cplusplus\n");
    Printf(f_wrappers, "extern \"C\" {\n");
    Printf(f_wrappers, "#endif\n\n");

    /* Emit code for children */
    Language::top(n);

    Printf(f_wrappers, "#ifdef __cplusplus\n");
    Printf(f_wrappers, "}\n");
    Printf(f_wrappers, "#endif\n");

    /* Write Smalltalk code to th file */
    Dump(f_class_declarations, f_fileout);
    Dump(f_class_methods, f_fileout);

    /* Write all to the file */
    Dump(f_runtime, f_begin);
    Dump(f_header, f_begin);
    Dump(f_wrappers, f_begin);
    Wrapper_pretty_print(f_init, f_begin);

    Delete(swig_types_hash);
    swig_types_hash = NULL;

    /* Cleanup strings */
    Delete(fileout);
    Delete(libname);
    Delete(category);
    Delete(imclass_name);
    Delete(module_class_name);

    /* Cleanup files */
    Delete(f_runtime);
    Delete(f_header);
    Delete(f_wrappers);
    Delete(f_init);
    Delete(f_begin);

    Delete(f_class_declarations);
    Delete(f_class_methods);
    Delete(f_fileout);

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
    String *im_outattributes = 0;
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
      Swig_warning(WARN_CSHARP_TYPEMAP_CTYPE_UNDEF, input_file, line_number, "No ctype typemap defined for %s\n", SwigType_str(type, 0));
    }

    if ((typeMap = Swig_typemap_lookup("imtype", n, "", 0))) {
      String *imtypeout = Getattr(n, "tmap:imtype:out");  // the type in the imtype typemap's out attribute overrides the type in the typemap
      if (imtypeout)
        typeMap = imtypeout;
      Printf(im_return_type, "%s", typeMap);
      im_outattributes = Getattr(n, "tmap:imtype:outattributes");
    } else {
      Swig_warning(WARN_CSHARP_TYPEMAP_CSTYPE_UNDEF, input_file, line_number, "No imtype typemap defined for %s\n", SwigType_str(type, 0));
    }

    is_void_return = (Cmp(c_return_type, "void") == 0);
    if (!is_void_return)
      Wrapper_add_localv(wrapper, "nbresult", c_return_type, "nbresult", NIL);

    Printv(wrapper->def, " SWIGEXPORT ", c_return_type, " SWIGSTDCALL ", wname, "(", NIL);

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
        Swig_warning(WARN_CSHARP_TYPEMAP_CTYPE_UNDEF, input_file, line_number, "No ctype typemap defined for %s\n", SwigType_str(param_type, 0));
      }

      /* Get the intermediary class parameter types of the parameter */
      if ((typeMap = Getattr(param, "tmap:imtype"))) {
        const String *inattributes = Getattr(param, "tmap:imtype:inattributes");
        Printf(im_param_type, "%s%s", inattributes ? inattributes : empty_string, typeMap);
      } else {
        Swig_warning(WARN_CSHARP_TYPEMAP_CSTYPE_UNDEF, input_file, line_number, "No imtype typemap defined for %s\n", SwigType_str(param_type, 0));
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
          Swig_warning(WARN_CSHARP_CANTHROW, input_file, line_number,
                       "Unmanaged code contains a call to SWIG_exception and C# code does not handle pending exceptions via the canthrow attribute.\n");
        } else if (Strstr(wrapper->code, "SWIG_CSharpSetPendingException")) {
          Swig_warning(WARN_CSHARP_CANTHROW, input_file, line_number,
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
    Parm *last_parm = 0;
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
      last_parm = param;

      String *arg = makeParameterName(n, param, i);
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
        Swig_warning(WARN_CSHARP_TYPEMAP_CSIN_UNDEF, input_file, line_number, "No nbin typemap defined for %s\n", SwigType_str(pt, 0));
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
      Swig_warning(WARN_CSHARP_TYPEMAP_CSOUT_UNDEF, input_file, line_number, "No nbout typemap defined for %s\n", SwigType_str(type, 0));
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

  /* -----------------------------------------------------------------------------
   * excodeSubstitute()
   * If a method can throw a C# exception, additional exception code is added to
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
	Swig_warning(WARN_CSHARP_EXCODE, input_file, line_number,
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
  String *makeParameterName(Node *n, Parm *p, int arg_num) {

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

};

extern "C" Language *
swig_pharo(void) {
  return new PHARO();
}


