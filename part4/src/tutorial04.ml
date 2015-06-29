open Llvm

let add_target_triple triple llm =
  Llvm_X86.initialize ();
  let lltarget  = Llvm_target.Target.by_triple triple in
  let llmachine = Llvm_target.TargetMachine.create ~triple:triple lltarget in
  let lldly     = Llvm_target.TargetMachine.data_layout llmachine in

  set_target_triple (Llvm_target.TargetMachine.triple llmachine) llm ;
  set_data_layout (Llvm_target.DataLayout.as_string lldly) llm ;
  ()


let _ =
  let llctx = global_context () in
  let llm = create_module llctx "mymodule" in

  add_target_triple "x86_64" llm ;
  let i8_t = i8_type llctx in
  let i32_t = i32_type llctx in
  let fty = function_type i32_t [| |] in

  let f = define_function "main" fty llm in
  let llbuilder = builder_at_end llctx (entry_block f) in

  let printf_ty = var_arg_function_type i32_t [| pointer_type i8_t |] in
  let printf = declare_function "printf" printf_ty llm in
  add_function_attr printf Attribute.Nounwind ;
  add_param_attr (param printf 0) Attribute.Nocapture ;

  let s = build_global_stringptr "Hello, world!\n" "" llbuilder in
  (* try commenting these two lines and compare the result *)
  let zero = const_int i32_t 0 in
  let s = build_in_bounds_gep s [| zero |] "" llbuilder in

  let _ = build_call printf [| s |] "" llbuilder in

  let _ = build_ret (const_int i32_t 0) llbuilder in

  Llvm_analysis.assert_valid_module llm ;
  dump_module llm ;
  ()
