open Llvm

let _ =
  let llctx = global_context () in
  let llm = create_module llctx "mymodule" in

  let i32_t = i32_type llctx in
  let fty = function_type i32_t [| |] in

  let f = define_function "main" fty llm in
  let llbuilder = builder_at_end llctx (entry_block f) in

  let _ = build_ret (const_int i32_t 0) llbuilder in

  if Array.length Sys.argv > 1
  then print_module Sys.argv.(1) llm
  else dump_module llm ;
  ()
