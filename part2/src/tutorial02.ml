let rec print_type llty =
  let ty = Llvm.classify_type llty in
  match ty with
  | Llvm.TypeKind.Integer  -> Printf.printf "  integer\n"
  | Llvm.TypeKind.Function -> Printf.printf "  function\n"
  | Llvm.TypeKind.Array    -> Printf.printf "  array of" ; print_type (Llvm.element_type llty)
  | Llvm.TypeKind.Pointer  -> Printf.printf "  pointer to" ; print_type (Llvm.element_type llty)
  | Llvm.TypeKind.Vector   -> Printf.printf "  vector of" ; print_type (Llvm.element_type llty)
  | _                      -> Printf.printf "  other type\n"

let print_val lv =
  Printf.printf "Value\n" ;
  Printf.printf "  name %s\n" (Llvm.value_name lv) ;
  let llty = Llvm.type_of lv in
  Printf.printf "  type %s\n" (Llvm.string_of_lltype llty) ;
  print_type llty ;
  ()

let print_fun lv =
  Llvm.iter_blocks
    (fun llbb ->
      Printf.printf "  bb: %s\n" (Llvm.value_name (Llvm.value_of_block (llbb))) ;
      Llvm.iter_instrs
        (fun lli ->
          Printf.printf "    instr: %s\n" (Llvm.string_of_llvalue lli)
        )
        llbb
    )
    lv

let _ =
  let llctx = Llvm.global_context () in
  let llmem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in
  (*Llvm.dump_module llm ;*)

  Printf.printf "*** lookup_function ***\n" ;
  let opt_lv = Llvm.lookup_function "main" llm in
  begin
  match opt_lv with
  | Some lv -> print_val lv
  | None    -> Printf.printf "'main' function not found\n"
  end ;

  Printf.printf "*** iter_functions ***\n" ;
  Llvm.iter_functions print_val llm ;

  Printf.printf "*** fold_left_functions ***\n" ;
  let count =
    Llvm.fold_left_functions
      (fun acc lv ->
        print_val lv ;
        acc + 1
      )
      0
      llm
  in
  Printf.printf "Functions count: %d\n" count ;

  Printf.printf "*** basic blocks/instructions ***\n" ;
  Llvm.iter_functions print_fun llm ;

  Printf.printf "*** iter_globals ***\n" ;
  Llvm.iter_globals print_val llm ;

  ()
