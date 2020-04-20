
let spf = Printf.sprintf

type dep =
  | Dep_decl of string
  | Dep_def of string

type ml_names = {decl: string; def: string option}
type node = {
  code: (string*dep list) lazy_t;
  ml_name: string;
  enum: bool;
  mutable seen: bool;
}
type t = {
  tbl_decl: (string, node) Hashtbl.t;
  tbl_def: (string, node) Hashtbl.t;
  tydefs: (string*string) list;
}

let find_ml_names self n =
  try
    let decl = (Hashtbl.find self.tbl_decl n).ml_name in
    let def =
      try Some (Hashtbl.find self.tbl_def n).ml_name with Not_found -> None in
    Some {decl; def}
  with Not_found -> None

let create ~tydefs () : t =
  { tbl_decl=Hashtbl.create 32;
    tbl_def=Hashtbl.create 32;
    tydefs;
  }

let add_decl ?(enum=false) self name ~ml_name ~code =
  Printf.eprintf "ty-g: graph.add-decl %s (enum=%B)\n%!" name enum;
  assert (not @@ Hashtbl.mem self.tbl_decl name);
  let n = {code;ml_name;seen=false;enum} in
  Hashtbl.add self.tbl_decl name n

let add_def self name ~ml_name ~code =
  Printf.eprintf "ty-g: graph.add-def %s\n%!" name;
  assert (not @@ Hashtbl.mem self.tbl_def name);
  let n = {code;ml_name; seen=false;enum=false} in
  Hashtbl.add self.tbl_def name n

let sorted self : string list =
  let out = ref [] in
  let rec traverse _name node =
   if not node.seen then (
     node.seen <- true;
     let code, deps = Lazy.force node.code in
     (* add dependencies first *)
     List.iter
       (function
         | Dep_decl n ->
           begin match Hashtbl.find self.tbl_decl n with
           | exception Not_found -> ()
           | node -> traverse n node
           end
         | Dep_def n ->
           begin match Hashtbl.find self.tbl_def n with
           | exception Not_found -> ()
           | node -> traverse n node
           end
       )
       deps;
     out := code :: !out;
   )
  in
  Hashtbl.iter traverse self.tbl_decl;
  Hashtbl.iter traverse self.tbl_def;
  List.rev !out

let to_file (self:t) f: unit =
  Printf.eprintf "ty-g: to-file %S\n%!" f;
  let oc = open_out f in
  Marshal.to_channel oc self []

let of_file f: t =
  Printf.eprintf "ty-g: of-file %S\n%!" f;
  let ic = open_in f in
  Marshal.from_channel ic

(* NOTE: handle the few cases here *)
let tr_fundef ~funptr = function
  | "const char*(*)(void* user_data)" ->
    spf "(%s @@ ptr void @-> returning string)" funptr, []
  | "bool(*)(void* data,int idx,const char** out_text)" ->
    spf "(%s @@ ptr void @-> int \
     @-> ptr string @-> returning bool)" funptr, []
  | "float(*)(void* data,int idx)" ->
    spf "(%s @@ ptr void @-> int @-> returning float)" funptr, []
  | "char*(*)(void* user_data)" ->
    spf "(%s @@ ptr void @-> returning string)"funptr, []
  | "void(*)(void* user_data,const char* text)" ->
    spf "(%s @@ ptr void @-> string @-> returning void)"funptr, []
  | "void(*)(int x,int y)" ->
    spf "(%s @@ int @-> int @-> returning void)"funptr, []
  | "void(*)(const ImDrawList* parent_list,const ImDrawCmd* cmd);" ->
    spf "(%s @@ ptr Decl_ImDrawList.t @-> ptr Decl_ImDrawCmd.t @-> \
     returning void)"funptr, [Dep_decl "ImDrawList"; Dep_decl "ImDrawCmd"]
  | "int(*)(ImGuiInputTextCallbackData *data);" ->
    spf "(%s @@ ptr Decl_ImGuiInputTextCallbackData.t @-> \
     returning int)"funptr, [Dep_decl "ImGuiInputTextCallbackData";]
  | "void*(*)(size_t sz,void* user_data)" ->
    spf "(%s @@ size_t @-> ptr void @-> \
     returning (ptr void))"funptr, []
  | "void(*)(void* ptr,void* user_data)" ->
    spf "(%s @@ ptr void @-> ptr void @-> \
     returning void)"funptr, []
  | "void(*)(ImGuiSizeCallbackData* data);" ->
    spf "(%s @@ ptr Decl_ImGuiSizeCallbackData.t @-> \
     returning void)"funptr, [Dep_decl "ImGuiSizeCallbackData"]
  | s ->
    failwith @@ spf "cannot translate function pointer type %s"
      (String.map (function '*' -> '$'|c->c)s) (* a bit of escaping *)

let def_unions = {|
module Union1 = struct
  type t = [`Union1] Ctypes.union
  let t : t typ = union "anon_union1"
  let val_i = field t "val_i" int
  let val_f = field t "val_f" float
  let val_p = field t "val_p" (ptr void)
  let () = seal t
end
|}

(* NOTE: handle the few cases here *)
let tr_union = function
  | "union { int val_i; float val_f; void* val_p;}" ->
    "Union1.t", []
  | s -> failwith @@ spf "cannot translate union %s" s

(* translate a type *)
let parse_ty
    ?(funptr="static_funptr")
    ?(ptr_top="ptr")
    ?(array_to_ptr=false)
    (self:t) s : _ * dep list =
  (* in_ptr: did we go through a pointer, nullifying the need for the def
     fdef: if true, follow typedefs *)
  let rec try_prim ~in_ptr ~fdef s =
    let s = String.trim s in
    match s with
    | "unsigned char" -> "uchar", []
    | "unsigned short" -> "ushort", []
    | "unsigned int" -> "uint", []
    | "int" -> "int", []
    | "short" -> "short", []
    | "char" -> "char", []
    | "float" -> "float", []
    | "double" -> "double", []
    | "bool" -> "bool", []
    | "const char*" -> "string", []
    | "size_t" -> "size_t", []
    | "void" -> "void", []
    | "void*" | "const void*" -> "voidp", []
    | s when s.[String.length s-1] = '*' ->
      let s = String.sub s 0 (String.length s-1) in
      let ty,  deps = expand_ty ~in_ptr:true ~fdef s in
      spf "(%s %s)" (if in_ptr then "ptr" else ptr_top) ty, deps
    | s when Str_.prefix "ImVector_" s ->
      spf "(abstract ~name:%S ~size:%d ~alignment:8 : unit abstract typ)" s (3 * 8),
      []
    | s when Str_.prefix "union" s ->
      tr_union s
    | s when s.[String.length s-1] = ']' ->
      (* convert arrays to pointers or "array" *)
      let i = String.rindex s '[' in
      let len =
        try int_of_string @@ String.sub s (i+1) (String.length s-i-2)
        with _ ->
          failwith @@ spf "cannot extract len from %s" s
      in
      if not in_ptr && array_to_ptr then (
        let ty, deps = expand_ty ~in_ptr:true ~fdef (String.sub s 0 i) in
        spf "(ptr %s (* array of len %d *))" ty len, deps
      ) else (
        let ty, deps = expand_ty ~in_ptr ~fdef (String.sub s 0 i) in
        spf "(array %d %s)" len ty, deps
      )
    | s when Str_.prefix "const " s ->
      (* drop const, ctypes ignores it *)
      let lenc = String.length "const " in
      expand_ty ~in_ptr ~fdef (String.sub s lenc (String.length s-lenc))
    | s when Str_.prefix "struct " s ->
      (* drop struct keyword *)
      let lenc = String.length "struct " in
      expand_ty ~in_ptr ~fdef (String.sub s lenc (String.length s-lenc))
    | _ -> lookup_ty ~in_ptr s
  and expand_ty ~in_ptr ~fdef s =
    if fdef then
      match List.assoc s self.tydefs with
      | s2 ->
        (* [s] is a typedef of [s2], but only expand it if it's not an enum *)
        let is_enum =
          (try (Hashtbl.find self.tbl_decl s).enum with Not_found -> false)
        in
        if is_enum then (
          (*Printf.eprintf "ty-g: do not expand typedef of enum '%s' to '%s'\n%!" s s2; *)
          try_prim ~in_ptr ~fdef:false s
        ) else (
          (*Printf.eprintf "ty-g: expand typedef of '%s' to '%s'\n%!" s s2; *)
          try_prim ~in_ptr ~fdef:false s2
        )
      | exception Not_found -> try_prim ~in_ptr ~fdef s
    else try_prim ~in_ptr ~fdef s
  and lookup_ty ~in_ptr s =
    match find_ml_names self s with
    | Some n -> n.decl, [if in_ptr then Dep_decl s else Dep_def s]
    | None when Str_.contains ~sub:"(*)" s ->
      tr_fundef ~funptr s
    | None ->
      Printf.eprintf "ty-g: cannot find name %S\n%!" s;
      failwith @@ spf "cannot translate type: %s" s
  in
  expand_ty ~fdef:true ~in_ptr:false s
