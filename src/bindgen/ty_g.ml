module Str_ = struct
  let lsplit_on_char c s =
    try
      (* find `c`*)
      let i = String.index s c in
      String.sub s 0 i
    with _ -> s

  let rsplit_on_char c s =
    try
      (* find `c` that is not trailing *)
      let i = String.rindex_from s (String.length s-2) c in
      String.sub s (i+1) (String.length s-i-1)
    with _ -> s

  let prefix s1 s2 =
    String.length s2 >= String.length s1 &&
    try for i=0 to String.length s1-1 do
        if s1.[i] <> s2.[i] then raise Exit;
      done; true
    with Exit -> false

  let contains_at_ ~sub i s j ~len =
    let rec check k =
      if k = len
      then true
      else sub.[i+k] = s.[j+k] && check (k+1)
    in
    j+len <= String.length s && check 0

  let contains s1 s2 =
    let rec try_at i =
      i + String.length s1 <= String.length s2 &&
      (contains_at_ ~sub:s1 0 s2 i ~len:(String.length s1) ||
       try_at (i+1))
    in
    try_at 0
end

let spf = Printf.sprintf

type dep =
  | Dep_decl of string
  | Dep_def of string

type ml_names = {decl: string; def: string option}
type node = {
  code: (string*dep list) lazy_t;
  ml_name: string;
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

let add_decl self name ~ml_name ~code =
  Printf.eprintf "graph.add-decl %s\n%!" name;
  assert (not @@ Hashtbl.mem self.tbl_decl name);
  let n = {code;ml_name; seen=false} in
  Hashtbl.add self.tbl_decl name n

let add_def self name ~ml_name ~code =
  Printf.eprintf "graph.add-def %s\n%!" name;
  assert (not @@ Hashtbl.mem self.tbl_def name);
  let n = {code;ml_name; seen=false} in
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
  let oc = open_out f in
  Marshal.to_channel oc self []

let of_file f: t=
  let ic = open_in f in
  Marshal.from_channel ic

(* translate a type *)
let parse_ty (self:t) s : _ * dep list =
  (* in_ptr: did we go through a pointer, nullifying the need for the def
     fdef: if true, follow typedefs *)
  let rec try_prim ~in_ptr ~fdef s =
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
      spf "(ptr %s)" ty, deps
    | s when Str_.prefix "ImVector_" s ->
      (* TODO: type annotation *)
      spf "(abstract ~name:%S ~size:%d ~alignment:8 : unit abstract typ)" s (3 * 8),
      []
    | s when s.[String.length s-1] = ']' ->
      let i = String.rindex s '[' in
      let len =
        try int_of_string @@ String.sub s (i+1) (String.length s-i-2)
        with _ ->
          failwith @@ spf "cannot extract len from %s" s
      in
      let ty, deps = expand_ty ~in_ptr ~fdef (String.sub s 0 i) in
      spf "(array %d %s)" len ty, deps
    | s when Str_.prefix "const " s ->
      (* drop const *)
      let lenc = String.length "const " in
      expand_ty ~in_ptr ~fdef (String.sub s lenc (String.length s-lenc))
    | s when Str_.prefix "struct " s ->
      (* drop struct *)
      let lenc = String.length "struct " in
      expand_ty ~in_ptr ~fdef (String.sub s lenc (String.length s-lenc))
    | _ -> lookup_ty ~in_ptr s
  and expand_ty ~in_ptr ~fdef s =
    if fdef then
      match List.assoc s self.tydefs with
      | s2 -> try_prim ~in_ptr ~fdef:false s2
      | exception Not_found -> try_prim ~in_ptr ~fdef s
    else try_prim ~in_ptr ~fdef s
  and lookup_ty ~in_ptr s =
    match find_ml_names self s with
    | Some n -> n.decl, [if in_ptr then Dep_decl s else Dep_def s]
    | None ->
      Printf.eprintf "cannot find name %S" s;
      let ty =
        if Str_.contains "(*)" s then "<fun ptr type>" else s in
      failwith @@ spf "cannot translate type: %s" ty
  in
  expand_ty ~fdef:true ~in_ptr:false s
