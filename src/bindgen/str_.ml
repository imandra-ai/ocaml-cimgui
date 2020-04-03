
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

let contains ~sub:s1 s2 =
  let rec try_at i =
    i + String.length s1 <= String.length s2 &&
    (contains_at_ ~sub:s1 0 s2 i ~len:(String.length s1) ||
     try_at (i+1))
  in
  try_at 0
