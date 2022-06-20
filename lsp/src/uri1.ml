open Import

module Private = struct
  let win32 = ref Sys.win32
end

type t =
  { scheme : string
  ; authority : string
  ; path : string
  }

let path t = t.path

let backslash_to_slash =
  String.map ~f:(function
    | '\\' -> '/'
    | _ as c -> c)

let slash_to_backslash =
  String.map ~f:(function
    | '/' -> '\\'
    | _ as c -> c)

let of_path path =
  let path = if !Private.win32 then backslash_to_slash path else path in
  let path, authority =
    let len = String.length path in
    if len = 0 then ("/", "") (* TODO: use String.is_prefix or start_with? *)
    else if len > 1 && path.[0] = '/' && path.[1] = '/' then
      let idx = String.index_from_opt path 2 '/' in
      match idx with
      | None -> ("/", String.sub path ~pos:2 ~len:(len - 2))
      | Some i ->
        let authority = String.sub path ~pos:2 ~len:(i - 2) in
        let path =
          let path = String.sub path ~pos:i ~len:(len - i) in
          if path = "" then "/" else path
        in
        (path, authority)
    else (path, "")
  in
  let path = if path.[0] <> '/' then "/" ^ path else path in
  (* Printf.printf "scheme: %s - authority: %s - path: %s\n" "file" authority
     path; *)
  { scheme = "file"; authority; path }

let to_path { path; authority; scheme } =
  let path =
    let len = String.length path in
    if len = 0 then "/"
    else if authority <> "" && len > 1 && scheme = "file" then
      "//" ^ authority ^ path
    else if len < 3 then path
    else
      let c0 = path.[0] in
      let c1 = path.[1] in
      let c2 = path.[2] in
      if
        c0 = '/'
        && ((c1 >= 'A' && c1 <= 'Z') || (c1 >= 'a' && c1 <= 'z'))
        && c2 = ':'
      then
        (* Char.escaped ? *)
        String.make 1 (Char.lowercase_ascii c1)
        ^ String.sub path ~pos:2 ~len:(String.length path - 2)
      else path
  in
  if !Private.win32 then slash_to_backslash path else path

let of_string s =
  let re =
    Re.Perl.re "^(([^:/?#]+?):)?(\\/\\/([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
    |> Re.compile
  in
  let res = Re.exec re s in
  let group re n = Re.Group.get_opt re n |> Option.value ~default:"" in
  let scheme = group res 2 in
  let authority = group res 4 in
  let path = group res 5 |> Uri.pct_decode in
  { scheme; authority; path }

let encode ?(allow_slash = false) s =
  let allowed_chars = if allow_slash then "/" else "" in
  Uri.pct_encode ~component:(`Custom (`Generic, allowed_chars, "")) s

(* let _encode s = let buff = Buffer.create (String.length s) in String.iter
   ~f:(function (*'a' .. 'z' | 'A' .. 'Z' | '1' .. '9' | '-' | '.' | '_' | '~'
   *) | '/' as c -> Buffer.add_char buff c | c -> Buffer.add_string buff
   (Uri.pct_encode ~component:`Generic (String.make 1 c))) s; Buffer.contents
   buff *)

let to_string { scheme; authority; path } =
  let res = ref "" in

  if scheme <> "" then res := scheme ^ ":";

  if authority = "file" || scheme = "file" then res := !res ^ "//";

  (*TODO: implement full logic *)
  (if authority <> "" then
   let value = String.lowercase_ascii authority in
   res := !res ^ encode value);

  (*TODO: needed ? *)
  if path <> "" then (
    let value = ref path in
    let len = String.length path in
    (*TODO: should we use charCode instead ? *)
    (if len >= 3 && path.[0] = '/' && path.[2] = ':' then (
     let code = path.[1] in
     if code >= 'A' && code <= 'Z' then
       value :=
         "/"
         ^ (String.make 1 code |> String.lowercase_ascii)
         ^ ":"
         ^ String.sub path ~pos:3 ~len:(len - 3))
    else if len >= 2 && path.[1] = ':' then
      let code = path.[0] in
      if code >= 'A' && code <= 'Z' then
        value :=
          "/"
          ^ (String.make 1 code |> String.lowercase_ascii)
          ^ ":"
          ^ String.sub path ~pos:2 ~len:(len - 2));
    res := !res ^ encode ~allow_slash:true !value);

  !res
