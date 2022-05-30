open Import

module Private = struct
  let win32 = ref Sys.win32
end

type t = Uri_lexer.t =
  { scheme : string option
  ; authority : string
  ; path : string
  }

let t_of_yojson json = Json.Conv.string_of_yojson json |> Uri_lexer.of_string

let to_string { scheme; authority; path } =
  let b = Buffer.create 64 in
  scheme
  |> Option.iter (fun s ->
         Buffer.add_string b s;
         Buffer.add_char b ':');
  Buffer.add_string b "//";
  Buffer.add_string b authority;
  if not (String.is_prefix path ~prefix:"/") then Buffer.add_char b '/';
  Buffer.add_string b path;
  Buffer.contents b

let yojson_of_t t = `String (to_string t)

let equal = ( = )

let compare (x : t) (y : t) = Stdlib.compare x y

let hash = Hashtbl.hash

let to_dyn { scheme; authority; path } =
  let open Dyn in
  record
    [ ("scheme", (option string) scheme)
    ; ("authority", string authority)
    ; ("path", string path)
    ]

let to_path t =
  let path = t.path |> Uri.pct_decode in
  if Sys.win32 then path else Filename.concat "/" path

let of_path (path : string) =
  let path = Uri_lexer.escape_path path in
  { path; scheme = Some "file"; authority = "" }
