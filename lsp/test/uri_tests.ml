module Uri0 = Lsp.Uri

let run_with_modes f =
  print_endline "Unix:";
  Lsp.Uri.Private.win32 := false;
  f ();
  print_endline "Windows:";
  Lsp.Uri.Private.win32 := true;
  f ()

let test_uri_parsing =
  let test s =
    let uri = Uri0.t_of_yojson (`String s) in
    Printf.printf "%s -> %s\n" s (Uri0.to_path uri)
  in
  fun uris -> run_with_modes (fun () -> List.iter test uris)

let%expect_test "test uri parsing" =
  test_uri_parsing
    [ "file:///Users/foo"
    ; "file:///c:/Users/foo"
    ; "file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp"
    ];
  [%expect
    {|
    Unix:
    file:///Users/foo -> /Users/foo
    file:///c:/Users/foo -> c:/Users/foo
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> /
    Windows:
    file:///Users/foo -> Users/foo
    file:///c:/Users/foo -> c:/Users/foo 
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> /
    |}]

let uri_of_path =
  let test path =
    let uri = Uri0.of_path path in
    Printf.printf "%s -> %s\n" path (Uri0.to_string uri)
  in
  fun uris -> run_with_modes (fun () -> List.iter test uris)

let%expect_test "uri of path" =
  uri_of_path [ "/foo/bar.ml"; "foo/bar.mli" ];
  [%expect
    {|
    Unix:
    /foo/bar.ml -> file:///foo/bar.ml
    foo/bar.mli -> file:///foo/bar.mli
    Windows:
    /foo/bar.ml -> file:///foo/bar.ml
    foo/bar.mli -> file:///foo/bar.mli |}]

let uri_of_path =
  let test path =
    let uri = Uri0.of_path path in
    Printf.printf "%s -> %s\n" path (Uri0.to_string uri)
  in
  fun uris -> List.iter test uris

let%expect_test "of_path -> to_path" =
  let paths =
    [ "c:/win/path"
    ; "c:/win/path/"
    ; "C:/win/path"
    ; "/c:/win/path"
    ; "./c/win/path"
    ; "/coding/c#/MYproject1"
    ; "file://path/to/file"
    ; "C:/win/path%3a"
    ]
  in
  let of_path_to_path =
    let test path =
      let uri = Uri0.of_path path in
      Printf.printf "%s -> %s\n" path (Uri0.to_path uri)
    in
    fun uris -> List.iter test uris
  in
  of_path_to_path paths;
  [%expect
    {|
    c:/win/path -> c:/win/path
    c:/win/path/ -> c:/win/path/
    C:/win/path -> c:/win/path
    /c:/win/path -> c:/win/path
    ./c/win/path -> /./c/win/path
    /coding/c#/MYproject1 -> /coding/c#/MYproject1
    file://path/to/file -> /file://path/to/file
    C:/win/path%3a -> c:/win/path%3a
  |}]

let%expect_test "of_path -> to_string" =
  let paths =
    [ "c:/win/path"
    ; "C:/win/path"
    ; "c:/win/path/"
    ; "/c:/win/path"
    ; "c:\\win\\path"
    ; "c:\\win/path"
    ; "a.file"
    ; "/Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js"
    ]
  in
  let of_path_to_path =
    let test path =
      let uri = Uri0.of_path path in
      Printf.printf "%s -> %s\n" path (Uri0.to_string uri)
    in
    fun uris -> List.iter test uris
  in
  of_path_to_path paths;
  [%expect
    {|
    c:/win/path -> file:///c%3A/win/path
    C:/win/path -> file:///c%3A/win/path
    c:/win/path/ -> file:///c%3A/win/path/
    /c:/win/path -> file:///c%3A/win/path
    c:\win\path -> file:///c%3A%5Cwin%5Cpath
    c:\win/path -> file:///c%3A%5Cwin/path
    a.file -> file:///a.file
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> file:///Users/jrieken/Code/_samples/18500/M%C3%B6del%20%2B%20Other%20Th%C3%AEng%C3%9F/model.js
  |}]

let%expect_test "of_string -> to_string" =
  let paths = [ "file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp" ] in
  let of_path_to_path =
    let test s =
      let uri = Uri0.t_of_yojson (`String s) in
      Printf.printf "%s -> %s\n" s (Uri0.to_string uri)
    in
    fun uris -> List.iter test uris
  in
  of_path_to_path paths;
  [%expect
    {|
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp/
  |}]
