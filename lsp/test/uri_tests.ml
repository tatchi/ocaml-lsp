module Uri = Lsp.Uri1

let run_with_modes f =
  print_endline "Unix:";
  Uri.Private.win32 := false;
  f ();
  print_endline "Windows:";
  Uri.Private.win32 := true;
  f ()

let%expect_test "test of_path -> to_path" =
  let test_of_path =
    let test s =
      let uri = Uri.of_path s in
      Printf.printf "%s -> %s\n" s (Uri.to_path uri)
    in
    fun uris -> run_with_modes (fun () -> List.iter test uris)
  in
  test_of_path
    [ "c:/win/path"
    ; "c:/win/path/"
    ; "C:/win/path"
    ; "/c:/win/path"
    ; "./c/win/path"
    ; "c:\\win\\path"
    ; "c:\\win/path"
    ; ""
    ; "a.file"
    ; "\\\\shäres\\path\\c#\\plugin.json"
    ; "//shares/files/c%23/p.cs"
    ; "c:\\test with %25\\path"
    ];
  [%expect
    {|
    Unix:
    c:/win/path -> c:/win/path
    c:/win/path/ -> c:/win/path/
    C:/win/path -> c:/win/path
    /c:/win/path -> c:/win/path
    ./c/win/path -> /./c/win/path
    c:\win\path -> c:\win\path
    c:\win/path -> c:\win/path
     -> /
    a.file -> /a.file
    \\shäres\path\c#\plugin.json -> /\\shäres\path\c#\plugin.json
    //shares/files/c%23/p.cs -> //shares/files/c%23/p.cs
    c:\test with %25\path -> c:\test with %25\path
    Windows:
    c:/win/path -> c:\win\path
    c:/win/path/ -> c:\win\path\
    C:/win/path -> c:\win\path
    /c:/win/path -> c:\win\path
    ./c/win/path -> \.\c\win\path
    c:\win\path -> c:\win\path
    c:\win/path -> c:\win\path
     -> \
    a.file -> \a.file
    \\shäres\path\c#\plugin.json -> \\shäres\path\c#\plugin.json
    //shares/files/c%23/p.cs -> \\shares\files\c%23\p.cs
    c:\test with %25\path -> c:\test with %25\path
    |}]

let%expect_test "test of_string -> to_path" =
  let test_of_path =
    let test s =
      let uri = Uri.of_string s in
      Printf.printf "%s -> %s\n" s (Uri.to_path uri)
    in
    fun uris -> run_with_modes (fun () -> List.iter test uris)
  in
  test_of_path
    [ "file:///c:/test/me"
    ; "file://shares/files/c%23/p.cs"
    ; "file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp"
    ; "file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins/c%23/plugin.json"
    ; "file:///c:/test %25/path"
    ; "file:?q"
    ];
  [%expect
    {|
    Unix:
    file:///c:/test/me -> c:/test/me
    file://shares/files/c%23/p.cs -> //shares/files/c#/p.cs
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> /
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins/c%23/plugin.json -> c:/Source/Zürich or Zurich (ˈzjʊərɪk,/Code/resources/app/plugins/c#/plugin.json
    file:///c:/test %25/path -> c:/test %/path
    file:?q -> /
    Windows:
    file:///c:/test/me -> c:\test\me
    file://shares/files/c%23/p.cs -> \\shares\files\c#\p.cs
    file://%2Fhome%2Fticino%2Fdesktop%2Fcpluscplus%2Ftest.cpp -> \
    file:///c:/Source/Z%C3%BCrich%20or%20Zurich%20(%CB%88zj%CA%8A%C9%99r%C9%AAk,/Code/resources/app/plugins/c%23/plugin.json -> c:\Source\Zürich or Zurich (ˈzjʊərɪk,\Code\resources\app\plugins\c#\plugin.json
    file:///c:/test %25/path -> c:\test %\path
    file:?q -> \
    |}]

let%expect_test "test of_path -> to_string" =
  let test_of_path =
    let test s =
      let uri = Uri.of_path s in
      Printf.printf "%s -> %s\n" s (Uri.to_string uri)
    in
    fun uris -> List.iter test uris
  in
  test_of_path
    [ "c:/win/path"
    ; "C:/win/path"
    ; "c:/win/path/"
    ; "/c:/win/path"
    ; "c:\\test with %\\path"
    ; "c:\\test with %25\\c#code"
    ; "/Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js"
    ];
  [%expect
    {|
    c:/win/path -> file:///c%3A/win/path
    C:/win/path -> file:///c%3A/win/path
    c:/win/path/ -> file:///c%3A/win/path/
    /c:/win/path -> file:///c%3A/win/path
    c:\test with %\path -> file:///c%3A/test%20with%20%25/path
    c:\test with %25\c#code -> file:///c%3A/test%20with%20%2525/c%23code
    /Users/jrieken/Code/_samples/18500/Mödel + Other Thîngß/model.js -> file:///Users/jrieken/Code/_samples/18500/M%C3%B6del%20%2B%20Other%20Th%C3%AEng%C3%9F/model.js
    |}]

let%expect_test "test of_path -> to_string (win-special)" =
  let test_of_path =
    let test s =
      let uri = Uri.of_path s in
      Printf.printf "%s -> %s\n" s (Uri.to_string uri)
    in
    fun uris -> run_with_modes (fun () -> List.iter test uris)
  in
  test_of_path [ "c:\\win\\path"; "c:\\win/path" ];
  [%expect
    {|
    Unix:
    c:\win\path -> file:///c%3A%5Cwin%5Cpath
    c:\win/path -> file:///c%3A%5Cwin/path
    Windows:
    c:\win\path -> file:///c%3A/win/path
    c:\win/path -> file:///c%3A/win/path

    |}]

let%expect_test "test of_string -> to_string" =
  let test_of_path =
    let test s =
      let uri = Uri.of_string s in
      Printf.printf "%s -> %s\n" s (Uri.to_string uri)
    in
    fun uris -> List.iter test uris
  in
  test_of_path [ "file://shä\\res/pröjects/c%23/" ];
  [%expect
    {|
    file://shä\res/pröjects/c%23/ -> file://sh%C3%A4%5Cres/pr%C3%B6jects/c%23/
    |}]
