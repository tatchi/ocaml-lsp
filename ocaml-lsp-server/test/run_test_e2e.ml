let () =
  let cmd =
    Filename.quote_command "yarn"
      [ "--cwd"
      ; "ocaml-lsp-server/test/e2e"
      ; "test"
      ; "workspace-symbol.test.ts"
      ]
  in
  Sys.command cmd |> exit
