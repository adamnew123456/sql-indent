[<EntryPoint>]
let main _ =
    System.Console.In.ReadToEnd()
    |> TextStream.create
    |> Tokenizer.tokenize
    |> Indenter.glue_tokens
    |> Indenter.position_tokens
    |> Indenter.format_tokens

    0
