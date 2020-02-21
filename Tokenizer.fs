module Tokenizer

/// <summary>
/// These characters always generate their own tokens regardless of context
/// </summary>
let self_tokens = Set.ofSeq ";.(),"

/// <summary>
/// These characters form one token as long as they are adjacent
/// </summary>
let operator_chars = Set.ofSeq "+-*/<>=~!|?"

/// <summary>
/// Delimiters used for quoted tokens
/// </summary>
let delimited_chars = Map.ofList [
    '"', '"'
    '\'', '\''
    '[', ']'
    '`', '`'
]

/// <summary>
/// Characters which can be used to start an identifier.
/// </summary>
let identifier_start_chars = Set.ofSeq "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

/// <summary>
/// Characters which can be used to continue an identifier.
/// </summary>
let identifier_cont_chars = Set.union identifier_start_chars (Set.ofSeq "0123456789$")

/// <summary>
/// Characters which mark the start of a parameter.
/// </summary>
let parameter_start_chars = Set.ofSeq ":@$"

/// <summary>
/// Reads a single operator from the input stream.
/// </summary>
let rec read_operator (buffer: char list) (stream: TextStream.T) =
    match TextStream.try_consume_alt operator_chars stream with
    | Some (stream, ch) ->
        read_operator (ch :: buffer) stream

    | None ->
        (stream, buffer)

/// <summary>
/// Reads a delimited token from the input stream. Assumes the opening delimiter has already been consumed.
/// </summary>
let rec read_quoted (delimiter: char) (buffer: char list) (stream: TextStream.T) =
    let double_delimiter = sprintf "%c%c" delimiter delimiter
    match TextStream.try_consume_seq double_delimiter stream with
    | Some stream ->
        read_quoted delimiter (delimiter :: delimiter :: buffer) stream

    | None ->
        match TextStream.try_consume_alt (Set.singleton delimiter) stream with
        | Some (stream, _) ->
            (stream, delimiter :: buffer)

        | None ->
            match TextStream.peek stream with
            | Some ch ->
                read_quoted delimiter (ch :: buffer) (TextStream.seek 1 stream)

            | None ->
                (stream, delimiter :: buffer)

/// <summary>
/// Reads a numeric token from the input stream.
/// </summary>
let read_number (buffer: char list) (stream: TextStream.T) =
    let rec read_digits buffer stream =
        match TextStream.try_consume_alt (Set.ofSeq "0123456789") stream with
        | Some (stream, ch) ->
            read_digits (ch :: buffer) stream

        | None ->
            (stream, buffer)

    let read_exponent buffer stream =
        match TextStream.try_consume_alt (Set.ofSeq "eE") stream with
        | Some (stream, _) ->
            let buffer = 'e' :: buffer
            let (stream, buffer) =
                match TextStream.peek stream with
                | Some ('+' as sign)
                | Some ('-' as sign) ->
                    (TextStream.seek 1 stream, sign :: buffer)

                | _ ->
                    (stream, buffer)

            read_digits buffer stream

        | None ->
            (stream, buffer)

    let (stream, buffer) = read_digits buffer stream
    let (stream, buffer) =
        match TextStream.try_consume_alt (Set.singleton '.') stream with
        | Some (stream, _) ->
            read_digits ('.' :: buffer) stream

        | None ->
            (stream, buffer)

    read_exponent buffer stream

/// <summary>
/// Reads an identifier token from the input stream.
/// </summary>
let rec read_identifier (buffer: char list) (stream: TextStream.T) =
    match TextStream.try_consume_alt identifier_cont_chars stream with
    | Some (stream, ch) ->
        read_identifier (ch :: buffer) stream

    | None ->
        (stream, buffer)

/// <summary>
/// Reads a parameter token from the input stream. Assumes the leading character
/// has been consumed.
/// </summary>
let read_parameter (buffer: char list) (stream: TextStream.T) =
    match TextStream.try_consume_alt identifier_start_chars stream with
    | Some (stream, ch) ->
        read_identifier (ch :: buffer) stream

    | None ->
        (stream, buffer)

/// <summary>
/// Reads until the end of the line, discarding the result.
/// </summary>
let rec discard_line (stream: TextStream.T) =
    match TextStream.peek stream with
    | Some '\r'
    | Some '\n' ->
        TextStream.seek 1 stream

    | Some _ ->
        discard_line (TextStream.seek 1 stream)

    | None ->
        stream

/// <summary>
/// Reads until the end of a block comment, discarding the result.
/// </summary>
let rec discard_block (stream: TextStream.T) =
    match TextStream.try_consume_seq "*/" stream with
    | Some stream ->
        stream

    | None ->
        match TextStream.peek stream with
        | Some _ ->
            discard_block (TextStream.seek 1 stream)

        | None ->
            stream

/// <summary>
/// Lexes the query, returning a list of tokens.
/// </summary>
let tokenize (stream: TextStream.T) =
    let rec step stream tokens =
        match TextStream.peek stream with
        | Some ch ->
            let after_ch = TextStream.seek 1 stream
            if System.Char.IsWhiteSpace(ch) then
                step after_ch tokens
            elif Set.contains ch self_tokens then
                step after_ch ([ch] :: tokens)
            elif System.Char.IsNumber(ch) then
                let (stream, buffer) = read_number [ch] after_ch
                step stream (List.rev buffer :: tokens)
            elif ch = '-' then
                match TextStream.try_consume_seq "--" stream with
                | Some stream ->
                    let stream = discard_line stream
                    step stream tokens
                | None ->
                    let (stream, buffer) = read_operator ['-'] after_ch
                    step stream (List.rev buffer :: tokens)
            elif ch = '/' then
                match TextStream.try_consume_seq "/*" stream with
                | Some stream ->
                    let stream = discard_block stream
                    step stream tokens
                | None ->
                    let (stream, buffer) = read_operator ['/'] after_ch
                    step stream (List.rev buffer :: tokens)
            elif Set.contains ch operator_chars then
                let (stream, buffer) = read_operator [ch] after_ch
                step stream (List.rev buffer :: tokens)
            elif Set.contains ch identifier_start_chars then
                let (stream, buffer) = read_identifier [ch] after_ch
                step stream (List.rev buffer :: tokens)
            elif Set.contains ch parameter_start_chars then
                let (stream, buffer) = read_parameter [ch] after_ch
                step stream (List.rev buffer :: tokens)
            elif Map.containsKey ch delimited_chars then
                let end_delimiter = Map.find ch delimited_chars
                let (stream, buffer) = read_quoted end_delimiter [ch] after_ch
                step stream (List.rev buffer :: tokens)
            else
                // Promote any unrecognized character to its own token
                step after_ch ([ch] :: tokens)

        | None ->
            tokens
            |> Seq.rev
            |> Seq.map (fun token -> System.String(Array.ofList token))
            |> Seq.toList

    step stream []
