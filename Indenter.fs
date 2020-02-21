module Indenter

/// <summary>
/// A token that includes positioning information
/// </summary>
type OutputToken = {
    Token: string
    Line: int
    Indent: int
}

/// <summary>
/// Tokens which appear on their own lines and indent their contents
/// </summary>
let block_tokens = Set.ofList [
    "SELECT"
    "FROM"
    "FULL JOIN"
    "LEFT JOIN"
    "RIGHT JOIN"
    "FULL OUTER JOIN"
    "LEFT OUTER JOIN"
    "RIGHT OUTER JOIN"
    "CROSS JOIN"
    "UNION JOIN"
    "INNER JOIN"
    "NATURAL JOIN"
    "JOIN"
    "AS"
    "ON"
    "WHERE"
    "ORDER BY"
    "GROUP BY"
    "AND"
    "OR"
]

/// <summary>
/// Tokens which trigger an indented block but do not require a new line
/// </summary>
let group_tokens = Set.ofList [
    "("
]

/// <summary>
/// Tokens which end an indented block but do not require a new line
/// </summary>
let end_group_tokens = Set.ofList [
    ")"
]

/// <summary>
/// Tokens which trigger the end of the line without any changes to the indentation
/// </summary>
let line_tokens = Set.ofList [
    ","
]

/// <summary>
/// Tokens which trigger a new line and reset the current indentation state
/// </summary>
let reset_tokens = Set.ofList [
    ";"
]

/// <summary>
/// Functionalized version of ToUpper
/// </summary>
let upper (s: string) = s.ToUpper()

/// <summary>
/// Glues adjacent tokens into one if they match certain patterns
/// </summary>
let glue_tokens (tokens: string list) =
    let input_tokens = Array.ofList tokens
    let rec step idx tokens output =
        match tokens with
        | "FULL" :: "JOIN" :: rest ->
            step (idx + 2) rest ("FULL JOIN" :: output)

        | "LEFT" :: "JOIN" :: rest ->
            step (idx + 2) rest ("LEFT JOIN" :: output)

        | "RIGHT" :: "JOIN" :: rest ->
            step (idx + 2) rest ("RIGHT JOIN" :: output)

        | "FULL" :: "OUTER" :: "JOIN" :: rest ->
            step (idx + 3) rest ("FULL OUTER JOIN" :: output)

        | "LEFT" :: "OUTER" :: "JOIN" :: rest ->
            step (idx + 3) rest ("LEFT OUTER JOIN" :: output)

        | "RIGHT" :: "OUTER" :: "JOIN" :: rest ->
            step (idx + 3) rest ("RIGHT OUTER JOIN" :: output)

        | "CROSS" :: "JOIN" :: rest ->
            step (idx + 3) rest ("CROSS JOIN" :: output)

        | "UNION" :: "JOIN" :: rest ->
            step (idx + 3) rest ("UNION JOIN" :: output)

        | "INNER" :: "JOIN" :: rest ->
            step (idx + 2) rest ("INNER JOIN" :: output)

        | "NATURAL" :: "JOIN" :: rest ->
            step (idx + 2) rest ("NATURAL JOIN" :: output)

        | "GROUP" :: "BY" :: rest ->
            step (idx + 2) rest ("GROUP BY" :: output)

        | "ORDER" :: "BY" :: rest ->
            step (idx + 2) rest ("ORDER BY" :: output)

        | _ :: "." :: _ :: rest ->
            let domain = Array.item idx input_tokens
            let name = Array.item (idx + 2) input_tokens
            step (idx + 3) rest (sprintf "%s.%s" domain name :: output)

        | _ :: rest ->
            let token = Array.item idx input_tokens
            step (idx + 1) rest (token :: output)

        | [] ->
            List.rev output

    step 0 (List.map upper tokens) []

/// <summary>
/// Takes tokens and generates OutputTokens using formatting rules
/// </summary>
let position_tokens (tokens: string list) =
    tokens
    |> List.fold (fun (outputs, line, indent, empty_line, groups) (token: string) ->
                  let token_norm = upper token
                  if Set.contains token_norm block_tokens then
                      let out_line = if empty_line then line else line + 1
                      let output = {
                          Token=token
                          Line=out_line
                          Indent=List.head groups
                      }

                      (output :: outputs,
                       out_line + 1,
                       (List.head groups) + 1,
                       true,
                       groups)
                  elif Set.contains token_norm group_tokens then
                      let output = {
                          Token=token
                          Line=line
                          Indent=indent
                      }

                      (output :: outputs,
                       line + 1,
                       indent + 1,
                       true,
                       (indent + 1) :: groups)
                  elif Set.contains token_norm end_group_tokens then
                      let out_line = if empty_line then line else line + 1
                      let output = {
                          Token=token
                          Line=out_line
                          Indent=(List.head groups) - 1
                      }

                      (output :: outputs,
                       out_line,
                       List.head groups - 1,
                       false,
                       List.tail groups)
                  elif Set.contains token_norm line_tokens then
                      let output = {
                          Token=token
                          Line=line
                          Indent=indent
                      }

                      (output :: outputs,
                       line + 1,
                       indent,
                       true,
                       groups)
                  elif Set.contains token_norm reset_tokens then
                      let output = {
                          Token=token
                          Line=line
                          Indent=indent
                      }

                      (output :: outputs,
                       line + 2,
                       0,
                       true,
                       [0])
                  else
                      let output = {
                          Token=token
                          Line=line
                          Indent=indent
                      }
                      (output :: outputs,
                       line,
                       indent,
                       false,
                       groups))
                 ([], 1, 0, true, [0])
    |> (fun (a, _, _, _, _) -> a)
    |> List.rev

/// <summary>
/// Writes a stream of OutputToken values to the output using their positions
/// </summary>
let format_tokens (outputs: OutputToken list) =
    outputs
    |> List.fold (fun (first, line) token ->
                  let newlines = token.Line - line
                  printf "%s" (String.replicate newlines "\n")
                  let indent =
                     if newlines > 0 then
                          String.replicate token.Indent " "
                     elif not first then
                          " "
                     else
                         ""
                  printf "%s%s" indent token.Token
                  (false, token.Line))
                 (true, 1)
    |> ignore
    printfn ""
