module TextStream

/// <summary>
/// A basic cursor on top of text
/// </summary>
type T = {
    Storage: string
    Index: int
}

/// <summary>
/// Creates a new TextStream at the start of the text.
/// </summary>
let create (str: string) =
    {Storage=str; Index=0}

/// <summary>
/// Advances the character stream by the given number of characters, either
/// forward or backward.
/// </summary>
let seek (amount: int) (stream: T)  =
    {stream with Index=System.Math.Clamp(stream.Index + amount, 0, stream.Storage.Length)}

/// <summary>
/// Returns the amount of data left in the stream
/// </summary>
let remaining (stream: T) =
    stream.Storage.Length - stream.Index

/// <summary>
/// Reads a character from the stream without advancing the stream
/// </summary>
let peek (stream: T) =
    if stream.Index >= stream.Storage.Length then
        None
    else
        Some <| stream.Storage.[stream.Index]

/// <summary>
/// Consumes characters from the stream if they match the given template.
/// </summary>
let try_consume_seq (text: string) (stream: T) =
    if text.Length <= remaining stream then
        if stream.Storage.Substring(stream.Index, text.Length) = text then
            Some <| seek text.Length stream
        else
            None
    else
        None

/// <summary>
/// Consumes the next character from the stream if it's in the given set.
/// </summary>
let try_consume_alt (alt: Set<char>) (stream: T) =
    match peek stream with
    | Some ch when Set.contains ch alt ->
        Some (seek 1 stream, ch)

    | _ ->
        None
