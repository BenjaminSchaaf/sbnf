pub fn is_identifier_char(chr: char) -> bool {
    chr.is_alphanumeric() || chr == '_' || chr == '-' || chr == '.'
}

// A fast way to convert an interval of iterators to a substring. Rust should
// at least have an easy way to get byte indices from Chars :(
pub fn str_from_iterators<'a>(
    string: &'a str,
    start: &std::str::Chars<'a>,
    end: &std::str::Chars<'a>,
) -> &'a str {
    // Convert start and end into byte offsets
    let bytes_start = string.as_bytes().len() - start.as_str().as_bytes().len();
    let bytes_end = string.as_bytes().len() - end.as_str().as_bytes().len();

    // SAFETY: As long as the iterators are from the string the byte offsets
    // will always be valid.
    unsafe {
        std::str::from_utf8_unchecked(
            &string.as_bytes()[bytes_start..bytes_end],
        )
    }
}
