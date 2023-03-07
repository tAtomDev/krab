pub fn is_char_valid_identifier(character: char) -> bool {
    !character.is_whitespace() && character.is_ascii_alphanumeric()
}
