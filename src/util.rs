pub fn can_lexer_skip(character: char) -> bool {
    character.is_whitespace() || !character.is_ascii_alphanumeric()
}
