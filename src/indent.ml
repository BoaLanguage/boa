open Parser

type indent_state = {
    mutable indent_levels: int list;
    mutable temp_tokens: token list;
}