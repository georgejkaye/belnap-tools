module ParjserInt = {
    type t
    type intOptions = {
        allowSign: bool,
        base: int
    }
    @val external int: (~options:intOptions = ?) => t = "int"
}
