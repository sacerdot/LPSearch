open Core.Term

type sym_name = Common.Path.t * string
val name_of_sym : sym -> sym_name

type 'a index

val empty : 'a index
val insert : 'a index -> term -> 'a -> 'a index
val search : 'a index -> term -> 'a list

module DB : sig
 type item = sym_name * Common.Pos.pos option
 val insert : term -> item -> unit
 val search : term -> item list
end 
