open Core.Term

type sym_name = Common.Path.t * string * Common.Pos.pos option
val name_of_sym : sym -> sym_name

type 'a index

val empty : 'a index
val insert : 'a index -> term -> 'a -> 'a index
val search : 'a index -> term -> 'a list

module DB : sig
 type ident = sym_name
 val insert : term -> ident -> unit
 val search : term -> ident list
end 


