open Legacy_types

type tag = int
val tag : 'a program -> tag program

val untag : 'a program -> unit program

val atag : 'a aprogram -> tag aprogram

val auntag : 'a aprogram -> unit aprogram
